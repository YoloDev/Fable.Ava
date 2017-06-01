const fs = require('fs-extra');
const path = require('path');
const glob = require('glob');
const babel = require('babel-core');
const client = require('fable-utils/client');
const babelPlugins = require('fable-utils/babel-plugins');
const istanbul = require('babel-plugin-istanbul').default;
const sourcemap = require('convert-source-map');
const chokidar = require('chokidar');
const rimraf = require('rimraf');
const pkg = require('./package.json');

const instrument = Boolean(process.env.INSTRUMENT_CODE);

const replaceExt = (file, ext) => 
  path.join(path.dirname(file), path.basename(file, path.extname(file))) + ext;

const fileGlob = (pattern, dir) => new Promise((resolve, reject) => {
  glob(pattern, { cwd: dir, nodir: true, absolute: true }, (err, files) => {
    if (err) {
      reject(err);
      return;
    }

    resolve(files);
  });
});

const rimrafP = (dir) => new Promise((resolve, reject) => rimraf(dir, err => {
  if (err) {
    reject(err);
    return;
  }

  resolve();
}));

const babelConfig = ({ isSrc = false, relSource, filename } = {}) => ({
  filename,
  sourceMaps: true,
  sourceFileName: relSource,
  plugins: [
    babelPlugins.getRemoveUnneededNulls(),
    babelPlugins.getTransformMacroExpressions(babel.template),
    ['import-rename', {
      '^(.*)\\.fsx?$': '$1'
    }],
    ...(isSrc && instrument ? [istanbul] : [])
  ],
  presets: [
    ['env', {
      'targets': {
        'node': 'current'
      }
    }]
  ]
});

const main = async (argv) => {
  console.log(`Instrument code: ${Boolean(process.env.INSTRUMENT_CODE)}`);
  const port = process.env.FABLE_SERVER_PORT;
  if (!port) {
    console.log(`fable port not set`);
    return 1;
  }

  let errors = 0;
  const outDir = path.join(path.resolve(__dirname), 'bin', 'js');
  await rimrafP(outDir);
  const projects = await fileGlob('**/*.fsproj', __dirname);
  const srcDir = path.normalize(path.join(__dirname, 'src')).toLowerCase() + path.sep;
  const testDir = path.normalize(path.join(__dirname, 'test')).toLowerCase() + path.sep;
  const compiled = [];

  for (const projFile of projects) {
    console.log(`Load ${projFile} into server.`);
    const msg = {
      path: projFile
    };

    await client.send(port, JSON.stringify(msg));
    
    const projDir = path.dirname(projFile);
    const isTest = path.normalize(projFile).toLowerCase().startsWith(testDir);
    const isSrc = path.normalize(projFile).toLowerCase().startsWith(srcDir);
    const files = await fileGlob('**/*.fs', projDir);

    for (const fsFile of files) {
      console.log(`Compile ${fsFile} (${JSON.stringify({isTest, isSrc})})`);
      const msg = { path: fsFile };
      const data = JSON.parse(await client.send(port, JSON.stringify(msg)));
      const { error = null, logs = {} } = data;

      if (error) throw new Error(error);
      if (logs.error) {
        for (const error of logs.error) {
          console.log(error);
        }
        errors++;
      }

      // TODO: Log warnings maybe?
      
      const fsCode = await fs.readFile(fsFile, 'utf-8');
      try {
        const relPath = path.relative(__dirname, fsFile);
        const config = babelConfig({ isSrc, isTest, filename: relPath });
        const transformed = babel.transformFromAst(data, fsCode, config);

        const outFile = path.join(outDir, replaceExt(relPath, '.js'));
        const outFileDir = path.dirname(outFile);
        if (transformed.map) {
          const relSrcPath = path.relative(outFileDir, fsFile);
          const map = sourcemap.fromObject(transformed.map).setProperty('sources', [relSrcPath]);
          transformed.code += '\n\n' + map.toComment() + '\n';

          // TODO: For debugging purposes
          //await fs.mkdirp(outFileDir);
          //await fs.writeFile(outFile + '.map', map.toJSON(2), { encoding: 'utf-8' });
        }

        compiled.push({ config, fsFile, projFile, isTest, isSrc, outFile, outFileDir });
        await fs.mkdirp(outFileDir);
        await fs.writeFile(outFile, transformed.code, { encoding: 'utf-8' });
      } catch (e) {
        errors++;
        console.error(e.stack || e.toString());
        const relPath = path.relative(__dirname, fsFile);
        const outFile = path.join(outDir, replaceExt(relPath, '.ast.json'));
        const outFileDir = path.dirname(outFile);
        await fs.mkdirp(outFileDir);
        await fs.writeFile(outFile, JSON.stringify(data, null, 2), { encoding: 'utf-8' });
      }
    }
  }

  // Copy fable core cause it's stupid
  const fableCorePath = path.join(__dirname, 'packages', 'Fable.Core', 'fable-core');
  const fableCoreRunPath = path.join(__dirname, 'bin', 'js', 'packages', 'Fable.Core', 'fable-core');
  await fs.mkdirp(fableCoreRunPath);

  const coreFiles = await fileGlob('**/*.js', fableCorePath);
  for (const file of coreFiles) {
    const result = await new Promise((resolve, reject) => {
      babel.transformFile(file, babelConfig(), (err, result) => {
        if (err) {
          reject(err);
          return;
        }

        resolve(result);
      });
    });

    const relPath = path.relative(fableCorePath, file);
    const newPath = path.join(fableCoreRunPath, relPath);
    const newDir = path.dirname(newPath);
    await fs.mkdirp(newDir);
    await fs.writeFile(newPath, result.code, { encoding: 'utf-8' });
  }

  if (errors > 0) {
    return errors;
  }

  if (argv.length === 3 && argv[2] === 'watch') {
    console.log(`Watching ${compiled.length} files for changes`);
    const watcher = chokidar.watch(compiled.map(c => c.fsFile), { persistent: true, ignoreInitial: true });
    watcher
      .on('add', p => console.log(`File ${p} has been added`))
      .on('change', p => console.log(`File ${p} has been changed`))
      .on('unlink', p => console.log(`File ${p} has been removed`))
      .on('ready', p => console.log('Watcher ready, watching:', watcher.getWatched()));

    watcher.on('change', async file => {
      const record = compiled.find(c => c.fsFile === file);
      if (!record) return;

      const { fsFile, projFile, config, isTest, isSrc, outFile, outFileDir } = record;
      console.log(`Start recompile ${fsFile}`);
      console.log(`Load ${projFile} into server.`);

      const msg = { path: projFile };
      await client.send(port, JSON.stringify(msg));

      console.log(`Compile ${fsFile} (${JSON.stringify({isTest, isSrc})})`);
      const msg2 = { path: fsFile };
      const data = JSON.parse(await client.send(port, JSON.stringify(msg)));
      const { error = null, logs = {} } = data;

      if (error) return;
      if (logs.error) {
        for (const error of logs.error) {
          console.log(error);
        }
        return;
      }

      const fsCode = await fs.readFile(fsFile, 'utf-8');
      try {
        const transformed = babel.transformFromAst(data, fsCode, config);
        if (transformed.map) {
          const relSrcPath = path.relative(outFileDir, fsFile);
          const map = sourcemap.fromObject(transformed.map).setProperty('sources', [relSrcPath]);
          transformed.code += '\n\n' + map.toComment() + '\n';
        }

        await fs.writeFile(outFile, transformed.code, { encoding: 'utf-8' });
      } catch (e) {
        console.error(e.stack || e.toString());
        return;
      }
    });
    
    await new Promise(r => {});
  }
};

main(process.argv).then(process.exit).catch(e => {
  console.log(`ERR: ${e.toString()}`);
  process.exit(1);
});