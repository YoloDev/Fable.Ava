const fs = require('fs-extra');
const path = require('path');
const glob = require('glob');
const babel = require('babel-core');
const client = require('fable-utils/client');
const babelPlugins = require('fable-utils/babel-plugins');
const istanbul = require('babel-plugin-istanbul').default;
const pkg = require('./package.json');

const instrument = process.env.INSTRUMENT_CODE;

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
  const port = process.env.FABLE_SERVER_PORT;
  if (!port) {
    console.log(`fable port not set`);
    return 1;
  }

  let errors = 0;
  const outDir = path.join(path.resolve(__dirname), 'bin', 'js');
  const projects = await fileGlob('**/*.fsproj', __dirname);
  for (const projFile of projects) {
    console.log(`Load ${projFile} into server.`);
    const msg = {
      path: projFile
    };

    await client.send(port, JSON.stringify(msg));
    
    const projDir = path.dirname(projFile);
    const isTest = projFile.startsWith(path.join(__dirname, 'test'));
    const isSrc = projFile.startsWith(path.join(__dirname, 'src'));
    const files = await fileGlob('**/*.fs', projDir);

    for (const fsFile of files) {
      console.log(`Compile ${fsFile}`);
      const msg = { path: fsFile };
      const data = JSON.parse(await client.send(port, JSON.stringify(msg)));
      const { error = null, logs = {} } = data;

      if (error) throw new Error(error);
      if (logs.error) {
        for (const error of logs.error) {
          console.log(error);
        }
      }

      // TODO: Log warnings maybe?
      
      const fsCode = await fs.readFile(fsFile, 'utf-8');
      try {
        const relPath = path.relative(__dirname, fsFile);
        const transformed = babel.transformFromAst(data, fsCode, babelConfig({ isSrc, isTest, filename: relPath }));

        const outFile = path.join(outDir, replaceExt(relPath, '.js'));
        const outFileDir = path.dirname(outFile);
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

  return errors;
};

main(process.argv).then(process.exit).catch(e => {
  console.log(`ERR: ${e.toString()}`);
  process.exit(1);
});