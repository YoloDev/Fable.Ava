# Fable GitHub Client

[![Greenkeeper badge](https://badges.greenkeeper.io/YoloDev/Fable.Ava.svg)](https://greenkeeper.io/)

## Using
Currently there is no packages available for `Fable.Ava`, I'm waiting for some fixes to a dependent project first.
However, the fixes I'm waiting on has to do with building and versioning, and has nothing to do with the library itself,
so if you would like to try it out please feel free to do so :).

To try out `Fable.Ava` add the following to your `paket.dependencies`:

```
github YoloDev/Fable.Ava src/Fable.Ava/bindings.fs
github YoloDev/Fable.Ava src/Fable.Ava/assert.fs
github YoloDev/Fable.Ava src/Fable.Ava/spec.fs
```

Then add this to any `paket.references` where you want to import the files:

```
File: bindings.fs ava
File: assert.fs ava
File: spec.fs ava
```
