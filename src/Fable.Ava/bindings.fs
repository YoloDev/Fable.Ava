module Fable.Import.Ava

open Fable.Core
open Fable.Import.JS

type IAsserter =
  [<Emit("$0.pass({{$1}})")>]
  abstract Pass: ?msg: string -> unit
  [<Emit("$0.fail({{$1}})")>]
  abstract Fail: ?msg: string -> unit
  [<Emit("$0.truthy($1,{{$2}})")>]
  abstract Truthy: value: 'a * ?msg: string -> unit
  [<Emit("$0.falsy($1,{{$2}})")>]
  abstract Falsy: value: 'a * ?msg: string -> unit
  [<Emit("$0.true($1,{{$2}})")>]
  abstract True: value: bool * ?msg: string -> unit
  [<Emit("$0.false($1,{{$2}})")>]
  abstract False: value: bool * ?msg: string -> unit
  [<Emit("$0.is($1,$2,{{$3}})")>]
  abstract Is: value: 'a * expected: 'a * ?msg: string -> unit
  [<Emit("$0.not($1,$2,{{$3}})")>]
  abstract Not: value: 'a * expected: 'a * ?msg: string -> unit
  [<Emit("$0.deepEqual($1,$2,{{$3}})")>]
  abstract DeepEqual: value: 'a * expected: 'a * ?msg: string -> unit
  [<Emit("$0.notDeepEqual($1,$2,{{$3}})")>]
  abstract NotDeepEqual: value: 'a * expected: 'a * ?msg: string -> unit
  [<Emit("$0.throws($1,null,{{$2}})")>]
  abstract Throws: fn: (unit -> unit) * ?msg: string -> unit
  [<Emit("$0.throws($1,null,{{$2}})")>]
  abstract Throws: fn: (unit -> Promise<unit>) * ?msg: string -> unit
  [<Emit("$0.notthrows($1,{{$2}})")>]
  abstract NotThrows: fn: (unit -> unit) * ?msg: string -> unit
  [<Emit("$0.notthrows($1,{{$2}})")>]
  abstract NotThrows: fn: (unit -> Promise<unit>) * ?msg: string -> unit
  [<Emit("$0.regex($1,$2,{{$3}})")>]
  abstract Regex: value: string * test: RegExp * ?msg: string -> unit
  [<Emit("$0.ifError($1,{{$2}})")>]
  abstract IfError: error: obj * ?msg: string -> unit
  [<Emit("$0.snapshot($1,{{$2}})")>]
  abstract Snapshot: error: obj * ?msg: string -> unit

type ITestContext =
  inherit IAsserter

type TestImpl = ITestContext -> Promise<unit>

[<Import("default", from="ava")>]
module Test =
  [<Emit("$0($1)")>]
  let create' (impl: TestImpl): unit = jsNative

  [<Emit("$0($1,$2)")>]
  let create (name: string) (impl: TestImpl): unit = jsNative

  [<Emit("$0.serial($1)")>]
  let serial' (impl: TestImpl): unit = jsNative

  [<Emit("$0.serial($1,$2)")>]
  let serial (name: string) (impl: TestImpl): unit = jsNative

  [<Emit("$0.only($1)")>]
  let only' (impl: TestImpl): unit = jsNative

  [<Emit("$0.only($1,$2)")>]
  let only (name: string) (impl: TestImpl): unit = jsNative

  [<Emit("$0.skip($1)")>]
  let skip' (impl: TestImpl): unit = jsNative

  [<Emit("$0.skip($1,$2)")>]
  let skip (name: string) (impl: TestImpl): unit = jsNative

  [<Emit("$0.todo($1)")>]
  let todo (name: string): unit = jsNative

  [<Emit("$0.failing($1)")>]
  let failing' (impl: TestImpl): unit = jsNative

  [<Emit("$0.failing($1,$2)")>]
  let failing (name: string) (impl: TestImpl): unit = jsNative

  [<Emit("$0.before($1)")>]
  let before' (impl: TestImpl): unit = jsNative

  [<Emit("$0.before($1,$2)")>]
  let before (name: string) (impl: TestImpl): unit = jsNative
  
  [<Emit("$0.after($1)")>]
  let after' (impl: TestImpl): unit = jsNative

  [<Emit("$0.after($1,$2)")>]
  let after (name: string) (impl: TestImpl): unit = jsNative

  [<Emit("$0.beforeEach($1)")>]
  let beforeEach' (impl: TestImpl): unit = jsNative

  [<Emit("$0.beforeEach($1,$2)")>]
  let beforeEach (name: string) (impl: TestImpl): unit = jsNative
  
  [<Emit("$0.afterEach($1)")>]
  let afterEach' (impl: TestImpl): unit = jsNative

  [<Emit("$0.afterEach($1,$2)")>]
  let afterEach (name: string) (impl: TestImpl): unit = jsNative