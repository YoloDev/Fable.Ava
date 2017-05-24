module Fable.Import.Ava

open Fable.Core
open Fable.Import.JS

[<Sealed; Erase>]
type TestContext internal () =
  [<Emit("$0.plan($1)")>]
  member x.Plan (n: int): unit = jsNative
  [<Emit("$0.pass({{$1}})")>]
  member x.Pass (?msg: string): unit = jsNative
  [<Emit("$0.fail({{$1}})")>]
  member x.Fail (?msg: string): unit = jsNative
  [<Emit("$0.truthy($1,{{$2}})")>]
  member x.Truthy (value: obj, ?msg: string): unit = jsNative
  [<Emit("$0.falsy($1,{{$2}})")>]
  member x.Falsy (value: obj, ?msg: string): unit = jsNative
  [<Emit("$0.true($1,{{$2}})")>]
  member x.True (value: bool, ?msg: string): unit = jsNative
  [<Emit("$0.false($1,{{$2}})")>]
  member x.False (value: bool, ?msg: string): unit = jsNative
  [<Emit("$0.is($1,$2,{{$3}})")>]
  member x.Is (value: 'a, expected: 'a, ?msg: string): unit = jsNative
  [<Emit("$0.not($1,$2,{{$3}})")>]
  member x.Not (value: 'a, expected: 'a, ?msg: string): unit = jsNative
  [<Emit("$0.deepEqual($1,$2,{{$3}})")>]
  member x.DeepEqual (value: 'a, expected: 'a, ?msg: string): unit = jsNative
  [<Emit("$0.notDeepEqual($1,$2,{{$3}})")>]
  member x.NotDeepEqual (value: 'a, expected: 'a, ?msg: string): unit = jsNative
  [<Emit("$0.throws($1,null,{{$2}})")>]
  member x.Throws (fn: (unit -> unit), ?msg: string): unit = jsNative
  [<Emit("$0.throws($1,null,{{$2}})")>]
  member x.Throws (promise: Promise<unit>, ?msg: string): Promise<unit> = jsNative
  [<Emit("$0.notThrows($1,{{$2}})")>]
  member x.NotThrows (fn: (unit -> unit), ?msg: string): unit = jsNative
  [<Emit("$0.notThrows($1,{{$2}})")>]
  member x.NotThrows (promise: Promise<unit>, ?msg: string): Promise<unit> = jsNative
  // TODO: Bug weird bug with regex
  //[<Emit("$0.regex($1,$2,{{$3}})")>]
  //member x.Regex (value: string, test: System.Text.RegularExpressions.Regex, ?msg: string): unit = jsNative
  [<Emit("$0.ifError($1,{{$2}})")>]
  member x.IfError (error: obj, ?msg: string): unit = jsNative
  [<Emit("$0.snapshot($1,{{$2}})")>]
  member x.Snapshot (error: obj, ?msg: string): unit = jsNative

type TestImpl = TestContext -> Promise<unit>

//[<Import("default", from="ava")>]
module Test =
  [<Import("default", from="ava"); Emit("$0($1)")>]
  let create' (impl: TestImpl): unit = jsNative

  [<Import("default", from="ava"); Emit("$0($1,$2)")>]
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