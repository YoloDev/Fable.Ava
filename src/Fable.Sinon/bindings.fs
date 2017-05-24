module Fable.Import.Sinon

open Fable.Core
open Fable.Import.JS

[<Sealed; Erase>]
type SpyCall internal () =
  [<Emit("$0.calledOn($1)")>]
  member call.CalledOn (obj: obj): bool = jsNative

  [<Emit("$0.calledWith(...$1)")>]
  member call.CalledWith (args: obj list): bool = jsNative

  [<Emit("$0.calledWithExactly(...$1)")>]
  member call.CalledWithExactly (args: obj list): bool = jsNative

  [<Emit("$0.notCalledWith(...$1)")>]
  member call.NotCalledWith (args: obj list): bool = jsNative

  [<Emit("$0.threw()")>]
  member call.Threw (): bool = jsNative

  [<Emit("$0.threw($1)")>]
  member call.Threw (typeName: string): bool = jsNative

  [<Emit("$0.threw($1)")>]
  member call.Threw (error: obj): bool = jsNative

  [<Emit("$0.thisValue")>]
  member call.ThisValue: obj = jsNative

  [<Emit("$0.args")>]
  member call.Args: obj list = jsNative

  [<Emit("$0.exception")>]
  member call.Exception: exn option = jsNative

  [<Emit("$0.returnValue")>]
  member call.ReturnValue: obj = jsNative

[<Erase>]
type Spy internal () =
  [<Emit("$0.callCount")>]
  member spy.CallCount: int = jsNative

  [<Emit("$0.called")>]
  member spy.Called: bool = jsNative

  [<Emit("$0.calledBefore($1)")>]
  member spy.CalledBefore (other: Spy): bool = jsNative

  [<Emit("$0.calledAfter($1)")>]
  member spy.CalledAfter (other: Spy): bool = jsNative

  [<Emit("$0.calledImmediatelyBefore($1)")>]
  member spy.CalledImmediatelyBefore (other: Spy): bool = jsNative

  [<Emit("$0.calledImmediatelyAfter($1)")>]
  member spy.CalledImmediatelyAfter (other: Spy): bool = jsNative

  [<Emit("$0.getCall($1)")>]
  member spy.GetCall (index: int): SpyCall = jsNative

  [<Emit("$0.reset()")>]
  member spy.Reset (): unit = jsNative

[<Sealed; Erase>]
type Spy1<'a, 'b> internal () =
  inherit Spy ()

  [<Emit("$0($1)")>]
  member spy.Apply (a: 'a): 'b = jsNative

[<Sealed; Erase>]
type Spy2<'a, 'b, 'c> internal () =
  inherit Spy ()

  [<Emit("$0($1,$2)")>]
  member spy.Apply (a: 'a) (b: 'b): 'c = jsNative

[<Sealed; Erase>]
type Spy3<'a, 'b, 'c, 'd> internal () =
  inherit Spy ()

  [<Emit("$0($1,$2,$3)")>]
  member spy.Apply (a: 'a) (b: 'b) (c: 'c): 'd = jsNative

[<Erase>]
type Stub internal () =
  inherit Spy ()

  [<Emit("$0.resetBehavior()")>]
  member stub.ResetBehavior (): unit = jsNative

  [<Emit("$0.resetHistory()")>]
  member stub.ResetHistory (): unit = jsNative

[<Sealed; Erase>]
type Stub1<'a, 'b> internal () =
  inherit Stub ()

  [<Emit("$0($1)")>]
  member stub.Apply (a: 'a): 'b = jsNative

  [<Emit("$0.returns($1)")>]
  member stub.Returns (ret: 'b): unit = jsNative

[<Sealed; Erase>]
type Stub2<'a, 'b, 'c> internal () =
  inherit Stub ()

  [<Emit("$0($1,$2)")>]
  member stub.Apply (a: 'a) (b: 'b): 'c = jsNative

  [<Emit("$0.returns($1)")>]
  member stub.Returns (ret: 'c): unit = jsNative

[<Sealed; Erase>]
type Stub3<'a, 'b, 'c, 'd> internal () =
  inherit Stub ()

  [<Emit("$0($1,$2,$3)")>]
  member stub.Apply (a: 'a) (b: 'b) (c: 'c): 'd = jsNative

  [<Emit("$0.returns($1)")>]
  member stub.Returns (ret: 'd): unit = jsNative

[<RequireQualifiedAccess>]
module Sinon =
  [<Import("default", from="sinon"); Emit("$0.spy()")>]
  let spy<'a, 'b> : Spy1<'a, 'b> = jsNative

  [<Import("default", from="sinon"); Emit("$0.spy()")>]
  let spy2<'a, 'b, 'c> : Spy2<'a, 'b, 'c> = jsNative

  [<Import("default", from="sinon"); Emit("$0.spy()")>]
  let spy3<'a, 'b, 'c, 'd> : Spy3<'a, 'b, 'c, 'd> = jsNative

  [<Import("default", from="sinon"); Emit("$0.stub()")>]
  let stub<'a, 'b> : Stub1<'a, 'b> = jsNative

  [<Import("default", from="sinon"); Emit("$0.stub()")>]
  let stub2<'a, 'b, 'c> : Stub2<'a, 'b, 'c> = jsNative

  [<Import("default", from="sinon"); Emit("$0.stub()")>]
  let stub3<'a, 'b, 'c, 'd> : Stub3<'a, 'b, 'c, 'd> = jsNative
