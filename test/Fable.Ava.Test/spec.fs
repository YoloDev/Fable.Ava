module Test

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import.Sinon
open Fable.Import.Ava
open Fable.Ava

let getCall (spy: Spy) (name: string) = spec {
  do! Assert.isDeepEqual spy.CallCount 1 <?> sprintf "%s should be called once" name
  return spy.GetCall 0
}

let checkCall (spy: Spy) (name: string) (args: obj list) = spec {
  let! call = getCall spy name
  do! Assert.isTrue (call.CalledWithExactly args) <?> sprintf "%s should be called with %A" name args
}

// TODO: Create test mock so we can inspect actual assert calls
type AsserterMock () =
  let passSpy = Sinon.spy
  let failSpy = Sinon.spy
  let truthySpy = Sinon.spy2
  let falsySpy = Sinon.spy2
  let trueSpy = Sinon.spy2
  let falseSpy = Sinon.spy2
  let isSpy = Sinon.spy3
  let notSpy = Sinon.spy3
  let deepEqualSpy = Sinon.spy3
  let notDeepEqualSpy = Sinon.spy3
  let throwsSpy = Sinon.spy2
  let notThrowsSpy = Sinon.spy2
  let throwsAsyncSpy = 
    let stub = Sinon.stub2
    stub.Returns <| async.Zero ()
    stub
  let notThrowsAsyncSpy = 
    let stub = Sinon.stub2
    stub.Returns <| async.Zero ()
    stub
  let regexSpy = Sinon.spy3
  let ifErrorSpy = Sinon.spy2
  let snapshotSpy = Sinon.spy2

  let asserter = {
    pass = passSpy.Apply
    fail = failSpy.Apply
    isTruthy = truthySpy.Apply
    isFalsy = falsySpy.Apply
    isTrue = trueSpy.Apply
    isFalse = falseSpy.Apply
    isSame = isSpy.Apply
    isNotSame = notSpy.Apply
    isDeepEqual = deepEqualSpy.Apply
    isNotDeepEqual = notDeepEqualSpy.Apply
    throws = throwsSpy.Apply
    doesNotThrow = notThrowsSpy.Apply
    asyncThrows = throwsAsyncSpy.Apply
    doesNotAsyncThrow = notThrowsAsyncSpy.Apply
    //matches = regexSpy.Apply
    ifError = ifErrorSpy.Apply
    snapshot = snapshotSpy.Apply
  }

  member mock.Asserter = asserter
  member private x._passSpy = passSpy
  member private x._failSpy = failSpy
  member private x._truthySpy = truthySpy
  member private x._falsySpy = falsySpy
  member private x._trueSpy = trueSpy
  member private x._falseSpy = falseSpy
  member private x._isSpy = isSpy
  member private x._notSpy = notSpy
  member private x._deepEqualSpy = deepEqualSpy
  member private x._notDeepEqualSpy = notDeepEqualSpy
  member private x._throwsSpy = throwsSpy
  member private x._notThrowsSpy = notThrowsSpy
  member private x._throwsAsyncSpy = throwsAsyncSpy
  member private x._notThrowsAsyncSpy = notThrowsAsyncSpy
  member private x._regexSpy = regexSpy
  member private x._ifErrorSpy = ifErrorSpy
  member private x._snapshotSpy = snapshotSpy

  static member pass (s: string option) (m: AsserterMock) = checkCall m._passSpy "pass" [s]
  static member fail (s: string option) (m: AsserterMock) = checkCall m._failSpy "fail" [s]
  static member isTruthy (v: obj, s: string option) (m: AsserterMock) = checkCall m._truthySpy "truthy" [v; s]
  static member isFalsy (v: obj, s: string option) (m: AsserterMock) = checkCall m._falsySpy "falsy" [v; s]
  static member isTrue (v: bool, s: string option) (m: AsserterMock) = checkCall m._trueSpy "true" [v; s]
  static member isFalse (v: bool, s: string option) (m: AsserterMock) = checkCall m._falseSpy "false" [v; s]
  static member isSame (v: obj, e: obj, s: string option) (m: AsserterMock) = checkCall m._isSpy "isSame" [v; e; s]
  static member isNotSame (v: obj, e: obj, s: string option) (m: AsserterMock) = checkCall m._notSpy "isNotSame" [v; e; s]
  static member isDeepEqual (v: obj, e: obj, s: string option) (m: AsserterMock) = checkCall m._deepEqualSpy "isDeepEqual" [v; e; s]
  static member isNotDeepEqual (v: obj, e: obj, s: string option) (m: AsserterMock) = checkCall m._notDeepEqualSpy "isNotDeepEqual" [v; e; s]
  static member throws (f: (unit -> unit), s: string option) (m: AsserterMock) = checkCall m._throwsSpy "throws" [f; s]
  static member doesNotThrow (f: (unit -> unit), s: string option) (m: AsserterMock) = checkCall m._notThrowsSpy "doesNotThrow" [f; s]
  static member asyncThrows (f: (unit -> Async<unit>), s: string option) (m: AsserterMock) = checkCall m._throwsAsyncSpy "asyncThrows" [f; s]
  static member doesNotAsyncThrow (f: (unit -> Async<unit>), s: string option) (m: AsserterMock) = checkCall m._notThrowsAsyncSpy "doesNotAsyncThrow" [f; s]
  static member matches (v: string, r: System.Text.RegularExpressions.Regex, s: string option) (m: AsserterMock) = checkCall m._regexSpy "matches" [v; r; s]
  static member ifError (e: obj, s: string option) (m: AsserterMock) = checkCall m._ifErrorSpy "ifError" [e; s]
  static member snapshot (v: obj, s: string option) (m: AsserterMock) = checkCall m._snapshotSpy "snapshot" [v; s]

let mockCall (a: Assert.Assert) (check: AsserterMock -> Spec<unit>): Spec<unit> = spec {
  let mock = AsserterMock ()
  do! Assert.run mock.Asserter a
  do! check mock
}

let messageTwiceFails (a: Assert.Assert) = spec {
  do! Assert.throws <| fun () -> a <?> "msg1" <?> "msg2" |> ignore
}

spec {
  do! Spec.plan 9

  do! messageTwiceFails Assert.pass
  do! Assert.isDeepEqual (Assert.pass) (Assert.Pass None)
  do! Assert.isDeepEqual (Assert.pass <?> "foo") (Assert.Pass (Some "foo"))
  do! mockCall (Assert.pass) (AsserterMock.pass None)
  do! mockCall (Assert.pass <?> "foo") (AsserterMock.pass (Some "foo"))

  do! Assert.pass
  do! Assert.pass <?> "pass"
} |> Spec.toTest |> Test.create "pass"

spec {
  do! Spec.plan 7

  do! messageTwiceFails Assert.fail
  do! Assert.isDeepEqual (Assert.fail) (Assert.Fail None)
  do! Assert.isDeepEqual (Assert.fail <?> "foo") (Assert.Fail (Some "foo"))
  do! mockCall (Assert.fail) (AsserterMock.fail None)
  do! mockCall (Assert.fail <?> "foo") (AsserterMock.fail (Some "foo"))

  // Assert.fail can't be used
} |> Spec.toTest |> Test.create "fail"

spec {
  do! Spec.plan 9

  do! messageTwiceFails (Assert.isTruthy true)
  do! Assert.isDeepEqual (Assert.isTruthy true) (Assert.Truthy (true, None))
  do! Assert.isDeepEqual (Assert.isTruthy false <?> "foo") (Assert.Truthy (false, Some "foo"))
  do! mockCall (Assert.isTruthy true) (AsserterMock.isTruthy (true, None))
  do! mockCall (Assert.isTruthy false <?> "foo") (AsserterMock.isTruthy (false, Some "foo"))

  do! Assert.isTruthy true
  do! Assert.isTruthy true <?> "isTruthy"
} |> Spec.toTest |> Test.create "isTruthy"

spec {
  do! Spec.plan 9

  do! messageTwiceFails (Assert.isFalsy true)
  do! Assert.isDeepEqual (Assert.isFalsy true) (Assert.Falsy (true, None))
  do! Assert.isDeepEqual (Assert.isFalsy false <?> "foo") (Assert.Falsy (false, Some "foo"))
  do! mockCall (Assert.isFalsy true) (AsserterMock.isFalsy (true, None))
  do! mockCall (Assert.isFalsy false <?> "foo") (AsserterMock.isFalsy (false, Some "foo"))

  do! Assert.isFalsy false
  do! Assert.isFalsy false <?> "isFalsy"
} |> Spec.toTest |> Test.create "isFalsy"

spec {
  do! Spec.plan 9

  do! messageTwiceFails (Assert.isTrue true)
  do! Assert.isDeepEqual (Assert.isTrue true) (Assert.True (true, None))
  do! Assert.isDeepEqual (Assert.isTrue false <?> "foo") (Assert.True (false, Some "foo"))
  do! mockCall (Assert.isTrue true) (AsserterMock.isTrue (true, None))
  do! mockCall (Assert.isTrue false <?> "foo") (AsserterMock.isTrue (false, Some "foo"))

  do! Assert.isTrue true
  do! Assert.isTrue true <?> "isTrue"
} |> Spec.toTest |> Test.create "isTrue"

spec {
  do! Spec.plan 9

  do! messageTwiceFails (Assert.isFalse true)
  do! Assert.isDeepEqual (Assert.isFalse true) (Assert.False (true, None))
  do! Assert.isDeepEqual (Assert.isFalse false <?> "foo") (Assert.False (false, Some "foo"))
  do! mockCall (Assert.isFalse true) (AsserterMock.isFalse (true, None))
  do! mockCall (Assert.isFalse false <?> "foo") (AsserterMock.isFalse (false, Some "foo"))

  do! Assert.isFalse false
  do! Assert.isFalse false <?> "isTrue"
} |> Spec.toTest |> Test.create "isFalse"

spec {
  do! Spec.plan 9

  let obj1 = createObj [ "obj1" ==> true ]
  let obj2 = createObj [ "obj2" ==> true ]

  do! messageTwiceFails (Assert.isSame obj1 obj2)
  do! Assert.isDeepEqual (Assert.isSame obj1 obj2) (Assert.Is (obj1, obj2, None))
  do! Assert.isDeepEqual (Assert.isSame obj1 obj1 <?> "foo") (Assert.Is (obj1, obj1, Some "foo"))
  do! mockCall (Assert.isSame obj1 obj2) (AsserterMock.isSame (obj1, obj2, None))
  do! mockCall (Assert.isSame obj1 obj1 <?> "foo") (AsserterMock.isSame (obj1, obj1, Some "foo"))

  do! Assert.isSame obj1 obj1
  do! Assert.isSame obj1 obj1 <?> "isSame"
} |> Spec.toTest |> Test.create "isSame"

spec {
  do! Spec.plan 9

  let obj1 = createObj [ "obj1" ==> true ]
  let obj2 = createObj [ "obj2" ==> true ]

  do! messageTwiceFails (Assert.isNotSame obj1 obj2)
  do! Assert.isDeepEqual (Assert.isNotSame obj1 obj2) (Assert.Not (obj1, obj2, None))
  do! Assert.isDeepEqual (Assert.isNotSame obj1 obj1 <?> "foo") (Assert.Not (obj1, obj1, Some "foo"))
  do! mockCall (Assert.isNotSame obj1 obj2) (AsserterMock.isNotSame (obj1, obj2, None))
  do! mockCall (Assert.isNotSame obj1 obj1 <?> "foo") (AsserterMock.isNotSame (obj1, obj1, Some "foo"))

  do! Assert.isNotSame obj1 obj2
  do! Assert.isNotSame obj1 obj2 <?> "isNotSame"
} |> Spec.toTest |> Test.create "isNotSame"

spec {
  do! Spec.plan 9

  let obj1 = createObj [ "test" ==> true ]
  let obj2 = createObj [ "test" ==> true ]

  do! messageTwiceFails (Assert.isDeepEqual obj1 obj2)
  do! Assert.isDeepEqual (Assert.isDeepEqual obj1 obj2) (Assert.DeepEqual (obj1, obj2, None))
  do! Assert.isDeepEqual (Assert.isDeepEqual obj1 obj1 <?> "foo") (Assert.DeepEqual (obj1, obj1, Some "foo"))
  do! mockCall (Assert.isDeepEqual obj1 obj2) (AsserterMock.isDeepEqual (obj1, obj2, None))
  do! mockCall (Assert.isDeepEqual obj1 obj1 <?> "foo") (AsserterMock.isDeepEqual (obj1, obj1, Some "foo"))

  do! Assert.isDeepEqual obj1 obj2
  do! Assert.isDeepEqual obj1 obj2 <?> "isDeepEqual"
} |> Spec.toTest |> Test.create "isDeepEqual"

spec {
  do! Spec.plan 9

  let obj1 = createObj [ "obj1" ==> true ]
  let obj2 = createObj [ "obj2" ==> true ]

  do! messageTwiceFails (Assert.isNotDeepEqual obj1 obj2)
  do! Assert.isDeepEqual (Assert.isNotDeepEqual obj1 obj2) (Assert.NotDeepEqual (obj1, obj2, None))
  do! Assert.isDeepEqual (Assert.isNotDeepEqual obj1 obj1 <?> "foo") (Assert.NotDeepEqual (obj1, obj1, Some "foo"))
  do! mockCall (Assert.isNotDeepEqual obj1 obj2) (AsserterMock.isNotDeepEqual (obj1, obj2, None))
  do! mockCall (Assert.isNotDeepEqual obj1 obj1 <?> "foo") (AsserterMock.isNotDeepEqual (obj1, obj1, Some "foo"))

  do! Assert.isNotDeepEqual obj1 obj2
  do! Assert.isNotDeepEqual obj1 obj2 <?> "isNotDeepEqual"
} |> Spec.toTest |> Test.create "isNotDeepEqual"

spec {
  do! Spec.plan 9

  let fn = fun () -> failwith "my message"

  do! messageTwiceFails (Assert.throws fn)
  do! Assert.isDeepEqual (Assert.throws fn) (Assert.Throws (fn, None))
  do! Assert.isDeepEqual (Assert.throws fn <?> "foo") (Assert.Throws (fn, Some "foo"))
  do! mockCall (Assert.throws fn) (AsserterMock.throws (fn, None))
  do! mockCall (Assert.throws fn <?> "foo") (AsserterMock.throws (fn, Some "foo"))

  do! Assert.throws fn
  do! Assert.throws fn <?> "throws"
} |> Spec.toTest |> Test.create "throws"

spec {
  do! Spec.plan 9

  let fn = fun () -> ()

  do! messageTwiceFails (Assert.doesNotThrow fn)
  do! Assert.isDeepEqual (Assert.doesNotThrow fn) (Assert.NotThrows (fn, None))
  do! Assert.isDeepEqual (Assert.doesNotThrow fn <?> "foo") (Assert.NotThrows (fn, Some "foo"))
  do! mockCall (Assert.doesNotThrow fn) (AsserterMock.doesNotThrow (fn, None))
  do! mockCall (Assert.doesNotThrow fn <?> "foo") (AsserterMock.doesNotThrow (fn, Some "foo"))

  do! Assert.doesNotThrow fn
  do! Assert.doesNotThrow fn <?> "doesNotThrow"
} |> Spec.toTest |> Test.create "doesNotThrow"

spec {
  do! Spec.plan 9

  let fn = fun () -> async { failwith "my message" }

  do! messageTwiceFails (Assert.asyncThrows fn)
  do! Assert.isDeepEqual (Assert.asyncThrows fn) (Assert.AsyncThrows (fn, None))
  do! Assert.isDeepEqual (Assert.asyncThrows fn <?> "foo") (Assert.AsyncThrows (fn, Some "foo"))
  do! mockCall (Assert.asyncThrows fn) (AsserterMock.asyncThrows (fn, None))
  do! mockCall (Assert.asyncThrows fn <?> "foo") (AsserterMock.asyncThrows (fn, Some "foo"))

  do! Assert.asyncThrows fn
  do! Assert.asyncThrows fn <?> "asyncThrows"
} |> Spec.toTest |> Test.create "asyncThrows"

spec {
  do! Spec.plan 9

  let fn = fun () -> async.Return ()

  do! messageTwiceFails (Assert.doesNotAsyncThrow fn)
  do! Assert.isDeepEqual (Assert.doesNotAsyncThrow fn) (Assert.NotAsyncThrows (fn, None))
  do! Assert.isDeepEqual (Assert.doesNotAsyncThrow fn <?> "foo") (Assert.NotAsyncThrows (fn, Some "foo"))
  do! mockCall (Assert.doesNotAsyncThrow fn) (AsserterMock.doesNotAsyncThrow (fn, None))
  do! mockCall (Assert.doesNotAsyncThrow fn <?> "foo") (AsserterMock.doesNotAsyncThrow (fn, Some "foo"))

  do! Assert.doesNotAsyncThrow fn
  do! Assert.doesNotAsyncThrow fn <?> "doesNotAsyncThrow"
} |> Spec.toTest |> Test.create "doesNotAsyncThrow"

// spec {
//   do! Spec.plan 9

//   let regex = System.Text.RegularExpressions.Regex "^ab*c?$"

//   do! messageTwiceFails (Assert.matches "abc" regex)
//   do! Assert.isDeepEqual (Assert.matches "abc" regex) (Assert.Regex ("abc", regex, None))
//   do! Assert.isDeepEqual (Assert.matches "abc" regex <?> "foo") (Assert.Regex ("abc", regex, Some "foo"))
//   do! mockCall (Assert.matches "abc" regex) (AsserterMock.matches ("abc", regex, None))
//   do! mockCall (Assert.matches "abc" regex <?> "foo") (AsserterMock.matches ("abc", regex, Some "foo"))

//   do! Assert.matches "abc" regex
//   do! Assert.matches "abc" regex <?> "matches"
// } |> Spec.toTest |> Test.create "matches"

spec {
  do! Spec.plan 9

  let err = null

  do! messageTwiceFails (Assert.ifError err)
  do! Assert.isDeepEqual (Assert.ifError err) (Assert.IfError (err, None))
  do! Assert.isDeepEqual (Assert.ifError err <?> "foo") (Assert.IfError (err, Some "foo"))
  do! mockCall (Assert.ifError err) (AsserterMock.ifError (err, None))
  do! mockCall (Assert.ifError err <?> "foo") (AsserterMock.ifError (err, Some "foo"))

  do! Assert.ifError err
  do! Assert.ifError err <?> "ifError"
} |> Spec.toTest |> Test.create "ifError"

spec {
  do! Spec.plan 9

  let obj = createObj [ "test" ==> true; "kind" ==> "snapshot"; "with" ==> "ava"; "num" ==> 1 ]

  do! messageTwiceFails (Assert.snapshot obj)
  do! Assert.isDeepEqual (Assert.snapshot obj) (Assert.Snapshot (obj, None))
  do! Assert.isDeepEqual (Assert.snapshot obj <?> "foo") (Assert.Snapshot (obj, Some "foo"))
  do! mockCall (Assert.snapshot obj) (AsserterMock.snapshot (obj, None))
  do! mockCall (Assert.snapshot obj <?> "foo") (AsserterMock.snapshot (obj, Some "foo"))
  
  do! Assert.snapshot obj
  do! Assert.snapshot obj <?> "snapshot with name"
} |> Spec.toTest |> Test.create "snapshot"
