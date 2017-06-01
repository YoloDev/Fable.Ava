module OtherTests

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import.Sinon
open Fable.Ava
open Fable.Ava.TestCheck

let addTest name spec = Test.create name spec

Prop.create "prop test" (Generator.tuple2 Generator.int Generator.int) <| fun (a, b) -> prop {
  do! SyncSpec.plan 1
  do! Assert.isSame (a + b) (b + a)
}

type Address = {
  street: string
  number: int
}

type Person = {
  name: string
  age: int
  address: Address option
}

let addressGen = gen {
  let! street = Generator.alphaNumString
  let! number = Generator.sPosInt
  
  return { street = street; number = number }
}

let personGen = gen {
  let! address = Generator.optional addressGen
  let! name = Generator.alphaNumString
  let! age = Generator.sPosInt

  return { name = name; age = age; address = address }
}

Prop.create "custom gen test" personGen <| fun p -> prop {
  do! Assert.isTruthy p <?> "person object is set"
  do! Assert.isTrue (p.age > 0) <?> "person has a non-negative age"
  match p.address with
  | None -> do! Assert.pass <?> "No address"
  | Some a -> do! Assert.isTrue (a.number > 0) <?> "address number is non-negative"
}

let peopleGen = Generator.listWithOptions (SizeOptions.size 5) personGen

Prop.create "list gen test" peopleGen <| fun ps -> prop {
  do! Assert.isTruthy ps <?> "person list is set"
  do! Assert.isSame (List.length ps) 5 <?> "list length is correct" 
}

let getCall (spy: Spy) (name: string) = prop {
  do! Assert.isDeepEqual spy.CallCount 1 <?> sprintf "%s should be called once" name
  return spy.GetCall 0
}

let checkCall (spy: Spy) (name: string) (args: obj list) = prop {
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

let mockCall (a: Assert.Assert) (check: AsserterMock -> SyncSpec<unit>): SyncSpec<unit> = prop {
  let mock = AsserterMock ()
  Assert.runSync mock.Asserter a
  do! check mock
}

let shouldThrow (a: Assert.Assert) = prop {
  let mock = AsserterMock ()
  do! Assert.throws <| fun () ->
    Assert.runSync mock.Asserter a
}

let messageTwiceFails (a: Assert.Assert) = prop {
  do! Assert.throws <| fun () -> a <?> "msg1" <?> "msg2" |> ignore
}

prop {
  do! SyncSpec.plan 9

  do! messageTwiceFails Assert.pass
  do! Assert.isDeepEqual (Assert.pass) (Assert.Pass None)
  do! Assert.isDeepEqual (Assert.pass <?> "foo") (Assert.Pass (Some "foo"))
  do! mockCall (Assert.pass) (AsserterMock.pass None)
  do! mockCall (Assert.pass <?> "foo") (AsserterMock.pass (Some "foo"))

  do! Assert.pass
  do! Assert.pass <?> "pass"
} |> addTest "pass"

prop {
  do! SyncSpec.plan 7

  do! messageTwiceFails Assert.fail
  do! Assert.isDeepEqual (Assert.fail) (Assert.Fail None)
  do! Assert.isDeepEqual (Assert.fail <?> "foo") (Assert.Fail (Some "foo"))
  do! mockCall (Assert.fail) (AsserterMock.fail None)
  do! mockCall (Assert.fail <?> "foo") (AsserterMock.fail (Some "foo"))

  // Assert.fail can't be used
} |> addTest "fail"

prop {
  do! SyncSpec.plan 9

  do! messageTwiceFails (Assert.isTruthy true)
  do! Assert.isDeepEqual (Assert.isTruthy true) (Assert.Truthy (true, None))
  do! Assert.isDeepEqual (Assert.isTruthy false <?> "foo") (Assert.Truthy (false, Some "foo"))
  do! mockCall (Assert.isTruthy true) (AsserterMock.isTruthy (true, None))
  do! mockCall (Assert.isTruthy false <?> "foo") (AsserterMock.isTruthy (false, Some "foo"))

  do! Assert.isTruthy true
  do! Assert.isTruthy true <?> "isTruthy"
} |> addTest "isTruthy"

prop {
  do! SyncSpec.plan 9

  do! messageTwiceFails (Assert.isFalsy true)
  do! Assert.isDeepEqual (Assert.isFalsy true) (Assert.Falsy (true, None))
  do! Assert.isDeepEqual (Assert.isFalsy false <?> "foo") (Assert.Falsy (false, Some "foo"))
  do! mockCall (Assert.isFalsy true) (AsserterMock.isFalsy (true, None))
  do! mockCall (Assert.isFalsy false <?> "foo") (AsserterMock.isFalsy (false, Some "foo"))

  do! Assert.isFalsy false
  do! Assert.isFalsy false <?> "isFalsy"
} |> addTest "isFalsy"

prop {
  do! SyncSpec.plan 9

  do! messageTwiceFails (Assert.isTrue true)
  do! Assert.isDeepEqual (Assert.isTrue true) (Assert.True (true, None))
  do! Assert.isDeepEqual (Assert.isTrue false <?> "foo") (Assert.True (false, Some "foo"))
  do! mockCall (Assert.isTrue true) (AsserterMock.isTrue (true, None))
  do! mockCall (Assert.isTrue false <?> "foo") (AsserterMock.isTrue (false, Some "foo"))

  do! Assert.isTrue true
  do! Assert.isTrue true <?> "isTrue"
} |> addTest "isTrue"

prop {
  do! SyncSpec.plan 9

  do! messageTwiceFails (Assert.isFalse true)
  do! Assert.isDeepEqual (Assert.isFalse true) (Assert.False (true, None))
  do! Assert.isDeepEqual (Assert.isFalse false <?> "foo") (Assert.False (false, Some "foo"))
  do! mockCall (Assert.isFalse true) (AsserterMock.isFalse (true, None))
  do! mockCall (Assert.isFalse false <?> "foo") (AsserterMock.isFalse (false, Some "foo"))

  do! Assert.isFalse false
  do! Assert.isFalse false <?> "isTrue"
} |> addTest "isFalse"

prop {
  do! SyncSpec.plan 9

  let obj1 = createObj [ "obj1" ==> true ]
  let obj2 = createObj [ "obj2" ==> true ]

  do! messageTwiceFails (Assert.isSame obj1 obj2)
  do! Assert.isDeepEqual (Assert.isSame obj1 obj2) (Assert.Is (obj1, obj2, None))
  do! Assert.isDeepEqual (Assert.isSame obj1 obj1 <?> "foo") (Assert.Is (obj1, obj1, Some "foo"))
  do! mockCall (Assert.isSame obj1 obj2) (AsserterMock.isSame (obj1, obj2, None))
  do! mockCall (Assert.isSame obj1 obj1 <?> "foo") (AsserterMock.isSame (obj1, obj1, Some "foo"))

  do! Assert.isSame obj1 obj1
  do! Assert.isSame obj1 obj1 <?> "isSame"
} |> addTest "isSame"

prop {
  do! SyncSpec.plan 9

  let obj1 = createObj [ "obj1" ==> true ]
  let obj2 = createObj [ "obj2" ==> true ]

  do! messageTwiceFails (Assert.isNotSame obj1 obj2)
  do! Assert.isDeepEqual (Assert.isNotSame obj1 obj2) (Assert.Not (obj1, obj2, None))
  do! Assert.isDeepEqual (Assert.isNotSame obj1 obj1 <?> "foo") (Assert.Not (obj1, obj1, Some "foo"))
  do! mockCall (Assert.isNotSame obj1 obj2) (AsserterMock.isNotSame (obj1, obj2, None))
  do! mockCall (Assert.isNotSame obj1 obj1 <?> "foo") (AsserterMock.isNotSame (obj1, obj1, Some "foo"))

  do! Assert.isNotSame obj1 obj2
  do! Assert.isNotSame obj1 obj2 <?> "isNotSame"
} |> addTest "isNotSame"

prop {
  do! SyncSpec.plan 9

  let obj1 = createObj [ "test" ==> true ]
  let obj2 = createObj [ "test" ==> true ]

  do! messageTwiceFails (Assert.isDeepEqual obj1 obj2)
  do! Assert.isDeepEqual (Assert.isDeepEqual obj1 obj2) (Assert.DeepEqual (obj1, obj2, None))
  do! Assert.isDeepEqual (Assert.isDeepEqual obj1 obj1 <?> "foo") (Assert.DeepEqual (obj1, obj1, Some "foo"))
  do! mockCall (Assert.isDeepEqual obj1 obj2) (AsserterMock.isDeepEqual (obj1, obj2, None))
  do! mockCall (Assert.isDeepEqual obj1 obj1 <?> "foo") (AsserterMock.isDeepEqual (obj1, obj1, Some "foo"))

  do! Assert.isDeepEqual obj1 obj2
  do! Assert.isDeepEqual obj1 obj2 <?> "isDeepEqual"
} |> addTest "isDeepEqual"

prop {
  do! SyncSpec.plan 9

  let obj1 = createObj [ "obj1" ==> true ]
  let obj2 = createObj [ "obj2" ==> true ]

  do! messageTwiceFails (Assert.isNotDeepEqual obj1 obj2)
  do! Assert.isDeepEqual (Assert.isNotDeepEqual obj1 obj2) (Assert.NotDeepEqual (obj1, obj2, None))
  do! Assert.isDeepEqual (Assert.isNotDeepEqual obj1 obj1 <?> "foo") (Assert.NotDeepEqual (obj1, obj1, Some "foo"))
  do! mockCall (Assert.isNotDeepEqual obj1 obj2) (AsserterMock.isNotDeepEqual (obj1, obj2, None))
  do! mockCall (Assert.isNotDeepEqual obj1 obj1 <?> "foo") (AsserterMock.isNotDeepEqual (obj1, obj1, Some "foo"))

  do! Assert.isNotDeepEqual obj1 obj2
  do! Assert.isNotDeepEqual obj1 obj2 <?> "isNotDeepEqual"
} |> addTest "isNotDeepEqual"

prop {
  do! SyncSpec.plan 9

  let fn = fun () -> failwith "my message"

  do! messageTwiceFails (Assert.throws fn)
  do! Assert.isDeepEqual (Assert.throws fn) (Assert.Throws (fn, None))
  do! Assert.isDeepEqual (Assert.throws fn <?> "foo") (Assert.Throws (fn, Some "foo"))
  do! mockCall (Assert.throws fn) (AsserterMock.throws (fn, None))
  do! mockCall (Assert.throws fn <?> "foo") (AsserterMock.throws (fn, Some "foo"))

  do! Assert.throws fn
  do! Assert.throws fn <?> "throws"
} |> addTest "throws"

prop {
  do! SyncSpec.plan 9

  let fn = fun () -> ()

  do! messageTwiceFails (Assert.doesNotThrow fn)
  do! Assert.isDeepEqual (Assert.doesNotThrow fn) (Assert.NotThrows (fn, None))
  do! Assert.isDeepEqual (Assert.doesNotThrow fn <?> "foo") (Assert.NotThrows (fn, Some "foo"))
  do! mockCall (Assert.doesNotThrow fn) (AsserterMock.doesNotThrow (fn, None))
  do! mockCall (Assert.doesNotThrow fn <?> "foo") (AsserterMock.doesNotThrow (fn, Some "foo"))

  do! Assert.doesNotThrow fn
  do! Assert.doesNotThrow fn <?> "doesNotThrow"
} |> addTest "doesNotThrow"

prop {
  do! SyncSpec.plan 5

  let fn = fun () -> async { failwith "my message" }

  do! messageTwiceFails (Assert.asyncThrows fn)
  do! Assert.isDeepEqual (Assert.asyncThrows fn) (Assert.AsyncThrows (fn, None))
  do! Assert.isDeepEqual (Assert.asyncThrows fn <?> "foo") (Assert.AsyncThrows (fn, Some "foo"))
  do! shouldThrow (Assert.asyncThrows fn)
  do! shouldThrow (Assert.asyncThrows fn <?> "foo")

  // not allowed in sync tests
  // do! Assert.asyncThrows fn 
  // do! Assert.asyncThrows fn <?> "asyncThrows"
} |> addTest "asyncThrows"

prop {
  do! SyncSpec.plan 5

  let fn = fun () -> async.Return ()

  do! messageTwiceFails (Assert.doesNotAsyncThrow fn)
  do! Assert.isDeepEqual (Assert.doesNotAsyncThrow fn) (Assert.NotAsyncThrows (fn, None))
  do! Assert.isDeepEqual (Assert.doesNotAsyncThrow fn <?> "foo") (Assert.NotAsyncThrows (fn, Some "foo"))
  do! shouldThrow (Assert.doesNotAsyncThrow fn)
  do! shouldThrow (Assert.doesNotAsyncThrow fn <?> "foo")

  // not allowed in sync tests
  // do! Assert.doesNotAsyncThrow fn
  // do! Assert.doesNotAsyncThrow fn <?> "doesNotAsyncThrow"
} |> addTest "doesNotAsyncThrow"

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
// } |> addTest "matches"

prop {
  do! SyncSpec.plan 9

  let err = null

  do! messageTwiceFails (Assert.ifError err)
  do! Assert.isDeepEqual (Assert.ifError err) (Assert.IfError (err, None))
  do! Assert.isDeepEqual (Assert.ifError err <?> "foo") (Assert.IfError (err, Some "foo"))
  do! mockCall (Assert.ifError err) (AsserterMock.ifError (err, None))
  do! mockCall (Assert.ifError err <?> "foo") (AsserterMock.ifError (err, Some "foo"))

  do! Assert.ifError err
  do! Assert.ifError err <?> "ifError"
} |> addTest "ifError"

prop {
  do! SyncSpec.plan 9

  let obj = createObj [ "test" ==> true; "kind" ==> "snapshot"; "with" ==> "ava"; "num" ==> 1 ]

  do! messageTwiceFails (Assert.snapshot obj)
  do! Assert.isDeepEqual (Assert.snapshot obj) (Assert.Snapshot (obj, None))
  do! Assert.isDeepEqual (Assert.snapshot obj <?> "foo") (Assert.Snapshot (obj, Some "foo"))
  do! mockCall (Assert.snapshot obj) (AsserterMock.snapshot (obj, None))
  do! mockCall (Assert.snapshot obj <?> "foo") (AsserterMock.snapshot (obj, Some "foo"))
  
  do! Assert.snapshot obj
  do! Assert.snapshot obj <?> "snapshot with name"
} |> addTest "snapshot"

type NativeSizeOpts = Fable.Import.Ava.SizeOptions
module NativeSizeOpts = Fable.Import.Ava.SizeOptions

Prop.create "bindings > SizeOpts.size" Generator.int <| fun size -> prop {
  do! SyncSpec.plan 1

  if (size < 0) then
    do! Assert.throws <| fun () -> NativeSizeOpts.size size |> ignore
  else
    let actual = NativeSizeOpts.size size
    let expected = { NativeSizeOpts.size = Some size; NativeSizeOpts.minSize = None; NativeSizeOpts.maxSize = None }
    do! Assert.isDeepEqual actual expected
}

Prop.create "bindings > SizeOpts.min" Generator.int <| fun size -> prop {
  do! SyncSpec.plan 1

  if (size < 0) then
    do! Assert.throws <| fun () -> NativeSizeOpts.min size |> ignore
  else
    let actual = NativeSizeOpts.min size
    let expected = { NativeSizeOpts.size = None; NativeSizeOpts.minSize = Some size; NativeSizeOpts.maxSize = None }
    do! Assert.isDeepEqual actual expected
}

Prop.create "bindings > SizeOpts.max" Generator.int <| fun size -> prop {
  do! SyncSpec.plan 1

  if (size < 0) then
    do! Assert.throws <| fun () -> NativeSizeOpts.max size |> ignore
  else
    let actual = NativeSizeOpts.max size
    let expected = { NativeSizeOpts.size = None; NativeSizeOpts.minSize = None; NativeSizeOpts.maxSize = Some size }
    do! Assert.isDeepEqual actual expected
}

Prop.create "bindings > SizeOpts.between" (Generator.tuple2 Generator.int Generator.int) <| fun (min, max) -> prop {
  do! SyncSpec.plan 1

  if (min < 0 || max < 0 || min >= max) then
    do! Assert.throws <| fun () -> NativeSizeOpts.between min max |> ignore
  else
    let actual = NativeSizeOpts.between min max
    let expected = { NativeSizeOpts.size = None; NativeSizeOpts.minSize = Some min; NativeSizeOpts.maxSize = Some max }
    do! Assert.isDeepEqual actual expected
}

Prop.create "testcheck > SizeOpts.size" Generator.int <| fun size -> prop {
  do! SyncSpec.plan 2

  if (size < 0) then
    do! Assert.throws <| fun () -> SizeOptions.size size |> ignore
    do! Assert.pass
  else
    let actual = SizeOptions.size size
    let expected = SizeOptions.Size size
    do! Assert.isDeepEqual actual expected
    do! Assert.isDeepEqual (SizeOptions.toSizeOptions actual) (NativeSizeOpts.size size)
}

Prop.create "testcheck > SizeOpts.min" Generator.int <| fun size -> prop {
  do! SyncSpec.plan 2

  if (size < 0) then
    do! Assert.throws <| fun () -> SizeOptions.min size |> ignore
    do! Assert.pass
  else
    let actual = SizeOptions.min size
    let expected = SizeOptions.Min size
    do! Assert.isDeepEqual actual expected
    do! Assert.isDeepEqual (SizeOptions.toSizeOptions actual) (NativeSizeOpts.min size)
}

Prop.create "testcheck > SizeOpts.max" Generator.int <| fun size -> prop {
  do! SyncSpec.plan 2

  if (size < 0) then
    do! Assert.throws <| fun () -> SizeOptions.max size |> ignore
    do! Assert.pass
  else
    let actual = SizeOptions.max size
    let expected = SizeOptions.Max size
    do! Assert.isDeepEqual actual expected
    do! Assert.isDeepEqual (SizeOptions.toSizeOptions actual) (NativeSizeOpts.max size)
}

Prop.create "testcheck > SizeOpts.between" (Generator.tuple2 Generator.int Generator.int) <| fun (min, max) -> prop {
  do! SyncSpec.plan 2

  if (min < 0 || max < 0 || min >= max) then
    do! Assert.throws <| fun () -> SizeOptions.between min max |> ignore
    do! Assert.pass
  else
    let actual = SizeOptions.between min max
    let expected = SizeOptions.Between (min, max)
    do! Assert.isDeepEqual actual expected
    do! Assert.isDeepEqual (SizeOptions.toSizeOptions actual) (NativeSizeOpts.between min max)
}

let checkGeneratorIsValid (g: Generator<_>) = prop {
  do! SyncSpec.plan 2

  do! Assert.isTruthy g

  let gen = Generator.unpack g
  do! Assert.isTruthy gen
}

let inline private const' v _ = v
let inline private testGenerator name g = addTest (sprintf "Generator.%s" name) (checkGeneratorIsValid g)

// These tests are there to ensure the Emite attributes are at least valid.
// In case there are string-name errors, this should fail.
testGenerator "nil" <| Generator.nil
testGenerator "optional" <| Generator.optional Generator.nil
testGenerator "notEmpty" <| Generator.notEmpty Generator.nil
testGenerator "filter" <| Generator.filter (const' true) Generator.nil
testGenerator "bind" <| Generator.bind (const' Generator.nil) Generator.nil
testGenerator "unit" <| Generator.unit ()
testGenerator "map" <| Generator.map id Generator.nil
testGenerator "scale" <| Generator.scale id Generator.nil
testGenerator "neverShrink" <| Generator.neverShrink Generator.nil
testGenerator "alwaysShrink" <| Generator.alwaysShrink Generator.nil
testGenerator "any" <| Generator.any
testGenerator "primitive" <| Generator.primitive
testGenerator "bool" <| Generator.bool
testGenerator "undefined" <| Generator.undefined
testGenerator "notANumber" <| Generator.notANumber
testGenerator "number" <| Generator.number
testGenerator "posNumber" <| Generator.posNumber
testGenerator "negNumber" <| Generator.negNumber
testGenerator "numberWithin" <| Generator.numberWithin 0. 10.
testGenerator "int" <| Generator.int
testGenerator "posInt" <| Generator.posInt
testGenerator "negInt" <| Generator.negInt
testGenerator "sPosInt" <| Generator.sPosInt
testGenerator "sNegInt" <| Generator.sNegInt
testGenerator "intWithin" <| Generator.intWithin 0 10
testGenerator "string" <| Generator.string
testGenerator "asciiString" <| Generator.asciiString
testGenerator "alphaNumString" <| Generator.alphaNumString
testGenerator "substring" <| Generator.substring "abc"
testGenerator "char" <| Generator.char
testGenerator "asciiChar" <| Generator.asciiChar
testGenerator "alphaNumChar" <| Generator.alphaNumChar
testGenerator "array" <| Generator.array Generator.nil
testGenerator "list" <| Generator.list Generator.nil
testGenerator "arrayWithOptions" <| Generator.arrayWithOptions (SizeOptions.size 5) Generator.nil
testGenerator "listWithOptions" <| Generator.listWithOptions (SizeOptions.size 5) Generator.nil
testGenerator "tuple2" <| Generator.tuple2 Generator.nil Generator.nil
testGenerator "tuple3" <| Generator.tuple3 Generator.nil Generator.nil Generator.nil
testGenerator "tuple4" <| Generator.tuple4 Generator.nil Generator.nil Generator.nil Generator.nil
testGenerator "tuple5" <| Generator.tuple5 Generator.nil Generator.nil Generator.nil Generator.nil Generator.nil
testGenerator "uniqueArray" <| Generator.uniqueArray Generator.nil
testGenerator "uniqueList" <| Generator.uniqueList Generator.nil
testGenerator "uniqueArrayWithOptions" <| Generator.uniqueArrayWithOptions (SizeOptions.size 5) Generator.nil
testGenerator "uniqueListWithOptions" <| Generator.uniqueListWithOptions (SizeOptions.size 5) Generator.nil
testGenerator "uniqueArrayBy" <| Generator.uniqueArrayBy id Generator.nil
testGenerator "uniqueListBy" <| Generator.uniqueListBy id Generator.nil
testGenerator "uniqueArrayByWithOptions" <| Generator.uniqueArrayByWithOptions (SizeOptions.size 5) id Generator.nil
testGenerator "uniqueListByWithOptions" <| Generator.uniqueListByWithOptions (SizeOptions.size 5) id Generator.nil
testGenerator "oneOf" <| Generator.oneOf [Generator.nil; Generator.nil]
testGenerator "sized" <| Generator.sized (const' Generator.nil)