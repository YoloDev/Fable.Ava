namespace Fable.Ava

open Fable.Core
open Fable.Import.Ava

type Asserter = {
  pass: string option -> unit
  fail: string option -> unit
  isTruthy: obj -> string option -> unit
  isFalsy: obj -> string option -> unit
  isTrue: bool -> string option -> unit
  isFalse: bool -> string option -> unit
  isSame: obj -> obj -> string option -> unit
  isNotSame: obj -> obj -> string option -> unit
  isDeepEqual: obj -> obj -> string option -> unit
  isNotDeepEqual: obj -> obj -> string option -> unit
  throws: (unit -> unit) -> string option -> unit
  doesNotThrow: (unit -> unit) -> string option -> unit
  asyncThrows: (unit -> Async<unit>) -> string option -> Async<unit>
  doesNotAsyncThrow: (unit -> Async<unit>) -> string option -> Async<unit>
  //matches: string -> System.Text.RegularExpressions.Regex -> string option -> unit
  ifError: obj -> string option -> unit
  snapshot: obj -> string option -> unit
}

module Assert =
  type Assert =
  | Pass of message: string option
  | Fail of message: string option
  | Truthy of value: obj * message: string option
  | Falsy of value: obj * message: string option
  | True of value: bool * message: string option
  | False of value: bool * message: string option
  | Is of value: obj * expected: obj * message: string option
  | Not of value: obj * expected: obj * message: string option
  | DeepEqual of value: obj * expected: obj * message: string option
  | NotDeepEqual of value: obj * expected: obj * message: string option
  | Throws of fn: (unit -> unit) * message: string option
  | NotThrows of fn: (unit -> unit) * message: string option
  | AsyncThrows of fn: (unit -> Async<unit>) * message: string option
  | NotAsyncThrows of fn: (unit -> Async<unit>) * message: string option
  //| Regex of value: string * regex: System.Text.RegularExpressions.Regex * message: string option
  | IfError of error: obj * message: string option
  | Snapshot of value: obj * message: string option

  let run (a: Asserter) = function
    | Pass (s)               -> a.pass s; async.Return ()
    | Fail (s)               -> a.fail s; async.Return ()
    | Truthy (v, s)          -> a.isTruthy v s; async.Return ()
    | Falsy (v, s)           -> a.isFalsy v s; async.Return ()
    | True (v, s)            -> a.isTrue v s; async.Return ()
    | False (v, s)           -> a.isFalse v s; async.Return ()
    | Is (v, e, s)           -> a.isSame v e s; async.Return ()
    | Not (v, e, s)          -> a.isNotSame v e s; async.Return ()
    | DeepEqual (v, e, s)    -> a.isDeepEqual v e s; async.Return ()
    | NotDeepEqual (v, e, s) -> a.isNotDeepEqual v e s; async.Return ()
    | Throws (f, s)          -> a.throws f s; async.Return ()
    | NotThrows (f, s)       -> a.doesNotThrow f s; async.Return ()
    | AsyncThrows (f, s)     -> a.asyncThrows f s
    | NotAsyncThrows (f, s)  -> a.doesNotAsyncThrow f s
    //| Regex (v, r, s)        -> a.matches v r s; async.Return ()
    | IfError (e, s)         -> a.ifError e s; async.Return ()
    | Snapshot (v, s)        -> a.snapshot v s; async.Return ()
  
  let runSync (a: Asserter) = function
    | Pass (s)               -> a.pass s
    | Fail (s)               -> a.fail s
    | Truthy (v, s)          -> a.isTruthy v s
    | Falsy (v, s)           -> a.isFalsy v s
    | True (v, s)            -> a.isTrue v s
    | False (v, s)           -> a.isFalse v s
    | Is (v, e, s)           -> a.isSame v e s
    | Not (v, e, s)          -> a.isNotSame v e s
    | DeepEqual (v, e, s)    -> a.isDeepEqual v e s
    | NotDeepEqual (v, e, s) -> a.isNotDeepEqual v e s
    | Throws (f, s)          -> a.throws f s
    | NotThrows (f, s)       -> a.doesNotThrow f s
    | AsyncThrows (f, s)     -> failwithf "Async asserts not supported"
    | NotAsyncThrows (f, s)  -> failwithf "Async asserts not supported"
    //| Regex (v, r, s)        -> a.matches v r s; async.Return ()
    | IfError (e, s)         -> a.ifError e s
    | Snapshot (v, s)        -> a.snapshot v s
  
  let internal setMessage msg = function
    | Pass (Some s)
    | Fail (Some s)
    | Truthy (_, Some s)
    | Falsy (_, Some s)
    | True (_, Some s)
    | False (_, Some s)
    | Is (_, _, Some s)
    | Not (_, _, Some s)
    | DeepEqual (_, _, Some s)
    | NotDeepEqual (_, _, Some s)
    | Throws (_, Some s)
    | NotThrows (_, Some s)
    | AsyncThrows (_, Some s)
    | NotAsyncThrows (_, Some s)
    //| Regex (_, _, Some s)
    | IfError (_, Some s)
    | Snapshot (_, Some s)         -> failwithf "Message already set: %s" s
    | Pass (None)                  -> Pass (Some msg)
    | Fail (None)                  -> Fail (Some msg)
    | Truthy (v, None)             -> Truthy (v, Some msg)
    | Falsy (v, None)              -> Falsy (v, Some msg)
    | True (v, None)               -> True (v, Some msg)
    | False (v, None)              -> False (v, Some msg)
    | Is (v, e, None)              -> Is (v, e, Some msg)
    | Not (v, e, None)             -> Not (v, e, Some msg)
    | DeepEqual (v, e, None)       -> DeepEqual (v, e, Some msg)
    | NotDeepEqual (v, e, None)    -> NotDeepEqual (v, e, Some msg)
    | Throws (f, None)             -> Throws (f, Some msg)
    | NotThrows (f, None)          -> NotThrows (f, Some msg)
    | AsyncThrows (f, None)        -> AsyncThrows (f, Some msg)
    | NotAsyncThrows (f, None)     -> NotAsyncThrows (f, Some msg)
    //| Regex (v, r, None)           -> Regex (v, r, Some msg)
    | IfError (e, None)            -> IfError (e, Some msg)
    | Snapshot (v, None)           -> Snapshot (v, Some msg)
  
  let pass = Pass None
  let fail = Fail None
  let isTruthy v = Truthy (v, None)
  let isFalsy v = Falsy (v, None)
  let isTrue v = True (v, None)
  let isFalse v = False (v, None)
  let isSame v e = Is (v, e, None)
  let isNotSame v e = Not (v, e, None)
  let isDeepEqual v e = DeepEqual (v, e, None)
  let isNotDeepEqual v e = NotDeepEqual (v, e, None)
  let throws f = Throws (f, None)
  let doesNotThrow f = NotThrows (f, None)
  let asyncThrows f = AsyncThrows (f, None)
  let doesNotAsyncThrow f = NotAsyncThrows (f, None)
  //let matches v r = Regex (v, r, None)
  let ifError e = IfError (e, None)
  let snapshot v = Snapshot (v, None)

[<AutoOpen>]
module AssertOperators =
  let (<?>) a s = Assert.setMessage s a
