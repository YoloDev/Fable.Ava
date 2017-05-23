namespace Fable.Ava

open Fable.Core
open Fable.Import.Ava

module Assert =
  type Assert<'t> =
  | Pass of message: string option
  | Fail of message: string option
  | Truthy of value: 't * message: string option
  | Falsy of value: 't * message: string option
  | True of value: bool * message: string option
  | False of value: bool * message: string option
  | Is of value: 't * expected: 't * message: string option
  | Not of value: 't * expected: 't * message: string option
  | DeepEqual of value: 't * expected: 't * message: string option
  | NotDeepEqual of value: 't * expected: 't * message: string option
  | Throws of fn: (unit -> unit) * message: string option
  | NotThrows of fn: (unit -> unit) * message: string option
  | AsyncThrows of fn: (unit -> Async<unit>) * message: string option
  | NotAsyncThrows of fn: (unit -> Async<unit>) * message: string option
  | Regex of value: string * regex: Fable.Import.JS.RegExp * message: string option
  | IfError of error: obj * message: string option
  | Snapshot of value: obj * message: string option

  let internal run (t: IAsserter) = function
    | Pass None                   -> t.Pass ()
    | Pass (Some s)               -> t.Pass s
    | Fail None                   -> t.Fail ()
    | Fail (Some s)               -> t.Fail s
    | Truthy (v, None)            -> t.Truthy v
    | Truthy (v, Some s)          -> t.Truthy (v, s)
    | Falsy (v, None)             -> t.Falsy v
    | Falsy (v, Some s)           -> t.Falsy (v, s)
    | True (v, None)              -> t.True v
    | True (v, Some s)            -> t.True (v, s)
    | False (v, None)             -> t.False v
    | False (v, Some s)           -> t.False (v, s)
    | Is (v, e, None)             -> t.Is (v, e)
    | Is (v, e, Some s)           -> t.Is (v, e, s)
    | Not (v, e, None)            -> t.Not (v, e)
    | Not (v, e, Some s)          -> t.Not (v, e, s)
    | DeepEqual (v, e, None)      -> t.DeepEqual (v, e)
    | DeepEqual (v, e, Some s)    -> t.DeepEqual (v, e, s)
    | NotDeepEqual (v, e, None)   -> t.NotDeepEqual (v, e)
    | NotDeepEqual (v, e, Some s) -> t.NotDeepEqual (v, e, s)
    | Throws (f, None)            -> t.Throws f
    | Throws (f, Some s)          -> t.Throws (f, s)
    | NotThrows (f, None)         -> t.NotThrows f
    | NotThrows (f, Some s)       -> t.NotThrows (f, s)
    | AsyncThrows (f, None)       -> t.Throws (f >> Async.StartAsPromise)
    | AsyncThrows (f, Some s)     -> t.Throws (f >> Async.StartAsPromise, s)
    | NotAsyncThrows (f, None)    -> t.NotThrows (f >> Async.StartAsPromise)
    | NotAsyncThrows (f, Some s)  -> t.NotThrows (f >> Async.StartAsPromise, s)
    | Regex (v, r, None)          -> t.Regex (v, r)
    | Regex (v, r, Some s)        -> t.Regex (v, r, s)
    | IfError (e, None)           -> t.IfError e
    | IfError (e, Some s)         -> t.IfError (e, s)
    | Snapshot (v, None)          -> t.Snapshot v
    | Snapshot (v, Some s)        -> t.Snapshot (v, s)
  
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
    | Regex (_, _, Some s)
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
    | Regex (v, r, None)           -> Regex (v, r, Some msg)
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
  let notThrows f = NotThrows (f, None)
  let asyncThrows f = AsyncThrows (f, None)
  let notAsyncThrows f = NotAsyncThrows (f, None)
  let matches v r = Regex (v, r, None)
  let ifError e = IfError (e, None)
  let snapshot v = Snapshot (v, None)

[<AutoOpen>]
module AssertOperators =
  let (<?>) a s = Assert.setMessage s a