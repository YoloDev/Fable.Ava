namespace Fable.Ava

open Fable.Core
open Fable.Import.Ava


type SpecContext internal (context: TestContext) =
  let asserter = {
    pass = fun s ->
      match s with
      | None -> context.Pass ()
      | Some s -> context.Pass s
    
    fail = fun s ->
      match s with
      | None -> context.Fail ()
      | Some s -> context.Fail s
    
    isTruthy = fun v s ->
      match s with
      | None -> context.Truthy v
      | Some s -> context.Truthy (v, s)
    
    isFalsy = fun v s ->
      match s with
      | None -> context.Falsy v
      | Some s -> context.Falsy (v, s)
    
    isTrue = fun v s ->
      match s with
      | None -> context.True v
      | Some s -> context.True (v, s)
    
    isFalse = fun v s ->
      match s with
      | None -> context.False v
      | Some s -> context.False (v, s)
    
    isSame = fun v e s ->
      match s with
      | None -> context.Is (v, e)
      | Some s -> context.Is (v, e, s)
    
    isNotSame = fun v e s ->
      match s with
      | None -> context.Not (v, e)
      | Some s -> context.Not (v, e, s)
    
    isDeepEqual = fun v e s ->
      match s with
      | None -> context.DeepEqual (v, e)
      | Some s -> context.DeepEqual (v, e, s)
    
    isNotDeepEqual = fun v e s ->
      match s with
      | None -> context.NotDeepEqual (v, e)
      | Some s -> context.NotDeepEqual (v, e, s)
    
    throws = fun f s ->
      match s with
      | None -> context.Throws f
      | Some s -> context.Throws (f, s)
    
    doesNotThrow = fun f s ->
      match s with
      | None -> context.NotThrows f
      | Some s -> context.NotThrows (f, s)
    
    asyncThrows = fun f s ->
      match s with
      | None -> context.Throws (f () |> Async.StartAsPromise) |> Async.AwaitPromise
      | Some s -> context.Throws (f () |> Async.StartAsPromise, s) |> Async.AwaitPromise
    
    doesNotAsyncThrow = fun f s ->
      match s with
      | None -> context.NotThrows (f () |> Async.StartAsPromise) |> Async.AwaitPromise
      | Some s -> context.NotThrows (f () |> Async.StartAsPromise, s) |> Async.AwaitPromise
    
    // matches = fun v r s ->
    //   match s with
    //   | None -> context.Regex (v, r)
    //   | Some s -> context.Regex (v, r, s)
    
    ifError = fun e s ->
      match s with
      | None -> context.IfError e
      | Some s -> context.IfError (e, s)
    
    snapshot = fun v s ->
      match s with
      | None -> context.Snapshot v
      | Some s -> context.Snapshot (v, s)
  }

  member c.Asserter = asserter
  member c.Plan n = context.Plan n

[<Erase>]
type Spec<'t> = SpecWrapper of (SpecContext -> Async<'t>)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Spec =
  let internal run (SpecWrapper ma) t = ma t

  let create fn = SpecWrapper (fn >> async.Return)

  let createAsync fn = SpecWrapper fn

  let createAssert a = createAsync (fun t -> Assert.run t.Asserter a)

  let zero = create <| ignore

  let unit x = create <| fun _ -> x

  let asyncUnit x = createAsync <| fun _ -> x
  
  let bind ma f = createAsync <| fun t -> async.Bind (run ma t, fun a -> run (f a) t)

  let plan n = create <| fun t -> t.Plan n
  
  let toTest (spec: Spec<unit>): TestImpl =
    fun t ->
      let context = SpecContext t
      run spec context |> Async.StartAsPromise

[<RequireQualifiedAccess>]
module SpecBuilder =
  type SpecBuilderImpl () =
    member __.Zero () = Spec.zero
    member __.Return x = Spec.unit x
    member __.Return asyncX = Spec.asyncUnit asyncX
    member __.Bind (ma, f) = Spec.bind ma f
    member __.Bind (asyncA, f) = Spec.bind (Spec.asyncUnit asyncA) f
    member __.Bind (ma, f) = Spec.bind (Spec.createAssert ma) f

[<AutoOpen>]
module SpecBuilderInst =
  let spec = SpecBuilder.SpecBuilderImpl ()