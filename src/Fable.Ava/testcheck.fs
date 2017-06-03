module Fable.Ava.TestCheck

open Fable.Core
open Fable.Import.Ava

type NativeGen<'t> = NativeGenerator<'t>
module NativeGen = NativeGenerator

[<RequireQualifiedAccess>]
module SizeOptions =
  let exact size =
    if (size < 0) then failwith "size needs to be a positive number"
    else Exact size
  let min size =
    if (size < 0) then failwith "min needs to be a positive number"
    else Min size
  let max size =
    if (size < 0) then failwith "max needs to be a positive number"
    else Max size
  let between min max =
    if   (min < 0) then failwith "min needs to be a positive number"
    elif (max < 0) then failwith "max needs to be a positive number"
    elif (min >= max) then failwith "min needs to be less than max"
    else Between (min, max)

[<Erase>]
type Generator<'t> = private Generator of NativeGen<'t>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Generator =
  let internal unpack (Generator g) = g

  let optional (Generator g) = g.Nullable |> Generator
  let notEmpty (Generator g) = g.NotEmpty |> Generator
  let filter f (Generator g) = g.SuchThat f |> Generator
  let bind f (Generator g) = g.Then (f >> unpack) |> Generator
  let unit x = Generator <| NativeGen.ret x
  let map f g = bind (f >> unit) g

  let scale f (Generator g) = g.Scale f |> Generator
  let neverShrink (Generator g) = g.NeverShrink |> Generator
  let alwaysShrink (Generator g) = g.AlwaysShrink |> Generator

  let any = Generator NativeGen.any
  let primitive = Generator NativeGen.primitive
  let bool = Generator NativeGen.bool
  let nil = Generator NativeGen.unit
  let undefined = Generator NativeGen.undefined
  let notANumber = Generator NativeGen.NaN
  let number = Generator NativeGen.number
  let posNumber = Generator NativeGen.posNumber
  let negNumber = Generator NativeGen.negNumber
  let numberWithin min max = Generator <| NativeGen.numberWithin (min, max)
  let int = Generator NativeGen.int
  let posInt = Generator NativeGen.posInt
  let negInt = Generator NativeGen.negInt
  let sPosInt = Generator NativeGen.sPosInt
  let sNegInt = Generator NativeGen.sNegInt
  let intWithin min max = Generator <| NativeGen.intWithin (min, max)
  let string = Generator NativeGen.string
  let asciiString = Generator NativeGen.asciiString
  let alphaNumString = Generator NativeGen.alphaNumString
  let substring s = Generator <| NativeGen.substring s
  let char = Generator <| NativeGen.char
  let asciiChar = Generator <| NativeGen.asciiChar
  let alphaNumChar = Generator <| NativeGen.alphaNumChar
  let array g = Generator <| NativeGen.array (unpack g)
  let list g = array g |> map List.ofArray
  let arrayWithOptions o g = Generator <| NativeGen.arrayWithOptions (unpack g, o)
  let listWithOptions o g = arrayWithOptions o g |> map List.ofArray
  let tuple2 g1 g2 = Generator <| NativeGen.tuple2 (unpack g1, unpack g2)
  let tuple3 g1 g2 g3 = Generator <| NativeGen.tuple3 (unpack g1, unpack g2, unpack g3)
  let tuple4 g1 g2 g3 g4 = Generator <| NativeGen.tuple4 (unpack g1, unpack g2, unpack g3, unpack g4)
  let tuple5 g1 g2 g3 g4 g5 = Generator <| NativeGen.tuple5 (unpack g1, unpack g2, unpack g3, unpack g4, unpack g5)
  let uniqueArray g = Generator <| NativeGen.uniqueArray (unpack g)
  let uniqueList g = uniqueArray g |> map List.ofArray
  let uniqueArrayWithOptions o g = Generator <| NativeGen.uniqueArrayWithOptions (unpack g, o)
  let uniqueListWithOptions o g = uniqueArrayWithOptions o g |> map List.ofArray
  let uniqueArrayBy f g = Generator <| NativeGen.uniqueArrayBy (unpack g, f)
  let uniqueListBy f g = uniqueArrayBy f g |> map List.ofArray
  let uniqueArrayByWithOptions o f g = Generator <| NativeGen.uniqueArrayByWithOptions (unpack g, f, o)
  let uniqueListByWithOptions o f g = uniqueArrayByWithOptions o f g |> map List.ofArray
  let oneOf gs = Generator <| NativeGen.oneOf (gs |> List.map unpack |> Array.ofList)
  let sized f = Generator <| NativeGen.sized (f >> unpack)

module GeneratorBuilder =
  type GeneratorBuilder () =
    member __.Bind (ga, f) = Generator.bind f ga
    member __.Return x = Generator.unit x

let gen = GeneratorBuilder.GeneratorBuilder ()

[<Erase>]
type SyncSpec<'t> = SpecWrapper of (SpecContext -> 't)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SyncSpec =
  [<Emit("Promise.resolve()")>]
  let private _mkResolvedPromise (): Fable.Import.JS.Promise<unit> = jsNative
  let internal resolvedPromise = _mkResolvedPromise ()

  let internal run (SpecWrapper ma) t = ma t

  let create fn = SpecWrapper fn

  let createAssert a = create (fun t -> Assert.runSync t.Asserter a)

  let zero = create <| ignore

  let unit x = create <| fun _ -> x

  let bind ma f = create <| fun t ->
    let a = run ma t
    let mb = f a
    run mb t

  let plan n = create <| fun t -> t.Plan n

module Test =
  let create (name: string) (test: SyncSpec<unit>) =
    Test.create name <| fun t ->
      SyncSpec.run test (SpecContext t)
      SyncSpec.resolvedPromise

[<RequireQualifiedAccess>]
module SyncSpecBuilder =
  type SyncSpecBuilderImpl () =
    member __.Zero () = SyncSpec.zero
    member __.Return x = SyncSpec.unit x
    member __.Bind (ma, f) = SyncSpec.bind ma f
    member __.Bind (assert', f) = SyncSpec.bind (SyncSpec.createAssert assert') f

[<AutoOpen>]
module SpecBuilderInst =
  let prop = SyncSpecBuilder.SyncSpecBuilderImpl ()

module private Hijack =
  [<Emit("$0._checkPlanCount=$1")>]
  let _hack (context: obj, fn: unit -> unit) = jsNative
  [<Emit("$0.assertCount")>]
  let _assertCount (context: obj): int = jsNative
  [<Emit("$0.planCount||null")>]
  let _plannedCount (context: obj): int option = jsNative
  [<Emit("$0.assertCount=0")>]
  let _resetAssertCount (context: obj): unit = jsNative
  [<Emit("$0.assertCount=$1")>]
  let _setAssertCount (context: obj, n: int): unit = jsNative
  [<Emit("$0.planCount=undefined")>]
  let _resetPlannedCount (context: obj): unit = jsNative
  [<Emit("$0.planCount=$1")>]
  let _setPlannedCount (context: obj, n: int): unit = jsNative
  [<Emit("$0.sync=true")>]
  let _setSync (context: obj): unit = jsNative
  [<Emit("$0.call($1,$2)")>]
  let _call (fn: 'a -> 'b, this: obj, arg: 'a): 'b = jsNative

  let inline reset (context: obj) =
    _resetAssertCount context
    _resetPlannedCount context

  let inline hack (context: obj) (plan: int option ref) (assertions: int ref) =
    let fn () =
      let assertCount: int = _assertCount context
      let plannedCount: int option = _plannedCount context
      plan := plannedCount
      assertions := assertCount
      match plannedCount with
      | None -> reset context
      | Some c when c = assertCount -> reset context
      | Some c -> failwithf "Planned for %d assertions, ran %d" assertCount c
    _setSync context
    _hack (context, fn)

  let _hijack (fn: 'a -> 'b): ('a -> 'b) =
    let plan = ref None
    let assertions = ref 0

    fun arg ->
      let test = JsInterop.jsThis
      hack test plan assertions
      let result = _call (fn, test, arg)
      match !plan with
      | None   -> _setPlannedCount (test, !assertions)
      | Some x -> _setPlannedCount (test, x)
      _setAssertCount (test, !assertions)
      result

  [<Emit("Hijack._hijack($0)")>]
  let hijack (fn: 'a -> 'b): ('a -> 'b) = jsNative

[<RequireQualifiedAccess>]
module Prop =
  let create (name: string) (g: Generator<'a>) (fn: 'a -> SyncSpec<unit>) =
    Hijack.hijack (Check.check (Generator.unpack g, System.Func<_,_,_> (fun t a -> SyncSpec.run (fn a) (SpecContext t))))
    |> Fable.Import.Ava.Test.create name
