namespace Fable.Ava

open Fable.Core
open Fable.Import.Ava

[<Erase>]
type Spec<'t> = SpecWrapper of (ITestContext -> Async<'t>)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Spec =
  let internal run (SpecWrapper ma) t = ma t

  let create fn = SpecWrapper (fn >> async.Return)

  let createAsync fn = SpecWrapper fn

  let createAssert a = create (fun t -> Assert.run t a)

  let zero = create <| fun _ -> ()

  let unit x = create <| fun _ -> x

  let asyncUnit x = createAsync <| fun _ -> x
  
  let bind ma f = createAsync <| fun t -> async.Bind (run ma t, fun a -> run (f a) t)

[<RequireQualifiedAccess>]
module SpecBuilder =
  type SpecBuilderImpl () =
    member __.Zero () = Spec.zero
    member __.Return x = Spec.unit x
    member __.Return asyncX = Spec.asyncUnit asyncX
    member __.Bind (ma, f) = Spec.bind ma f
    member __.Bind (ma, f) = Spec.bind (Spec.createAssert ma) f
    member __.Run ma = Spec.run ma >> Async.StartAsPromise

[<AutoOpen>]
module SpecBuilderInst =
  let spec = SpecBuilder.SpecBuilderImpl ()