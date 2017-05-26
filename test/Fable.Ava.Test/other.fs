module OtherTests

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import.Sinon
open Fable.Import.Ava
open Fable.Ava

let addTest name spec = Test.create name (Spec.toTest spec)

// This file is here mainly so that there is more than 1 test file.
spec {
  do! Spec.plan 1
  do! Assert.pass
} |> addTest "other test"