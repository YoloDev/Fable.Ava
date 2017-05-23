module Test

open Fable.Import.Ava
open Fable.Ava

spec {
  do! Assert.isTrue true <?> "true is true"
  do! Assert.isFalse false <?> "false is false"
  do! Assert.isTruthy true <?> "true should not be falsy :P"
} |> Test.create "test"