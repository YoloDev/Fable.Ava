module Fable.Import.Ava

open System
open Fable.Core
open Fable.Import.JS

// ava
//

[<Sealed; Erase>]
type TestContext internal () =
  [<Emit("$0.plan($1)")>]
  member x.Plan (n: int): unit = jsNative
  [<Emit("$0.pass({{$1}})")>]
  member x.Pass (?msg: string): unit = jsNative
  [<Emit("$0.fail({{$1}})")>]
  member x.Fail (?msg: string): unit = jsNative
  [<Emit("$0.truthy($1,{{$2}})")>]
  member x.Truthy (value: obj, ?msg: string): unit = jsNative
  [<Emit("$0.falsy($1,{{$2}})")>]
  member x.Falsy (value: obj, ?msg: string): unit = jsNative
  [<Emit("$0.true($1,{{$2}})")>]
  member x.True (value: bool, ?msg: string): unit = jsNative
  [<Emit("$0.false($1,{{$2}})")>]
  member x.False (value: bool, ?msg: string): unit = jsNative
  [<Emit("$0.is($1,$2,{{$3}})")>]
  member x.Is (value: 'a, expected: 'a, ?msg: string): unit = jsNative
  [<Emit("$0.not($1,$2,{{$3}})")>]
  member x.Not (value: 'a, expected: 'a, ?msg: string): unit = jsNative
  [<Emit("$0.deepEqual($1,$2,{{$3}})")>]
  member x.DeepEqual (value: 'a, expected: 'a, ?msg: string): unit = jsNative
  [<Emit("$0.notDeepEqual($1,$2,{{$3}})")>]
  member x.NotDeepEqual (value: 'a, expected: 'a, ?msg: string): unit = jsNative
  [<Emit("$0.throws($1,null,{{$2}})")>]
  member x.Throws (fn: (unit -> unit), ?msg: string): unit = jsNative
  [<Emit("$0.throws($1,null,{{$2}})")>]
  member x.Throws (promise: Promise<unit>, ?msg: string): Promise<unit> = jsNative
  [<Emit("$0.notThrows($1,{{$2}})")>]
  member x.NotThrows (fn: (unit -> unit), ?msg: string): unit = jsNative
  [<Emit("$0.notThrows($1,{{$2}})")>]
  member x.NotThrows (promise: Promise<unit>, ?msg: string): Promise<unit> = jsNative
  // TODO: Bug weird bug with regex
  //[<Emit("$0.regex($1,$2,{{$3}})")>]
  //member x.Regex (value: string, test: System.Text.RegularExpressions.Regex, ?msg: string): unit = jsNative
  [<Emit("$0.ifError($1,{{$2}})")>]
  member x.IfError (error: obj, ?msg: string): unit = jsNative
  [<Emit("$0.snapshot($1,{{$2}})")>]
  member x.Snapshot (error: obj, ?msg: string): unit = jsNative

type TestImpl = TestContext -> Promise<unit>

module Test =
  [<Import("default", from="ava"); Emit("$0($1)")>]
  let create' (impl: TestImpl): unit = jsNative

  [<Import("default", from="ava"); Emit("$0($1,$2)")>]
  let create (name: string) (impl: TestImpl): unit = jsNative

  [<Import("default", from="ava"); Emit("$0.serial($1)")>]
  let serial' (impl: TestImpl): unit = jsNative

  [<Import("default", from="ava"); Emit("$0.serial($1,$2)")>]
  let serial (name: string) (impl: TestImpl): unit = jsNative

  [<Import("default", from="ava"); Emit("$0.only($1)")>]
  let only' (impl: TestImpl): unit = jsNative

  [<Import("default", from="ava"); Emit("$0.only($1,$2)")>]
  let only (name: string) (impl: TestImpl): unit = jsNative

  [<Import("default", from="ava"); Emit("$0.skip($1)")>]
  let skip' (impl: TestImpl): unit = jsNative

  [<Import("default", from="ava"); Emit("$0.skip($1,$2)")>]
  let skip (name: string) (impl: TestImpl): unit = jsNative

  [<Import("default", from="ava"); Emit("$0.todo($1)")>]
  let todo (name: string): unit = jsNative

  [<Import("default", from="ava"); Emit("$0.failing($1)")>]
  let failing' (impl: TestImpl): unit = jsNative

  [<Import("default", from="ava"); Emit("$0.failing($1,$2)")>]
  let failing (name: string) (impl: TestImpl): unit = jsNative

  [<Import("default", from="ava"); Emit("$0.before($1)")>]
  let before' (impl: TestImpl): unit = jsNative

  [<Import("default", from="ava"); Emit("$0.before($1,$2)")>]
  let before (name: string) (impl: TestImpl): unit = jsNative
  
  [<Import("default", from="ava"); Emit("$0.after($1)")>]
  let after' (impl: TestImpl): unit = jsNative

  [<Import("default", from="ava"); Emit("$0.after($1,$2)")>]
  let after (name: string) (impl: TestImpl): unit = jsNative

  [<Import("default", from="ava"); Emit("$0.beforeEach($1)")>]
  let beforeEach' (impl: TestImpl): unit = jsNative

  [<Import("default", from="ava"); Emit("$0.beforeEach($1,$2)")>]
  let beforeEach (name: string) (impl: TestImpl): unit = jsNative
  
  [<Import("default", from="ava"); Emit("$0.afterEach($1)")>]
  let afterEach' (impl: TestImpl): unit = jsNative

  [<Import("default", from="ava"); Emit("$0.afterEach($1,$2)")>]
  let afterEach (name: string) (impl: TestImpl): unit = jsNative


// ava-check
//

[<Pojo>]
type SizeOptions = {
  size: int option
  minSize: int option
  maxSize: int option
}

[<Pojo>]
type CheckOptions = {
  times: int option
  maxSize: int option
  seed: int option
}

module SizeOptions =
  let size n = 
    if (n < 0) then failwith "size has to be a positive number"
    else { size = Some n; minSize = None; maxSize = None }
  let min n = 
    if (n < 0) then failwith "min has to be a positive number"
    else { size = None; minSize = Some n; maxSize = None }
  let max n = 
    if (n < 0) then failwith "max has to be a positive number"
    else { size = None; minSize = None; maxSize = Some n }
  let between min max = 
    if   (min < 0) then failwith "min has to be a positive number"
    elif (max < 0) then failwith "max has to be a positive number"
    elif (max <= min) then failwith "min has to be greater than max"
    else { size = None; minSize = Some min; maxSize = Some max }

[<Sealed; Erase>]
type NativeGenerator<'t> private () =
  /// Creates a new Generator which also sometimes generates null values.
  [<Emit("$0.nullable()")>]
  member gen.Nullable: NativeGenerator<'t option> = jsNative

  /// Creates a new Generator which generates non-empty values.
  /// 
  /// Examples of empty values are 0, "", null, [], and {}
  [<Emit("$0.notEmpty()")>]
  member gen.NotEmpty: NativeGenerator<'t> = jsNative

  /// Creates a new Generator which ensures that all values generated adhere to
  /// the given predicate function.
  ///
  /// For example, to create a Generator of any number except multiples of 5:
  ///
  /// ```
  /// var genAnythingBut5s = gen.int.suchThat(n => n % 5 !== 0)
  /// ```
  ///
  /// Note: Care is needed to ensure there is a high chance the predicate will
  /// pass, after ten attempts, an exception will throw.
  [<Emit("$0.suchThat($1)")>]
  member gen.SuchThat (fn: 't -> bool): NativeGenerator<'t> = jsNative

  /// Creates a new Generator that depends on the values of this Generator.
  [<Emit("$0.then($1)")>]
  member gen.Then<'u> (fn: 't -> NativeGenerator<'u>): NativeGenerator<'u> = jsNative

  /// Creates a new Generator which grows at a different scale.
  [<Emit("$0.scale($1)")>]
  member gen.Scale (fn: int -> int): NativeGenerator<'t> = jsNative

  /// Creates a new Generator which will never shrink.
  /// This is useful when shrinking is taking a long time or is not applicable.
  [<Emit("$0.neverShrink()")>]
  member gen.NeverShrink: NativeGenerator<'t> = jsNative

  /// Creates a new Generator which will always consider shrinking, even if the
  /// property passes (up to one additional level).
  [<Emit("$0.alwaysShrink()")>]
  member gen.AlwaysShrink: NativeGenerator<'t> = jsNative

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module NativeGenerator =
  // JS Primitives
  //

  /// Generates any JS value, including Arrays and Objects (possibly nested).
  [<Import("gen", from="ava-check"); Emit("$0.any")>]
  let any: NativeGenerator<obj> = jsNative

  /// Generates any primitive JS value:
  /// strings, numbers, booleans, null, undefined, or NaN.
  [<Import("gen", from="ava-check"); Emit("$0.primitive")>]
  let primitive: NativeGenerator<obj> = jsNative

  [<Import("gen", from="ava-check"); Emit("$0.boolean")>]
  let bool: NativeGenerator<bool> = jsNative

  [<Import("gen", from="ava-check"); Emit("$0.null")>]
  let unit: NativeGenerator<unit> = jsNative

  [<Import("gen", from="ava-check"); Emit("$0.undefined")>]
  let undefined: NativeGenerator<unit> = jsNative

  [<Import("gen", from="ava-check"); Emit("$0.NaN")>]
  let NaN: NativeGenerator<float> = jsNative

  // Numbers
  //

  /// Generates floating point numbers (including +Infinity, -Infinity, and NaN).
  [<Import("gen", from="ava-check"); Emit("$0.number")>]
  let number: NativeGenerator<float> = jsNative

  /// Generates only positive numbers (0 though +Infinity), does not generate NaN.
  [<Import("gen", from="ava-check"); Emit("$0.posNumber")>]
  let posNumber: NativeGenerator<float> = jsNative

  /// Generates only negative numbers (0 though -Infinity), does not generate NaN.
  [<Import("gen", from="ava-check"); Emit("$0.negNumber")>]
  let negNumber: NativeGenerator<float> = jsNative

  /// Generates a floating point number within the provided (inclusive) range.
  /// Does not generate NaN or +-Infinity.
  [<Import("gen", from="ava-check"); Emit("$0.numberWithin($1,$2)")>]
  let numberWithin (min: float, max: float): NativeGenerator<float> = jsNative

  /// Generator integers (32-bit signed) including negative numbers and 0.
  [<Import("gen", from="ava-check"); Emit("$0.int")>]
  let int: NativeGenerator<int> = jsNative

  /// Generates positive integers, including 0.
  [<Import("gen", from="ava-check"); Emit("$0.posInt")>]
  let posInt: NativeGenerator<int> = jsNative

  /// Generates negative integers, including 0.
  [<Import("gen", from="ava-check"); Emit("$0.negInt")>]
  let negInt: NativeGenerator<int> = jsNative

  /// Generates only strictly positive integers, not including 0.
  [<Import("gen", from="ava-check"); Emit("$0.sPosInt")>]
  let sPosInt: NativeGenerator<int> = jsNative

  /// Generates only strictly negative integers, not including 0.
  [<Import("gen", from="ava-check"); Emit("$0.sNegInt")>]
  let sNegInt: NativeGenerator<int> = jsNative

  /// Generates an integer within the provided (inclusive) range.
  /// The resulting Generator is not shrinkable.
  [<Import("gen", from="ava-check"); Emit("$0.intWithin($1,$2)")>]
  let intWithin (min: int, max: int): NativeGenerator<int> = jsNative

  // Strings
  //

  /// Generates strings of arbitrary characters.
  ///
  /// Note: strings of arbitrary characters may result in higher-plane Unicode
  /// characters and non-printable characters.
  [<Import("gen", from="ava-check"); Emit("$0.string")>]
  let string: NativeGenerator<string> = jsNative

  /// Generates strings of printable ascii characters.
  [<Import("gen", from="ava-check"); Emit("$0.asciiString")>]
  let asciiString: NativeGenerator<string> = jsNative

  /// Generates strings of only alpha-numeric characters: a-z, A-Z, 0-9.
  [<Import("gen", from="ava-check"); Emit("$0.alphaNumString")>]
  let alphaNumString: NativeGenerator<string> = jsNative

  /// Generates substrings of an original string (including the empty string).
  [<Import("gen", from="ava-check"); Emit("$0.substring($1)")>]
  let substring (original: string): NativeGenerator<string> = jsNative

  /// Generates arbitrary 1-byte characters (code 0 through 255).
  [<Import("gen", from="ava-check"); Emit("$0.char")>]
  let char: NativeGenerator<char> = jsNative

  /// Generates only printable ascii characters (code 32 through 126).
  [<Import("gen", from="ava-check"); Emit("$0.asciiChar")>]
  let asciiChar: NativeGenerator<char> = jsNative

  /// Generates only alpha-numeric characters: a-z, A-Z, 0-9.
  [<Import("gen", from="ava-check"); Emit("$0.alphaNumChar")>]
  let alphaNumChar: NativeGenerator<char> = jsNative

  // Collections: Arrays and Objects
  //

  [<Import("gen", from="ava-check"); Emit("$0.array($1)")>]
  let array (gen: NativeGenerator<'a>): NativeGenerator<'a array> = jsNative

  [<Import("gen", from="ava-check"); Emit("$0.array($1,$2)")>]
  let arrayWithOptions (gen: NativeGenerator<'a>, opts: SizeOptions): NativeGenerator<'a array> = jsNative

  [<Import("gen", from="ava-check"); Emit("$0.array([$1,$2])")>]
  let tuple2 (g1: NativeGenerator<'a>, g2: NativeGenerator<'b>): NativeGenerator<'a * 'b> = jsNative

  [<Import("gen", from="ava-check"); Emit("$0.array([$1,$2,$3])")>]
  let tuple3 (g1: NativeGenerator<'a>, g2: NativeGenerator<'b>, g3: NativeGenerator<'c>): NativeGenerator<'a * 'b * 'c> = jsNative

  [<Import("gen", from="ava-check"); Emit("$0.array([$1,$2,$3,$4])")>]
  let tuple4 (g1: NativeGenerator<'a>, g2: NativeGenerator<'b>, g3: NativeGenerator<'c>, g4: NativeGenerator<'d>): NativeGenerator<'a * 'b * 'c * 'd> = jsNative

  [<Import("gen", from="ava-check"); Emit("$0.array([$1,$2,$3,$4,$5])")>]
  let tuple5 (g1: NativeGenerator<'a>, g2: NativeGenerator<'b>, g3: NativeGenerator<'c>, g4: NativeGenerator<'d>, g5: NativeGenerator<'e>): NativeGenerator<'a * 'b * 'c * 'd * 'e> = jsNative

  [<Import("gen", from="ava-check"); Emit("$0.uniqueArray($1)")>]
  let uniqueArray (gen: NativeGenerator<'a>): NativeGenerator<'a array> = jsNative

  [<Import("gen", from="ava-check"); Emit("$0.uniqueArray($1,$2)")>]
  let uniqueArrayWithOptions (gen: NativeGenerator<'a>, opts: SizeOptions): NativeGenerator<'a array> = jsNative

  [<Import("gen", from="ava-check"); Emit("$0.uniqueArray($1,$2)")>]
  let uniqueArrayBy (gen: NativeGenerator<'a>, fn: 'a -> 'b): NativeGenerator<'a array> = jsNative

  [<Import("gen", from="ava-check"); Emit("$0.uniqueArray($1,$2,$3)")>]
  let uniqueArrayByWithOptions (gen: NativeGenerator<'a>, fn: 'a -> 'b, opts: SizeOptions): NativeGenerator<'a array> = jsNative

  // TODO: Figure out how to type object generators - likely using Map, or a new "magic wrapper erased" type

  // Generator Creators
  //

  [<Import("gen", from="ava-check"); Emit("$0.oneOf($1)")>]
  let oneOf (gens: NativeGenerator<'a> array): NativeGenerator<'a> = jsNative

  /// Creates a Generator which will always generate the provided value.
  [<Import("gen", from="ava-check"); Emit("$0.return($1)")>]
  let ret (x: 'a): NativeGenerator<'a> = jsNative

  /// Creates a Generator that relies on a size. Size allows for the "shrinking"
  /// of Generators. Larger "size" should result in a larger generated value.
  ///
  /// For example, `gen.int` is shrinkable because it is implemented as:
  /// ```
  /// var gen.int = gen.sized(size => gen.intWithin(-size, size))
  /// ```
  [<Import("gen", from="ava-check"); Emit("$0.sized($1)")>]
  let sized (fn: int -> NativeGenerator<'a>): NativeGenerator<'a> = jsNative

module Check =
  [<Import("check", from="ava-check"); Emit("$0($1,$2)")>]
  let check (g: NativeGenerator<'a>, fn: Func<TestContext, 'a, unit>): TestImpl = jsNative
  [<Import("check", from="ava-check"); Emit("$0($1,$2,$3)")>]
  let check2 (g1: NativeGenerator<'a>, g2: NativeGenerator<'b>, fn: Func<TestContext, 'a, 'b, unit>): TestImpl = jsNative
