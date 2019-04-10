# Variadic Generics

* Proposal: [SE-NNNN](https://github.com/apple/swift-evolution/blob/master/proposals/NNNN-name.md)
* Author(s): Andrea Tomarelli ([technicated](https://forums.swift.org/u/technicated/summary))
* Status: **[Awaiting review](#rationale)**
* Review manager: TBD

## Introduction
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

Variadic Generics is a feature listed in the [Generics Manifesto](https://github.com/apple/swift/blob/master/docs/GenericsManifesto.md)
document as one of the major features that needs to be implemented to "complete"
generics. It's also a feature that has lately come up in various threads on the
[Swift Forums](https://forums.swift.org), like for example (in no particular
order) in [[1]](https://forums.swift.org/t/emulating-variadic-generics-in-swift/20046),
[[2]](https://forums.swift.org/t/map-sorting/21421/12), [[3]](https://forums.swift.org/t/variadic-generics/20320),
[[4]](https://forums.swift.org/t/implementing-expressiblebytupleliteral-how-hard/21169).

This document is kind of a follow up to a [previos one](https://github.com/austinzheng/swift-evolution/blob/az-variadic-generics/proposals/XXXX-variadic-generics.md)
by [Austin Zheng](https://forums.swift.org/u/Austin) - "kind of" because I've
read the document, but not too in depth. This was intentional: I've never worked
too much with variadic generics in my programming life and I wanted to start
"fresh" and as unbiased as possible on the topic, to see if I could come up with
new or interesting ideas; but it's also possible that this document ends up sharing
a lot of similarities with Austin's!
Obviously the information contained in the mentioned document and collected on
the Swift Forums are going to influence what's presented there.

Swift-evolution thread: [Variadic Generics](https://forums.swift.org/t/variadic-generics/20320)

## Motivation
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

Today it's impossible or very difficult to express patterns related to an
unbounded amount of generic types or values. The closest thing we have is
*variadic functions*, but unfortunately such functions require all of their
arguments to be of the same concrete type:
```swift
func variadicFn<Values : SomeProto>(values: Values...) { }

// This will raise a compile-time error if the concrete type of `v1`, `v2` and
// `v3` is different, even if all those types conform to `SomeProto`.
variadicFn(v1, v2, v3)
```
This means that in order to create functions (or types) that are generic over
different concrete types one must use the following trick:
```swift
struct Generic<A: SomeProto, B: SomeProto>
  where /* the same set of constraints for both A and B */ {

  var a: A
  var b: B

  [...]
}
struct Generic<A: SomeProto, B: SomeProto, C: SomeProto>
  where /* the same set of constraints for both A, B and C*/ {

  var a: A
  var b: B
  var c: C

  [...]
}
// Duplicate up to some point
```
This is obviously very tedious and error-prone, and if something is changed in
one place it has to be replicated in all the other definitions. Tools exist that
ease this problem ([Sourcery](https://github.com/krzysztofzablocki/Sourcery)),
but its still desirable to have language support for this kind of feature.

Let's take a look at some examples that are hard or impossible to "scale up"
today with Swift.

### Example 1: zip
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

Currently Swift has a `zip` [free function](https://github.com/apple/swift/blob/45429ffa2419472829607fcc1cbbdd3e1f751714/stdlib/public/core/Zip.swift#L45)
that takes in any two `Sequence`s and return a new `Sequence` containing pairs
of elements from the original ones:
```swift
func zip<Sequence1: Sequence, Sequence2: Sequence>(
  _ sequence1: Sequence1, _ sequence2: Sequence2
) -> Zip2Sequence<Sequence1, Sequence2> {
  return Zip2Sequence(sequence1, sequence2)
}
 ```
The problem here is that this function can only zip two sequences, and has to
return a specific type `Zip2Sequence` that holds said two sequences. Programmers
that want to zip three sequences have to create a new `Zip3Sequence` type and
overload `zip`, and this leads to a lot of code duplication:
```swift
struct Zip3Sequence<Sequence1: Sequence, Sequence2: Sequence, Sequence3: Sequence> {
  [...]
}

extension Zip3Sequence {
  struct Iterator {
    [...]
  }
}

func zip<Sequence1: Sequence, Sequence2: Sequence, Sequence3: Sequence>(
  _ sequence1: Sequence1, _ sequence2: Sequence2, _ sequence3: Sequence3
) -> Zip3Sequence<Sequence1, Sequence2, Sequence3> {
  return Zip3Sequence(sequence1, sequence2, sequence3)
}
```
With Variadic Generics only a single `ZipSequence<AnyNumOfSequences>` and a
single `zip` function would need to exist.
\
\
Reference: [Zip.swift @ apple/swift](https://github.com/apple/swift/blob/45429ffa2419472829607fcc1cbbdd3e1f751714/stdlib/public/core/Zip.swift#L45)

### Example 2: combineLatest
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

Reactive programming [[?](https://gist.github.com/staltz/868e7e9bc2a7b8c1f754)]
libraries generally have a `combineLatest` function that takes various streams
of data (`Observable`s, `Signal`s, etc) and *combine* their *latest* outputs in
a single value, eventually transformed by a function:
```swift
// This example contains types from RxSwift (more or less)

protocol ObservableType {
  /// The type of the element that this data stream produces
  associatedtype E
}

extension ObservableType {
  /// Merge two streams together into one single stream, by using the specified
  /// function, any time one of the input streams produces an element.
  func combineLatest<O1: ObservableType, O2: ObservableType>(
    _ o1: O1, _ o2: O2, transformation: (O1.E, O2.E) -> E
  ) -> Observable<E> {
    return CombineLatest2([...])
  }

  /// Merge three streams together into one single stream, by using the speci-
  /// fied function, any time one of the input streams produces an element.
  func combineLatest<O1: ObservableType, O2: ObservableType, O3: ObservableType>(
    _ o1: O1, _ o2: O2, _ o3: O3, transformation: (O1.E, O2.E, O3.E) -> E
  ) -> Observable<E> {
    return CombineLatest3([...])
  }

  // and so on up to some arity
  [...]
}

class Observable<Element>: ObservableType { }

class Producer<Element>: Observable<Element> { }

class CombineLatest2<[...], R>: Producer<R> { }
class CombineLatest3<[...], R>: Producer<R> { }
[...]
class CombineLatest8<[...], R>: Producer<R> { }
```
RxSwift uses code generation to avoid writing all the duplicate code, but the
core of the problem still remains. This example is also similar to the `zip`
one, but there's actually a difference: the function definition contains a
closure whose *shape depends on the number of generic parameters*.
\
\
Reference: [combineLatest @ RxSwift](https://github.com/ReactiveX/RxSwift/blob/master/RxSwift/Observables/CombineLatest+arity.tt#L22)
\
Reference: [combineLatest @ ReactiveSwift](https://github.com/ReactiveCocoa/ReactiveSwift/blob/master/Sources/Signal.swift#L1917)

### Example 3: variadic sorting
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

Let's imagine we have the following `Animal` struct and an `Array` of said
`Animal`s:
```swift
struct Animal {
  let name: String
  let age: Int
  let weight: Double
}

var someAnimals: [Animal] = giveMeAnArrayOfAnimals()
```
What if we want to sort this array by multiple properties? We might like to do
the following
```swift
someAnimals.sort(\.name, \.age, \.weight)
```
so that animals are sorted by name, and if the name is the same they are sorted
by their age, and so on. But if we try to declare the function in the naïve way
we get an error:
```swift
extension Array {
  func sort<T: Comparable>(_ sortProperties: KeyPath<Element, T>...) {
    [...]
  }
}

someAnimals.sort(\.name, \.age, \.weight)
// error: type of expression is ambiguous without more context
// someAnimals.sort(\.name, \.age, \.weight)
//                  ^~~~~~
```
This is because parameters passed to variadic functions must **all** resolve to
the *same* concrete type.

This example might again seem similar to the `zip` one, but is actually
different in that in this case Variadic Generics are not used "directly" in the
function signature, but instead to construct another type that depends on them
i.e. `KeyPath<Element, T>`.
\
\
Reference: [Emulating variadic generics in Swift @ Swift Forums](https://forums.swift.org/t/emulating-variadic-generics-in-swift/20046)

### Example 4: curry
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

A curried function is one that takes multiple arguments, like a "normal"
function would, but one at a time - in other words a curried function takes the
first argument and returns a new function taking the second argument and so on
until all arguments are used up.
```swift
func uncurriedFn<A, B, C, D>(a: A, b: B, c: C) -> D {
  // some computation using `a`, `b` and `c`
}

func curriedFn<A, B, C, D>(_ a: A) -> (B) -> (C) -> D {
  return { b in { c in /* the same computation using `a`, `b` and `c` */ } }
}
```
*Currying* is the process of transforming a function of `n` parameters into a
curried function. From [Wikipedia](https://en.wikipedia.org/wiki/Currying):
"currying is the technique of translating the evaluation of a function that
takes multiple arguments into evaluating a sequence of functions, each with a
single argument":
```swift
func curry<A, B, C>(_ f: @escaping (A, B) -> C) -> (A) -> (B) -> C {
  return { a in { b in f(a, b) } }
}

curry(+)(1)(2) // prints 3
curry(+)("Hello")("World") // prints "HelloWorld"
curry(min)(10)(20) // prints 10
```
Unfortunately, at the moment one `curry` function must exist for each possible
input function, up to some predetermined arity:
```swift
func curry<A, B, C>(_ f: @escaping (A, B) -> C) -> (A) -> (B) -> C {
  return { a in { b in f(a, b) } }
}

func curry<A, B, C, D>(_ f: @escaping (A, B, C) -> D) -> (A) -> (B) -> (C) -> D {
  return { a in { b in { c in f(a, b, c) } } }
}

[...]

func curry<A, B, [...], N>(_ f: @escaping (A, B, [...], M) -> N) -> (A) -> (B) -> [...] -> N {
  return { a in { b in [...] f(a, b, [...], n) }
}
```
This example is interesting because a generic `curry` function
\
Reference: [Curry Library @ thoughtbot/Curry](https://github.com/thoughtbot/Curry)

### Example 5
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

More examples are welcome.

## Proposed solution
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

Enter Variadic Generics. Let's define a **Variadic Generic** as a generic
parameter that can refer to *multiple instances* of *different types*, all
eventually conforming to the parameter definition.

This means, for example, that if there exists a function parametrised by a
Variadic Generic conforming to the `Collection` protocol you can pass to this
function an `Array`, a `Dictionary` and a `String` - all toghether - because
all these types conform to `Collection`.

Let's take a look at how the `zip` example might look like using Variadic
Generics, while exploring the basics of the new syntax.

Variadic Generics are declared using the `variadic` keyword and use the exact
same syntax of "standard" generics:
```swift
struct ZipSequence<S1: Sequence, S2: Sequence, variadic Ss: Sequence> { }
```
Inside of a generic context, a Variadic Generic parameter can be used as a type
like any other generic parameter, both for properties and function arguments:
```swift
struct ZipSequence<S1 : Sequence, S2 : Sequence, variadic Ss : Sequence> {
  private let s1: S1
  private let s2: S2
  private let ss: Ss

  init(_ s1: S1, s2: S2, ss: Ss) {
    (self.s1, self.s2, self.ss) = (s1, s2, ss)
  }
}
```
Common properties of a variadic type or value can be accessed in the standard
way. The result of this operation is a new set of types / values containing the
value of the accessed property:
```swift
extension ZipSequence {
  struct Iterator {
    var baseStream1: S1.Iterator
    var baseStream2: S2.Iterator
    var otherStreams: SS.Iterator

    var reachedEnd: Bool = false

    init(_ i1: S1.Iterator, _ i2: S2.Iterator, _ is: Ss.Iterator) {
      (self.baseStream1, self.baseStream2, self.otherStreams) = (i1, i2, is)
    }
  }
}
```
Variadic Generics and tuples are friends: you can create a tuple "unpacking" a
variadic type or value together with other types or values. The `...` syntax
makes the operation explicit:
```swift
extension ZipSequence.Iterator: IteratorProtocol {
  func next() -> (S1.Element, S2.Element, Ss.Element...)? { }
}
```
Optional pattern matching work on variadic values. The variable is bound only if
all the elements inside the variadic value are not-nil, and will itself be
itself a varidic value:
```swift
extension ZipSequence.Iterator: IteratorProtocol {
  func next() -> (S1.Element, S2.Element, Ss.Element...)? {
    if reachedEnd { return nil }

    guard let e1 = baseStream1.next(),
          let e2 = baseStream2.next(),
          let es = otherStreams.next() else {
      reachedEnd = true
      return nil
    }

    return (e1, e2, es...)
  }
}
```
A variadic value can also be passed as the variadic argument of a variadic
function. The operation must again be made explicit by using the `...` syntax to
indicate that the elements of the variadic values should be passed to the
function one by one:
```swift
extension ZipSequence: Sequence {
  func makeIterator() -> Iterator {
    return Iterator(s1.makeIterator(), s2.makeIterator(), ss.makeIterator())
  }

  var underestimatedCount: Int {
    return Swift.min(s1.underestimatedCount,
                     s2.underestimatedCount,
                     ss.underestimatedCount...)
  }
}
```
Variadic values of compatible type / shape can be directly passed to one
another:
```swift
func zip<S1 : Sequence, S2 : Sequence, variadic Ss : Sequence>(
  _ s1: S1, _ s2: S2, _ ss: Ss
) -> ZipSequence<S1, S2, Ss> {
  return ZipSequence(s1, s2, ss)
}

// Valid usages
zip(anIntArray, aStringToDoubleDictionary)
zip(aPlainOldString, aDoubleIntTupleArray, sequence(first: nil) { _ in 42 }, myRandomSequence)

// Invalid usages
zip(aSingleCollection)
zip()
```
The important thing to note in this example is that the code is practically the
same of the current `zip` implementation, the only difference being that `zip`
can now be used to zip any number of arbirtary and different sequences togheter.

## Detailed design
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

In this section we are going how Variadic Generics can be declared and used in
various contexts.

In some examples we are going to use the following types:
```swift
protocol P1 { 
  var member: Any { get }
  func function() -> Any
}
extension P1 {
  var member: Any { return self }
  func function() -> Any { return self }
}

protocol P2 {
  associatedtype AT
  func getAssociatedValues() -> [AT]
}

extension Int : P1 {}
extension String : P1 {}

extension String : P2 {
  func getAssociatedValues() -> [Double] {
    return [0.42, 42, 42.42]
  }
}
```

### Declaring and using a Variadic Generic type
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

```swift
// =============================================================================
// A Variadic Generic parameter is declared by marking it with the `variadic`
// keyword in the generic argument clause. All `T`s are "parameter packs" and
// can be directly used as types without any special syntax.
// In a concrete context a Variadic Generic parameter can be specialized by
// passing a variable number of types, and is seen as a *tuple* of types by the
// type system.
// If there is ambiguity in case of multiple Variadic Generics, the type system
// prefers to assign the maximum number of types to the first generic parameter.
// The parameter name can be used to manually resolve the ambiguity, and for the
// happines of the type checker this concept is generalized to all generic pa-
// rameters even if this is not strictly needed.
// =============================================================================

// Without constraints
struct|class|enum Variadic1<variadic T> { }
// With constraints
struct|class|enum Variadic2<variadic T : P1> { }
// With constraints and other generics
struct|class|enum Variadic3<A, B, variadic T : P1, variadic U : P2> { }

let vg1: Variadic1<Int, String, Double>
// vg1: Variadic1<(Int, String, Double)>

let vg2: Variadic2<Int, String, String>
// vg2: Variadic2<(Int, String, String)>

let vg3: Variadic3<Double, Int, Int, String, String, String>
// vg3: Variadic3<Double, Int, (Int, String, String, String), ()>

let vg4: Variadic3<A: Double, /* B: */ Int, /* T: */ Int, String, U: String, String>
// vg4: Variadic3<Double, Int, (Int, String), (String, String)>

// -----------------------------------------------------------------------------

// With constraints and other generics, no ambiguity
struct|class|enum Variadic4<A, B, variadic T : P2, variadic U : P1> { }

let vg5: Variadic4<Double, Int, String, String, Int, String>
// vg5: Variadic4<Double, Int, (String, String), (Int, String)>

// =============================================================================
// The following are valid usages and the Variadic Generic specialization is
// simply done with no types at all.
// =============================================================================

let vg6: Variadic3<Double, Int>
// vg6: Variadic3<Double, Int, (), ()>

let vg7: Variadic1<>
// vg7: Variadic1<()>

// =============================================================================
// The following are instead invalid usages: the passed types do not conform to
// the requirements of the Variadic Generic parameter.
// =============================================================================

// `Double` does not conform to protocol `P1`
// let vg8: Variadic2<Int, String, Double>

// `String` conforms to protocol `P1`, but `Double` does not
// let vg9: Variadic2<Int, String, String, Double>

// =============================================================================
// Variadic Generic types can be passes as specialization of both "standard"
// generics and other Variadic Generics. In the first case they will create a
// variadic version of the enclosing type, while in the second case the types
// are automatically "unpacked" and passed to the new Variadic Generic parame-
// ter.
// There exist an exception for the first case. When used to specialize a gener-
// ic type in a function signature, a variadic version of that type cannot be
// made because it doesn't make any sense. What can be done instead is using the
// `(T...)` syntax to explicitly transform the Variadic Generic into a tuple
// (more on this later).
// =============================================================================

extension Variadic3 {
  // Remember that inside this scope `T` and `U` are Variadic Generics

  typealias SomeOptionals = Optional<T> // this works with sugar, too

  func awesomeFunc1(gimmeVg vg: Variadic1<SomeOptionals>) { }

  func awesomeFunc2(gimmeVg vg: Variadic3<(T...), Int, U>) { }

  // This is not valid since elements of `Self.T` conform to `P2` but elements
  // of `Variadic2.T` conform to `P1`, and `P1` and `P2` are not related in any
  // way...
  //
  // func notSoAwesomeFunc(gimmeVg vg: Variadic2<T>) { }
}

type(of: vg4).SomeOptionals.self
// Variadic3<Double, Int, (Int, String), (String, String)>.SomeOptionals.Type = (Int?, String?)

vg4.awesomeFunc1 // (Variadic1<(Int, String)>) -> Void
vg4.awesomeFunc2 // (Variadic3<(Int, String), Int, (String, String), ()>) -> Void
```

### Using a Variadic Generic as a type
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

```swift
// =============================================================================
// Inside a Variadic Generic type or function, the Variadic Generic Parameter
// can be used as any other type. Variables declared of that type are somewhat
// "special" because they actually are an "aggregate" of values. If `T` is the
// Variadic Generic Parameter, the type of the variable is `variadic T`. Multi-
// ple Variadic Generic Parameters can be declared in the same scope.
// =============================================================================

struct VariadicType<variadic Values, variadic Constrained : P1 & P2> {
  let values: Values
  let constrained: Constrained

  func test() {
    // When typing `val` autocomplete will show `variadic Values | values` like
    // for an inout parameter it shows `inout Int | someInt`
    val

    // When typing `con` autocomplete will show `variadic P1 & P2 | constrained`
    con
  }
}

// =============================================================================
// As seen at the type level, values whose type is a Variadic Generic can be
// transformed into tuples using the `(v...)` syntax. Outside of a generic con-
// text these values are instead automatically seen as tuples.
// =============================================================================

extension VariadicType {
  func considerationAboutTuples() {
    let thisIsAnActualTuple = (values...)
  }
}

let v: VariadicType<Values: Int, Double, [Int], Constrained: String, String>

print(
  // `v.val` shows `(Int, Double [Int]) | values`
  v.val,
  // `v.con` shows `(String, String) | constrained`
  v.con
)
```

### Variadic value / type expansion
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

```swift
// =============================================================================
// Previous sections showed the usage the usage of the `(T...)` syntax. This
// syntax is actually based on another more basic syntax: `...`. This syntax can
// be used in some cases to expand the variadic value or type into the surround-
// ing context.
// When using this syntax to build a tuple, any number of additional elements
// can be added to the result.
// This syntax can also be used to pass a variadic value as the variadic argu-
// ment of a variadic function.
// =============================================================================

struct VariadicContext<variadic T> {
  let values: T

  func test() {
    let moreValues: (Int, T..., Int)
    let anInt = 42

    moreValues = (anInt, values..., anInt)

    // This will convert `values` to a tuple and print a single value
    print(values, separator: " ~ ")

    // This will expand the variadic value and print zero or more values
    print(values..., separator: " ~ ") 
  }
}
```

### Declaring and using Variadic Generic functions
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

```swift
// =============================================================================
// Like for types, Variadic Generics in functions are declared using the
// `variadic` keyword.
// Unlike variadic function parameters though, Variadic Generics Parameters do
// not want the `...` syntax - an error is issued if such syntax is used.
// =============================================================================

func variadicGenericFunction<variadic T>(ts: T) { }
func nonVariadicGenericFunction(ts: Any...) { }

// The following will cause a compile-time error like "Variadig generic argument
// does not need `...`. Remove it." - a fixit can also be suggested
//
// func wrongVariadicFunction<variadic T>(ts: T...) { }

// =============================================================================
// The two functions declared above are actually equivalent from the outside be-
// cause `T` was not constrained in any way. The only difference is that in the
// second function `ts` is of type `[Any]` while in the first function is of
// type `variadic T`.
// =============================================================================

variadicFunction1(1, "Hello", anArray, aSuperComplexDataType)
nonVariadicFunction1(1, "Hello", anArray, aSuperComplexDataType)

// =============================================================================
// The next functions are instead different: some can take zero arguments, some
// can take arguments of different types, some can take arguments of protocol
// type, some cannot do any of those things.
// =============================================================================

let anInt: Int = 42
let aString: String = "Hello, World!"
let anIntP1: P1 = 42
let aStringP1: P1 = "Hello, World!"

// This function can:
// - take zero arguments: √
// - take a variable number of arguments: √
// - take arguments of different types: √
// - take arguments whose type is a protocol: X
//
func variadicGenericFunction2<variadic T : P1>(_ ts: T) { }

variadicGenericFunction2()
variadicGenericFunction2(anInt, anInt, anInt, anInt)
variadicGenericFunction2(anInt, aString, anInt, aString)
// variadicGenericFunction2(anIntP1, aStringP1)

// This function can:
// - take zero arguments: √
// - take a variable number of arguments: √
// - take arguments of different types: ~ (you can pass different types, but they all become `P1`s)
// - take arguments whose type is a protocol: √
//
func nonVariadicGenericFunction2(_ ts: P1...) { }

nonVariadicGenericFunction2()
nonVariadicGenericFunction2(anInt, anInt, anInt, anInt)
nonVariadicGenericFunction2(anInt, aString, anInt, aString)
nonVariadicGenericFunction2(anIntP1, aStringP1)

// This function can:
// - take zero arguments: X
// - take a variable number of arguments: √
// - take arguments of different types: X
// - take arguments whose type is a protocol: X
//
func nonVariadicGenericFunction3<P : P1>(_ ts: P...) { }

// nonVariadicGenericFunction3()
nonVariadicGenericFunction3(anInt, anInt, anInt, anInt)
// nonVariadicGenericFunction3(anInt, aString, anInt, aString)
// nonVariadicGenericFunction3(anIntP1, aStringP1)

// =============================================================================
// Functions can be declared using the `(T...)` syntax in order to pass a tuple
// as a parameter. Two functions declarered with both `T` and `(T...)` syntax
// are different will participate in overload resolution.
// =============================================================================

func overloaded<variadic T : P1>(ts: T) { }
func overloaded<variadic T : P1>(ts: (T...)) { }

overloaded(ts: 1, 2, 3, "anything can go here") // calls first overload
overloaded(ts: (1, 2, 3, "anything can go here")) // calls second overload

// =============================================================================
// Note that the type checker will consider the tuple version as the most spe-
// cific one, so that if one day tuples can conform to protocols the following
// code will behave as illustrated.
// =============================================================================

extension Tuple: P1 { }

overloaded(ts: (anInt, aString, aDoule)) // calls second overload
overloaded(ts: (anInt, aString, aDoule), (aDoule, anArray)) // calls first overload

// =============================================================================
// A Variadic Generic cannot directly be used as the result type of a function.
// The `(T...)` syntax must be used in order to explicitly transform `T` in a
// tuple. A fix-it can help users to spot the mistake. When using this syntax
// other types can be added to the output tuple.
// The return value of such functions can directly be a variaduc value and it
// will automatically be converted into a tuple.
// =============================================================================

func makeTuple1<variadic T>(_ values: T) -> (T...) {
  return (values...) // explicitly converted into a tuple
}

func makeTuple2<variadic T>(_ values: T) -> (T...) {
  return values // automatically converted into a tuple
}

func wrongMakeTuple<variadic T>(_ values: T) -> T {
  return (values...)
}
// error: a Variadic Generic used as the return type of a function must use the
// `(T...)` syntax etc.

func makeTupleWithUniverseAnswer<variadic T>(_ values: T) -> (Int, T...) {
  return (42, values...)
}

// =============================================================================
// Passing a variadic value to an other variadic value does not require or allow
// the `...` syntax. A fix-it will suggest to remove the `...`.
// =============================================================================

func makeTupleProxy<variadic T>(_ values: T) -> (T...) {
  return makeTuple1(values)
}

func makeTupleProxyGoneWrong<variadic T>(_ values: T) -> (T...) {
  return makeTuple1(values...) // error + fix-it: remove `...`
}
```

### Accessing members of a variable of Variadic Generic type
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

```swift
// =============================================================================
// Common members of a `variadic T` type can be accessed by dot, subscript or
// any normal syntax. Any member that can be statically resolved can be used.
// =============================================================================

func unconstrainedSequences<variadic Sequences : Sequence>(_ sequences: Sequences) {
  let iterators = sequences.makeIterator()
  // iterator: variadic IteratorProtocol

  let elements = iterators.next()
  // elements: variadic Sequence.Element?
}

// -----------------------------------------------------------------------------

func constrainedSequences<variadic Sequences : Sequence>(_ sequences: Sequences) where Sequences.Element == Int {
  let iterators = sequences.makeIterator()
  // iterator: variadic IteratorProtocol

  let elements = iterators.next()
  // elements: variadic Int?

  let descriptions = elements.description
  // elements: variadic String?
}

// -----------------------------------------------------------------------------

func getFirstElements<variadic Sequences : Sequence>(_ sequences: Sequences) -> (Sequences.Element?...) {
  return sequences.makeIterator().next()
}

/**
 * Wow, the `(Sequences.Element?...)` bit is actually a bit tricky. Let's try to
 * explain it:
 * 1) `Sequences` is a variadic generic parameter
 * 2) `Sequences.Element` accesses the `Element` of every memeber of
 *    `Sequences`, creating another variadic generic
 * 3) `Sequences.Element?` creates a new variadic generic applying the sugar `?`
 *    to every member of `Sequences.Element`
 * 4) `(Sequences.Element?...)` transforms the variadic generic into a tuple
 *    consisting of each and all of the variadic generic members, like it was
 *    `(Sequences.0.Element?, Sequences.1.Element?, Sequences.2.Element?, ...)`
 */

let elements = getFirstElements(
  [1, 2, 3],
  ["a": "Hello", "b": "World"],
  sequence(first: 42.0) { _ in Double.random(in: 0...42) }
)
// elements: (Int?, (key: String, value: String)?, Double?) = (1, (key: "b": value: "World"), 42.0)

let elements = getFirstElements(
  Array<Int>(),
  ["a": "Hello", "b": "World"],
  sequence(state: 42.0) { _ -> Double? in return nil }
)
// elements: (Int?, (key: String, value: String)?, Double?) = (nil, (key: "a": value: "Hello"), nil)

// -----------------------------------------------------------------------------

func getFirstElements<variadic Cs : Collection>(_ cs: Cs) -> (Cs.Element...) where Cs.Index == Int {
  return collections[0]
}

let elements = getFirstElements(
  [1, 2, 3],
  Array<Any>()
)
// elements: (Int, Any) - but this will cause a runtime failure!

// -----------------------------------------------------------------------------

func getFirstElements<variadic Cs : Collection>(_ cs: Cs) -> (Cs.Element...) {
  return collections[collections.indices.first!]
}

let elements = getFirstElements(
  [1, 2, 3],
  ["a": "Hello", "b": "World"],
  Array<Any>()
)
// elements: (Int, (key: String, value: String), Any) - but this will cause a runtime failure!

// -----------------------------------------------------------------------------

func withAssociatedResult<variadic Cs : Collection>(_ cs: Cs) -> (Cs.Element.AT?...) where Cs.Element : P2 {
  return collections[collections.indices.first!].getAssociatedValues().first
}

let elements = withAssociatedResult(
  ["1", "2", "3"],
  ["a": "Hello", "b": "World"].values
)
// elements: (Double?, Double?) = (0.42, 0.42)

// -----------------------------------------------------------------------------

func getExplicitOptionalTuple<variadic Cs : Collection>(_ cs: Cs) -> (Cs.Element...)? where Cs.Index == Int {
  return nil
}

let thisIsNil = getExplicitOptionalTuple(
  [1, 2, 3],
  ["a": "Hello", "b": "World"].values
)
// thisIsNil : (Int, String)? = nil
```

### Optionals pattern matching
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

```swift
// =============================================================================
// Values whose type is a Variadic Generic can participate in optional pattern
// matching expressions.
// =============================================================================

func optionalPatternMatching<variadic T>(_ ts: T?) {
  // The following expressions will match iff all values in `ts` are `.some`. If
  // that's the case, `x` will be a variadic expression contain all the un-
  // wrapped values

  if let x = ts { }

  if case .some(let x) = ts { }

  if case let x? = ts { }
}

// The last example of the previous section can be then rewritten as:

func getExplicitOptionalTuple<variadic Cs : Collection>(_ cs: Cs) -> (Cs.Element...)? where Cs.Index == Int {
  guard let result? = cs.first else {
    return nil
  }

  return result
}

let thisIsNotNil = getExplicitOptionalTuple(
  [1, 2, 3],
  ["a": "Hello", "b": "World"].values
)
// thisIsNotNil : (Int, String)? = (1, "Hello")
```

### for ... in
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

```swift
// =============================================================================
// Members of a variadic value can be accessed one at a time by using the
// `for ... in` construct.
// =============================================================================

func useProto(p1: P1) { /* useful stuff with p1 */ }

struct ForIn<variadic T : P1> {
  let values: T

  func boringExample() {
    for v in values {
      // here, `v` is of type `P1` and `v.member` is `Any`(as declared in P1)
      print(v.member)
    }
  }

  func interestingExample() {   
    for v in values {
      // again, `v` is of type `P1`
      useProto(p1: v)
    }
  }
}
```

### #head, #tail, #length, #reduce, #ifempty
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

```swift
// =============================================================================
// Compile time helpers are available to work with variadic types and values.
// `#head` returns the first member of a variadic type (or value), or the empty
// tuple is the variadic type (or value) contains no elements.
// `#tail` returns a new variadic containing all the members of a variadic type
// (or value) but the first.
// `#length` returns an `Int` containing the number of members that the variadic
// type (or value) is currently holding.
// `#reduce` collects all the members of a variadic value into a single value.
// The mutating (`into`) variant of this operation is also available.
// `#ifempty` returns a boolean indicating wether the passed variadic type (or
// value) is either empty or contains any memeber.
//
// [... TODO: directly conform variadic types & values to Collection? ...]
// =============================================================================

func countMembers<variadic T>(_ values: T) -> Int {
  return #length(values)
}

countMembers(1, 2, "Hello", ["W", "o", "r", "l", "d"]) // 4


func getFirstMember<variadic T>(_ values: T) -> #head(T) {
  return #head(values)
}

getFirstMember() // ()
getFirstMember(42) // 42
getFirstMember(["W", "o", "r", "l", "d"], "Hello?", 42) // ["W", "o", "r", "l", "d"]


func getNonFirstMember<variadic T>(_ values: T) -> (#tail(T)...) {
  return #tail(values)
}

getNonFirstMember() // ()
getNonFirstMember(42) // ()
getNonFirstMember(["W", "o", "r", "l", "d"], "Hello?", 42) // ("Hello?", 42)


func reduceVariadic<variadic T : P2>(_ values: T) -> Double {
  return #reduce(values, 0) { (carry: Double, item: P2) -> Double in
    return carry + item.getAssociatedValues().reduce(0, +)
  }
}

reduceVariadic("Hello", "World") // 169.68000000000001

// =============================================================================
// The *Curry* example can then be revisited as follows.
// [... Recursive Variadic Generics ...]
//
// !!!  CURRENTLY THIS DOES NOT WORK BECAUSE THE `...` SYNTAX DOESN'T APPLY  !!!
// !!!                      TO TUPLES IN THIS DOCUMENT!                      !!!
// =============================================================================

struct CurryHelper<variadic T, Result> {
#ifempty(T)
  typealias Fn = Result
#else
  typealias Fn = (#head(T)) -> CurryHelper<#tail(T), Result: Result>.Fn
#endif
}

func curry<A, B, variadic C, Result>(_ fn: @escaping ((A, B, C...)) -> Result) -> CurryHelper<A, B, C..., Result>.Fn {
#ifempty(C)
  return { a in { b in fn(a, b) } }
#else
  return { first in
    curry { others in
      fn(first, others...)
    }
  }
#endif
}

typealias Int1 = Int
typealias Int2 = Int
typealias Int3 = Int
typealias Int4 = Int
typealias Int5 = Int

func sum4(a: Int1, b: Int2, c: Int3, d: Int4) -> Int5 {
  return a + b + c + d
}

curry(sum4)
// A => Int1
// B => Int2
// C => <Int3, Int4>
// Result => Int5
//
// CurryHelper<Int1, Int2, Int3, Int4, Int5>.Fn
// (Int1) -> CurryHelper<Int2, Int3, Int4, Result: Int5>.Fn
// (Int1) -> (Int2) -> CurryHelper<Int3, Int4, Result: Int5>.Fn
// (Int1) -> (Int2) -> (Int3) -> CurryHelper<Int4, Result: Int5>.Fn
// (Int1) -> (Int2) -> (Int3) -> (Int4) -> CurryHelper<Result: Int5>.Fn
// (Int1) -> (Int2) -> (Int3) -> (Int4) -> Int
```

<!---### Grammar of Variadic Generics--->
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

<!---##### GRAMMAR OF A GENERIC PARAMETER CLAUSE--->

<!---*generic-parameter-clause* → **<** generic-parameter-list **>** \
*generic-parameter-list* → generic-parameter-element | generic-parameter-element **,** generic-parameter-list \
*generic-parameter-element* → generic-parameter | variadic-generic-parameter \
*generic-parameter* → type-name \
*generic-parameter* → type-name **:** type-identifier \
*generic-parameter* → type-name **:** protocol-composition-type \
*variadic-generic-parameter* → **variadic** generic-parameter \--->

<!---##### GRAMMAR OF A TUPLE TYPE--->

<!---*tuple-type* → **(** **)** | **(** tuple-type-element **,** tuple-type-element-list **)** \
*tuple-type-element-list* → tuple-type-element | tuple-type-element **,** tuple-type-element-list \
*tuple-type-element* → element-name type-annotation | type | variadic-type-expansion \
*element-name* → identifier \
*variadic-type-expansion* → type **...** \--->

<!---##### GRAMMAR OF A TUPLE EXPRESSION--->

<!---*tuple-expression* → **(** **)** | **(** tuple-element **,** tuple-element-list **)** \
*tuple-element-list* → tuple-element | tuple-element **,** tuple-element-list \
*tuple-element* → expression | identifier **:** expression | variadic-value-expansion \
*variadic-value-expansion* → expression **...** \--->

## Impact on existing code
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

Describe the impact that this change will have on existing code. Will some
Swift applications stop compiling due to this change? Will applications still
compile but produce different behavior than they used to? Is it
possible to migrate existing Swift code to use a new feature or API
automatically?

## Alternatives considered

Describe alternative approaches to addressing the same problem, and
why you chose this approach instead.

-------------------------------------------------------------------------------

# Rationale

On [Date], the core team decided to **(TBD)** this proposal.
When the core team makes a decision regarding this proposal,
their rationale for the decision will be written here.
