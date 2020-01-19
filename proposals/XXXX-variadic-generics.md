# Variadic Generics

* Proposal: [SE-NNNN](https://github.com/apple/swift-evolution/blob/master/proposals/NNNN-name.md)
* Author(s): Andrea Tomarelli ([technicated](https://forums.swift.org/u/technicated/summary))
* Status: **[Awaiting review](#rationale)**
* Review manager: TBD

## Introduction
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

Variadic Generics is a feature listed in the [Generics Manifesto](https://github.com/apple/swift/blob/master/docs/GenericsManifesto.md) document as one of the major features that needs to be implemented to "complete" generics. It's also a feature that has come up in various threads on the [Swift Forums](https://forums.swift.org), like for example (in no particular order) in [[1]](https://forums.swift.org/t/emulating-variadic-generics-in-swift/20046), [[2]](https://forums.swift.org/t/map-sorting/21421/12), [[3]](https://forums.swift.org/t/variadic-generics/20320), [[4]](https://forums.swift.org/t/implementing-expressiblebytupleliteral-how-hard/21169), [[5]](https://forums.swift.org/t/pitch-function-builders/25167).

This document is kind of a follow up to a [previos one](https://github.com/austinzheng/swift-evolution/blob/az-variadic-generics/proposals/XXXX-variadic-generics.md) by [Austin Zheng](https://forums.swift.org/u/Austin) - "kind of" because I've read the document, but not too in depth - and this was intentional. I've never worked too much with variadic generics in my programming life and I wanted to start "fresh" and as unbiased as possible on the topic, to see if I could come up with new or interesting ideas; but it's also possible that this document ends up sharing a lot of similarities with Austin's! Obviously the information contained in the mentioned document and collected on the Swift Forums are going to influence what's presented there.

Swift-evolution thread: [Variadic Generics](https://forums.swift.org/t/variadic-generics/20320)

## Motivation
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

Today it is impossible or very difficult to express patterns related to an *unbounded amount* of generic types or values. Maybe the closest thing we have is *variadic functions*, but unfortunately such functions require all of their arguments to be of the *same concrete type*:
```swift
func variadicFn<Values: SomeProto>(values: Values...) { }

// This will raise a compile-time error if the concrete type of `v1`, `v2` and
// `v3` is different, even if all those types conform to `SomeProto`.
variadicFn(v1, v2, v3)
```
This means that in order to create functions (or types) that are generic over a different number of concrete types one must declare such types one by one:
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

// Duplicate up to a certain number of parameters
```
This is obviously very tedious and error-prone, and if something is changed in one place it has to be replicated in all the other definitions. Tools exist that ease this problem (e.g. [Sourcery](https://github.com/krzysztofzablocki/Sourcery)), but it is still desirable to have language support for this kind of features.

Let's take a look at some examples that are hard or impossible to "scale up" today with regualr Swift.

### Example 1: zip
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

Currently Swift has a `zip` [free function](https://github.com/apple/swift/blob/06f82a53b5da26b67a8fd9414d8f8877eca8a3e1/stdlib/public/core/Zip.swift#L45) that takes in any two `Sequence`s and return a new `Sequence` containing pairs of elements from the original ones:
```swift
func zip<Sequence1: Sequence, Sequence2: Sequence>(
  _ sequence1: Sequence1, _ sequence2: Sequence2
) -> Zip2Sequence<Sequence1, Sequence2> {
  return Zip2Sequence(sequence1, sequence2)
}
 ```
The problem here is that this function can only zip *two* sequences, and has to return a specific `Zip2Sequence` type that holds said two sequences. Users who want to zip three sequences have to create a new `Zip3Sequence` type and overload `zip`, and this leads to unnecessary code duplication:
```swift
struct Zip3Sequence<
  Sequence1: Sequence, 
  Sequence2: Sequence, 
  Sequence3: Sequence> {
  
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
With Variadic Generics only a single `ZipSequence<any number of sequences>` and a single `zip` function would need to exist.
\
\
Reference: [Zip.swift @ apple/swift](https://github.com/apple/swift/blob/06f82a53b5da26b67a8fd9414d8f8877eca8a3e1/stdlib/public/core/Zip.swift#L45)

### Example 2: combineLatest
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

Reactive programming [[?](https://gist.github.com/staltz/868e7e9bc2a7b8c1f754)] libraries generally have a `combineLatest` function that takes multiple streams of data (`Observable`s, `Signal`s, etc.) and *combine* their *latest* outputs in a single value, eventually transformed by a function:
```swift
// Please note that this example will contain types "inspired" from RxSwift

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
}

class Observable<Element>: ObservableType { }

class Producer<Element>: Observable<Element> { }

class CombineLatest2<[...], R>: Producer<R> { }
class CombineLatest3<[...], R>: Producer<R> { }
[...]
class CombineLatest8<[...], R>: Producer<R> { }
```
RxSwift uses code generation to avoid writing all the duplicate code, but the core of the problem still remains. This example is also similar to the `zip` one, but there's actually a difference: the function definition contains a closure whose "*shape*" depends on the number of generic parameters.
\
\
Reference: [combineLatest @ RxSwift](https://github.com/ReactiveX/RxSwift/blob/bd04ea59744c0887e7e6e7eace6bd13a070d0f09/RxSwift/Observables/CombineLatest+arity.tt#L22)
\
Reference: [combineLatest @ ReactiveSwift](https://github.com/ReactiveCocoa/ReactiveSwift/blob/7cc885eedd0739242d75f6f30ca4d7f497496806/Sources/Signal.swift#L1927)

### Example 3: variadic sorting
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

Let's imagine we have the following `Animal` struct and an `Array` of said `Animal`s:
```swift
struct Animal {
  let name: String
  let age: Int
  let weight: Double
}

var someAnimals: [Animal] = createZoo()
```
What if we want to sort this array by multiple properties? For whatever reason, we might like to do the following:
```swift
someAnimals.sort(by: \.name, \.age, \.weight)
```
In this way animals are sorted by name, and if their name is the same they are sorted by their age, and so on. But if we try to declare the function in the naïve way we get an error:
```swift
extension Array {
  func sort<T: Comparable>(_ sortProperties: KeyPath<Element, T>...) {
    [...]
  }
}

someAnimals.sort(\.name, \.age, \.weight)
// Apple Swift version 5.1.2 (swiftlang-1100.0.278 clang-1100.0.33.9)
// error: type of expression is ambiguous without more context
// someAnimals.sort(\.name, \.age, \.weight)
//                  ^~~~~~
```
This is because, as said in the introduction, parameters passed to variadic functions must *all resolve to the same concrete type*.

This example might again seem similar to the `zip` one. The difference in this case is that Variadic Generics are not used "directly" in the function signature, but are used instead to construct another type that depends on them i.e. `KeyPath<Element, T>`.
\
\
Reference: [Emulating variadic generics in Swift @ Swift Forums](https://forums.swift.org/t/emulating-variadic-generics-in-swift/20046)

### Example 4: SwiftUI
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

SwiftUI and its `ViewBuilder` [function builder](https://forums.swift.org/t/function-builders/25167) type suffer from the same problem. `ViewBuilder` has a fixed amount of `buildBlock` methods that take `C0`, `C1`, `C2` and so on subviews, leading again to code duplication and clutterness.

SwiftUI cannot avoid this problem using arrays or other collection types because it wants to propagate subview types into the return type of `buildBlock` in order to enable optimizations ([source](https://forums.swift.org/t/function-builders/25167/19)).

### Example 5: curry
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

A curried function is one that takes multiple arguments but one at a time - in other words a curried function takes the first argument and returns a new function taking the second argument and so on until all arguments are used and the result is returned.
```swift
func uncurriedFn<A, B, C, D>(a: A, b: B, c: C) -> D {
  // some computation using `a`, `b` and `c`
}

func curriedFn<A, B, C, D>(_ a: A) -> (B) -> (C) -> D {
  return { b in { c in /* the same computation using `a`, `b` and `c` */ } }
}
```
*Currying* is the process of transforming a function of `n` parameters into a curried function. From [Wikipedia](https://en.wikipedia.org/wiki/Currying): "currying is the technique of translating the evaluation of a function that takes multiple arguments into evaluating a sequence of functions, each with a single argument":
```swift
func curry<A, B, C>(_ f: @escaping (A, B) -> C) -> (A) -> (B) -> C {
  return { a in { b in f(a, b) } }
}

curry(+)(1)(2) // prints 3
curry(+)("Hello")("World") // prints "HelloWorld"
curry(min)(10)(20) // prints 10
```
Unfortunately, at the moment one `curry` function must exist for each possible input function, up to some predetermined arity:
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
This example is interesting because a generic `curry` function would have to be kind of recursive. While writing this document I'm really not sure if Variadic Generics alone can be used to create such a complex function, but sure this is an interesting example to consider.
\
\
Reference: [Curry Library @ thoughtbot/Curry](https://github.com/thoughtbot/Curry)

## Proposed solution
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

Enter Variadic Generics. Let's define a **Variadic Generic** as a generic parameter that can refer to *multiple instances* of *different types*, all eventually conforming to the parameter definition.

This means, for example, that if there exists a function parametrised by a Variadic Generic conforming to the `Collection` protocol you can pass to this function an `Array`, a `Dictionary` and a `String` - all toghether - because all these types conform to `Collection`.

To introduce Variadic Generics, let's take a look at how `zip` could be reimplemented using them.

A Variadic Generic is declared using the `variadic` keyword and uses the exact same syntax of "standard" generics:
```swift
struct ZipSequence<S1: Sequence, S2: Sequence, Ss: variadic Sequence> { }
//                                                 ^~~~~~~~
```
Inside of a generic context a Variadic Generic can be directly used as a type. A value whose type is a Variadic Generic is called a **variadic value**, and is a **collection** of *zero or more values* (more on this later) implementing the `Collection` protocol:
```swift
struct ZipSequence<S1: Sequence, S2: Sequence, Ss: variadic Sequence> {
  private let s1: S1
  private let s2: S2
  private let ss: Ss // used as a type here, in a property declaration...

  // ... and here in a function signature
  init(_ s1: S1, _ s2: S2, _ ss: Ss) {
    (self.s1, self.s2, self.ss) = (s1, s2, ss)
  }
}
```
Only variadic values of compatible type and shape can be assigned to each other, so the following is not valid:
```swift
struct DoubleVariadic<C1: variadic Collection, C2: variadic Collection> {
  let c1: C1
  let c2: C2

  init(c1: C1, c2: C2) {
    // `c1` and `c2` are both variadic values containing some amount of
    // `Collection`s, but they cannot be used interchangeably
    self.c1 = c2
    self.c2 = c1
  }
}

// Obviously the  assignemnt is not valid even if from the outside exactly the
// same variables are passed to the function
let arr1 = [1, 2, 3]
let arr2 = ["a", "b", "c"]
DoubleVariadic(c1: arr1, arr2, c2: arr1, arr2)
```
At the type level, accessing an associated type of a Variadic Generic returns a new Variadic Generic constrained to the definition of the associated type itself:
```swift
extension ZipSequence {
  struct Iterator {
    var baseStream1: S1.Iterator
    var baseStream2: S2.Iterator
    // The type of `otherStreams` is `variadic Iterator` and the concerte types
    // inside this wrapper type are going to be the `Iterator`s of every
    // `Sequence` in `SS`
    var otherStreams: SS.Iterator
    
    var reachedEnd: Bool = false

    init(_ i1: S1.Iterator, _ i2: S2.Iterator, _ ii: Ss.Iterator) {
      // `ii` can be assigned to `otherStreams` because their type is the same
      // Variadic Generic
      (self.baseStream1, self.baseStream2, self.otherStreams) = (i1, i2, ii)
    }
  }
}
```
Variadic Generics are not tuples, but are friends with them: tuples can be created by *unpacking* a variadic value together with other (optional) members. The postfix `...` syntax is used to perform this operation and can be used both at type and instance level. **`...` is *not* a postfix operator in this context!**
```swift
extension ZipSequence.Iterator: IteratorProtocol {
  // The result of `next` is a tuple containing an `Element` of `S1`, an
  // `Element` of `S2` and an element of each member of `Ss`
  func next() -> (S1.Element, S2.Element, Ss.Element...)? {
    if reachedEnd { return nil }

    // We are going to talk about `project` later, in the *detailed design*
    // section
    // Moreover, please note that a variadic value of optional elements can be
    // directly used in pattern matching expressions
    guard let e1 = baseStream1.next(),
          let e2 = baseStream2.next(),
          let es = otherStreams.project({ $0.next() }) else {
      reachedEnd = true
      return nil
    }

    return (e1, e2, es...)
  }
}
```
A variadic value can also be passed as the variadic argument of a variadic function (yay!). I mildly suggest the operation is again performed by using the `...` syntax to explicitly indicate what's happening (but here, much more than in the tuple context, the `...` can be mistaken for a postifx operator... suggestions are welcome):
```swift
extension ZipSequence: Sequence {
  func makeIterator() -> Iterator {
    // here we use an overload of `map` that does not return an Array, but a
    // new variadic value - as before, more on this later
    return Iterator(s1.makeIterator(),
                    s2.makeIterator(),
                    ss.map { $0.makeIterator() })
  }

  var underestimatedCount: Int {
    return Swift.min(s1.underestimatedCount,
                     s2.underestimatedCount,
                     ss.underestimatedCount...)
  }
}
```
As said before, variadic values of compatible type can be directly passed to one another; moreover Variadic Generics can also be specialized with no types at all:
```swift
func zip<S1: Sequence, S2: Sequence, Ss: variadic Sequence>(
  _ s1: S1, _ s2: S2, _ ss: Ss
) -> ZipSequence<S1, S2, Ss> {
  return ZipSequence(s1, s2, ss)
}

// Valid usages
zip(aPlainOldString, aDoubleIntTupleArray, sequence(first: nil) { _ in 42 }, myRandomSequence)
zip(anIntArray, aStringToDoubleDictionary) // `Ss` will contain no types at all

// Invalid usages `s1` and `s2` require that `zip` receives at least two parameters
zip(aSingleCollection)
zip()
```
What can be noted in this example is that the code is practically the same of the current, non-variadic `zip` implementation. The only real difference is that `zip` can now be used to zip any number of arbirtary and different sequences together!

## Detailed design
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

In this section we are going to see how Variadic Generics can be declared and used in various contexts.

In some examples we are going to use the following types:
```swift
protocol P1 { 
  var someMember: Any { get }
  func someFunction() -> Any
}
extension P1 {
  var someMember: Any { return self }
  func someFunction() -> Any { return self }
}

protocol P2 {
  associatedtype AT
  func getAssociatedValues() -> [AT]
}

extension Int: P1 {}
extension String: P1 {}

extension String: P2 {
  func getAssociatedValues() -> [Double] {
    return [0.42, 42, 42.42]
  }
}
```

### Declaring and using a Variadic Generic
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

```swift
// =============================================================================
// A Variadic Generic parameter is declared by marking it with the `variadic`
// keyword in the generic argument clause. All the `T`s and `U`s in the follow-
// ing code are Variadic Generics. They can be directly used as types without
// any special syntax.
//
// A Variadic Generic parameter can be specialized by passing a variable number
// of types to it, even zero, and the type system keeps each type separate in
// the generic signature:
//
// ```
// let s = sequence(state: someState, next: fnSomeStateToInt)
// type(of: zip(anArrayOfInt, aString, aDictionaryOfIntToString, s))
// // ZipSequence<[Int], String, [Int: String], UnfoldSequence<Int, SomeState>>
// ```
//
// When there is ambiguity in case of multiple Variadic Generics, the type sys-
// tem prefers to assign the maximum number of types to the first generic param-
// eter. The parameter name can be used to manually resolve the ambiguity, and
// for the happines of the compiler this concept may be generalized to all
// generic parameters even if this is not strictly needed. This new syntax does
// not allow the reordering of the generic parameters. The compiler is updated
// to show new and more detailed informations about generic parameters.
// =============================================================================

// With constraints
struct|class|enum SimpleVariadic<T: variadic P1> { }
// With multiple constraints and other generics
struct|class|enum ComplexVariadic<A, B, T: variadic P1, U: variadic P2> { }

// If a generic parameter is a Variadic Generic, the types that it contains are
// enclosed between angled brackets "< >"
let vg1: SimpleVariadic<Int, String, String>
// vg1: SimpleVariadic<T: <Int, String, String>>

// All types are compatible with `T`, `U` gets no love
let vg2: ComplexVariadic<Double, Int, Int, String, String, String>
// vg2: ComplexVariadic<A: Double, B: Int, T: <Int, String, String, String>, U: <>>

// The name of the generic parameter can be used to specify how the
// specialization should be done; note that in this case all the labels are
// specified for clarity but not all of them are really required
let vg3: ComplexVariadic<A: Double, B: Int, T: Int, String, U: String, String>
// vg3: ComplexVariadic<A: Double, B: Int, T: <Int, String>, U: <String, String>>

// -----------------------------------------------------------------------------

struct|class|enum AnotherComplexVariadic<A, B, T: variadic P2, U: variadic P1> { }

// `Int` does not conform to `P2`, so there is no ambiguity; still, one might
// want to use `T:` and `U:` in the specialization clause to render his/her
// intent clearer
let vg4: AnotherComplexVariadic<Double, Int, String, String, Int, String>
// vg4: AnotherComplexVariadic<A: Double, B: Int, T: <String, String>, U: <Int, String>>

// =============================================================================
// The following are valid usages and the Variadic Generic specialization is
// simply done with no types at all.
// =============================================================================

let vg5: ComplexVariadic<Double, Int>
// vg5: ComplexVariadic<A: Double, B: Int, T: <>, U: <>>

let vg6: SimpleVariadic<>
// vg6: SimpleVariadic<T: <>>

// Only `T` is empty, beacuse `U:` was specified right after the parameter for
// `B` and therefore `T:` was skipped
let vg7: ComplexVariadic<Double, Int, U: String>
// vg7: ComplexVariadic<A: Double, B: Int, T: <>, U: <String>>

// =============================================================================
// The following are instead *invalid* usages: the types do not conform to the
// requirements of the Variadic Generic parameter.
// =============================================================================

// `Double` does not conform to protocol `P1`
let vg8: SimpleVariadic<Int, String, Double>
//  error: type 'Double' does not conform to protocol 'P1'

// `String` and `Int` both conform to protocol `P1`, but again `Double` does not
let vg9: AnotherComplexVariadic<Int, String, U: String, Int, Double>
//                                                           ^~~~~~
//                                                            WRONG
//  error: type 'Double' does not conform to protocol 'P1'

// =============================================================================
// A Variadic Generic type (let's say `T`) can be used to specialize both stan-
// dard generics and other Variadic Generics.
//
// In the first case a "variadic version" of the enclosing type is created, such
// that the elements wrap, in order, every type contained in `T`.
//
// In the second case the operation can be thought as of "unpacking" `T`, taking
// the types inside it and passing them, one by one and in order, to the new
// Variadic Generic.
// =============================================================================

// A new type whose variadic parameter can contain anything
struct|class|enum AnyVariadic<T: variadic Any> { }

extension ComplexVariadic {
  // Remember: inside this scope `T` and `U` are Variadic Generics

  typealias SomeOptionals = Optional<T> // this works with sugar (`T?`), too!
  // `SomeOptionals` is a Variadic Generic `Optional<variadic P1>`

  // The elements of `SomeOptionals`, one by one, become the types passed to the
  // Variadic Generic parameter `T` of `AnyVariadic`. This is ok since
  // `AnyVariadic.T` is unconstrained.
  func awesomeFunc1(gimmeVg vg: AnyVariadic<SomeOptionals>) { }

  // All the types inside `T` and `U` are collected into a tuple (more on this
  // later), and that tuple becomes the first generic parameter of
  // `ComplexVariadic`. `T` will be the same as this type's `T`, and `U` will
  // contain no parameters at all.
  func awesomeFunc2(gimmeVg vg: ComplexVariadic<(T..., U...), Int, T>) { }

  // This is not valid since elements of `U` conform to `P2` but elements of
  // `SimpleVariadic.T` must conform to `P1`, and `P1` and `P2` are not related
  // in any way...
  // func notSoAwesomeFunc(gimmeVg vg: SimpleVariadic<U>) { }
}

vg3.awesomeFunc1
// (AnyVariadic<T: <Int?, String?>>) -> Void
vg3.awesomeFunc2
// (ComplexVariadic<A: (Int, String, String, String), B: Int, T: <Int, String>, U: <>>) -> Void
```

### Using a Variadic Generic as a type
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

```swift
// =============================================================================
// As said in the previous section, inside a Variadic Generic type (or function)
// the Variadic Generic Parameter can be used like any other type. Variables
// whose type is a Variadic Generic are called **variadic values**. If `T` is a
// Variadic Generic Parameter and `someVar` is declared like `var someVar: T`,
// the type of the value is shown as `variadic T`. The following code will show
// some examples of this.
// =============================================================================

func testingTypesAndAutocomplete<T1, T2: P1 & P2>(a_param: inout T1, b_param: inout T2) {
  // When typing `param` autocomplete will show:
  // `inout T1 | a_param`
  // `inout P1 & P2 | b_param` (and **not** `inout T2 | b_param`)
  param
}

struct TestingTypesAndAutocomplete<T1, T2: P1 & P2> {
  var a_prop: T1
  var b_prop: T2

  func test() {
    // When typing `prop` autocomplete will show:
    // `T1 | a_prop`
    // `P1 & P2 | b_prop` (and again **not** `T2 | b_prop`)
    prop
  }
}

struct VariadicType<Values: variadic Any, Constrained: variadic P1 & P2> {
  let values: Values
  let constrained: Constrained

  func test() {
    // When typing `val` autocomplete will show `variadic Values | values`
    val

    // When typing `con` autocomplete will show `variadic P1 & P2 | constrained`
    con
  }
}
```

### Variadic values
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

```
// =============================================================================
// Variadic values are somewhat "special", because both inside and outside of a
// generic context act like a `Collection` - or better *are* a `Collection`. The
// `Element` of this collection is the constraint of the Variadic Generic. In-
// side a generic context, the `...` syntax allows users to convert this collec-
// tion to a tuple whose shape and types will be the concrete types passed to
// the Variadic Generic parameter.
// =============================================================================

extension SimpleVariadic {
  var someTs: T { /* ... */ }

  func testCollection() -> (T...) {
    // All your `map`, `filter`, etc are here!
    let arr1: [String] = someTs.map { "\($0)" }
    let arr2: [P1] = someTs.filter { $0 is Int }
    let first: P1? = someTs.first
    
    return (someTs...)
  }
}

let v: SimpleVariadic<Int, String> = /* ... */

// `v.som` shows `Collection | someTs`
v.som

for elem in v.someTs {
  print(type(of: elem))
}
// prints `Int` and then `String`

print(v.testCollection())
// prints something like (42, "hello, world")

// =============================================================================
// The `...` syntax allows users to add any other type (or value) to the tuple,
// and multiple variadic values can be "joined" together.
// =============================================================================

extension SimpleVariadic {
  var repeatedTs: (T... , Int, T...) {
    return (someTs... , 999, someTs...)
  }
  
  func takeTupleParameter(tuple: (Int, T...)) { }
}

print(v.repeatedTs)
// prints something like (42, "hello, world", 999, 42, "hello, world")

let myInt = 42
v.takeTupleParameter(tuple: (myInt, 999, "my string"))
```

### Declaring and using Variadic Generic functions
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

```swift
// =============================================================================
// Like for types, Variadic Generics in functions are declared using the
// `variadic` keyword. Unlike variadic function parameters though, Variadic
// Generics Parameters do not want the `...` syntax - an error is issued if such
// syntax is used.
// =============================================================================

func variadicGenericFunction<T: variadic Any>(ts: T) { }
func nonVariadicGenericFunction(ts: Any...) { }

// The following code will cause a compile-time error like "Variadig generic
// argument does not need `...`. Remove it." - a fixit can also be suggested
func wrongVariadicGenericFunction<T: variadic Any>(ts: T...) { }

// =============================================================================
// The two functions declared above are somewhat equivalent from the outside be-
// cause `T` was not constrained in any way. The only difference is that in the
// second function `ts` is of type `[Any]` while in the first function is of
// type `variadic Any`.
//
// In the following snippet we instead actually have a difference: the second
// function can inded accept parameters of any type, but only if all of them are
// of the **same** type! The first function does not have this limitation.
// =============================================================================

func variadicGenericFunction<T: variadic Any>(ts: T) { }
func nonVariadicGenericFunction<T>(ts: T...) { }

// Valid call
variadicGenericFunction(ts: 1, 2, 3, [1, 2, 3])

// Invalid call
// error: cannot convert value of type '[Int]' to expected argument type 'Int'
nonVariadicGenericFunction(ts: 1, 2, 3, [1, 2, 3])

// =============================================================================
// The code below illustrates the differences between Variadic Generic / only
// variadic / variadic + standard generics functions; some can take zero argu-
// ments, some can take arguments of different types, some can take arguments of
// protocol type, some cannot do almost any of those things.
// =============================================================================

let anInt: Int = 42
let aString: String = "Hello, World!"
let anIntAsP1: P1 = 42
let aStringAsP1: P1 = "Hello, World!"

// The following function can:
//
// +-----------------------------------------+-----+
// |                     take zero arguments | YES |
// +-----------------------------------------+-----+
// |     take a variable number of arguments | YES |
// +-----------------------------------------+-----+
// |       take arguments of different types | YES |
// +-----------------------------------------+-----+
// | take arguments whose type is a protocol | NO  |
// +-----------------------------------------+-----+
// |       mantain concrete type information | YES |
// +-----------------------------------------+-----+
//
func variadicGenericFunction<T: variadic P1>(_ ts: T) { }

variadicGenericFunction()
variadicGenericFunction(anInt, anInt, anInt, anInt)
variadicGenericFunction(anInt, aString, anInt, aString)
// error: only concrete types can conform to protocols
// variadicGenericFunction(anIntP1, aStringP1)

// This function can:
//
// +-----------------------------------------+-----+
// |                     take zero arguments | YES |
// +-----------------------------------------+-----+
// |     take a variable number of arguments | YES |
// +-----------------------------------------+-----+
// |       take arguments of different types | YES |
// +-----------------------------------------+-----+
// | take arguments whose type is a protocol | YES |
// +-----------------------------------------+-----+
// |       mantain concrete type information | NO  |
// +-----------------------------------------+-----+
//
func variadicNonGenericFunction(_ ts: P1...) { }

variadicNonGenericFunction()
variadicNonGenericFunction(anInt, anInt, anInt, anInt)
variadicNonGenericFunction(anInt, aString, anInt, aString)
variadicNonGenericFunction(anIntP1, aStringP1)

// This function can:
//
// +-----------------------------------------+-----+
// |                     take zero arguments | NO  |
// +-----------------------------------------+-----+
// |     take a variable number of arguments | YES |
// +-----------------------------------------+-----+
// |       take arguments of different types | NO  |
// +-----------------------------------------+-----+
// | take arguments whose type is a protocol | NO  |
// +-----------------------------------------+-----+
// |       mantain concrete type information | YES |
// +-----------------------------------------+-----+
//
// Concrete type information can be mantained if the compiler decides to
// create a specialized version of the function
//
func variadicAndGenericFunction<P: P2>(_ ts: P...) { }

// error: generic parameter 'P' could not be inferred
// variadicAndGenericFunction()
variadicAndGenericFunction(anInt, anInt, anInt, anInt)
// error: cannot convert value of type 'String' to expected argument type 'Int'
// variadicAndGenericFunction(anInt, aString, anInt, aString)
// error: only concrete types can conform to protocols
// variadicAndGenericFunction(anIntP1, aStringP1)

// =============================================================================
// The `(T...)` syntax is also taken into account, allowing a tuple to be passed
// as a parameter to a Variadic Generic function. Two functions declarered with
// both `T` and `(T...)` syntax are different will participate in overload reso-
// lution.
// =============================================================================

func overloaded<T: variadic P1>(ts: T) { }
func overloaded<T: variadic P1>(ts: (T...)) { }

overloaded(ts: 1, 2, 3, "anything can go here") // calls first overload
overloaded(ts: (1, 2, 3, "anything can go here")) // calls second overload
// in both functions `T` will contain 3 `Int`s and a `String`

// =============================================================================
// A Variadic Generic can directly be used as the result type of a function, and
// it will be automatically considered a `Collection` in concrete code. The us-
// age of the `(T...)` syntax always makes the result of a function a tuple,
// even in generic contexts.
// =============================================================================

// ===============================================================================================
// ***************************************  YOU ARE HERE!  ***************************************
// ===============================================================================================

// Concrete users will get a `Collection`
// Generic users will get a variadic value
func collectionOfValues<T: variadic Any>(_ values: T) -> T {
  return values
}

// The result type of the function is some `Collection` so `forEach` is available
collectionOfValues(1, 2, "hello", [1, "hi"]).forEach {
  // `$0` is `Any`
  print(type(of: $0), $0)
}

// prints (`Int`, 1), (`Int`, 2), (`String`, "hello"), (`[Any]`, [1, "hi"])
 
// Concrete users will get a tuple
// Generic users will get a tuple, too
func explicitlyMakeTuple<variadic T>(_ values: T) -> (T...) {
  return (values...) // explicit conversion needed
}

func wrongExplicitlyMakeTuple<variadic T>(_ values: T) -> (T...) {
  // error: a Variadic Generic used as the return type of a function must use the
  // `(T...)` syntax (or something similar). A fix-it may be offered
  return values 
}

func makeTupleAddingUniverseAnswer<variadic T>(_ values: T) -> (Int, T...) {
  return (42, values...)
}

// =============================================================================
// Passing a variadic value to another variadic value does not require or allow
// the `...` syntax. A fix-it will suggest to remove the `...`.
// =============================================================================

func explicitlyMakeTupleWrapper<variadic T>(_ values: T) -> (T...) {
  return explicitlyMakeTuple(values)
}

func wrongExplicitlyMakeTupleWrapper<variadic T>(_ values: T) -> (T...) {
  // error: passing a variadic value as a Variadic Generic does not require the
  // `...` syntax. Remove it + fix-it.
  return explicitlyMakeTuple(values...)
}

func implicitlyMakeTupleWrapper<variadic T>(_ values: T) -> (T...) {
  // `implicitlyMakeTuple` returns a variadic value, so `(v...)` is needed!
  return (implicitlyMakeTuple(values)...)
}
```

### Accessing members of a variable of Variadic Generic type
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

```swift
// =============================================================================
// Common members of a variadic value or type can be directly accessed by dot,
// subscript or any other standard syntax. Any member that can be statically
// resolved is accessible.
// =============================================================================

func withUnconstrainedSequences<variadic Sequences: Sequence>(_ sequences: Sequences) {
  let iterators = sequences.makeIterator()
  // iterator: variadic IteratorProtocol

  let elements = iterators.next()
  // elements: variadic Sequence.Element?
}

// -----------------------------------------------------------------------------

func withConstrainedSequences<variadic Sequences: Sequence>(
  _ sequences: Sequences
) where Sequences.Element == Int {
  let iterators = sequences.makeIterator()
  // iterator: variadic IteratorProtocol

  let elements = iterators.next()
  // elements: variadic Int?

  let descriptions = elements.description
  // elements: variadic String?
}

// -----------------------------------------------------------------------------

func getFirstElements<variadic Sequences: Sequence>(
  _ sequences: Sequences
) -> (Sequences.Element?...) {
  // The result has to always be a tuple, so we remembered to use the `(v...)`
  // syntax here!
  return (sequences.makeIterator().next()...)
}

// =============================================================================
// Wow, the `(Sequences.Element?...)` bit is actually a bit tricky. Let's try to
// explain it bit by bit:
// 1) `Sequences` is a Variadic Generic parameter
// 2) `Sequences.Element` accesses the `Element` of every memeber of
//    `Sequences`, creating another variadic generic
// 3) `Sequences.Element?` creates a new variadic generic applying the sugar `?`
//    to every member of `Sequences.Element`
// 4) `(Sequences.Element?...)` transforms the variadic generic into a tuple
//    consisting of each and all of the variadic generic members, like it was
//    `(Sequences.0.Element?, Sequences.1.Element?, Sequences.2.Element?, ...)`
//
// In the next snippet we are going to see this function in action.
// =============================================================================

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

func reallyGetFirstElements<variadic Collections: Collection>(
  _ collections: Collections
) -> Collections.Element where Collections.Index == Int {
  return collections[0]
}

let elements = reallyGetFirstElements(
  [1, 2, 3],
  Array<Any>()
)
// elements: (Int, Any) - but this will cause a runtime failure!

// -----------------------------------------------------------------------------

func reallyGetFirstElements<variadic Collections: Collection>(
  _ collections: Collections
) -> (Collections.Element...) {
  return (collections[collections.indices.first!]...)
}

let elements = reallyGetFirstElements(
  [1, 2, 3],
  ["a": "Hello", "b": "World"],
  ["1", 2, "3"] as [Any]
)
// elements: (Int, (key: String, value: String), Any) = (1, (key: "a": value: "Hello"), "1")

// -----------------------------------------------------------------------------

// This is a super tricky (and contrived) example!
// Pro tip: look at the definition of `P2` and its conformance on `String`
func withAssociatedtypeResult<variadic Collections: Collection>(
  _ collections: Collections
) -> Collections.Element.AT? where Collections.Element: P2 {
  return collections[collections.indices.first!].getAssociatedValues().first
}

let elements = withAssociatedResult(
  ["1", "2", "3"],
  ["a": "Hello", "b": "World"].values
)
// elements: (Double?, Double?) = (0.42, 0.42)
```

### Pattern matching with Optional's
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

```swift
// =============================================================================
// Variadic values can can participate in `Optional`s pattern matching expres-
// sions. The pattern is valid if every item in the variadic value is an
// Optional and will match if and only if all the Optional's are `.some` (i.e.
// are not `nil`).
// =============================================================================

func optionalPatternMatching<variadic T>(_ ts: T?) {
  // If the following expressions do match, `x` will be a variadic value con-
  // taining all the unwrapped values of `ts`

  if let x = ts {
    // use `x` as a variadic value of non-Optional values here
  }

  if case .some(let x) = ts {
    // use `x` as a variadic value of non-Optional values here
  }

  if case let x? = ts {
    // use `x` as a variadic value of non-Optional values here  
  }
}

// =============================================================================
// Thanks to this feature users have the power to declare functions returning
// either an optional tuple or a tuple of Optional's.
// =============================================================================

func getTupleOfOptionals<variadic Collections: Collection>(
  _ collections: Collections
) -> (Collections.Element?...) {
  return (collections.first...)
}

func getOptionalTuple<variadic Collections: Collection>(
  _ collections: Collections
) -> (Collections.Element...)? {
  // Optional's pattern matching
  guard let result = collections.first else {
    return nil
  }
  
  return (result...)
}

getTupleOfOptionals(
  [1, 2, 3],
  ["a": "Hello", "b": "World"].values  
)
// (Int?, String?) = (1, "Hello")

getTupleOfOptionals(
  [Int](),
  ["a": "Hello", "b": "World"].values  
)
// (Int?, String?) = (nil, "Hello")

getOptionalTuple(
  [1, 2, 3],
  ["a": "Hello", "b": "World"].values  
)
// (Int, String)? = (1, "Hello")

getOptionalTuple(
  [Int](),
  ["a": "Hello", "b": "World"].values  
)
// (Int, String)? = nil
```

### \[YOU ARE HERE\] for ... in
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

```swift
// =============================================================================
// Members of a variadic value can be accessed one at a time by using the
// `for ... in` construct.
// =============================================================================

func useProto(p1: P1) { /* useful stuff with p1 */ }

struct ForIn<variadic T: P1> {
  let values: T

  func boringExample() {
    for v in values {
      // here, `v` is of type `P1` and `v.member` is `Any`(as declared in P1)
      print(v.member)
    }
  }

  func interestingExample() {   
    for v in values {
      // again, `v` is of type `P1`, so this is fully valid!
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
// I'm not very happy with this section, but this is the best I could come up
// with at the moment.
//
// It may be interesting to see if we can / want / should make variadic values
// conform to Collection! In that case we should consider how we can avoid con-
// fusing the methods of the `Collection` protocol as a conformance with that of
// the same name on the "shared supertype" of the variadic value (for example in
// the trivial case in which the variadic value if of type
// `variadic Cs: Collection`).
//
//
// `#length` returns an `Int` representing the number of members that the
// variadic type (or value) is holding.
//
// `#head` returns the first member of a variadic type (or value), or the empty
// tuple if the variadic type (or value) contains no elements.
//
// `#tail` returns a new variadic type (or value) containing all the members in-
// side a variadic type (or value) but the first.
//
// `#reduce` collects all the members of a variadic value into a single value.
// A mutating variant of this operation is also available (works like the
// `reduce(into:_:)` method of `Array`).
//
// `#ifempty` will evaluate the subsequent block of code only if the variadic
// type (of value) contains no elements. The `#endif` directive is required, and
// an `#else` directive is optional.
// =============================================================================

func countMembers<variadic T>(_ values: T) -> Int {
  return #length(values)
}

countMembers(1, 2, "Hello", ["W", "o", "r", "l", "d"]) // 4

// -----------------------------------------------------------------------------

func getFirstMember<variadic T>(_ values: T) -> #head(T) {
  return #head(values)
}

getFirstMember() // ()
getFirstMember(42) // 42
getFirstMember(["W", "o", "r", "l", "d"], "Hello?", 42) // ["W", "o", "r", "l", "d"]

// -----------------------------------------------------------------------------

// **Not** using the explicit tuple syntax for the return value
func getNonFirstMembers<variadic T>(_ values: T) -> #tail(T) {
  return #tail(values)
}

getNonFirstMember() // ()
getNonFirstMember(42) // ()
getNonFirstMember(["W", "o", "r", "l", "d"], "Hello?", 42) // ("Hello?", 42)

// -----------------------------------------------------------------------------

func reduceVariadic<variadic T: P2>(_ values: T) -> Double {
  return #reduce(values, 0) { (carry: Double, item: P2) -> Double in
    // Note that the `reduce` below is that of `Array`!
    return carry + item.getAssociatedValues().reduce(0, +)
  }
}

reduceVariadic("Hello", "World") // 169.68000000000001

// -----------------------------------------------------------------------------

func checkIfVariadicIsEmpty<variadic T>(_ values: T) -> Bool {
#ifempty values 
  return true
#else
  return false
#endif
}

checkIfVariadicIsEmpty(1, "2")
// false

checkIfVariadicIsEmpty()
// true

// =============================================================================
// The *Curry* example can then be resolved as follows. In the next snippet we
// are going to see recursive Variadic Generics! Except that this is not true,
// because at the moment the result type of the variadic `curry` function cannot
// be expressed.
//
// Anyhow, the idea should be that when calling an overloaded function the com-
// piler should prefer to choose the function that **does not** have Variadic
// Generic arguments (if possible).
// =============================================================================

func curry<A, B, Result>(_ fn: @escaping (A, B) -> Result) -> (A) -> (B) -> Result {
  return { a in { b in fn(a, b) } }
}

func curry<A, B, C, variadic D, Result>(_ fn: @escaping (A, B, C, D...) -> Result) -> ??? {
  return { a in
    curry { otherValues in
      fn(a, otherValues...)
    }
  }
}

curry(functionTakingFourParameters)(1)(2.0)("three")(["f", "o", "u", "r"])

// =============================================================================
// With the following hack, the result type of the second `curry` function might
// become expressible.
// =============================================================================

enum CurryHelper<variadic T, Result> {
#ifempty T
  typealias Fn = Result
#else
  typealias Fn = (#head(T)) -> CurryHelper<#tail(T), Result: Result>.Fn
#endif
}

// CurryHelper<A, B, C, R>.Fn
// CurryHelper<A, B, C, R>.Fn = (A) -> CurryHelper<B, C, Result: R>.Fn
//                                  -> (B) -> CurryHelper<C, Result: R>.Fn
//                                         -> (C) -> CurryHelper<Result: R>.Fn
//                                                -> R
// CurryHelper<A, B, C, R>.Fn = (A) -> (B) -> (C) -> R

func curry<A, B, C, variadic D, Result>(
  _ fn: @escaping (A, B, C, D...) -> Result
) -> CurryHelper<A, B, C, D..., Result>.Fn {
  return { a in
    curry { otherValues in
      fn(a, otherValues...)
    }
  }
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
//
// A => Int1
// B => Int2
// C => Int3
// D => Int4
// Result => Int5
//
// CurryHelper<Int1, Int2, Int3, Int4, Int5>.Fn
// (Int1) -> CurryHelper<Int2, Int3, Int4, Result: Int5>.Fn
// (Int1) -> (Int2) -> CurryHelper<Int3, Int4, Result: Int5>.Fn
// (Int1) -> (Int2) -> (Int3) -> CurryHelper<Int4, Result: Int5>.Fn
// (Int1) -> (Int2) -> (Int3) -> (Int4) -> CurryHelper<Result: Int5>.Fn
// (Int1) -> (Int2) -> (Int3) -> (Int4) -> Int5
```

### Grammar of Variadic Generics
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

**PLEASE NOTE THAT THIS SECTION IS NOT UP-TO-DATE!!!**

##### GRAMMAR OF A GENERIC PARAMETER CLAUSE

*generic-parameter-clause* → **<** generic-parameter-list **>** \
*generic-parameter-list* → generic-parameter-element | generic-parameter-element **,** generic-parameter-list \
*generic-parameter-element* → generic-parameter | variadic-generic-parameter \
*generic-parameter* → type-name \
*generic-parameter* → type-name **:** type-identifier \
*generic-parameter* → type-name **:** protocol-composition-type \
*variadic-generic-parameter* → **variadic** generic-parameter \

##### GRAMMAR OF A TUPLE TYPE

*tuple-type* → **(** **)** | **(** tuple-type-element **,** tuple-type-element-list **)** \
*tuple-type-element-list* → tuple-type-element | tuple-type-element **,** tuple-type-element-list \
*tuple-type-element* → element-name type-annotation | type | variadic-type-expansion \
*element-name* → identifier \
*variadic-type-expansion* → type **...** \

##### GRAMMAR OF A TUPLE EXPRESSION

*tuple-expression* → **(** **)** | **(** tuple-element **,** tuple-element-list **)** \
*tuple-element-list* → tuple-element | tuple-element **,** tuple-element-list \
*tuple-element* → expression | identifier **:** expression | variadic-value-expansion \
*variadic-value-expansion* → expression **...** \

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
