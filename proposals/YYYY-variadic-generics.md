# Variadic Generics

* Proposal: [SE-NNNN](https://github.com/apple/swift-evolution/blob/master/proposals/NNNN-name.md)
* Author(s): Andrea Tomarelli ([technicated](https://forums.swift.org/u/technicated/summary))
* Status: **[Awaiting review](#rationale)**
* Review manager: TBD

## Introduction
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

Variadic Generics is a feature listed in the [Generics Manifesto](https://github.com/apple/swift/blob/master/docs/GenericsManifesto.md) document as one of the major features that needs to be implemented to "complete" generics. It's also a feature that has lately come up in various threads on the [Swift Forums](https://forums.swift.org), like for example (in no particular order) in [[1]](https://forums.swift.org/t/emulating-variadic-generics-in-swift/20046), [[2]](https://forums.swift.org/t/map-sorting/21421/12), [[3]](https://forums.swift.org/t/variadic-generics/20320), [[4]](https://forums.swift.org/t/implementing-expressiblebytupleliteral-how-hard/21169).

This document is kind of a follow up to a [previos one](https://github.com/austinzheng/swift-evolution/blob/az-variadic-generics/proposals/XXXX-variadic-generics.md) by [Austin Zheng](https://forums.swift.org/u/Austin) - "kind of" because I've read the document, but not too in depth. This was intentional: I've never worked too much with variadic generics in my programming life and I wanted to start "fresh" and as unbiased as possible on the topic, to see if I could come up with new or interesting ideas; but it's also possible that this document ends up sharing a lot of similarities with Austin's! Obviously the information contained in the mentioned document and collected on the Swift Forums are going to influence what's presented there.

Swift-evolution thread: [Variadic Generics](https://forums.swift.org/t/variadic-generics/20320)

## Motivation
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

Today it's impossible or very difficult to express patterns related to an *unbounded amount* of generic types or values. Maybe the closest thing we have is *variadic functions*, but unfortunately such functions require all of their arguments to be of the *same concrete type*:
```swift
func variadicFn<Values : SomeProto>(values: Values...) { }

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

Currently Swift has a `zip` [free function](https://github.com/apple/swift/blob/45429ffa2419472829607fcc1cbbdd3e1f751714/stdlib/public/core/Zip.swift#L45) that takes in any two `Sequence`s and return a new `Sequence` containing pairs of elements from the original ones:
```swift
func zip<Sequence1: Sequence, Sequence2: Sequence>(
  _ sequence1: Sequence1, _ sequence2: Sequence2
) -> Zip2Sequence<Sequence1, Sequence2> {
  return Zip2Sequence(sequence1, sequence2)
}
 ```
The problem here is that this function can only zip two sequences, and has to return a specific type `Zip2Sequence` that holds said two sequences. Users who want to zip three sequences have to create a new `Zip3Sequence` type and overload `zip`, and this leads to unnecessary code duplication:
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
With Variadic Generics only a single `ZipSequence<AnyNumberOfSequences>` and a single `zip` function would need to exist.
\
\
Reference: [Zip.swift @ apple/swift](https://github.com/apple/swift/blob/45429ffa2419472829607fcc1cbbdd3e1f751714/stdlib/public/core/Zip.swift#L45)

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
RxSwift uses code generation to avoid writing all the duplicate code, but the core of the problem still remains. This example is also similar to the `zip` one, but there's actually a difference: the function definition contains a closure whose *shape* depends on the number of generic parameters.
\
\
Reference: [combineLatest @ RxSwift](https://github.com/ReactiveX/RxSwift/blob/master/RxSwift/Observables/CombineLatest+arity.tt#L22)
\
Reference: [combineLatest @ ReactiveSwift](https://github.com/ReactiveCocoa/ReactiveSwift/blob/master/Sources/Signal.swift#L1917)

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
someAnimals.sort(\.name, \.age, \.weight)
```
In this way animals are sorted by name, and if their name is the same they are sorted by their age, and so on. But if we try to declare the function in the naïve way we get an error:
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
This is because, as said in the introduction, parameters passed to variadic functions must **all** resolve to the *same* concrete type.

This example might again seem similar to the `zip` one. The difference in this case is that Variadic Generics are not used "directly" in the function signature, but are used instead to construct another type that depends on them i.e. `KeyPath<Element, T>`.
\
\
Reference: [Emulating variadic generics in Swift @ Swift Forums](https://forums.swift.org/t/emulating-variadic-generics-in-swift/20046)

### Example 4: curry
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

### Example 5
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

More examples are welcome.

## Proposed solution
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

Enter Variadic Generics. Let's define a **Variadic Generic** as a generic parameter that can refer to *multiple instances* of *different types*, all eventually conforming to the parameter definition.

This means, for example, that if there exists a function parametrised by a Variadic Generic conforming to the `Collection` protocol you can pass to this function an `Array`, a `Dictionary` and a `String` - all toghether - because all these types conform to `Collection`.

To introduce Variadic Generics, let's take a look at how `zip` could be reimplemented using them.

A Variadic Generic is declared using the `variadic` keyword and uses the exact same syntax of "standard" generics:
```swift
struct ZipSequence<S1: Sequence, S2: Sequence, variadic Ss: Sequence> { }
//                                             ^~~~~~~~
```
Inside of a generic context a Variadic Generic can be directly used as a type. A value whose type is a Variadic Generic is called a **variadic value**, and can be thought of as (*but it is not*) a tuple of variadic length:
```swift
struct ZipSequence<S1: Sequence, S2: Sequence, variadic Ss: Sequence> {
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
struct DoubleVariadic<variadic C1: Collection, variadic C2: Collection> {
  let c1: C1
  let c2: C2

  init(c1: C1, c2: C2) {
    // `c1` and `c2` are both variadic values containing some amount of
    // `Collection`s, but they are *not* compatible with each other
    self.c1 = c2
    self.c2 = c1
  }
}

// The assignemnt is not valid even if the type is constructed like this
let arr1 = [1, 2, 3]
let arr2 = ["a", "b", "c"]
DoubleVariadic(c1: arr1, arr2, c2: arr1, arr2)
```
Common properties of a variadic type or value can be accessed with standard Swift syntax (dot syntax, subscript, etc.). The result of this operation is a new variadic value containing the result of accessing the specified member on all the variadic value's elements:
```swift
extension ZipSequence {
  struct Iterator {
    var baseStream1: S1.Iterator
    var baseStream2: S2.Iterator
    // This accesses the `Iterator` of every sequence in `SS`
    var otherStreams: SS.Iterator

    var reachedEnd: Bool = false

    init(_ i1: S1.Iterator, _ i2: S2.Iterator, _ ii: Ss.Iterator) {
      (self.baseStream1, self.baseStream2, self.otherStreams) = (i1, i2, ii)
    }
  }
}
```
Variadic Generics are not tuples, but are friends with them: users can create a tuple by "unpacking" a variadic value together with other members (the presence of other members is optional). The postfix `...` syntax is used to perform this operation:
```swift
extension ZipSequence.Iterator: IteratorProtocol {
  // The result of `next` is a tuple containing an `Element` of `S1`, an
  // `Element` of `S2` and an element of each member of `Ss`, if it is not empty
  func next() -> (S1.Element, S2.Element, Ss.Element...)? { }
}
```
`Optional`s pattern matching work on variadic values, too. The result variable is bound only if all the elements inside the variadic value are not-`nil`, and will itself be a varidic value:
```swift
extension ZipSequence.Iterator: IteratorProtocol {
  func next() -> (S1.Element, S2.Element, Ss.Element...)? {
    if reachedEnd { return nil }

    // If `e1` is `nil`, `e2` is `nil`, or any member of `es` is `nil` the
    // `guard` barrier will not be passed!
    guard let e1 = baseStream1.next(),
          let e2 = baseStream2.next(),
          let es = otherStreams.next() else {
      reachedEnd = true
      return nil
    }

    // `es` was bound as a variadic value and can be unpacked inside a tuple
    return (e1, e2, es...)
  }
}
```
A variadic value can also be passed as the variadic argument of a variadic function (what a mouthful!). The operation is again performed by using the `...` syntax to indicate at the call site that the elements of the variadic values should be passed to the function one by one:
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
As said before, variadic values of compatible type can be directly passed to one another; moreover Variadic Generics can also be specialized with no types at all:
```swift
func zip<S1: Sequence, S2: Sequence, variadic Ss: Sequence>(
  _ s1: S1, _ s2: S2, _ ss: Ss
) -> ZipSequence<S1, S2, Ss> {
  return ZipSequence(s1, s2, ss)
}

// Valid usages
zip(aPlainOldString, aDoubleIntTupleArray, sequence(first: nil) { _ in 42 }, myRandomSequence)
zip(anIntArray, aStringToDoubleDictionary) // `Ss` will contain no types at all

// Invalid usages - `zip` requires at least two parameter, for `s1` and `s2`
zip(aSingleCollection)
zip()
```
What can be noted in this example is that the code is practically the same of the current, non-variadic `zip` implementation. Te only real difference is that `zip` can now be used to zip any number of arbirtary and different sequences togheter!

## Detailed design
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

In this section we are going to see how Variadic Generics can be declared and used in various contexts.

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

extension Int: P1 {}
extension String: P1 {}

extension String: P2 {
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
// keyword in the generic argument clause. All the `T`s and `U`s in the follow-
// ing code are Variadic Generics and can be tought of as "parameter packs".
// They can be directly used as types without any special syntax.
//
// In a **concrete** context a Variadic Generic parameter can be specialized by
// passing a variable number of types to it, and once specialized is seen as a
// *tuple* of types by the type system. This makes possible for concrete code to
// refer to a Variadic Generic parameter as a "normal" type, and even allows for
// specialization with no parameters at all (represented by an empty tuple).
// However, remember that this is only valid in a *concrete context*; in a
// generic context a Variadic Generic is **never** a tuple but what is called a
// *variadic value* (see also the *Using a Variadic Generic as a type* section).
//
// When there is ambiguity in case of multiple Variadic Generics, the type sys-
// tem prefers to assign the maximum number of types to the first generic param-
// eter. The parameter name can be used to manually resolve the ambiguity, and
// for the happines of the compiler this concept may be generalized to all
// generic parameters even if this is not strictly needed. This new syntax does
// not allow the reordering of the generic parameters.
// =============================================================================

// Without constraints
struct|class|enum SimpleVariadic<variadic T> { }
// With constraints
struct|class|enum VariadicWithConstraints<variadic T: P1> { }
// With constraints and other generics
struct|class|enum ComplexVariadic<A, B, variadic T: P1, variadic U: P2> { }

let vg1: SimpleVariadic<Int, String, Double>
// vg1: SimpleVariadic<(Int, String, Double)>

let vg2: VariadicWithConstraints<Int, String, String>
// vg2: VariadicWithConstraints<(Int, String, String)>

let vg3: ComplexVariadic<Double, Int, Int, String, String, String>
// vg3: ComplexVariadic<Double, Int, (Int, String, String, String), ()>

// Using the name of the generic parameter to specify how the specialization should be done
// Note that `A:` may be omitted because there is no ambiguity to resolve
let vg4: ComplexVariadic<A: Double, /* B: */ Int, /* T: */ Int, String, U: String, String>
// vg4: ComplexVariadic<Double, Int, (Int, String), (String, String)>

// -----------------------------------------------------------------------------

struct|class|enum AnotherComplexVariadic<A, B, variadic T: P2, variadic U: P1> { }

// `Int` does not conform to `P2`, so there is no ambiguity
// Still, the use of `T:` and `U:` in the specialization clause might render the intent clearer
let vg5: AnotherComplexVariadic<Double, Int, String, String, Int, String>
// vg5: AnotherComplexVariadic<Double, Int, (String, String), (Int, String)>

// =============================================================================
// The following are valid usages and the Variadic Generic specialization is
// simply done with no types at all.
// =============================================================================

let vg6: ComplexVariadic<Double, Int>
// vg6: ComplexVariadic<Double, Int, (), ()>

let vg7: SimpleVariadic<>
// vg7: SimpleVariadic<()>

// =============================================================================
// The following are instead *invalid* usages: the types do not conform to the
// requirements of the Variadic Generic parameter.
// =============================================================================

// `Double` does not conform to protocol `P1`
// let vg8: VariadicWithConstraints<Int, String, Double>

// `String` and `Ind` both conform to protocol `P1`, but again `Double` does not
// let vg9: VariadicWithConstraints<Int, String, String, Int, Double>
//                                               ^~~~~~~~~~~  ^~~~~~
//                                                THIS IS OK   WRONG

// =============================================================================
// A Variadic Generic type (say `T`) can only be used to specialize other
// Variadic Generics. The operation can be thought as of "unpacking" `T`, taking
// the types inside it and passing them, one by one, to the new Variadic
// Generic.
// =============================================================================

extension ComplexVariadic {
  // Remember that inside this scope `T` and `U` are Variadic Generics

  // The elements of `U`, one by one, become the types passed to the Variadic
  // Generic parameter `T` of `SimpleVariadic`. This is ok since `Variadic1.T`
  // is unconstrained.
  func awesomeFunc1(gimmeVg vg: SimpleVariadic<U>) { }

  // All the types inside `T` and `U` are collected into a tuple (more on this
  // later!), and that tuple becomes the first generic parameter of
  // `ComplexVariadic`. `T` will be the same as this type's `T`, and `U` will
  // contain no parameters at all.
  func awesomeFunc2(gimmeVg vg: ComplexVariadic<(T..., U...), Int, T>) { }

  // This is not valid since elements of `U` conform to `P2` but elements of
  // `VariadicWithConstraints.T` must conform to `P1`, and `P1` and `P2` are not
  // related in any way...
  // func notSoAwesomeFunc(gimmeVg vg: VariadicWithConstraints<U>) { }
}

vg4.awesomeFunc1 // (SimpleVariadic<(String, String)>) -> Void
vg4.awesomeFunc2 // (ComplexVariadic<(Int, String, String, String), Int, (Int, String), ()>) -> Void
```

### Using a Variadic Generic as a type
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

```swift
// =============================================================================
// Like said in the previous section, inside a Variadic Generic type or function
// the Variadic Generic Parameter can be used like any other type. Variables de-
// clared of that type are somewhat "special" because they actually are an *ag-
// gregate* of values. They are *like* tuples but **are not** tuples: these val-
// ues are instead what's called a **variadic value**. If `T` is a Variadic
// Generic Parameter and `someVar` is declared like `var someVar: T`, the type
// of the value is `variadic T`. The following code will show some examples of
// this.
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
    // `P1 & P2 | b_prop` (and **not** `T2 | b_prop`)
    prop
  }
}

struct VariadicType<variadic Values, variadic Constrained: P1 & P2> {
  let values: Values
  let constrained: Constrained

  func test() {
    // When typing `val` autocomplete will show `variadic Values | values`
    val

    // When typing `con` autocomplete will show `variadic P1 & P2 | constrained`
    con
  }
}

// =============================================================================
// Outside of a generic context, variadic values are automatically seen as
// tuples, the same ways as their type does. As said in the previous section,
// implicit conversion to a tuple is possible using the (v...) syntax (again,
// more on that later).
// =============================================================================

// `String` is the only type assignable to `P1 & P2`
// Using the parameter names as explained in the previous section!
let v: VariadicType<Values: Int, Double, [Int], Constrained: String, String> = ...

// `v.val` shows `(Int, Double, [Int]) | values`
v.val,
// `v.con` shows `(String, String) | constrained`
v.con
```

### Variadic value / type expansion
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

```swift
// =============================================================================
// Previous sections briefly showed the usage of the `(T...)` syntax. This syn-
// tax is used to *explicitly* transform a variadic type or value into a
// **tuple**. This might not be very useful all of itself, given that the type
// system automatically converts Variadic Generics and values into tuple in con-
// crete contexts.
//
// The real power of this feature is the possibility to combine and mix multiple
// types in the expression, even non-variadic ones! This makes it possible e.g.
// to easily express the result type of a function that takes a required parame-
// ter and a list of variadic ones, like the in the `zip` example.
//
// In reality, the foundation of this feature is the `...` syntax, which makes
// it possible to "splat" a variadic value in its surrounding context (when this
// is appropriate). This more basic syntax even allows users to pass the values
// contained in a variadic value to a (standard) variadic function.
// =============================================================================

struct VariadicContext<variadic T> {
  let values: T

  func test() {
    // `moreValues` is a tuple ready to contain an `Int`, all the types con-
    // tained in `T`, and then another `Int`
    let moreValues: (Int, T..., Int)
    let anInt = 42

    moreValues = (anInt, values..., anInt)

    // Not using the `...` syntax: the variadic value will be implicitly convert-
    // ed into a tuple and `print` will print a single value of tuple type
    print(values, separator: " ~ ")

    // Using the `...` syntax: the variadic value will be expanded and `print`
    // will print zero or more values depending on the composition of `T`
    print(values..., separator: " ~ ") 
  }
}

let v = VariadicContext(values: 42.42, "Hello")
// Inside `test`, `moreValues` will be of type `(Int, Double, String, Int)` and
// will contain `(42, 42.42, "Hello", 42)`
v.test()
// (42.42, "Hello")
// 42.42 ~ "Hello"
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

func variadicGenericFunction<variadic T>(ts: T) { }
func nonVariadicGenericFunction(ts: Any...) { }

// The following will cause a compile-time error like "Variadig generic argument
// does not need `...`. Remove it." - a fixit can also be suggested
// func wrongVariadicGenericFunction<variadic T>(ts: T...) { }

// =============================================================================
// The two functions declared above are actually equivalent from the outside be-
// cause `T` was not constrained in any way. The only difference is that in the
// second function `ts` is of type `[Any]` while in the first function is of
// type `variadic T`.
//
// In the following snippet we instead actually have a difference: the second
// function can inded accept parameters of any type, but only if all of them are
// of the **same** type! The first function does not have this limitation.
// =============================================================================

func variadicGenericFunction<variadic T>(ts: T) { }
func nonVariadicGenericFunction<T>(ts: T...) { }

// Valid call
variadicGenericFunction(ts: 1, 2, 3, [1, 2, 3])

// Invalid call
nonVariadicGenericFunction(ts: 1, 2, 3, [1, 2, 3])

// =============================================================================
// The code below illustrates the differences between Variadic Generic / only
// variadic / variadic + standard generics  functions; some can take zero argu-
// ments, some can take arguments of different types, some can take arguments of
// protocol type, some cannot do almost any of those things.
// =============================================================================

let anInt: Int = 42
let aString: String = "Hello, World!"
let anIntAsP1: P1 = 42
let aStringAsP1: P1 = "Hello, World!"

// This function can:
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
//
func variadicGenericFunction<variadic T: P1>(_ ts: T) { }

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
//
//
func nonVariadicNonGenericFunction(_ ts: P1...) { }

nonVariadicNonGenericFunction()
nonVariadicNonGenericFunction(anInt, anInt, anInt, anInt)
nonVariadicNonGenericFunction(anInt, aString, anInt, aString)
nonVariadicNonGenericFunction(anIntP1, aStringP1)

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
//
func nonVariadicGenericFunction<P: P2>(_ ts: P...) { }

// error: generic parameter 'P' could not be inferred
// nonVariadicGenericFunction()
nonVariadicGenericFunction(anInt, anInt, anInt, anInt)
// error: cannot convert value of type 'String' to expected argument type 'Int'
// nonVariadicGenericFunction(anInt, aString, anInt, aString)
// error: only concrete types can conform to protocols
// nonVariadicGenericFunction(anIntP1, aStringP1)

// =============================================================================
// The `(T...)` syntax is also taken in account, to allow a tuple to be passed
// as a parameter to a Variadic Generic function. Two functions declarered with
// both `T` and `(T...)` syntax are different will participate in overload reso-
// lution.
//
// The compiler will consider the tuple version as the most specific one when
// resolving the overload, so that if / when one day tuples can conform to pro-
// tocols code will behave as illustrated.
// =============================================================================

func overloaded<variadic T: P1>(ts: T) { }
func overloaded<variadic T: P1>(ts: (T...)) { }

overloaded(ts: 1, 2, 3, "anything can go here") // calls first overload
overloaded(ts: (1, 2, 3, "anything can go here")) // calls second overload

// Year 20XX
extension Tuple: P1 { }

overloaded(ts: (anInt, aString, aDoule)) // calls second overload
overloaded(ts: (anInt, aString, aDoule), (aDoule, anArray)) // calls first overload
overloaded(ts: (anInt, aString, aDoule) as P1) // explicit annotation, calls first overload

// =============================================================================
// A Variadic Generic can directly be used as the result type of a function, and
// it will be implicitly converted into a tuple in concrete code.
//
// The usage of the `(T...)` syntax always makes the result of a function a
// tuple, even in generic contexts. Moreover, automatic conversion between
// variadic values and tuples is *never* performed, and so the `(v...)` is re-
// quired when the result type is `(T...)`.
// =============================================================================

// Concrete users will get a tuple
// Generic users will get a variadic value
func implicitlyMakeTuple<variadic T>(_ values: T) -> T {
  return values
}

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

### \[YOU ARE HERE\] Accessing members of a variable of Variadic Generic type
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
