# Variadic Generics

* Proposal: [SE-NNNN](https://github.com/apple/swift-evolution/blob/master/proposals/NNNN-name.md)
* Author(s): Andrea Tomarelli ([technicated](https://forums.swift.org/u/technicated/summary))
* Status: **[Awaiting review](#rationale)**
* Review manager: TBD

## Introduction
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

Variadic Generics is a feature listed in the [Generics Manifesto](https://github.com/apple/swift/blob/master/docs/GenericsManifesto.md) document as one of the major features that needs to be implemented to "complete" generics. It's also a feature that has come up in various threads on the [Swift Forums](https://forums.swift.org), like for example (in no particular order) in [[1]](https://forums.swift.org/t/emulating-variadic-generics-in-swift/20046), [[2]](https://forums.swift.org/t/map-sorting/21421/12), [[3]](https://forums.swift.org/t/variadic-generics/20320), [[4]](https://forums.swift.org/t/implementing-expressiblebytupleliteral-how-hard/21169), [[5]](https://forums.swift.org/t/pitch-function-builders/25167).

This document is kind of a follow up to a [previous one](https://github.com/austinzheng/swift-evolution/blob/az-variadic-generics/proposals/XXXX-variadic-generics.md) by [Austin Zheng](https://forums.swift.org/u/Austin) - "kind of" because I've read the document, but not too in-depth - and this was intentional. I've never worked too much with variadic generics in my programming life and I wanted to start "fresh" and as unbiased as possible on the topic to see if I could come up with new or interesting ideas, but it's also possible that this document ends up sharing a lot of similarities with Austin's! The information contained in the mentioned document and collected on the Swift Forums are going to influence what's presented there.

Swift-evolution thread: [Variadic Generics](https://forums.swift.org/t/variadic-generics/20320)

## Motivation
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

Today it is impossible or very difficult to express patterns related to an *unbounded amount* of generic types or values. Maybe the closest thing we have is *variadic functions*, but unfortunately, such functions require all of their arguments to be of the *same concrete type*:
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

Let's take a look at some examples that are hard or impossible to "scale up" today with regular Swift.

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
In this way, animals are sorted by name, and if their name is the same they are sorted by their age, and so on. But if we try to declare the function in the naïve way we get an error:
```swift
extension Array {
  mutating func sort<T: Comparable>(by sortProperties: KeyPath<Element, T>...) {
    [...]
  }
}

someAnimals.sort(by: \.name, \.age, \.weight)
// Apple Swift version 5.1.2 (swiftlang-1100.0.278 clang-1100.0.33.9)
// error: type of expression is ambiguous without more context
// someAnimals.sort(by: \.name, \.age, \.weight)
//                      ^~~~~~
```
This is because, as said in the introduction, parameters passed to variadic functions must *all resolve to the same concrete type*. Vith Variadic Generics we might write:
```swift
extension Array {
  mutating func sort<variadic T: Comparable>(by sortProperties: variadic KeyPath<Element, T>) {
    // please don't judge me!
  
    this.sort {
      for sortProperty in sortProperties {
        if $0[keyPath: sortProperty] < $1[keyPath: sortProperty] {
          return true
        }
      }
      
      return false
    }
  }
}
```

This example might again seem similar to the `zip` one. The difference, in this case, is that Variadic Generics are not used "directly" in the function signature, but are used instead to construct another type that depends on them i.e. `variadic KeyPath<Element, T>`.
\
\
Reference: [Emulating variadic generics in Swift @ Swift Forums](https://forums.swift.org/t/emulating-variadic-generics-in-swift/20046)

### Example 4: SwiftUI
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

SwiftUI and its `ViewBuilder` [function builder](https://forums.swift.org/t/function-builders/25167) type suffer from the same problem. `ViewBuilder` has a fixed amount of `buildBlock` methods that take `C0`, `C1`, `C2` and so on subviews, leading again to code duplication and clutter.

SwiftUI cannot avoid this problem using arrays or other collection types because it wants to propagate subview types into the return type of `buildBlock` in order to enable optimizations ([source](https://forums.swift.org/t/function-builders/25167/19)).

### Example 5: curry \[TO BE UPDATED\]
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

A curried function is one that takes multiple arguments but one at a time - in other words, a curried function takes the first argument and returns a new function taking the second argument and so on until all arguments are used and the result is returned.
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
This example is interesting because a generic `curry` function would have to be kind of recursive. Probably Variadic Generics alone (as presented in this document as a *dynamic* feature) are not enough to implement such function, we might need some kind of preprocessor (*static* transformation).
\
\
Reference: [Curry Library @ thoughtbot/Curry](https://github.com/thoughtbot/Curry)

## Proposed solution
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

Enter Variadic Generics. Let's define a **Variadic Generic** as a generic parameter that can refer to *multiple instances* of *different types*, all eventually conforming to the parameter definition.

This means, for example, that if there exists a function parametrized by a Variadic Generic conforming to the `Collection` protocol you can pass to this function an `Array`, a `Dictionary` and a `String` - all together - because all these types conform to `Collection`.

To introduce Variadic Generics, let's take a look at how `zip` could be reimplemented using them.

A Variadic Generic is declared using the `variadic` keyword and uses the exact same syntax of "standard" generics:
```swift
struct ZipSequence<S1: Sequence, S2: Sequence, variadic Ss: Sequence> { }
//                                             ^~~~~~~~
```
Inside of a generic context, a Variadic Generic can be directly used as a type. A value whose type is a Variadic Generic is called a **variadic value**, and is a **collection** (not a `Collection`) of *zero or more values* (more on this later):
```swift
struct ZipSequence<S1: Sequence, S2: Sequence, variadic Ss: Sequence> {
  private let s1: S1
  private let s2: S2
  private let ss: Ss // `Ss` used as a type here, in a property declaration...

  // ... and here in a function signature
  init(_ s1: S1, _ s2: S2, _ ss: Ss) {
    (self.s1, self.s2, self.ss) = (s1, s2, ss)
  }
}
```
Only variadic values of compatible *type* and *shape* can be assigned to each other, so the following is not valid:
```swift
struct DoubleVariadic<variadic C1: Collection, variadic C2: Collection> {
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
Variadic Generics are not tuples, but are friends with them: tuples can be created by *unpacking* a variadic value together with other optional members. The postfix `...` syntax is used to perform this operation and can be used both at type and instance level. **Please note that `...` is *not* a postfix operator if used in this way!**
```swift
extension ZipSequence.Iterator: IteratorProtocol {
  // The result of `next` is a tuple containing an `Element` of `S1`, an
  // `Element` of `S2` and an element of each member of `Ss`
  func next() -> (S1.Element, S2.Element, Ss.Element...)? {
    if reachedEnd { return nil }

    // We are going to talk about `project` later, in the *Variaidc Value API*
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
A variadic value cannot be directly passed as an argument to a function, but must be explictly unpacked first with the `...` syntax. In this way, all the values contained in the pack are passed one by one to the function:
```swift
extension ZipSequence: Sequence {
  func makeIterator() -> Iterator {
    // Again, we are going to talk about `map` later, in the *Variadic Value
    // API* section
    return Iterator(s1.makeIterator(),
                    s2.makeIterator(),
                    ss.map({ $0.makeIterator() })...)
  }

  var underestimatedCount: Int {
    return Swift.min(s1.underestimatedCount,
                     s2.underestimatedCount,
                     ss.map({ $0.underestimatedCount })...)
  }
}
```
As said before, a variadic value must be unpacked to be passed as an argument to a function. Variadic Generics can also be specialized with no types at all:
```swift
func zip<S1: Sequence, S2: Sequence, variadic Ss: Sequence>(
  _ s1: S1, _ s2: S2, _ ss: Ss
) -> ZipSequence<S1, S2, Ss> {
  return ZipSequence(s1, s2, ss...)
}

// Valid usages
zip(aPlainOldString, aDoubleIntTupleArray, sequence(first: nil) { _ in 42 }, myRandomSequence)
zip(anIntArray, aStringToDoubleDictionary) // `Ss` will contain no types at all

// Invalid usages: `s1` and `s2` require that `zip` receives at least two parameters
zip(aSingleCollection)
zip()
```
What can be noted in this example is that the code is practically the same as the current, non-variadic `zip` implementation. The only real difference is that `zip` can now be used to zip any number of arbitrary and different sequences together!

## Detailed design
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

In this section, we are going to see how Variadic Generics can be declared and used in various contexts.

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
  
  mutating func aMutatingMethod() -> AT  
}

extension Int: P1 {}
extension String: P1 {}

extension String: P2 {
  func getAssociatedValues() -> [Double] {
    return [0.42, 42, 42.42]
  }
  
  mutating func aMutatingMethod() -> AT {
    self = "42"
    return 42
  }
}
```

### Variadic Generics at the type level
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

A Variadic Generic parameter is declared by marking it with the `variadic` keyword in the generic argument clause. All the `T`s in the following code are Variadic Generics. Aside from the `variadic` keyword, a Variadic Generic follows all the rules of "standard" generics.

```swift
// This is an unconstrained Variadic Generic. Because of this, types inside of
// `T` will expose the `Any` API.
//
struct Variadic1<variadic T> { }

// This is a constrained Variadic Generic. Because of this, types inside of `T`
// will expose the `P1` protocol API.
//
struct Variadic2<variadic T: P1> { }

// This is a Variadic Generic constrained to a protocol composition. Types
// inside of `T` will expose both the `P1` and the `P2` API. The associated type
// of `P2` was not constrained, so `T.AT` will expose the `Any` API.
//
struct Variadic3<variadic T: P1 & P2> { }

// This is a Variadic Generic constrained to a protocol composition. Types
// inside of `T` will expose both the `P1` and the `P2` API. The associated type
// of `P2` was constrained to `Numeric`, so `T.AT` will expose the `Numeric`
// API.
//
struct Variadic4<variadic T: P1 & P2> where T.AT: Numeric { }

// This is a Variadic Generic constrained to a protocol composition. Types
// inside of `T` will expose both the `P1` and the `P2` API. The associated type
// of `P2` was constrained to `Int`, so `T.AT` will expose exaclty the `Int`
// API.
//
struct Variadic5<variadic T: P1 & P2> where T.AT == Int { }
```

The same rules as above apply when declaring a Variadic Generic function or method.

---

A type with a Variadic Generic in its generic argument clause is called a **Variadic Generic Type**. A function with a Variadic Generic in its generic argument clause is called a **Variadic Generic function**.

Once a Variadic Generic (let's say `T`) has been declared, it can be used as-is as a type in every expression. In this context, `T` is called a **Variadic Type** (and please note that this is different from *Variadic Generic Type*).

The compiler shows the type of a Variadic Type in one of the following two ways, like it does for other "standard" generics and for `inout` parameters:
- `variadic <name-of-the-unconstrained-Variadic-Generic>`
- `variadic <constraints-of-the-Variadic-Generic>`

```swift
struct Variadic<variadic T: P1> {
  // A constant / variable declaration.
  //
  var t: T

  // In a function / method signature, as a prameter type and as the result
  // type.
  //
  func someFunction(t: T) -> T { ... }

  // As the specialization of a standard generic, creating a Variadic Type that
  // is a *variadic version* of the enclosing type. More on this later.
  //
  typealias VariadicOptionals = Optional<T>

  // As the specialization of another Variadic Generic, passing the types
  // enclosed in the Variadic Generic one by one. The two Variadic Generic
  // declarations must be compatible.
  //
  typealias Variadic1ofTs = Variadic1<T>
}

// -----------------------------------------------------------------------------
// Autocompletion for "standard" generics
// -----------------------------------------------------------------------------

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

// -----------------------------------------------------------------------------
// Autocompletion for `inout` parameters
// -----------------------------------------------------------------------------

func testingTypesAndAutocomplete<T1, T2: P1 & P2>(
  a_param: inout T1,
  b_param: inout T2) {
  
  // When typing `param` autocomplete will show:
  // `inout T1 | a_param`
  // `inout P1 & P2 | b_param` (and **not** `inout T2 | b_param`)
  param
}

// -----------------------------------------------------------------------------
// Autocompletion for Variadic Types
// -----------------------------------------------------------------------------

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
```

Where appropriate, the `...` syntax can be used to "unpack" the elements of a Variadic Generic type as a comma-separated list of types.

```swift
enum VariadicEnum<variadic T: P1> {
  // Converting a Variadic Generic type into a tuple type.
  //
  func someFunctionreturningATuple() -> (T...) { ... }

  // Converting a Variadic Generic type into a tuple type, with two additional
  // types packed in (before).
  //
  func someFunctionReturRningAnExtendedTuple<A, B>() -> (A, B, T...) { ... }

  // Converting a Variadic Generic type into a tuple type, with two additional
  // types packed in (after). Please note that no comma is needed after the
  // `...` syntax.
  //
  func anotherFunctionReturningAnExtendedTuple<A, B>() -> (T... A, B) { ... }

  // As the generic parameters of a type. `Inner` will have as many generic
  // parameters as the number of types passed to specialize `T`.
  //
  struct Inner<T...> { ... }

  // --------------------  enum cases  --------------------

  case none

  // Enum case taking several parameters.
  //
  case someThings(T...)

  // Enum case taking a single tuple - whose shape depends on the Variadic
  // Generic - as its only parameter.
  //
  case other((T...))
}
```

The compiler shows the concrete types bound to a Variadic Generic inside pointy brackets (i.e. `<` and `>`); the types **are not flattened into a single list**. The compiler is also modified to always show the name of all the generics parameters of a type, regardless of whether they are variadic or not.

```swift
struct MixedType<T, N: Numeric, variadic Ss: Sequence> { }

MixedType<String, Int, [Int], [String: Double]>.self
// MixedType<T: String, N: Int, SS: <[Int], [String: Double]>>.Type
//   = A<T: String, N: Int, Ss: <[Int], [String: Double]>>

// A Variadic Generic type can also be specialized with no types at all
MixedType<String, Int>.self
// MixedType<T: String, N: Int, SS: <>>.Type
//   2= MixedType<T: String, N: Int, SS: <>>
// MixedType<T: String, N: Int, SS: <>>.Type = MixedType<T: String, N: Int, SS: <>>
```

This pointy brackets stuff is not only a visual hint for the developer, but is actually needed in order to distinguish different specializations of the same generic type when the generics clause contains multiple Variadic Generics.

Moreover, when there are multiple possibile choices for matching concrete types to multiple Variadic Generics, the compiler prefers to assign the maximum number of types to the leftmost parameter. The name of the parameter can be used to manually resolve the ambiguity.

```swift
struct AmbiguousVariadic<variadic T: P1, variadic U: P1> { 
  init(t: T, u: U) { }
}

// -----------------------------------------------------------------------------
// Without pointy brackets (i.e. if we simply flattened the generic argument
// list) there will be no difference between this...
// -----------------------------------------------------------------------------

type(of: AmbiguousVariadic(
  t: 1, "hello",
  u: 2, "hello_2"))
// AmbiguousVariadic<Int, String, Int, String>.Type
//   = AmbiguousVariadic<Int, String, Int, String>

// -----------------------------------------------------------------------------
// ... and this:
// -----------------------------------------------------------------------------

type(of: AmbiguousVariadic(
  t: 1, "hello", 2,
  u: "hello_2"))
// AmbiguousVariadic<Int, String, Int, String>.Type
//   = AmbiguousVariadic<Int, String, Int, String>

// -----------------------------------------------------------------------------
// With pointy brackets we instead have:
// -----------------------------------------------------------------------------

type(of: AmbiguousVariadic(
  t: 1, "hello",
  u: 2, "hello_2"))
// AmbiguousVariadic<T: <Int, String>, U: <Int, String>>.Type
//   = AmbiguousVariadic<T: <Int, String>, U: <Int, String>>

type(of: AmbiguousVariadic(
  t: 1, "hello", 2,
  u: "hello_2"))
// AmbiguousVariadic<T: <Int, String, Int>, U: <String>>.Type
//   = AmbiguousVariadic<T: <Int, String, Int>, U: <String>>

// -----------------------------------------------------------------------------
// Below we have more examples showing the usage of the parameter name to
// manually bind concrete types to the different variadic Generic parameters.
// -----------------------------------------------------------------------------

struct DoubleVariadic<variadic T: P2, variadic U: P1> { }

// -----------------------------------------------------------------------------
// There is no ambiguity here, since `Int` only conforms to `P1`
// -----------------------------------------------------------------------------

DoubleVariadic<String, String, Int, String>.self
// DoubleVariadic<T: <String, String>, U: <Int, String>>.Type
//   = DoubleVariadic<T: <String, String>, U: <Int, String>>

// -----------------------------------------------------------------------------
// There might be ambiguity here, so the developer might want to use the
// parameters' names
// -----------------------------------------------------------------------------

DoubleVariadic<String, String, String, String>
// DoubleVariadic<T: <String, String, String, String>, U: <>>.Type
//   = DoubleVariadic<T: <String, String, String, String>, U: <>>

Variadic<T: String, String, U: String, String>
// DoubleVariadic<T: <String, String>, U: <String, String>>.Type
//   = DoubleVariadic<T: <String, String>, U: <String, String>>
```

But what is a *variadic version* of a type? This is a new concept, the first introcuced so far - but it simply means that the "variadicity" of the type is not at top-level, but is nested inside the type itself.

The variadic version of a non-Variadic-Generic-type acts in all and for all like other Variadic Generics, and it also keeps track of nested concrete type information.

Moreover, a variadic version of a type can happen to be *degenerate*, i.e. it actually has no variadicity at all. This can happen, for example, when accessing the constrained associatedtype of a type.

It is not possible to create a variadic version of a type using more than one Variadic Generic.

```swift
struct Variadic1<Root, variadic T: Comparable> {
  typealias VariadicKeyPath = KeyPath<Root, T>
}

// `Variadic<String, Int, String.Index>.VariadicKeyPath` will be something that
// contains a `KeyPath<String, Int>` and a `KeyPath<String, String.Index>`

struct Variadic2<variadic T, variadic U> {
  typealias VariadicKeyPath = KeyPath<T, U>
  // error: something warning about using two Variadic Generics `T` and `U` with
  // a non-variadic-generic-type
}
```

Before showing more examples, we should see how the compiler shows the type of a Variadic Type (beware, not a Variadic Generic Type!). Printing the type of a Variadic Type gives the following output:

`variadic <list of bound values>`.

When the variadicity is nested inside the type, the output is instead the following:

`variadic ContainerType<ZeroOrMoreTypesBefore, variadic <list of bound values>, ZeroOrMoreTypesAfter>`.

```swift
struct Variadic<variadic T> {
  static func printTypeOfT() {
    print(T.self)
  }
}

Variadic<Double, Int>.printTypeOfT()
// variadic <Double, Int>

extension Variadic {
  typealias KP = KeyPath<String, T>
}

Variadic<Double, Int>.KP.self
// variadic KeyPath<String, variadic <Double, Int>>
```














```swift
// =============================================================================
// A Variadic Generic parameter is declared by marking it with the `variadic`
// keyword in the generic argument clause. All the `T`s and `U`s in the follow-
// ing code are Variadic Generics. They can be directly used as types without
// any special syntax.
//
// A Variadic Generic parameter can be specialized by passing a variable number
// of types to it, even zero, and the type system keeps each type separate in
// the generic signature (see below for the "<>" syntax):
//
// ```
// let s = sequence(state: someState, next: fnSomeStateToInt)
// type(of: zip(anArrayOfInt, aString, aDictionaryOfIntToString, s))
// // ZipSequence<S1: [Int], S2: String, Ss: <[Int: String], UnfoldSequence<Int, SomeState>>>
// ```
//
// When there is ambiguity in case of multiple Variadic Generics, the type sys-
// tem prefers to assign the maximum number of types to the first generic param-
// eter. The parameter name can be used to manually resolve the ambiguity, and
// for the happines of the compiler, this concept may be generalized to all
// generic parameters even if this is not strictly needed. This new syntax does
// not allow the reordering of the generic parameters. The compiler is updated
// to show new and more detailed informations about generic parameters.
// =============================================================================

// With constraints
struct|class|enum SimpleVariadic<variadic T: P1> { }
// With multiple constraints and other generics
struct|class|enum ComplexVariadic<A, B, variadic T: P1, variadic U: P2> { }

// If a generic parameter is a Variadic Generic, the types that it contains are
// enclosed between angled brackets "< >"
let vg1: SimpleVariadic<Int, String, String>
// vg1: SimpleVariadic<T: <Int, String, String>>

// All types are compatible with `T`, `U` gets no love
let vg2: ComplexVariadic<Double, Int, Int, String, String, String>
// vg2: ComplexVariadic<A: Double, B: Int, T: <Int, String, String, String>, U: <>>

// The name of the generic parameter can be used to specify how the
// specialization should be done; note that in this case, all the labels are
// specified for clarity but not all of them are really required
let vg3: ComplexVariadic<A: Double, B: Int, T: Int, String, U: String, String>
// vg3: ComplexVariadic<A: Double, B: Int, T: <Int, String>, U: <String, String>>

// -----------------------------------------------------------------------------

struct|class|enum AnotherComplexVariadic<A, B, variadic T: P2, variadic U: P1> { }

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
// that the elements wrap, in order, every type contained in `T`. The `variadic`
// spelling is required to remember users what's happening under the hood.
//
// In the second case the operation can be thought as of "unpacking" `T`, taking
// the types inside it and passing them, one by one and in order, to the new
// Variadic Generic. This operation requires the `...` sytnax.
// =============================================================================

// A new type whose variadic parameter can contain anything
struct|class|enum AnyVariadic<variadic T> { }

extension ComplexVariadic {
  // Remember: inside this scope `T` and `U` are Variadic Generics

   // this works with sugar (`variadic T?`), too!
  typealias SomeOptionals = variadic Optional<T>
  // `SomeOptionals` is a Variadic Generic `Optional<variadic P1>`

  // The elements of `SomeOptionals`, one by one, become the types passed to the
  // Variadic Generic parameter `T` of `AnyVariadic`. This is ok since
  // `AnyVariadic.T` is unconstrained.
  func awesomeFunc1(gimmeVg vg: AnyVariadic<SomeOptionals...>) { }

  // All the types inside `T` and `U` are collected into a tuple (more on this
  // later), and that tuple becomes the first generic parameter of
  // `ComplexVariadic`. `T` will be the same as this type's `T`, and `U` will
  // contain no parameters at all.
  func awesomeFunc2(gimmeVg vg: ComplexVariadic<(T..., U...), Int, T...>) { }

  // This is not valid since elements of `U` conform to `P2` but elements of
  // `SimpleVariadic.T` must conform to `P1`, and `P1` and `P2` are not related
  // in any way...
  // func notSoAwesomeFunc(gimmeVg vg: SimpleVariadic<U>) { }
}

// [Remember]
// vg3: ComplexVariadic<A: Double, B: Int, T: <Int, String>, U: <String, String>>

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
// As said before, Variadic Generics can be used to specialize "standard" gener-
// ics. The API of the wrapper type remains the same, but every "entry" is con-
// sidered incompatible with each other when regarding the parametrised generic.
// =============================================================================

extension Array {
  mutating func sort<variadic T: Comparable>(by sortProperties: variadic KeyPath<Element, T>) {
    // let's assume we can access individual elements inside a variadic value by index
    let p_0_0 = self[0][keyPath: sortProperties[0]]
    let p_0_1 = self[0][keyPath: sortProperties[1]]

    // this is not valid: `p_0_0` and `p_0_1` are some `Comparable`s, but there
    // is nothing indicating they are the *same* `Comparable`
    // let booleanValue = p_0_0 < p_0_1

    let p_1_0 = self[1][keyPath: sortProperties[0]]

    // this, instead, *is* valid: `p_0_0` and `p_1_0` are the same `Comparable`
    let booleanValue = p_0_0 < p_1_0
  }  
}

// The first example remains invalid even if called this way
zooArray.sort(by: \.age, \.numberOfLegs)
```

### Variadic Values
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

```swift
// =============================================================================
// Variadic values are a new type of declaration in Swift. They share some com-
// mon traits with `Sequenece`s and `Collection`s, like the support of
// `for ... in` and `map`, but are none of them. Inside a generic context, as it
// is for Variadic Generics, the `...` syntax can be used to convert a variadic
// value into a tuple. A variadic value can never leave a generic context - it
// must always be transformed in something else to do so.
//
// The `...` syntax can also be used to pass variadic values as argument to
// functions. This can only be done if the called function is variadic or has
// Variadic Generic parameters.
// =============================================================================

struct VariadicValueApi<
  variadic Unconstrained: P1 & P2,
  variadic Constrained: P2>
where Constrained.AT: CustomStringConvertible {  
  var unconstrained: Unconstrained { /* ... */ }
  var constrained: Constrained { /* ... */ }

  func variadicValuesApi() {
    for elem in unconstrained {
      // `elem` is a `P1 & P2`, all their API is available here
      print(elem.someMember, elem.someFunction(), elem.getAssociatedValues())
      
      // given that Unconstrained.AT was not constrained, nor on the protocol
      // neither on `VariadicValueApi`, no API is available on the result of
      // `elem.getAssociatedValues()`
      //
      // print(elem.getAssociatedValues().first?.nothingCanBePutHere})
    }

    for elem in constrained {
      // `elem` is a `P2`, its API is available here
      print(elem.getAssociatedValues())

      // P2.AT was constrained, so `CustomStringConvertible` members are
      // available
      print(elem.getAssociatedValues().first?.description)
    }

    // `someCustomStringConvertibles` is a variadic value of `CustomStringConvertible`s
    let someCustomStringConvertibles = constrained.map { $0.getAssociatedValues().first }

    // `variadicAnys` is a variadic value of `Any`s
    let variadicAnys = unconstrained.map { $0.someMember }
    
    // A `project` method is available to map elements of a variadic value while
    // mutating the original variadic value itself
    // `unconstrained` is a var, so `aMutatingMethod` can be called here
    unconstrained.project { $0.aMutatingMethod() }
  }
}

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

func variadicGenericFunction<variadic T>(ts: T) { }
func nonVariadicGenericFunction(ts: Any...) { }

// The following code will cause a compile-time error like "Variadic generic
// argument does not need `...`. Remove it." - a fix-it can also be suggested
func wrongVariadicGenericFunction<variadic T>(ts: T...) { }

// =============================================================================
// The two functions declared above are somewhat equivalent from the outside be-
// cause `T` was not constrained in any way. The only difference is that in the
// second function, `ts` is of type `[Any]`, while in the first function, `ts`
// is of type `variadic Any`.
//
// In the following snippet, we instead actually have a difference: the second
// function can inded accept parameters of any type, but only if all of them are
// of the **same** type! The first function does not have this limitation.
// =============================================================================

func variadicGenericFunction<variadic T>(ts: T) { }
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
// as a parameter to a Variadic Generic function. Two functions declared with
// both `T` and `(T...)` syntax are different and will participate in overload
// resolution.
// =============================================================================

func overloaded<variadic T: P1>(ts: T) { }
func overloaded<variadic T: P1>(ts: (T...)) { }

overloaded(ts: 1, 2, 3, "anything can go here") // calls first overload
overloaded(ts: (1, 2, 3, "anything can go here")) // calls second overload
// in both functions `T` will contain 3 `Int`s and a `String`

// =============================================================================
// A Variadic Generic can directly be used as the result type of a function, and
// it will be automatically considered a `Collection` in concrete code. Note,
// the `(T...)` syntax is used to make the result type a tuple.
// =============================================================================

// Concrete users will get a `Collection`
// Generic users will get a variadic value
func collectionOfValues<variadic T>(_ values: T) -> T {
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
  // error: cannot convert variadic value to return type `(T...)`.
  // A fix-it may be offered
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
```

### Variadic Value API ###
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

```swift
// =============================================================================
// Variadic values conform to the `RandomAccessCollection` protocol and enhance
// it in various ways. All the standard `Collection` API can be used, and all
// the standard functionality (for ... in loops, etc) is available. Moreover,
// all the "transforming" members are overloaded to return another variadic val-
// ue instead of an `Array`, and due to how overload resolution works, these
// functions will be the preferred one when no type context is given.
// =============================================================================

extension ComplexVariadic {
  func collectionApi(t: T, u: U) {
    for elem in t {
      // do stuff with every `P1` in `t`
    }
    
    // the number of the elements contained in the variadic value
    print(t.count)
    
    // tells wether `T` was specialized with 0 parameters
    // (and therefore `t` contains no elements at all)
    print(t.isEmpty)
    
    // If `t` was not empty, get the first value passed to `collectionApi`
    print(t.first)
    
    // Get one of the `P1` elements passed to `t`
    print(t.randomElement())
    
    // The `Index` type is an `Int` and subscripts are 0-based
    // The same as `t.first` or a nice crash
    print(t[0])
  }
  
  func overloadedApis(t: T) {
    // No type specified - this is a variadic value of `Any`s
    let aVariadicValue = t.map { $0.someMember }

    // A type (`[Any]`) was specified - this is an array of `Any`s
    let anArray: [Any] = t.map { $0.someMember }
    
    // `SubSequence` is another variadic value, whose type is still `variadic P1`
    let subVariadicValue = t.dropFirst()
    
    // This works, subVariadicValue is unpacked and passed to the parameter
    // `values` of the called function
    _ = explicitlyMakeTuple(subVariadicValue)
  }
  
  func recursiveFunction(t: T) {
    guard
      let firstElement = t.first
      else { return }

    print(firstElement)

    recursiveFunction(t.dropFirst().reversed())
  }
}

// =============================================================================
// The "transforming" functions need a little bit more attention. We have two
// overloads of `map`, one that returns an `Array` and another that returns a
// variadic value - but those overloads do not allow modifications to the under-
// lying collection element. And this is fair because programmers expect a `map`
// function to be pure.
// However sometimes it might be useful to mutate the original variadic value
// while creating a new one. For this reason variadic values offer an original
// `project` method.
//
// *NOTE:* the name is subject to debate, `project` is the best (and sound) that
// I could find.
// =============================================================================

// the function is defined something like this
mutating func project<T>(_ transform: (inout Self.Element) throws -> T) rethrows -> <variadic value of T's> {
  return try indices.map { index in
    try transform(&self[index])
  }
}

// Cannot use `map`!
extension ZipSequence.Iterator: IteratorProtocol {
  func next() -> (S1.Element, S2.Element, Ss.Element...)? {
    if reachedEnd { return nil }

    guard let e1 = baseStream1.next(),
          let e2 = baseStream2.next(),
          let es = otherStreams.map({ $0.next() }) else {
// error: cannot use mutating member on immutable value: '$0' is immutable
      reachedEnd = true
      return nil
    }

    return (e1, e2, es...)
  }
}

// Use `project` instead!
extension ZipSequence.Iterator: IteratorProtocol {
  func next() -> (S1.Element, S2.Element, Ss.Element...)? {
    if reachedEnd { return nil }

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

### Pattern matching with Optional's
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

```swift
// =============================================================================
// Variadic values can can participate in `Optional`s pattern matching expres-
// sions. The pattern is valid if every item in the variadic value is an
// `Optional` and will match if and only if all the values are not `nil`.
// =============================================================================

func optionalPatternMatching<variadic T>(_ ts: T?) {
  // `ts` is a variadic value of `Optional`s containing `Any`
  
  // If the following expressions do match, `x` will be a variadic value
  // containing all the unwrapped values of `ts`

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
```

### Grammar of Variadic Generics
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

**PLEASE NOTE THAT THIS SECTION IS NOT GOING TO BE UPDATED FOR A WHILE!!!**

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
