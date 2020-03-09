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
protocol P1 {}

extension Int: P1 {}
extension String: P1 {}

protocol P2 {
  associatedtype Associated
  var associated: Associated { get }
}

extension Double: P2 {
  typealias Associated = Self
  var associated: Associated { self }
}

extension String: P2 {
  var associated: Double { Double(self) ?? 42 }
}

struct S1<Associated>: P2, CustomStringConvertible {
  let associated: Associated

  var description: String { "S1 {associated: \(associated)}" }
}

struct S2: P2 {
  var associated: Double { 0 }
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
// of `P2` was not constrained, so `T.Associated` will expose the `Any` API.
//
struct Variadic3<variadic T: P1 & P2> { }

// This is a Variadic Generic constrained to a protocol composition. Types
// inside of `T` will expose both the `P1` and the `P2` API. The associated type
// of `P2` was constrained to `Numeric`, so `T.Associated` will expose the `Numeric`
// API.
//
struct Variadic4<variadic T: P1 & P2> where T.Associated: Numeric { }

// This is a Variadic Generic constrained to a protocol composition. Types
// inside of `T` will expose both the `P1` and the `P2` API. The associated type
// of `P2` was constrained to `Int`, so `T.Associates` will expose exaclty the `Int`
// API.
//
struct Variadic5<variadic T: P1 & P2> where T.Associated == Int { }
```

The same rules as above apply when declaring a Variadic Generic function or method.

### *Variadic Generic Types* and *Variadic Types*
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

A type with a Variadic Generic in its generic argument clause is called a **Variadic Generic Type**. A function with a Variadic Generic in its generic argument clause is called a **Variadic Generic function**.

Once a Variadic Generic (let's say `T`) has been declared, it can be used as-is as a type in every expression. In this context, `T` is called a **Variadic Type** (and please note that this is different from *Variadic Generic Type*!).

```swift
// `Variadic` is a *Variadic Generic Type*
// Inside of `Variadic`, `T` is a *Variadic Type*
//
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
```

The compiler shows the type of a variable whose type is a Variadic Type in one of the following two ways, like it does for other "standard" generics and for `inout` parameters:
- `variadic <name-of-the-unconstrained-Variadic-Generic> | <name-of-the-variable>`
- `variadic <constraints-of-the-Variadic-Generic> | <name-of-the-variable>`

```swift
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
  // `inout P1 & P2 | b_param`
  param
}

// -----------------------------------------------------------------------------
// Autocompletion for Variadic Types
// -----------------------------------------------------------------------------

struct VariadicType<variadic Values, variadic Constrained: P1 & P2> {
  let unconstrained: Values
  let constrained: Constrained

  func test() {
    // When typing `constra` autocomplete will show:
    // `variadic Values | unconstrained`
    // `variadic P1 & P2 | constrained`
    constra
  }
}
```

### Unpacking Variadic Types
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

Where appropriate, the `...` syntax can be used to "unpack" the elements of a Variadic Type as a comma-separated list of types.

```swift
enum VariadicEnum<variadic T: P1> {
  // Converting a Variadic Type into a tuple type.
  //
  func someFunctionReturningATuple() -> (T...) { ... }

  // Converting a Variadic Type into a tuple type, with two additional types
  // packed in (before).
  //
  func someFunctionReturningAnExtendedTuple<A, B>() -> (A, B, T...) { ... }

  // Converting a Variadic Type into a tuple type, with two additional types
  // packed in (after). Please note that no comma is needed after the `...`
  // syntax so it's nicer to read.
  //
  func anotherFunctionReturningAnExtendedTuple<A, B>() -> (T... A, B) { ... }

  // --------------------  enum cases  --------------------

  case none

  // Enum case taking several parameters.
  //
  case someThings(T...)

  // Enum case taking a single tuple - whose shape depends on the Variadic
  // Type - as its only parameter.
  //
  case other((T...))
}
```

### Variadic Types and the compiler
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

The compiler shows the concrete types bound to a Variadic Generic inside pointy brackets (i.e. `<` and `>`); the types **are not flattened into a single list**. The compiler is also modified to always show the name of all the generics parameters of a type, regardless of whether they are variadic or not.

```swift
struct MixedType<T, N: Numeric, variadic Ss: Sequence> { }

MixedType<String, Int, [Int], [String: Double]>.self
// MixedType<T: String, N: Int, SS: <[Int], [String: Double]>>.Type
//   = MixedType<T: String, N: Int, Ss: <[Int], [String: Double]>>

// A Variadic Generic type can also be specialized with no types at all
MixedType<String, Int>.self
// MixedType<T: String, N: Int, SS: <>>.Type
//   = MixedType<T: String, N: Int, SS: <>>
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

DoubleVariadic<T: String, String, U: String, String>
// DoubleVariadic<T: <String, String>, U: <String, String>>.Type
//   = DoubleVariadic<T: <String, String>, U: <String, String>>
```

### Variadic version of a "standard" type
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

But what is a *variadic version* of a type? This is a new concept, the first introcuced so far - but it simply means that the "variadicity" of the type is not at top-level, but is nested inside the type itself.

The variadic version of a non-Variadic-Generic-type acts in all and for all like other Variadic Generics, and it also keeps track of nested concrete type information.

Moreover, a variadic version of a type can happen to be *degenerate*, i.e. it actually has no variadicity at all, or in other words all the types contained in the Variadic Type are the same. This can happen, for example, when accessing the constrained `associatedtype` of a type.

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

Before showing more examples, we should see how the compiler shows the type of a Variadic Type (beware, not a Variadic Generic Type! We already addressed that in a previous section). Printing the type of a Variadic Type gives the following output:

`variadic <list of bound values>`.

When the variadicity is nested inside the type, the output is instead the following:

`variadic ContainerType<ZeroOrMoreTypesBefore, variadic <list of bound values>, ZeroOrMoreTypesAfter>`.

Some examples should clarify the syntax:

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

And now, some examples where some of the Variadic Types are degenerate:

```swift
struct Variadic1<variadic V: P2> {
  static func printTypeOfV() {
    print(V.self)
  }

  static func printTypeOfAssociated() {
    print(V.Associated.self)
  }
}

Variadic1<String, S1<String>>.printTypeOfT()
// variadic <String, S1<Associated: String>>

Variadic1<String, S1<String>>.printTypeOfAssociated()
// variadic <Double, String>

Variadic1<String, S2, Double>.printTypeOfT()
// variadic <String, S2, Double>

Variadic1<String, S2, Double>.printTypeOfAssociated()
// variadic <Double, Double, Double>
// This is a *degenerate* Variadic Type!

extension KeyPath {
  static func printTypeOfRoot() {
    print(Root.self)
  }

  static func printTypeOfValue() {
    print(Value.self)
  }
}

struct Variadic2<variadic R: P2, Value: P1> {
  typealias KP1 = KeyPath<R, Value>
  typealias KP2 = KeyPath<R.Associated, Value>

  static func printTypeOfKP1() {
    print(KP1.self)
  }

  static func printTypeOfKP2() {
    print(KP2.self)
  }
}

Variadic2<String, Double, S1<Double> S2, Int>.printTypeOfKP1()
// variadic KeyPath<variadic <String, Double, S1<Associated: Double> S2>, Int>

Variadic2<String, Double, S1<Double> S2, Int>.printTypeOfKP2()
// variadic KeyPath<variadic <Double, Double, Double, Double>, Int>
// Another *degenerate* Variadic Type!

Variadic2<String, Double, S1<Double> S2, Int>.KP1.printTypeOfRoot()
// variadic <String, Double, S1<Double> S2>

Variadic2<String, Double, S1<Double> S2, Int>.KP1.printTypeOfValue()
// Int

Variadic2<String, Double, S1<Double> S2, Int>.KP2.printTypeOfRoot()
// variadic <Double, Double, Double, Double>
// Another one!

Variadic2<String, Double, S1<Double> S2, Int>.KP2.printTypeOfValue()
// Int
```

### Variadic Generics at the value level
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

A value whose type is a Variadic Type is called a **Variadic Value**. Once a Variadic Value is defined, what's the API exposed by it? It was decided that direct member access to automatically "map" the contained values was to be forbidden; instead, Variadic Values have their own unique API, extending that of `Collection`.

Before showing the Variadic Type API, let's see how Variadic Values are printed:

```swift
struct Variadic<variadic Values> {
  let values: Values
}

let variadic1 = Variadic(values:
  0,
  42.42,
  "hello")

print(variadic1.values)
// variadic <0, 42.42, "hello">

print(type(of: variadic1.values))
// variadic <Int, Double, String>

let variadic2 = Variadic(values:
  0,
  42.42,
  "hello",
  S1(associated: ()),
  S2(),
  S1(associated: S2()))

// The `CustomStringConvertible` conformance is used!
print(variadic2.values)
// variadic <0, 42.42, "hello", S1 {associated: ()}, S2(), S1 {associated: S2()}>
//                              ^~~~~~~~~~~~~~~~~~~        ^~~~~~~~~~~~~~~~~~~~~

print(type(of: variadic2.values))
// variadic <Int, Double, String, S1<Void>, S2, S1<S2>>
```

As you can see, the representation is very similar to that of the Variadic Type:
- `variadic <value1, value2, value3, ...>`

### Variadic Values API
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

As said in the previous section, a Variadic Value is a `Collection`, to be more specific a `RandomAccessCollection`. In addition to the protocol's standard API, overloads for useful methods like `map` exists, to continue to work with Variadic Values and not transform them into `Array`s. A Variadic Type is its own `SubSequence`.

To this day, it still is to be decided if all methods that return an Array in the protocol should also receive an overload that return another Variadic Value.

Let's now see some examples of these overloads. We are going to pretend that `Variadic Type` is actually a type, just for the sake of syntax.

```swift
protocol VariadicType: RandomAccessCollection {}

extension VariadicType {
  func map<T>(_ transform: (Element) throws -> T) rethrows -> VariadicType<T> {}
}

struct Variadic<variadic T: P2> {
  var t: T

  func mappingValues() {
    // This is a Variadic Value containing the `associated` value of every value
    // inside `t`.
    //
    let associatedValues = t.map { $0.associated }

    // This is a degenerate Variadic Value containing all `0 as Int`, but a
    // Variadic Value it is nonetheless.
    //
    let degenerateZeros = t.map { _ in 0 }
  }

  func letsSeeTheMappedValues() -> T.Associated {
    t.map { $0.associated }
  }
}

let mappedValues = Variadic(t: 0.42, "hello", S1(associated: ())), S2())
  .letsSeeTheMappedValues()

print(type(of: mappedValues), "\n", mappedValues)
// variadic <Double, Double, Void, Double>
// variadic <0.42, 42.0, (), 0.0>
```

A method that does not exist in the `RandomAccessCollection` protocol and is unique to Variadic values is `project`. This method is again used to map a Variadic Value into another Variadic Value, but it allows the current element to be modified:

```swift
extension VariadicType {
  func project<T>(_ transform: (inout Element) throws -> T) rethrows -> VariadicType<T> {}
}

struct Variadic<variadic Iterators: IteratorProtocol> {
  var iterators: Iterators

  func projectingValues() -> (Iterators.Element...) {
    // `next` is a mutating method, so it could have not be used inside `map`!
    //
    let elements = iterators.project { i in i.next() }

    return (elements...)
  }
}

let variadic = Variadic(iterators: [1, 2, 3].makeIterator(), ["hello"].makeIterator())

var projectedValues = variadic.projectingValues()

print(type(of: projectedValues), "\n", projectedValues)
// (Int?, String?)
// (1, "hello")

print(type(of: variadic.iterators), "\n", variadic.iterators)
// variadic <IndexingIterator<Array<Int>>, IndexingIterator<Array<String>>>
// variadic <{IndexingIterator description, _position: 1}, {IndexingIterator description, _position: 1}>

projectedValues = variadic.projectingValues()

print(type(of: projectedValues), "\n", projectedValues)
// (Int?, String?)
// (2, nil)

print(type(of: variadic.iterators), "\n", variadic.iterators)
// variadic <IndexingIterator<Array<Int>>, IndexingIterator<Array<String>>>
// variadic <{IndexingIterator description, _position: 2}, {IndexingIterator description, _position: 1}>
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
