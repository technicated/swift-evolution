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
new or interesting ideas.
Obviously the information contained in the mentioned document and collected on
the Swift Forums are going to influence what's presented there.

Swift-evolution thread: [Variadic Generics](https://forums.swift.org/t/variadic-generics/20320)

## Motivation
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

Let's take a look at some examples that are hard or impossible to "scale up"
today with Swift, and see how Variadic Generics can improve them.

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
that need to zip three sequences have to create a new `Zip3Sequence` type and
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
libraries have a function `combineLatest` or something similar that takes
various streams of data (`Observable`s, `Signal`s, etc) and *combine* their
*latest* outputs in a single value, eventually transformed by a function:
```swift
// Here I am using - more or less - RxSwift types

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

  /// Merge three streams together into one single stream, by using the
  /// specified function, any time one of the input streams produces an element.
  func combineLatest<O1: ObservableType, O2: ObservableType, O3: ObservableType>(
    _ o1: O1, _ o2: O2, _ o3: O3, transformation: (O1.E, O2.E, O3.E) -> E
  ) -> Observable<E> {
    return CombineLatest3([...])
  }

  // and so on up to some arity
  [...]  
}

class Observable<Element> : ObservableType { [...] }

class Producer<Element> : Observable<Element> { [...] }

class CombineLatest[#]<[...], R> : Producer<R> { [...] }
```
Again, here we have some code duplication that might be avoided with Variadic
Generics. This example is similar to the `zip` one, but there's actually a
difference: the function definition contains a closure whose *shape depends on
the number of previous parameters*.
This indicates that we need to be able to retrieve the actual number of
concerete types that are passed to a Variadic Generic in its instantiation.
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

This example might seem similar to the `zip` one, but is actually different in
that in this case Variadic Generics are not used "directly" in the function
signature, but instead to construct another type that depends on them i.e.
`KeyPath<Element, T>`.
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
Reference: [Curry Library @ thoughtbot/Curry](https://github.com/thoughtbot/Curry)

### Example 5
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

More examples are welcome.

## Proposed solution
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

Enter Variadic Generics. Let's define a **Variadic Generic** as a generic type
that can refer to *multiple instances* of *different types*, all eventually
conforming to the parameter definition.

This means, for example, that if there exists a function parametrised by a
Variadic Generic conforming to the `Collection` protocol you can pass to this
function an `Array`, a `Dictionary` and a `String` - all toghether - because
all these types conform to `Collection`.

Let's see how the `zip` example might look like using Variadic Generics:
```swift
struct ZipSequence<S1 : Sequence, S2 : Sequence, OtherSequences... : Sequence> {
  typealias Sequences = (S1, S2, OtherSequences...)

  private let sequences: (Sequences...)

  init(_ sequences: Sequences...) {
    self.sequences = sequences
  }
 
  init(sequences: (Sequences...)) {
    self.sequences = sequences
  }
}

extension ZipSequence {
  struct Iterator {
    var baseStreams: (Sequences.Iterator...)
    var reachedEnd: Bool = false

    init(_ iterators: Sequences.Iterator...) {
      baseStreams = iterators
    }
  }
}

extension ZipSequence.Iterator: IteratorProtocol {  
  func next() -> (Sequences.Element...)? {
    if reachedEnd { return nil }
    
    if case let (elements...?) = baseStreams.next() {
      return elements
    } else {
      reachedEnd = true
      return nil
    }
  }
}

extension ZipSequence: Sequence {
  func makeIterator() -> Iterator {
    return Iterator(sequences.makeIterator()...)
  }

  var underestimatedCount: Int {
    return Swift.min(sequences.underestimatedCount...)
  }
}

func zip<S1 : Sequence, S2 : Sequence, OtherSequences... : Sequence>(
  _ s1: S1, _ s2: S2, _ otherSequences: OtherSequences...
) -> ZipSequence<S1, S2, OtherSequences> {
  return ZipSequence(s1, s2, otherSequences...)
}

// Valid usages
zip(anIntArray, aStringToDoubleDictionary)
zip(aPlainOldString, aDoubleIntTupleArray, sequence(first: nil) { _ in 42 }, myRandomSequence)

// Invalid usages
zip(aSingleCollection)
zip()
```
There is a lot of new syntax and concepts here, but the important thing to note
here is that the code is practically the same of the current `zip`
implementation. The only difference is that `zip` can now be used to zip any
number of arbirtary and different sequences togheter.

## Detailed design
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

### Step 1: empowering tuples
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

Tuples are a great tool in the belt of Swift developers, allowing one to pack
related values together without the need to declare a completely new type. In
order to support the design of Variadic Generics we need additional language
support for tuples.

[SE-0029](https://github.com/apple/swift-evolution/blob/master/proposals/0029-remove-implicit-tuple-splat.md)
was accepted in order to remove implicit "tuple splatting", a somewhat useful
behavior that was not implemented reliably and caused complexity in both the
language and the type checker. Given a function like this
```swift
func foo(a : Int, b : Int) { }
```
implicit tuple splatting allowed programmers to implicitly pass an entire argu-
ment list as a properly-named tuple:
```swift
let x = (a: 1, b: 2)
foo(x)
```
I suggest to bring this feature back and make it better and, of course, explic-
it. I refer to this new feature as **unpacking** and **repacking** tuples. This
feature will allow tuples to be "expanded" into the surrounding context where
appropriate, and for multiple tuples to be combined together more easily:
```swift
func sum4(a: Int, b: Int, c: Int, d: Int) -> Int {
  return a + b + c + d
}

let t1 = (a: 1, b: 2)
let t2 = (c: 3, d: 4)

// --- Unpack (using `...`) and repack
let bigTuple = (t1... , t2...)
// (a: 1, b: 2, c: 3, d: 4)

// --- Unpack only
sum4(bigTuple...)

// --- Works with a single tuple, too
let copyOfT1 = t1
let awkwardCopyOfT1 = (t1...)

// --- Unpack and repack with additional elements
let bigTuple2 = (a: 1, 2, t2...)
// (a: 1, 2, c: 3, d: 4)

// --- Invalid call to `sum4`
sum4(bigTuple2...)
// error: arguments do not match

// --- Valid call to `sum4`
let bigTuple3 = (t1... , c: 3, d: 4)
// (a: 1, b: 2, c: 3, d: 4)
sum4(bigTuple3...)

// --- Works with types, too
typealias IntPair = (Int, Int)
typealias IntQuad = (IntPair... , IntPair...)
// (Int, Int, Int, Int)

// But beware: the following is *still* a standard variadic function, and in
// this specific case this means that you can "only" pass infinite `(Int, Int)`
// values
func variadicFunction(_ values: IntPair...) { }

// --- These functions take a *single* `(Int, Int)` value
func fn1(_ values: IntPair) { }
func fn2(_ values: (IntPair...)) { }

// --- The `...` syntax works with destructuring, too
let (x, y, others...) = (a: 1, 2, c: 3, 4, e: 5, 6)
// x => 1
// y => 2
// others => (c: 3, 4, e: 5, 6)

// Just gimme the tuple without the first element...
let (_, allButFirst...) = others
// allButFirst => (4, e: 5, 6)

// ... or without the *last* element
let (allButLast... , _) = others
// allButLast => (c: 3, 4, e: 5)
```

### Step 2: implement Variadic Generics
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

In this section we are going how Variadic Generics can be declared and used in
various contexts.

In all examples we are going to use the following types:
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

#### Declaring and using a Variadic Generic type

```swift
// =============================================================================
// All `T`s are "parameter packs" and cannot be directly used as types, with one
// exception: they can be passed as parameters to other Variadic Generics
// =============================================================================

// Without constraints
struct|class|enum Variadic1<T...> { }
// With constraints
struct|class|enum Variadic2<T... : P1> { }
// With constraints and other generics
struct|class|enum Variadic3<A, B : P1, T... : P2> { }

let vg1: Variadic1<Int, String, Double>
// (T...) => (Int, String, Double)
let vg2: Variadic2<Int, String, String>
// (T...) => (Int, String, String)
let vg3: Variadic3<Double, Int, String, String, String>
// (T...) => (String, String, String)

// =============================================================================
// These are valid usages and the Variadic Generic specialization is done with
// no types at all
// =============================================================================

let vg4: Variadic3<Double, Int>
// (T...) => ()
let vg5: Variadic1<>
// (T...) => ()

// =============================================================================
// Specializing a Variadic Generic using another one
// =============================================================================

extension Variadic3 {
  func awesomeFunc(gimmeVg vg: Variadic1<T>) { }

  // This is not valid since elements of `Self.T` conform to `P2` but elements
  // of `Variadic2.T` conform to `P1`, and `P1` and `P2` are not related in any
  // way...
  //
  // func notSoAwesomeFunc(gimmeVg vg: Variadic2<T>) { }
}
```

#### Using a Variadic Generic as a type

```swift
// =============================================================================
// Syntax #1
//
// This syntax can be used for both variable and function argument declaration.
// It takes a Variadic Generic and expands it into a tuple. Multiple Variadic
// Generics can be expanded together and mixed with "standard" generic parame-
// ters.
//
// This is the only syntax that can be used for variables, an this imply that
// variables whose type is a Variadic Generic are always treated as tuples whose
// arity is not determined or partially determined (see below).
//
// (T...)
// (T, U, V...)
// (T, U..., V...)
// =============================================================================

struct|class|enum Variadic<A, B, T... : P1> {
  typealias AllElements = (A, B, T...)

  // `ts` is a tuple of some arity
  private var ts: (T...)

  // `allElements` is a tuple of some arity + 2
  // This is equivalent to: `private var allElements: (A, B, T...)`
  private var allElements: (AllElements...)
  
  // --- Initializer #1
  // `ts` is a tuple of some arity
  //
  // Users of this function must pass a tuple as the third argument
  //
  init(a: A, b: B, ts: (T...)) { }
  
  // --- Initializer #2
  // `allElements` is a tuple of some arity + 2
  //
  // Users of this function must pass a tuple as the only argument, and that tu-
  // ple must contain at least two elements
  //
  init(allElements: (AllElements...)) { }
  // Same as: init(allElements: (A, B, T...)) { }
}

// =============================================================================
// Syntax #2
//
// This syntax can be only be used for function argument declaration. It takes a
// Variadic Generic and transforms it into a variadic function argument. Parame-
// ters passed to this variadic argument does not have to be all of the same
// type.
//
// func someFunc(_ ts: T...)
// =============================================================================

extension Variadic {
  // --- Initializer #3
  // `ts` is a tuple of some arity
  //
  // Users of this function pass a variable number of arguments, as if this was
  // a "standard" variadic function
  //
  // Definition of both Initializer #3 and #4 will lead to "error: ambiguous use
  // of 'init' when initializing `Variadic`
  //
  init(_ a: A, _ b: B, _ ts: T...) { }
  
  // --- Initializer #4
  // `allElements` is a tuple of some arity + 2
  //
  // Users of this function must pass at least two arguments
  //
  // Definition of both Initializer #3 and #4 will lead to "error: ambiguous use
  // of 'init' when initializing `Variadic`
  //
  init(_ allElements: AllElements...) { }
}

// =============================================================================
// Examples of valid usages for each initializer
// =============================================================================

// --- Initializer #1
//
Variadic(a: 1, b: 2, ts: ("3", 4, "Hello"))
//
// Variadic<Int, Int, String, Int, String>
//          A    B    (T...             )
//
// a => 1
// b => 2
// ts => ("3", 4, "Hello")

// --- Initializer #2
//
Variadic(allElements: (1, 2, "3", 4, "Hello"))
//
// Variadic<Int, Int, String, Int, String>
//          A    B    (T...             )
//
// allElements => (1, 2, "3", 4, "Hello")

// --- Initializer #3
//
Variadic(1, 2, "3", 4, "Hello")
//
// Variadic<Int, Int, String, Int, String>
//          A    B    (T...             )
//
// a => 1
// b => 2
// ts => ("3", 4, "Hello")

// --- Initializer #4
//
Variadic(1, 2, "3", 4, "Hello")
//
// Variadic<Int, Int, String, Int, String>
//          A    B    (T...             )
//
// allElements => (1, 2, "3", 4, "Hello")

// =============================================================================
// Examples of invalid usages for each initializer
// =============================================================================

// --- Initializer #1
//
Variadic(a: 1, b: 2, ts: "3", 4, "Hello")
//
// error: variadic parameter `ts` must be passed as a tuple, add ()

// --- Initializer #2
//
Variadic(allElements: ("only one param"))
//
// error: variadic parameter `allElements` must contain at least two elements

// --- Initializer #3
//
Variadic("only one param")
//
// error: missing argument for parameter #2 in call

// --- Initializer #4
//
Variadic("only one param")
//
// error: variadic parameter #0 requires at least two arguments
```

#### Declaring and using Variadic Generic function

```swift
// =============================================================================
// Functions are declared as seen in the previous section. One can use both syn-
// tax #1 or syntax #2 and the resulting functions will have different signa-
// tures so they can coexist.
// =============================================================================

func variadicFunction<T...>(ts: T...) { }
func variadicFunction<T...>(ts: (T...)) { }

// This calls the `T...` function
variadicFunction(ts: 1, 2, "Hello", [1.1, 2.2, 3.3], Optional<(Int, String)>.none as Any)

// This calls the `(T...)` function
variadicFunction(ts: (1, 2, "Hello", [1.1, 2.2, 3.3], Optional<(Int, String)>.none as Any))

// This always calls the `(T...)` function
let myTuple = (1, 2, "Hello", [1.1, 2.2, 3.3], Optional<(Int, String)>.none as Any)
variadicFunction(ts: myTuple)
```

#### Accessing members of a variable of Variadic Generic type

```swift
// This is the same type as before
struct|class|enum Variadic<A, B, T... : P1> {
  typealias AllElements = (A, B, T...)

  private var ts: (T...)
  private var allElements: (AllElements...)
  
  init(a: A, b: B, ts: (T...)) {
    self.ts = ts
    self.allElements = (a, b, ts...)
  }
  
  init(allElements: (AllElements...)) {
    self.ts = #tail(allElements, 2)
    self.allElements = allElements
  }
}

extension Variadic {
  init(_ a: A, _ b: B, _ ts: T...) {
    self.ts = ts
    self.allElements = (a, b, ts...)
  }
  
  init(_ allElements: AllElements...) {
    self.ts = #tail(allElements, 2)
    self.allElements = allElements
  }
}
```

```swift
struct VariadicOne<T... : P1> {
  var storage: (T...)
  
  init(storing storage: T...) {
    self.storage = storage
  }
  
  func doStuff() {
    // Accessing a "common" member on a variable of Variadic Generic type
    // creates a new tuple where each element is the application of the member
    // access to each member of the original tuple. In this case this is like
    // doing `let members = (storage.0.member, storage.1.member, ...)`.
    // The type of this expression is `(Any...)`
    let members = (storage.member...)
    
    // The type of this expression is `(Any...)`
    let fnResults = (storage.function()...)
  }
}

struct VariadicTwo<T... : P2> {
  var storage: (T...)
  
  init(storing storage: T...) {
    self.storage = storage
  }
  
  func doStuff() {
    // The type of this expression is `([T.AT]...)`
    let associatedValues = (storage.getAssociatedValues()...)

    // The type of this expression is `(T.AT?...)`
    let firstAssociatedValues = (storage.getAssociatedValues().first...)
  }
}
```

#### Pattern matching
```swift
func matchSomeOptionalsOrBurn<E...>(_ optionals: Optional<E>...) -> (E...) {
  if case let (elements...?) = optionals {
    return elements
  } else {
    fatalError("Could not match optionals!")
  }
}

// The "type" of the function is (Int?, Int?, Int?) -> (Int, Int, Int)
// Will print `(1, 2, 3)`
matchSomeOptionalsOrBurn(1, 2, 3)

// The "type" of the function is (Int?, String?, Double?) -> (Int, String, Double)
// Will crash
matchSomeOptionalsOrBurn(1, "2", Double?.none)
```

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
