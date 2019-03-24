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

Let's take a look at some ways Variadic Generics can improve Swift.

### Example 1: zip
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

Currently Swift has a `zip` free function that takes in any two `Sequence`s and
return a new `Sequence` containing pairs of elements from the original ones:
```swift
func zip<Sequence1: Sequence, Sequence2: Sequence>(
  _ sequence1: Sequence1, _ sequence2: Sequence2
) -> Zip2Sequence<Sequence1, Sequence2> {
  return Zip2Sequence(sequence1, sequence2)
}
 ```
The problem here is that this function can only zip two sequences, and has to
return a specific type `Zip2Sequence` that holds said two sequences. If one
needs to zip three sequences he has to create a `Zip3Sequence` and overload
`zip`, and this leads to a lot of code duplication:
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
With Variadic Generics only a `ZipSequence<AnyNumOfSequences>` and a single
`zip` function would need to exist.
\
\
Reference: [Zip.swift @ apple/swift](https://github.com/apple/swift/blob/master/stdlib/public/core/Zip.swift)

### Example 2: combineLatest
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

[Reactive programming](https://gist.github.com/staltz/868e7e9bc2a7b8c1f754)
libraries have a function `combineLatest` or something similar that takes
various streams of data (`Observable`s, `Signal`s, etc) and combine their latest
outputs in a single value, eventually transformed by a closure:
```swift
// Here I'm using `ObservableType` as a protocol for data stream types, and
// `Observable` as a concrete type implementing `ObservableType`

protocol ObservableType {
  /// The type of the element that this data stream outputs
  associatedtype E
}

class Observable<E> {
  /// Merge two streams together into one single stream by using the specified
  /// function any time one of the input streams produces an element.
  func combineLatest<O1: ObservableType, O2: ObservableType>(
    _ o1: O1, _ o2: O2, transformation: (O1.E, O2.E) -> E
  ) -> Observable<E> {
    return CombineLatest2([...])
  }

  /// Merge three streams together into one single stream by using the specified
  /// function any time one of the input streams produces an element.
  func combineLatest<O1: Observable, O2: Observable, O3: Observable>(
    _ o1: O1, _ o2: O2, _ o3: O3, transformation: (O1.E, O2.E, O3.E) -> E
  ) -> Observable<E> {
    return CombineLatest3([...])
  }

  // and so on up to some arity
  [...]  
}
```
Again, here we have some code duplication that might be avoided with Variadic
Generics. This example is similar to the `zip` one, but here we have a
difference: the function definition contains a *closure whose shape depends on
the number of previous parameters*.
This indicates that we need to be able to retrieve the actual number of
concerete types that are passed to a Variadic Generic in its instantiation.
\
\
Reference: [combineLatest @ RxSwift](https://github.com/ReactiveX/RxSwift/blob/master/RxSwift/Observables/CombineLatest+arity.tt#L22)
Reference: [combineLatest @ ReactiveSwift](https://github.com/ReactiveCocoa/ReactiveSwift/blob/master/Sources/Signal.swift#L1917)

### Example 3: variadic sorting
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

Let's imagine we have the following `Animal` struct and an `Array` of said
animals:
```swift
struct Animal {
  let name: String
  let age: Int
  let weight: Double
}

var someAnimals: [Animal] = giveMeAnArrayOfAnimals()
```
What if we want to sort this array by multiple properties? We might like to do
the following:
```swift
someAnimals.sort(\.name, \.age, \.weight)
```
But if we try to declare the function we get this error:
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
This example might seem similar to the `zip` one, but is actually different in
that in this case Variadic Generics are not used"directly" in the function
signature, but instead to construct another type that depends on them i.e.
`KeyPath<Element, T>`.
\
\
Reference: [Emulating variadic generics in Swift @ Swift Forums](https://forums.swift.org/t/emulating-variadic-generics-in-swift/20046)

### Example 4: curry
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

A curried function is one that takes multiple arguments - like a "normal"
function would - but one at a time, or in other words a curried function takes
the first argument and returns a new function taking the second argument and so
on until all arguments are used up.
```swift
func uncurried<A, B, C, D>(a: A, b: B, c: C) -> D {
  // some computation using `a`, `b` and `c`
}

func curried<A, B, C, D>(_ a: A) -> (B) -> (C) -> D {
  return { b in { c in /* some computation using `a`, `b` and `c` */ } }
}
```
Currying allows you to transform a function of `n` parameters into a curried
function. From [Wikipedia](https://en.wikipedia.org/wiki/Currying): "currying is
the technique of translating the evaluation of a function that takes multiple
arguments into evaluating a sequence of functions, each with a single argument":
```swift
func curry<A, B, C>(_ f: (A, B) -> C) -> (A) -> (B) -> C {
  return { a in { b in f(a, b) } }
}
```
Unfortunately, at the moment one `curry` function must exist for each possible
input function, up to some predetermined arity:
```swift
func curry<A, B, C>(_ f: (A, B) -> C) -> (A) -> (B) -> C {
  return { a in { b in f(a, b) } }
}

func curry<A, B, C, D>(_ f: (A, B, C) -> D) -> (A) -> (B) -> (C) -> D {
  return { a in { b in { c in f(a, b, c) } } }
}

[...]

func curry<A, B, ..., N>(_ f: (A, B, ..., N-1) -> N) -> (A) -> (B) -> ... -> N {
  return { a in { b in ... f(a, b, ..., n) }
}
```
\
Reference: [Curry Library @ thoughtbot/Curry](https://github.com/thoughtbot/Curry)

### Example 5
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

More examples are welcome.

## Proposed solution
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

*Disclaimer: both the syntax and the grammar for Variadic Generics is obviously
subjected to debate and / or change.*

Enter Variadic Generics. Let's define a **Variadic Generic** as a generic type
that can refer to *multiple instances* of *different types*, all eventually
conforming to the parameter definition.

This means, for example, that if there exists a function parametrised by a
Variadic Generic conforming to the `Collection` protocol you can pass to this
function an `Array`, a `Dictionary` and a `String` - all toghether - because
all these types conform to `Collection`.

Let's see how the `zip` example might look like using Variadic Generics:
```swift
struct ZipSequence<Sequences... : Sequence> {
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

    init(_ iterators: (Sequences.Iterator...)) {
      baseStreams = iterators
    }
  }
}

extension ZipSequence.Iterator: IteratorProtocol {
  func next() -> (Sequences.Element...)? {
    if reachedEnd { return nil }
    
    // todo: check if this syntax is ok
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
    return Iterator(sequences.makeIterator())
  }

  var underestimatedCount: Int {
    // todo: this is not good syntax
    return Swift.min(#expand(sequences.underestimatedCount))
  }
}

func zip<Sequences... : Sequence>(_ sequences: Sequences...) -> ZipSequence<Sequences> {
  return ZipSequence(sequences)
}

// usage

zip(anIntArray, aStringToDoubleDictionary)
zip(aPlainOldString, aDoubleIntTupleArray, sequence(first: nil) { _ in 42 })
```
Foreign syntax apart, the code is practically the same of the current `zip`
implementation, but can now be used to zip any number of arbirtary and different
sequences togheter.

Let's see the `combineLatest` example (using RxSwift as reference):
```swift
extension ObservableType {
  static func combineLatest<O1: ObservableType, O2: ObservableType, Os...: ObservableType>(
    _ source1: O1, _ source2: O2, _ sources: Os,
    resultSelector: (O1.Element, 02.Element, #expand(Os.Element)) throws -> Element
  ) -> Observable<Element> {
    return CombineLatest(source1, source2, #expand(sources), resultSelector: resultSelector)
  }
}

class CombineLatestSink<Elements..., O: ObserverType> : CombineLatestSink<O> {
    typealias R = O.E
    typealias Parent = CombineLatest<Elements, R>

    let parent: Parent

    var latestElement: #unpack(Elements)

<%= (Array(1...i).map {
"    var _latestElement\($0): E\($0)! = nil"
}).joined(separator: "\n") %>

    init(parent: Parent, observer: O, cancel: Cancelable) {
        self._parent = parent
        super.init(arity: <%= i %>, observer: observer, cancel: cancel)
    }

    func run() -> Disposable {
<%= (Array(1...i).map {
"        let subscription\($0) = SingleAssignmentDisposable()"
}).joined(separator: "\n") %>

<%= (Array(1...i).map {
"        let observer\($0) = CombineLatestObserver(lock: self._lock, parent: self, index: \($0 - 1), setLatestValue: { (e: E\($0)) -> Void in self._latestElement\($0) = e }, this: subscription\($0))"
}).joined(separator: "\n") %>

<%= (Array(1...i).map {
"         subscription\($0).setDisposable(self._parent._source\($0).subscribe(observer\($0)))"
}).joined(separator: "\n") %>

        return Disposables.create([
<%= (Array(1...i).map { "                subscription\($0)" }).joined(separator: ",\n") %>
        ])
    }

    override func getResult() throws -> R {
        return try self._parent._resultSelector(<%= (Array(1...i).map { "self._latestElement\($0)" }).joined(separator: ", ") %>)
    }
}

final class CombineLatest<Elements..., R>: Producer<R> {
  typealias ResultSelector = (#expand(Elements)) throws -> R

  let sources: Os
  let resultSelector: ResultSelector

  init(_ sources: Os, resultSelector: ResultSelector) {
    self.sources = sources
    self.resultSelector = resultSelector
  }
  
  override func run<O: ObserverType>(
    _ observer: O, cancel: Cancelable
  ) -> (sink: Disposable, subscription: Disposable) where O.E == R {
    let sink = CombineLatestSink(parent: self, observer: observer, cancel: cancel)
    let subscription = sink.run()
    return (sink: sink, subscription: subscription)
  }
}
```

## Detailed design
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

This section is going to describe how to use Variadic Generics, how to interact
with them and how in general they fit into Swift.

### Variadic Generics and tuples
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

[TODO] Remove mixing VG and VFA 

In Swift, variadic function arguments are presented as `Array`s to the function
body, are passed without any particular syntax i.e. like normal arguments and
can appear anywhere in the function signature, as long as all subsequent
arguments have an external label:
```swift
func myPrint(_ args: Any...) {
  // Here, `args` is of type [Any]
}

myPrint(1, "Hello", getAny())

// This is valid
func myPrint1(_ noLabel: Any, _ stuff: Any...) { [...] }

// This is also valid
func myPrint2(_ noLabel: Any, _ stuff: Any..., label: Any) { [...] }

// This is NOT valid
// func myPrint3(_ noLabel: Any, _ stuff: Any..., _ noLabelAgain: Any) { [...] }
```

This array representation makes sense, because variadic arguments are always
*homogenous*. But since types passed to Variadic Generics are generally
*heterogeneous*, it makes sense to represent them as **tuples**, both at the
usage site and at the call site.
In this way, calls to the `zip` function might then look like the following:
```swift
func zip<Sequences...: Sequence>(_ sequences: Sequences) -> ZipSequence<Sequences> {
  // Here, `sequences` is a tuple - don't know the shape, but a tuple it is!
}

zip((anArray, aDictionary, aString))
zip((anArray, anotherArray, stillAnArrary, aCustomSequence))
```
One might argue that Variadic Generics are *homogenous* too in some sense,
because we generally constrain generics to protocols or at least, if no
constraint is specified, there is the top type `Any` (i.e. `protocol<>`) to
unify them. So why not represent Variadic Generics as arrays, too?

To me, the answer lies in another question: what can the type of `sequences` be
in the `zip` example, given that one can pass arbitrary sequences, each of which
can have a different `Element` type?

Moreover, treating Variadic Generics as tuples makes for the combination of them
with variadic arguments easy (if we like to have functionality too):
```swift
func haveFun<T...: SomeProto>(args: T...) {
  // Here, `args` is an *array of tuples* of some shape
}

// Let's assume that `Int` and `String` both conform to `SomeProto`
haveFun((1, "one"), (2, "two"), (3, "three"), (4, "four"))
haveFun(("Sorry ATM", "I cannot"), ("find any", "real world example"), ("for this", "..."))
```

With that said, how can one use Variadic Generics inside a type / a function?
All we now is that variables referring to Variadic Generics are a tuple, but we
don't actually know the *shape* (or *-arity*) of the tuple. In order to do
anything useful with this variable we need a way to interact with them.

[TODO] Specify grammar, member access, #hashUtilities.

<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->
Describe the design of the solution in detail. If it involves new
syntax in the language, show the additions and changes to the Swift
grammar. If it's a new API, show the full API and its documentation
comments detailing what it does. The detail in this section should be
sufficient for someone who is *not* one of the authors to be able to
reasonably implement the feature.

### Syntax
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

In this section we are going to see in more detail how Variadic Generics can be
declared and used in various contexts.

But first, ome stuff we are going to use for our examples:
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
struct|class|enum Variadic3<A, B: P1, T... : P2> { }

let vg1: Variadic1<Int, String, Double>
// (T...) => (Int, String, Double)
let vg2: Variadic2<Int, String, String>
// (T...) => (Int, String, String)
let vg3: Variadic3<Double, String, String, String, String>
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
// (T...)
// (T, U, V...)
// (T, U..., V...)
// =============================================================================

struct|class|enum Variadic<A, B, T... : P1> {
  typealias AllElements = (A, B, T...)

  // `ts` is a tuple of some arity
  private var ts: (T...)

  // `allElements` is a tuple of some arity + 2
  private var allElements: (AllElements...)
  // Same as: private var allElements: (A, B, T...)
  
  // --- Initializer #1 - `ts` is a tuple of some arity
  //
  // Users of this function must pass a tuple as the third argument
  //
  init(a: A, b: B, ts: (T...)) { }
  
  // --- Initializer #2 - `allElements` is a tuple of some arity + 2
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
// Variadic Generic and transforms it into a variadic function argument.
//
// func someFunc(_ ts: T...)
// =============================================================================

extension Variadic {
  // --- Initializer #3 - `ts` is a tuple of some arity
  //
  // Users of this function pass a variable number of arguments, as if this was
  // a "standard" variadic function
  //
  // Definition of both Initializer #3 and #4 will lead to "error: ambiguous use
  // of 'init'
  //
  init(_ a: A, _ b: B, _ ts: T...) { }
  
  // --- Initializer #4 - `allElements` is a tuple of some arity + 2
  //
  // Users of this function must pass at least two arguments
  //
  // Definition of both Initializer #3 and #4 will lead to "error: ambiguous use
  // of 'init'
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
// This is called passing a variable numebr of arguments
func vgFuncWithoutConstraints<T...>(ts: T...) { }
// This is called passing a tuple
func vgFuncWithoutConstraints<T...>(ts: (T...)) { }

// N.B. The name of the above two functions is the same because their signature
// is different!

func vgFuncWithConstraints<T... : P1>(ts: T...) { }
func vgFuncWithOtherGenerics<A, B: P1, T... : P2>(a: A, b: B, ts: T...) { }
func vgFuncWithOtherGenerics<A, B: P1, T... : P2>(a: A, b: B, ts: (T...)) { }


// This calls the `T...` function
vgFuncWithoutConstraints(ts: 1, 2, "Hello", [1.1, 2.2, 3.3], Optional<(Int, String)>.none as Any)

// This calls the `(T...)` function
vgFuncWithoutConstraints(ts: (1, 2, "Hello", [1.1, 2.2, 3.3], Optional<(Int, String)>.none as Any))

vgFuncWithConstraints(ts: 1, "two", 3)
vgFuncWithOtherGenerics(a: 123.45, b: 1, ts: "1", "two", "...")
vgFuncWithOtherGenerics(a: 123.45, b: 1, ts: ("1", "two", "..."))
```

#### Accessing members of a variable of Variadic Generic type
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

#### Compile-time support
```swift
func lengthOfVgValue<T...>(values: T...) -> Int {
  return #length(values)
}

lengthOfVgValue() // `0`
lengthOfVgValue(42) // `1`
lengthOfVgValue("a", "b", "c") // `3`

func firstElementOfVgValue<T...>(values: T...) -> #head(T) {
  return #head(values)
}

firstElementOfVgValue() // `()`
firstElementOfVgValue(42) // `42`
firstElementOfVgValue("a", "b", "c") // `"a"`

func elementsOfVgValueButHead<T...>(values: T...) -> #tail(T) {
  return #tail(values)
}

elementsOfVgValueButHead() // `()`
elementsOfVgValueButHead(42) // `(42)` aka `42`
elementsOfVgValueButHead("a", "b", "c") // `("b", "c")`
```

#### Expanding a variadic value into the surrounding context
```swift
struct ZipSequence<S1 : Sequence, S2 : Sequence, OtherSequences... : Sequence> {
  let sequences: (S1, S2, OtherSequences...)

  init(_ s1: S1, _ s2: S2, _ others: Sequences...) {
    self.sequences = (s1, s2, others...)
  }
  
  var underestimatedCount: Int {
    // `ucs` contains at least two members, so there always exists a `min`
    // function to call
    let ucs = (sequences.underestimatedCount...)
    return min(ucs...)

    // equivalent to:
    // return min((sequences.underestimatedCount...)...)
  }
}

// struct ZipSequence<[Int], [String: Double], String, MySequence> {
//   let sequences: ([Int], [String: Double], String, MySequence)
//
//   init(_ s1: [Int], _ s2: [String: Double], others: (String, MySequence)) {
//     self.sequences = (s1, s2, others.0, others.1)
//   }
//
//   var underestimatedCount: Int {
//     // public func min<T>(_ x: T, _ y: T, _ z: T, _ rest: T...) -> T where T : Comparable
//     return min(
//       sequences.0.underestimatedCount,
//       sequences.1.underestimatedCount,
//       sequences.2.underestimatedCount,
//       sequences.3.underestimatedCount
//     )
//   }
// }
ZipSequence([1, 2, 3], ["a": 42.0, "b": 42.0, "c": 42.0], "Hello World!", MySequence())

// struct ZipSequence<[Int], [String: Double], String> {
//   let sequences: ([Int], [String: Double], String)
//
//   init(_ s1: [Int], _ s2: [String: Double], others: String) {
//     self.sequences = (s1, s2, others)
//   }
//
//   var underestimatedCount: Int {
//     // public func min<T>(_ x: T, _ y: T, _ z: T, _ rest: T...) -> T where T : Comparable
//     return min(
//       sequences.0.underestimatedCount,
//       sequences.1.underestimatedCount,
//       sequences.2.underestimatedCount
//     )
//   }
// }
ZipSequence([1, 2, 3], ["a": 42.0, "b": 42.0, "c": 42.0], "Hello World!")

// struct ZipSequence<[Int], [String: Double]> {
//   let sequences: ([Int], [String: Double])
//
//   init(_ s1: [Int], _ s2: [String: Double], others: Void = ()) {
//     self.sequences = (s1, s2)
//   }
//
//   var underestimatedCount: Int {
//     // public func min<T>(_ x: T, _ y: T) -> T where T : Comparable
//     return min(
//       sequences.0.underestimatedCount,
//       sequences.1.underestimatedCount
//     )
//   }
// }
ZipSequence([1, 2, 3], ["a": 42.0, "b": 42.0, "c": 42.0])
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
