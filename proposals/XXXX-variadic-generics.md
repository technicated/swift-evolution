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

### Example 3: variadic sorting
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

[Reference](https://forums.swift.org/t/emulating-variadic-generics-in-swift/20046).
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
But if we try to declare the function we got this error:
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
I'm not actually sure if this example is different from the `zip` one. I feel
there is some difference because in this case Variadic Generics are not used
"directly" in the function signature, but instead to construct another type that
depends on them i.e. `KeyPath<Element, T>`.

### Example 4
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

More examples are welcome.

## Proposed solution
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

*Disclaimer: both the syntax and the grammar for Variadic Generics is obviously
subjected to debate and / or change.*

Enter Variadic Generics. Let's define a **Variadic Generic** as a generic type
that can refer to *multiple instances* of *different concrete types*, all
eventually conforming to the parameter definition.

This means, for example, that if there exists a function parametrised by a
Variadic Generic conforming to the `Collection` protocol you can pass to this
function an `Array`, a `Dictionary` and a `String` all toghether, because all
these types conform to `Collection`.

Let's see how the `zip` example might look like using Variadic Generics:
```swift
struct ZipSequence<Sequences...: Sequence> {
  private let sequences: Sequences

  init(_ sequences: Sequences) {
    this.sequences = sequences
  }
}

extension ZipSequence {
  struct Iterator {
    var baseStreams: Sequences.Iterator
    var reachedEnd: Bool = false

    init(_ iterators: Sequences.Iterator) {
      baseStreams = iterators
    }
  }
}

extension ZipSequence.Iterator: IteratorProtocol {
  func next() -> Sequences.Element? {
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
    return Iterator(sequences.makeIterator())
  }

  var underestimatedCount: Int {
    return Swift.min(#expand(sequences.underestimatedCount))
  }
}

func zip<Sequences...: Sequence>(_ sequences: Sequences)-> ZipSequence<Sequences> {
  return ZipSequence(sequences)
}
```
Some foreign syntax apart, the code is practically the same of the current `zip`
implementation, but can now be used to zip any number of arbirtary and different
sequences togheter.

## Detailed design
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

This section is going to describe how to use Variadic Generics, how to interact
with them and how they fit into Swift.

### Variadic Generics and tuples
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

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
constraint is specified, there is at least the top type `Any` (i.e. `protocol<>`)
to unify them. So why not represent Variadic Generics as arrays, too?
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

<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->
Describe the design of the solution in detail. If it involves new
syntax in the language, show the additions and changes to the Swift
grammar. If it's a new API, show the full API and its documentation
comments detailing what it does. The detail in this section should be
sufficient for someone who is *not* one of the authors to be able to
reasonably implement the feature.

## Impact on existing code

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
