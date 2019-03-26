## Detailed design
### Part 1: empowering tuples
<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->

Tuples are a great tool in the belt of Swift developers, allowing one to pack
related values together without the need to declare a completely new type. In
order to support the design of Variadic Generics we need additional language
support for tuples, but these features are so general that we suggest to add
them regardless of Variadic Generics.

#### Unpack / repack

The first thing is the ability to unpack and repack tuple values, the former al-
so known as "tuple splatting". This will allow for tuples to be "expanded" into
the surrounding context where appropriate, and for multiple tuples to be com-
bined together more easily:
```swift
func sum4(a: Int, b: Int, c: Int, d: Int) -> Int {
  return a + b + c + d
}

let t1 = (a: 1, b: 2)
let t2 = (c: 3, d: 4)

// Unpack and repack
let bigTuple = (t1..., t2...) // (a: 1, b: 2, c: 3, d: 4)

// Unpack only
sum4(bigTuple...) // 10

let copyOfT1 = t1
let awkwardCopyOfT1 = (t1...)

// Unpack and repack with additional elements
let bigTuple2 = (a: 1, 2, t2...) // (a: 1, 2, c: 3, d: 4)
sum4(bigTuple2...) // error: arguments do not match
let bigTuple3 = (t1..., c: 3, d: 4) // (a: 1, b: 2, c: 3, d: 4)
sum4(bigTuple3...) // valid

// Works with types, too
typealias IntPair = (Int, Int)
typealias IntQuad = (IntPair..., IntPair...) // (Int, Int, Int, Int)

// But beware: this is still a standard variadic function, and in this specific
// case you can simply pass infinite `(Int, Int)` values
func variadicFunction(_ values: IntPair...) { }

// These functions take a single `(Int, Int)` value
func fn1(_ values: IntPair) { }
func fn2(_ values: (IntPair...)) { }
```


<!---    1         2         3         4         5         6         7      --->
<!---67890123456789012345678901234567890123456789012345678901234567890123456--->


The <#n> thing is a new syntax to access a tuple's elements. Today one can access tuple elements by label or index:
```swift
let unlabeledTuple = (1, "Hello")
let tuple = (aNumber: 42, aString: "World!")

unlabeledTuple.0 // 1
unlabeledTuple.1 // "Hello"
tuple.aNumber // 42
tuple.aString // "World!"
tuple.0 // 42
tuple.1 // "World!"
```
