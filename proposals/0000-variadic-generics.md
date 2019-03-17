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

Describe the problems that this proposal seeks to address. If the
problem is that some common pattern is currently hard to express, show
how one can currently get a similar effect and describe its
drawbacks. If it's completely new functionality that cannot be
emulated, motivate why this new functionality would help Swift
developers create better Swift code.

## Proposed solution

Describe your solution to the problem. Provide examples and describe
how they work. Show how your solution is better than current
workarounds: is it cleaner, safer, or more efficient?

## Detailed design

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
