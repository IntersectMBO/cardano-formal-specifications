# Cardano Formal Specifications

This repository is used to provide a top level entry point to the full
collection of formal specifications for Cardano for current and future
features.

Most specifications are in their own repositories which we link to
below. The Cardano performance model will be is in this repo. In the
future it may also house a 'Cardano Node properties' library, which
would combine the Consensus and Ledger specifications and prove
combined properties about them that cannot be stated or proven about
either of them alone.

This readme is in three sections: section one is a table of contents
listing all the areas the specifications cover; section two goes into
more detail about the current state of each component of the node,
with links to pdf artifacts, and source repositories; section three
covers new features for Cardano that are currently in the R&D phase.

# Table of Contents

| Components  |
|-------------|
| [Plutus Core](#plutus-specifications) |
| [Ledger](#ledger-specifications) |
| [Consensus](#consensus-specifications) |
| [Networking](#networking-specifications) |
| [Performance model](#performance-model) |

| New features |
|--------------|  
| Babel fees   |
| Peras        |
| Leios        |

# Cardano Specifications

The Cardano Specifications are intended to provide a language agnostic
single-source-of-truth reference for key components of the
system. The are intended to communicate how the system works to
implementors, innovators, and technical community members.

A second important use beyond a reference of how the system is now is
to provide a baseline and communication medium for developing new
feastures.

The specifications are presented in formal notation using computer
science concepts familiar to somebody who has completed the first
year of a undergradate computer science course or who has equivalent
professional experience.

The approach we have taken to write the specs has evolved over
time. We started with PDF documents written in LaTeX and now we
generate PDFs and also executable specifications (reference
implementations/test oracles usually in Haskell). While aiming to stay
as close as possible to the style for the reader we now write our
specifications in Agda and generate both the readable PDF and the
executable specifations from the Agda source code. This gives us the
benefit of type checking our definitions, using the executable
specifations for conformance testing and also the ability to prove
properties about the design and have these formally verified.

## Plutus specifications

Plutus has two specifications:

1. The [metatheory](https://github.com/IntersectMBO/plutus/tree/master/plutus-metatheory)
is an Agda specification of both the typed and untyped layers of the
Plutus Core language, and includes a specification for the CEK machine
which evaluates them.

2. Additionally, there is the
"[paper](https://plutus.cardano.intersectmbo.org/resources/plutus-core-spec.pdf)"
specification document which definies the language elements, and has
denotational semantics for each of the supported builtin operations.

Discussions are ongoing about how to merge or otherwise get the both
of both worlds here.

### Properties

### Testing

Plutus also has a conformance test suite which can be used to ensure
conformant behaviour between executable specification, Haskell and
other implementations. This was previously used to conformance test a
uplc implementation built by RV.

## Ledger specifications

The ledger is our biggest and longest running specification project.

We have specifications going all the way back to Byron and the whole
system up to the present Conway era is covered.

Earlier specs (Byron to Babbage) are documents written in
mathematical/Computer Science notation. They are written in LaTeX and
readable in PDF.

Since Conway we have moved to a machine checked specification written
in Agda, again readable in PDF but also type checked, executable and
with machine verified proofs of key properties. The aim of this
project was to stay as close as possible to the style of the earlier
specifications for readability, while gaining the benefits of type
checking the definitions, and new abilities to compile to Haskell for
testing purposes and supporting machine checked proofs of properties
such as preservation of value.

Earlier specs followed an approach of adding a 'delta' spec for the
functionality added by each new era rather than a complete new self
contained spec. We did this also for Conway but we're currently
working adding all the functionality from previous eras into a
complete spec - contributions welcome!


Era | Design Documents | Formal Specification | CDDL
----|------------------|----------------------|-----
Byron | | [Chain Spec](https://github.com/intersectmbo/cardano-ledger/releases/latest/download/byron-blockchain.pdf "Specification of the Blockchain Layer"), [Ledger Spec](https://github.com/intersectmbo/cardano-ledger/releases/latest/download/byron-ledger.pdf "A Formal Specification of the Cardano Ledger") | [CDDL](https://github.com/intersectmbo/cardano-ledger/tree/master/eras/byron/cddl-spec/byron.cddl), [PDF](https://github.com/intersectmbo/cardano-ledger/releases/latest/download/byron-binary.pdf)
Shelley | [Design](https://github.com/intersectmbo/cardano-ledger/releases/latest/download/shelley-delegation.pdf "Design Specification for Delegation and Incentives in Cardano") | [Spec](https://github.com/intersectmbo/cardano-ledger/releases/latest/download/shelley-ledger.pdf "A Formal Specification of the Cardano Ledger") | [CDDL](https://github.com/intersectmbo/cardano-ledger/tree/master/eras/shelley/impl/cddl-files)
Allegra | Same as Mary era below | Same as Mary era below | [CDDL](https://github.com/intersectmbo/cardano-ledger/tree/master/eras/allegra/impl/cddl-files)
Mary | [Multi-Currency](https://eprint.iacr.org/2020/895 "Multi-Currency Ledgers"), [UTXOma](https://iohk.io/en/research/library/papers/utxoma-utxo-with-multi-asset-support/ "UTXOma:UTXO with Multi-Asset Support") | [Spec](https://github.com/intersectmbo/cardano-ledger/releases/latest/download/mary-ledger.pdf "A Formal Specification of the Cardano Ledger with a Native Multi-Asset Implementation") | [CDDL](https://github.com/intersectmbo/cardano-ledger/tree/master/eras/mary/impl/cddl-files)
Alonzo | [eUTXO](https://iohk.io/en/research/library/papers/the-extended-utxo-model/ "The Extended UTXO Model")| [Spec](https://github.com/intersectmbo/cardano-ledger/releases/latest/download/alonzo-ledger.pdf "A Formal Specification of the Cardano Ledger integrating Plutus Core") | [CDDL](https://github.com/intersectmbo/cardano-ledger/tree/master/eras/alonzo/impl/cddl-files)
Babbage | [batch-verification](https://iohk.io/en/research/library/papers/on-uc-secure-range-extension-and-batch-verification-for-ecvrf/ "On UC-Secure Range Extension and Batch Verification for ECVRF"), [CIP-31](https://github.com/cardano-foundation/CIPs/pull/159 "Reference inputs"), [CIP-32](https://github.com/cardano-foundation/CIPs/pull/160 "Inline datums"), [CIP-33](https://github.com/cardano-foundation/CIPs/pull/161 "Reference scripts") | [Spec](https://github.com/intersectmbo/cardano-ledger/releases/latest/download/babbage-ledger.pdf "Formal Specification of the Cardano Ledger for the Babbage era") | [CDDL](https://github.com/intersectmbo/cardano-ledger/tree/master/eras/babbage/impl/cddl-files)
| Conway | | https://github.com/IntersectMBO/formal-ledger-specifications |

Other Documents:
- [Non-integer calculations specification](https://github.com/intersectmbo/cardano-ledger/releases/latest/download/non-integer-calculations.pdf): details on the parts of the Shelley specification that use real numbers.
- [Stake pool ranking specification](https://github.com/intersectmbo/cardano-ledger/releases/latest/download/pool-ranking.pdf): details for a robust stake pool ranking mechanism.
- [Explanation of the small-step-semantics framework](https://github.com/intersectmbo/cardano-ledger/releases/latest/download/small-step-semantics.pdf): a guide to the notation and style used by our ledger rules.

### Properties

### Testing

The ledger specification is actively used for conformance testing. As
more previous era features are added more coverage will be possible.

## Consensus specifications

The consensus specification is a relatively new effort that in the
first instance focusses on block headers. This is essentially the part
of the chain that the consensus layer cares about.

Generated PDF file:
https://ouroboros-consensus.cardano.intersectmbo.org/assets/files/consensus-spec-agda-fdee8d65f730471bd62e2177650a579d.pdf

Source repository:
https://github.com/IntersectMBO/ouroboros-consensus/tree/main/docs/agda-spec

A formal model of Ouroboros Praos and its corresponding proofs of
safety and liveness are currently WIP, see
https://github.com/input-output-hk/ouroboros-praos-formal-spec.

### Properties

### Testing

The block header specification is executable and usable for conformance testing.

## Networking specifications


## Performance model

This repository includes a subdirectory src/performance that contains
a literate Haskell file that documents a performance model of Cardano
block diffusion. This is intended to provide a baseline from which the
potential consequences of parameter or design changes on the
timeliness of block diffusion can be investigated.

# New features

## Babel fees

## Peras

## Leios
