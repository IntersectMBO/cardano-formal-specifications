# Cardano Formal Specifications

This repository is used to collect various formal methods projects on Cardano. In the future it may also house a 'Cardano Node properties' library, which would combine the Consensus and Ledger specifications and prove combined properties about them that cannot be stated or proven about either of them alone.

| Project                                          | Link                                                                                                                       |
|--------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------|
| Consensus Specification (Block Header) [^1] [^2] | https://ouroboros-consensus.cardano.intersectmbo.org/assets/files/consensus-spec-agda-fdee8d65f730471bd62e2177650a579d.pdf https://github.com/IntersectMBO/ouroboros-consensus/tree/main/docs/agda-spec |
| Ledger Specification                             | https://github.com/IntersectMBO/formal-ledger-specifications                                                               |
| Plutus Metatheory                                | https://github.com/IntersectMBO/plutus/tree/master/plutus-metatheory                                                       |

[^1]: The block header specification is executable and usable for conformance testing.

[^2]: A formal model of Ouroboros Praos and its corresponding proofs of safety and liveness are currently WIP, see https://github.com/input-output-hk/ouroboros-praos-formal-spec.
