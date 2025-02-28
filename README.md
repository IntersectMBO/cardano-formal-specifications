# Cardano Formal Specifications

This repository is used to collect various formal methods projects on Cardano. In the future it may also house a 'Cardano Node properties' library, which would combine the Consensus and Ledger specifications and prove combined properties about them that cannot be stated or proven about either of them alone.

| Project                 | Link                                                                         |
|-------------------------|------------------------------------------------------------------------------|
| Consensus Specification | https://github.com/IntersectMBO/ouroboros-consensus/tree/main/docs/agda-spec |
| Ledger Specification    | https://github.com/IntersectMBO/formal-ledger-specifications                 |
| Plutus Metatheory       | https://github.com/IntersectMBO/plutus/tree/master/plutus-metatheory         |

## Ledger specifications

Beside the specifications in the above repository, there are older specification documents for the eras before Conway. They can be found here:

Era | Design Documents | Formal Specification | CDDL
----|------------------|----------------------|-----
Byron | | [Chain Spec](https://github.com/intersectmbo/cardano-ledger/releases/latest/download/byron-blockchain.pdf "Specification of the Blockchain Layer"), [Ledger Spec](https://github.com/intersectmbo/cardano-ledger/releases/latest/download/byron-ledger.pdf "A Formal Specification of the Cardano Ledger") | [CDDL](https://github.com/intersectmbo/cardano-ledger/tree/master/eras/byron/cddl-spec/byron.cddl), [PDF](https://github.com/intersectmbo/cardano-ledger/releases/latest/download/byron-binary.pdf)
Shelley | [Design](https://github.com/intersectmbo/cardano-ledger/releases/latest/download/shelley-delegation.pdf "Design Specification for Delegation and Incentives in Cardano") | [Spec](https://github.com/intersectmbo/cardano-ledger/releases/latest/download/shelley-ledger.pdf "A Formal Specification of the Cardano Ledger") | [CDDL](https://github.com/intersectmbo/cardano-ledger/tree/master/eras/shelley/impl/cddl-files)
Allegra | Same as Mary era below | Same as Mary era below | [CDDL](https://github.com/intersectmbo/cardano-ledger/tree/master/eras/allegra/impl/cddl-files)
Mary | [Multi-Currency](https://eprint.iacr.org/2020/895 "Multi-Currency Ledgers"), [UTXOma](https://iohk.io/en/research/library/papers/utxoma-utxo-with-multi-asset-support/ "UTXOma:UTXO with Multi-Asset Support") | [Spec](https://github.com/intersectmbo/cardano-ledger/releases/latest/download/mary-ledger.pdf "A Formal Specification of the Cardano Ledger with a Native Multi-Asset Implementation") | [CDDL](https://github.com/intersectmbo/cardano-ledger/tree/master/eras/mary/impl/cddl-files)
Alonzo | [eUTXO](https://iohk.io/en/research/library/papers/the-extended-utxo-model/ "The Extended UTXO Model")| [Spec](https://github.com/intersectmbo/cardano-ledger/releases/latest/download/alonzo-ledger.pdf "A Formal Specification of the Cardano Ledger integrating Plutus Core") | [CDDL](https://github.com/intersectmbo/cardano-ledger/tree/master/eras/alonzo/impl/cddl-files)
Babbage | [batch-verification](https://iohk.io/en/research/library/papers/on-uc-secure-range-extension-and-batch-verification-for-ecvrf/ "On UC-Secure Range Extension and Batch Verification for ECVRF"), [CIP-31](https://github.com/cardano-foundation/CIPs/pull/159 "Reference inputs"), [CIP-32](https://github.com/cardano-foundation/CIPs/pull/160 "Inline datums"), [CIP-33](https://github.com/cardano-foundation/CIPs/pull/161 "Reference scripts") | [Spec](https://github.com/intersectmbo/cardano-ledger/releases/latest/download/babbage-ledger.pdf "Formal Specification of the Cardano Ledger for the Babbage era") | [CDDL](https://github.com/intersectmbo/cardano-ledger/tree/master/eras/babbage/impl/cddl-files)

Other Documents:
- [Non-integer calculations specification](https://github.com/intersectmbo/cardano-ledger/releases/latest/download/non-integer-calculations.pdf): details on the parts of the Shelley specification that use real numbers.
- [Stake pool ranking specification](https://github.com/intersectmbo/cardano-ledger/releases/latest/download/pool-ranking.pdf): details for a robust stake pool ranking mechanism.
- [Explanation of the small-step-semantics framework](https://github.com/intersectmbo/cardano-ledger/releases/latest/download/small-step-semantics.pdf): a guide to the notation and style used by our ledger rules.
