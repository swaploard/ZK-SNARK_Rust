# zk-SNARK Rust Workspace

A modular, end-to-end **zk-SNARK implementation in Rust**, structured as a Cargo workspace. This project is designed to clearly separate **cryptographic primitives**, **circuit definition**, **trusted setup**, **proving**, **verification**, and **network-level protocol concerns**.

The goal is **educational + production-aligned**: to understand how zk-SNARK systems work under the hood while keeping the architecture close to how real-world systems (Groth16-style flows) are built.

---

## High-level Architecture

```text
Witness ──▶ Circuit ──▶ Trusted Setup ──▶ Prover ──▶ Proof
                                             │
                                             ▼
                                        Network / Protocol
                                             │
                                             ▼
                                          Verifier
```

Each stage is isolated into its own crate to avoid monolithic design and to make responsibilities explicit.

---

## Workspace Members

### 1. `circuit`

Defines the **constraint system** used by the zk-SNARK.

**Responsibilities:**

* Constraint representation (R1CS-style)
* Witness assignment
* Field arithmetic abstractions
* Circuit validation

This crate answers:

> *What is being proven?*

---

### 2. `trusted_setup`

Implements the **trusted setup phase**.

**Responsibilities:**

* Generation of proving key (PK)
* Generation of verification key (VK)
* Uses structured reference string (SRS)

This crate answers:

> *What public parameters are required for this circuit?*

In production systems, this would be replaced with:

* Multi-party computation (MPC), or
* A universal setup

---

### 3. `prover`

Generates zk-SNARK proofs.

**Responsibilities:**

* Takes witness + proving key
* Performs polynomial commitments
* Computes proof elements using elliptic curve operations

This crate answers:

> *How is the proof generated without revealing the witness?*

---

### 4. `verifier`

Verifies zk-SNARK proofs.

**Responsibilities:**

* Takes proof + public inputs + verification key
* Runs pairing checks
* Outputs a boolean validity result

This crate answers:

> *Can this proof be verified efficiently and trustlessly?*

---

### 5. `protocol`

Defines the **logical zk-SNARK protocol flow**.

**Responsibilities:**

* Message formats
* Proof lifecycle (setup → prove → verify)
* Shared data structures across prover/verifier/network

This crate answers:

> *How do all components talk to each other coherently?*

---

### 6. `network`

Handles **communication and transport**.

**Responsibilities:**

* Async networking
* Proof submission
* Verification requests
* Protocol-level message routing

Built on `tokio` and `futures`.

This crate answers:

> *How are proofs transmitted between parties?*

---

### 7. `logger`

Centralized logging and tracing utilities.

**Responsibilities:**

* Structured logs
* Trace-level debugging for cryptographic flows
* Shared logging configuration

This crate answers:

> *How do we observe and debug the system safely?*

---

## Shared Dependencies

Defined at the workspace level for consistency:

* **Cryptography**

  * `ff` – finite field traits
  * `bls12_381` – pairing-friendly curve

* **Async & Concurrency**

  * `tokio` (multi-thread runtime)
  * `futures`

* **Utilities**

  * `serde` – serialization
  * `eyre` – error handling
  * `itertools`, `indexmap`
  * `test-log` – traceable tests

---

## Getting Started

### Build the workspace

```bash
cargo build --workspace
```

### Run tests

```bash
cargo test --workspace
```

### Typical Flow

1. Define a circuit in `circuit`
2. Run trusted setup via `trusted_setup`
3. Generate proof using `prover`
4. Transmit proof via `network`
5. Verify proof using `verifier`

---

## Design Goals

* Clear separation of concerns
* Minimal magic, maximum clarity
* Cryptography-first design
* Async-ready networking
* Suitable for blockchain / rollup / privacy systems

---

## Security Notes

* This project is **not audited**
* Trusted setup is **single-party**
* Not production-safe without MPC / universal setup

Use for **learning, experimentation, and research**.

---

## Inspiration

* Groth16 zk-SNARKs
* Circom / snarkjs architecture
* Zcash Sapling design
* Halo / PLONK system layouts

---

## Who is this for?

* Rust developers learning zero-knowledge proofs
* Blockchain engineers building privacy layers
* Researchers experimenting with zk protocols


