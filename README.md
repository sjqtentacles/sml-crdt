# sml-crdt

[![CI](https://github.com/sjqtentacles/sml-crdt/actions/workflows/ci.yml/badge.svg)](https://github.com/sjqtentacles/sml-crdt/actions/workflows/ci.yml)

State-based convergent replicated data types (CvRDTs) for Standard ML.

A CvRDT is a value together with a join-semilattice `merge` -- a commutative,
associative, idempotent least-upper-bound -- and monotone mutators. Replicas
exchange whole states and merge them; because `merge` is a semilattice join,
replicas that have observed the same set of updates converge to the same value
regardless of the order or number of merges (strong eventual consistency).

This library provides four classic CvRDTs:

- **`GCounter`** -- a grow-only counter. State is a per-replica count; `value`
  is the sum across replicas; `merge` takes the pointwise maximum.
- **`PNCounter`** -- an increment/decrement counter built from two `GCounter`s
  (a positive tally and a negative tally); `value` is their difference.
- **`LWWRegister`** -- a last-write-wins register holding a single `'a` value
  tagged with a caller-supplied timestamp and replica. `merge` keeps the higher
  timestamp; ties are broken deterministically by the higher replica id.
- **`ORSet`** -- an observed-remove set with correct add / remove / re-add
  semantics. Each `add` stamps the element with a globally unique tag
  (`replica * sequence`); `remove` tombstones exactly the tags observed locally;
  an element is present iff it has an untombstoned add. A concurrent add whose
  tag the remove never observed survives the merge.

Everything is exported through the single opaque structure `Crdt` (see
`crdt.sig`).

## Portability

The library is pure Standard ML using only the Basis library -- no FFI, no
threads, no external dependencies. Verified on **MLton** and **Poly/ML**, with
byte-identical, deterministic test output across both.

The test suite additionally uses [`sml-test`](https://github.com/sjqtentacles/sml-test)
for property-based testing; it is a **test-only** dependency, vendored under
`lib/` and wired in from `test/sources.mlb`. The library's own basis
(`lib/github.com/sjqtentacles/sml-crdt/sources.mlb`) stays dependency-free.

## Building and testing

```sh
make test        # build + run the suite under MLton (default)
make test-poly   # run the suite under Poly/ML
make all-tests   # run under both
make clean
```

## Installing with smlpkg

```sh
smlpkg add github.com/sjqtentacles/sml-crdt
smlpkg sync
```

Then reference the library basis from your own `.mlb`:

```
lib/github.com/sjqtentacles/sml-crdt/sml-crdt.mlb
```

For Poly/ML, `use` the `crdt.sig` and `crdt.sml` sources in order.

## Examples

```sml
(* A grow-only counter across two replicas, merged. *)
val a = Crdt.GCounter.incBy 0 3 Crdt.GCounter.empty
val b = Crdt.GCounter.incBy 1 4 Crdt.GCounter.empty
val v = Crdt.GCounter.value (Crdt.GCounter.merge (a, b))   (* 7 *)

(* Last-write-wins register; higher timestamp wins. *)
val r = Crdt.LWWRegister.merge
          ( Crdt.LWWRegister.set {ts=1, replica=0} "old" Crdt.LWWRegister.empty
          , Crdt.LWWRegister.set {ts=2, replica=1} "new" Crdt.LWWRegister.empty )
val cur = Crdt.LWWRegister.value r   (* SOME "new" *)

(* Observed-remove set over ints. *)
val s = Crdt.ORSet.empty (op = : int * int -> bool)
val s = Crdt.ORSet.add 0 42 s
val present = Crdt.ORSet.member 42 (Crdt.ORSet.remove 42 s)   (* false *)
```

Running [`examples/demo.sml`](examples/demo.sml) with `make example` simulates
two replicas mutating a `GCounter`, a `PNCounter`, an `LWWRegister`, and an
`ORSet` independently, then merges each pair to show convergence (output is
byte-identical under MLton and Poly/ML):

```
sml-crdt demo

GCounter (replicas 1 and 2 increment independently):
  replica 1 value = 6
  replica 2 value = 3
  merged value    = 9
  merged canonical = [(1,6),(2,3)]

PNCounter (replica 1 increments, replica 2 decrements):
  replica 1 value = 9
  replica 2 value = ~4
  merged value    = 5

LWWRegister (higher logical timestamp wins on merge):
  merge ts=1 "hello" with ts=2 "world" -> world
  tie at ts=5, replica 1 "A" vs replica 2 "B" -> B (higher replica wins)

ORSet (concurrent add survives a remove that never observed it):
  replica 1 after remove: member 42 = false
  replica 2 concurrent add: member 42 = true
  merged: member 42 = true (survives: the concurrent add's tag was never tombstoned)
  single-replica add then remove: member 99 = false
```

## Design notes

- **Canonical state.** Counters keep per-replica maps as assoc lists sorted by
  replica with zero entries dropped; the OR-set keeps its adds and tombstones
  as tag-sorted, deduped lists. Canonical representations make the semilattice
  laws hold structurally and make equality-by-value well defined and
  deterministic -- which is what the property tests compare.
- **OR-set element equality.** `ORSet` is value-polymorphic, so it cannot use
  SML's built-in `=`. Each set carries an explicit `eq : 'a * 'a -> bool`
  supplied at construction (`ORSet.empty eq`); all of the set's operations use
  it. This keeps the library dependency-free while supporting arbitrary element
  types.
- **Property tests.** The algebraic laws (commutativity, associativity,
  idempotence of `merge`) are checked with `Test.Prop` from the vendored
  `sml-test`, using a fixed seed for reproducibility. Random states are built by
  generating small operation logs (lists of `(replica, op)`) and replaying them.
  Because OR-set tags must be globally unique, independently generated operands
  draw from disjoint replica-id pools so they never mint the same tag for
  different elements.
