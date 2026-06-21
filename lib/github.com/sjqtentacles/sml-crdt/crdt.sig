(* crdt.sig

   State-based convergent replicated data types (CvRDTs) for Standard ML.

   A CvRDT is a value together with a join-semilattice `merge` (a commutative,
   associative, idempotent least-upper-bound) and monotone mutators. Replicas
   exchange whole states and merge them; because `merge` is a semilattice join,
   replicas that have seen the same set of updates converge to the same value
   regardless of the order or number of merges (strong eventual consistency).

   Replica identities are explicit `int`s. Each sub-structure exposes:
     - type t                  the replica-local state;
     - val value  : t -> ...   the observable value (the query);
     - val merge  : t * t -> t  the semilattice join;
     - the type-specific mutators.

   This module depends on nothing beyond the Basis. *)

signature CRDT =
sig
  (* A replica identifier. *)
  type replica = int

  (* ---- GCounter: grow-only counter --------------------------------------
     State is a per-replica count. `inc r` bumps replica r's own count; the
     observable value is the sum across replicas; `merge` takes the pointwise
     maximum of the two replicas' counts (the semilattice join). Counts only
     ever grow, so the pointwise max is monotone. *)
  structure GCounter :
  sig
    type t
    val empty : t
    (* Increment replica r's count by one. *)
    val inc   : replica -> t -> t
    (* Increment replica r's count by n (n >= 0). Raises Domain if n < 0. *)
    val incBy : replica -> int -> t -> t
    val value : t -> int
    val merge : t * t -> t
    (* A canonical, order-independent view of the state: (replica, count)
       pairs sorted by replica, omitting zero counts. Two states are equal as
       CRDTs iff their canonical forms are equal. *)
    val canonical : t -> (replica * int) list
  end

  (* ---- PNCounter: increment/decrement counter ---------------------------
     A counter that supports both `inc` and `dec`, built from two GCounters: a
     positive tally `p` and a negative tally `n`. The value is sum(p) - sum(n);
     `merge` joins the two halves componentwise. Because each half is a
     grow-only GCounter, the whole is still a semilattice. *)
  structure PNCounter :
  sig
    type t
    val empty : t
    val inc   : replica -> t -> t
    val dec   : replica -> t -> t
    (* Add/subtract n (n >= 0). Raise Domain if n < 0. *)
    val incBy : replica -> int -> t -> t
    val decBy : replica -> int -> t -> t
    val value : t -> int
    val merge : t * t -> t
    (* Canonical form: (positive half, negative half) each as sorted assoc
       lists, so equality-by-value is well defined. *)
    val canonical : t -> (replica * int) list * (replica * int) list
  end

  (* ---- LWWRegister: last-write-wins register ----------------------------
     A register holding a single 'a value, tagged with the timestamp and
     replica of the write that set it. `merge` keeps the write with the higher
     timestamp; ties are broken deterministically by the higher replica id
     (so merge is commutative even at equal timestamps). An untouched register
     holds no value.

     Timestamps are caller-supplied logical clocks (`int`); the library does
     not invent them. This keeps the type dependency-free and lets callers use
     whatever clock (Lamport, wall-clock, ...) they like. *)
  structure LWWRegister :
  sig
    type 'a t
    (* A register that has never been written. *)
    val empty : 'a t
    (* set {ts, replica} v r: record that replica wrote v at time ts. *)
    val set   : { ts : int, replica : replica } -> 'a -> 'a t -> 'a t
    (* The current value, or NONE if never written. *)
    val value : 'a t -> 'a option
    (* The full winning entry (value plus its timestamp/replica), or NONE. *)
    val entry : 'a t -> { ts : int, replica : replica, value : 'a } option
    val merge : 'a t * 'a t -> 'a t
  end

  (* ---- ORSet: observed-remove set ---------------------------------------
     A set with correct add / remove / re-add semantics. Each `add` stamps the
     element with a globally unique tag (replica id paired with a per-replica
     sequence number); `remove` tombstones exactly the tags observed in the
     local state at that moment. An element is present iff it has at least one
     tagged add that has not been tombstoned. Re-adding after a remove mints a
     fresh tag, so the element comes back; and a concurrent add whose tag was
     never observed by the remove survives the merge (observed-remove).

     Element equality: the structure is value-polymorphic, so it cannot rely
     on SML's built-in `=`. Each set therefore carries an explicit
     `eq : 'a * 'a -> bool` supplied at construction via `empty`. All
     operations on a set use that set's `eq`; `merge` uses the `eq` of its
     first argument (the two sides are expected to describe the same logical
     set and so share an equality). This keeps the library dependency-free
     while supporting arbitrary element types. *)
  structure ORSet :
  sig
    type 'a t

    (* A tag uniquely identifying one `add`: (replica, per-replica sequence). *)
    type tag = replica * int

    (* An empty set using the given element equality. *)
    val empty : ('a * 'a -> bool) -> 'a t

    (* add r e s: replica r adds e under a fresh unique tag. *)
    val add    : replica -> 'a -> 'a t -> 'a t
    (* Tombstone every currently-observed tag for e (an observed remove). *)
    val remove : 'a -> 'a t -> 'a t

    (* Membership and the set of distinct live elements (in the order their
       surviving tags were first added). *)
    val member : 'a -> 'a t -> bool
    val value  : 'a t -> 'a list

    val merge  : 'a t * 'a t -> 'a t

    (* Canonical state for equality-by-value in tests: the live (element, tag)
       adds and the tombstoned tags, each as a list. Elements appear via their
       tags, so the lists are comparable with `=` for an equality element
       type. Tags are globally unique, giving a deterministic canonical form. *)
    val canonical : 'a t -> { adds : ('a * tag) list, tombstones : tag list }
  end
end
