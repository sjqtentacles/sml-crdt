(* Tests for sml-crdt.

   Concrete scenarios use the plain `Harness` (deterministic, dependency-free
   output). The algebraic semilattice laws -- commutativity, associativity,
   idempotence of `merge` -- are checked with property-based testing via
   `Test.Prop` from the vendored sml-test, comparing states by their canonical
   value so equality is well defined and reproducible. *)

structure CrdtTests =
struct
  open Harness
  structure P = Test.Prop

  structure GC = Crdt.GCounter

  (* ---- Property-test plumbing -------------------------------------------
     A "program" is a list of (replica, n) increments. We replay programs to
     build random CRDT states, then check the laws on those states. Replica
     ids are drawn from a small set so independent programs share replicas
     (otherwise merges would be trivial). *)
  val genReplica = P.Gen.intRange (0, 3)
  val genAmount  = P.Gen.intRange (0, 5)
  val genOp      = P.Gen.tuple2 (genReplica, genAmount)
  val genProgram = P.Gen.list genOp

  fun gcOf ops = List.foldl (fn ((r, n), s) => GC.incBy r n s) GC.empty ops
  val genGC = P.Gen.map gcOf genProgram

  (* Equality of GCounters by canonical form. *)
  fun gcEq (a, b) = GC.canonical a = GC.canonical b

  (* PNCounter generator: a program of signed ops (replica, delta). *)
  structure PN = Crdt.PNCounter
  val genSigned = P.Gen.tuple2 (genReplica, P.Gen.intRange (~5, 5))
  fun pnOf ops =
    List.foldl (fn ((r, d), s) =>
                  if d >= 0 then PN.incBy r d s else PN.decBy r (~d) s)
               PN.empty ops
  val genPN = P.Gen.map pnOf (P.Gen.list genSigned)
  fun pnEq (a, b) = PN.canonical a = PN.canonical b

  (* LWWRegister generator: a sequence of writes. Each write is a (ts, replica)
     pair; the stored value is a deterministic function of the tag so that two
     writes sharing a (ts, replica) carry the SAME value -- otherwise merge
     could not be commutative at a genuine tie, and such a clash represents an
     impossible history (one writer, one instant, two values). *)
  structure LWW = Crdt.LWWRegister
  val genWrite = P.Gen.tuple2 (P.Gen.intRange (0, 4), genReplica)  (* (ts, replica) *)
  fun valOf (ts, r) = ts * 10 + r
  fun lwwOf writes =
    List.foldl (fn ((ts, r), s) =>
                  LWW.set { ts = ts, replica = r } (valOf (ts, r)) s)
               LWW.empty writes
  val genLWW = P.Gen.map lwwOf (P.Gen.list genWrite)
  fun lwwEq (a, b) = LWW.entry a = LWW.entry b

  (* ORSet over int elements with integer equality. A program is a list of
     ops, each either an add (replica, elem) or a remove (elem); replaying a
     program against ORSet.empty builds a random state. We compare states by
     canonical form (live adds + tombstones), which is deterministic because
     tags are globally unique and kept sorted. *)
  structure OR = Crdt.ORSet
  val intEq : int * int -> bool = (op =)
  datatype orop = Add of int * int | Rem of int
  val genElem = P.Gen.intRange (0, 3)
  (* Build an ORSet program over a GIVEN set of replica ids. In a real system
     tags (replica, seq) are globally unique; two independently generated
     states must therefore not reuse replica ids, or they could mint the same
     tag for different elements and break merge's commutativity. We honour that
     invariant by parameterising the generator over a disjoint replica pool per
     operand. *)
  fun genOrOpOn replicas =
    P.Gen.oneof
      [ P.Gen.map Add (P.Gen.tuple2 (P.Gen.choose replicas, genElem))
      , P.Gen.map Rem genElem ]
  fun orOf ops =
    List.foldl (fn (Add (r, e), s) => OR.add r e s
                 | (Rem e, s) => OR.remove e s)
               (OR.empty intEq) ops
  fun genOROn replicas = P.Gen.map orOf (P.Gen.list (genOrOpOn replicas))
  (* Disjoint replica pools so independent operands never collide on a tag. *)
  val genOR  = genOROn [0, 1]
  val genORb = genOROn [2, 3]
  val genORc = genOROn [4, 5]
  fun orEq (a, b) =
    let val ca = OR.canonical a and cb = OR.canonical b
    in #adds ca = #adds cb andalso #tombstones ca = #tombstones cb end
  (* Portable insertion sort (avoid compiler-specific ListMergeSort). *)
  fun sortInts xs =
    let fun ins (x, []) = [x]
          | ins (x, y :: ys) = if x <= y then x :: y :: ys else y :: ins (x, ys)
    in List.foldr ins [] xs end

  fun run () =
    let
      val () = section "GCounter: concrete inc / value"
      val g0 = GC.empty
      val () = checkInt "empty is 0" (0, GC.value g0)
      val g1 = GC.inc 1 (GC.inc 1 (GC.inc 0 g0))   (* r0:1, r1:2 *)
      val () = checkInt "three incs sum to 3" (3, GC.value g1)
      val () = checkInt "incBy adds n" (10, GC.value (GC.incBy 2 7 g1))
      val () = checkRaises "incBy negative raises" (fn () => GC.incBy 0 ~1 g0)

      val () = section "GCounter: merge is pointwise max"
      (* a: r0=3 ; b: r0=1, r1=4. Max => r0=3, r1=4 => 7. *)
      val a = GC.incBy 0 3 GC.empty
      val b = GC.incBy 1 4 (GC.incBy 0 1 GC.empty)
      val () = checkInt "merge takes max per replica" (7, GC.value (GC.merge (a, b)))
      val () = checkBool "merge is symmetric here"
                 (true, gcEq (GC.merge (a, b), GC.merge (b, a)))

      val () = section "GCounter: 3-replica convergence (any merge order)"
      (* Three replicas each make local progress, then exchange in different
         orders; all orders must converge to the same value. *)
      val ra = GC.incBy 0 2 GC.empty
      val rb = GC.incBy 1 5 GC.empty
      val rc = GC.incBy 2 1 (GC.incBy 0 1 GC.empty)
      val left  = GC.merge (GC.merge (ra, rb), rc)
      val right = GC.merge (ra, GC.merge (rb, rc))
      val perm  = GC.merge (rc, GC.merge (rb, ra))
      val () = checkBool "merge associative across 3 replicas" (true, gcEq (left, right))
      val () = checkBool "merge order-independent" (true, gcEq (left, perm))
      val () = checkInt "converged value" (8, GC.value left)

      val () = section "GCounter: semilattice laws (Prop)"
      val () = check "merge commutative"
        (case P.check P.defaultSeed
                (P.forAll (P.Gen.tuple2 (genGC, genGC))
                   (fn (a, b) => gcEq (GC.merge (a, b), GC.merge (b, a))))
           of P.Passed _ => true | _ => false)
      val () = check "merge associative"
        (case P.check P.defaultSeed
                (P.forAll (P.Gen.tuple2 (genGC, P.Gen.tuple2 (genGC, genGC)))
                   (fn (a, (b, c)) =>
                      gcEq (GC.merge (GC.merge (a, b), c),
                            GC.merge (a, GC.merge (b, c)))))
           of P.Passed _ => true | _ => false)
      val () = check "merge idempotent"
        (case P.check P.defaultSeed
                (P.forAll genGC (fn a => gcEq (GC.merge (a, a), a)))
           of P.Passed _ => true | _ => false)

      val () = section "PNCounter: concrete inc / dec / value"
      val () = checkInt "empty is 0" (0, PN.value PN.empty)
      val p1 = PN.dec 0 (PN.inc 1 (PN.inc 0 PN.empty))   (* +1 +1 -1 = 1 *)
      val () = checkInt "inc/inc/dec = 1" (1, PN.value p1)
      val p2 = PN.decBy 2 5 (PN.incBy 0 3 PN.empty)       (* 3 - 5 = -2 *)
      val () = checkInt "can go negative" (~2, PN.value p2)
      val () = checkRaises "incBy negative raises" (fn () => PN.incBy 0 ~1 PN.empty)
      val () = checkRaises "decBy negative raises" (fn () => PN.decBy 0 ~1 PN.empty)

      val () = section "PNCounter: merge"
      (* a: r0 +4 ; b: r0 +1, r0 -2. Join p: r0=4; n: r0=2 => 4-2 = 2. *)
      val pa = PN.incBy 0 4 PN.empty
      val pb = PN.decBy 0 2 (PN.incBy 0 1 PN.empty)
      val () = checkInt "merge joins both halves" (2, PN.value (PN.merge (pa, pb)))
      val () = checkBool "merge symmetric"
                 (true, pnEq (PN.merge (pa, pb), PN.merge (pb, pa)))

      val () = section "PNCounter: semilattice laws (Prop)"
      val () = check "merge commutative"
        (case P.check P.defaultSeed
                (P.forAll (P.Gen.tuple2 (genPN, genPN))
                   (fn (a, b) => pnEq (PN.merge (a, b), PN.merge (b, a))))
           of P.Passed _ => true | _ => false)
      val () = check "merge associative"
        (case P.check P.defaultSeed
                (P.forAll (P.Gen.tuple2 (genPN, P.Gen.tuple2 (genPN, genPN)))
                   (fn (a, (b, c)) =>
                      pnEq (PN.merge (PN.merge (a, b), c),
                            PN.merge (a, PN.merge (b, c)))))
           of P.Passed _ => true | _ => false)
      val () = check "merge idempotent"
        (case P.check P.defaultSeed
                (P.forAll genPN (fn a => pnEq (PN.merge (a, a), a)))
           of P.Passed _ => true | _ => false)

      val () = section "LWWRegister: concrete set / value"
      val () = check "empty has no value" (LWW.value LWW.empty = NONE)
      val r1 = LWW.set { ts = 1, replica = 0 } "a" LWW.empty
      val () = check "first write visible" (LWW.value r1 = SOME "a")
      val r2 = LWW.set { ts = 2, replica = 0 } "b" r1
      val () = check "later write overwrites" (LWW.value r2 = SOME "b")

      val () = section "LWWRegister: higher timestamp wins"
      val hi = LWW.set { ts = 5, replica = 0 } "new" LWW.empty
      val lo = LWW.set { ts = 3, replica = 9 } "old" LWW.empty
      val () = check "higher ts wins regardless of replica"
                 (LWW.value (LWW.merge (lo, hi)) = SOME "new")
      val () = check "merge symmetric (ts)"
                 (LWW.value (LWW.merge (hi, lo)) = SOME "new")

      val () = section "LWWRegister: tie broken by replica id"
      val tieA = LWW.set { ts = 7, replica = 1 } "A" LWW.empty
      val tieB = LWW.set { ts = 7, replica = 2 } "B" LWW.empty
      val () = check "equal ts: higher replica wins"
                 (LWW.value (LWW.merge (tieA, tieB)) = SOME "B")
      val () = check "tie-break is symmetric"
                 (LWW.value (LWW.merge (tieB, tieA)) = SOME "B")

      val () = section "LWWRegister: semilattice laws (Prop)"
      val () = check "merge commutative"
        (case P.check P.defaultSeed
                (P.forAll (P.Gen.tuple2 (genLWW, genLWW))
                   (fn (a, b) => lwwEq (LWW.merge (a, b), LWW.merge (b, a))))
           of P.Passed _ => true | _ => false)
      val () = check "merge associative"
        (case P.check P.defaultSeed
                (P.forAll (P.Gen.tuple2 (genLWW, P.Gen.tuple2 (genLWW, genLWW)))
                   (fn (a, (b, c)) =>
                      lwwEq (LWW.merge (LWW.merge (a, b), c),
                             LWW.merge (a, LWW.merge (b, c)))))
           of P.Passed _ => true | _ => false)
      val () = check "merge idempotent"
        (case P.check P.defaultSeed
                (P.forAll genLWW (fn a => lwwEq (LWW.merge (a, a), a)))
           of P.Passed _ => true | _ => false)

      val () = section "ORSet: add / remove / re-add"
      val s0 = OR.empty intEq
      val () = checkBool "empty: not a member" (false, OR.member 1 s0)
      val s1 = OR.add 0 1 s0
      val () = checkBool "after add: member" (true, OR.member 1 s1)
      val () = checkIntList "value lists the element" ([1], OR.value s1)
      val s2 = OR.remove 1 s1
      val () = checkBool "after remove: absent" (false, OR.member 1 s2)
      val () = checkIntList "value empty after remove" ([], OR.value s2)
      val s3 = OR.add 0 1 s2
      val () = checkBool "re-add brings it back" (true, OR.member 1 s3)
      val () = checkIntList "value has it again" ([1], OR.value s3)

      val () = section "ORSet: distinct elements"
      val m = OR.add 1 2 (OR.add 0 2 (OR.add 0 1 (OR.empty intEq)))
      val () = checkIntList "duplicates collapse in value" ([1, 2], sortInts (OR.value m))
      val () = checkBool "removing one elem keeps others"
                 (true, OR.member 2 (OR.remove 1 m))

      val () = section "ORSet: concurrent add + remove (observed-remove)"
      (* Replica A adds e and observes only its own tag, then removes e.
         Replica B concurrently adds e under a tag A never observed. Merging
         A's state (e tombstoned) with B's unobserved add must leave e PRESENT,
         because the remove only tombstoned the tags it had observed. *)
      val base = OR.empty intEq
      val a0 = OR.add 0 7 base          (* A: add e, tag (0,0) *)
      val aRem = OR.remove 7 a0          (* A: remove -> tombstones (0,0) *)
      val b0 = OR.add 1 7 base          (* B: add e, tag (1,0), unobserved by A *)
      val merged = OR.merge (aRem, b0)
      val () = checkBool "unobserved concurrent add survives remove"
                 (true, OR.member 7 merged)
      (* But if the remove observed B's add first, e is gone. *)
      val both = OR.merge (a0, b0)        (* observes (0,0) and (1,0) *)
      val removedBoth = OR.remove 7 both
      val () = checkBool "remove of observed adds clears element"
                 (false, OR.member 7 removedBoth)
      val () = checkBool "merging tombstones back in stays absent"
                 (false, OR.member 7 (OR.merge (removedBoth, both)))

      val () = section "ORSet: merge convergence (order-independent)"
      val left  = OR.merge (OR.merge (a0, b0), aRem)
      val right = OR.merge (a0, OR.merge (b0, aRem))
      val () = checkBool "associative across 3 states" (true, orEq (left, right))
      val () = checkBool "same value regardless of order"
                 (true, sortInts (OR.value left) = sortInts (OR.value right))

      val () = section "ORSet: semilattice laws (Prop)"
      val () = check "merge commutative"
        (case P.check P.defaultSeed
                (P.forAll (P.Gen.tuple2 (genOR, genORb))
                   (fn (a, b) => orEq (OR.merge (a, b), OR.merge (b, a))))
           of P.Passed _ => true | _ => false)
      val () = check "merge associative"
        (case P.check P.defaultSeed
                (P.forAll (P.Gen.tuple2 (genOR, P.Gen.tuple2 (genORb, genORc)))
                   (fn (a, (b, c)) =>
                      orEq (OR.merge (OR.merge (a, b), c),
                            OR.merge (a, OR.merge (b, c)))))
           of P.Passed _ => true | _ => false)
      val () = check "merge idempotent"
        (case P.check P.defaultSeed
                (P.forAll genOR (fn a => orEq (OR.merge (a, a), a)))
           of P.Passed _ => true | _ => false)
    in
      ()
    end
end
