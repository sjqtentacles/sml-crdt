(* demo.sml - two simulated replicas independently mutate each CvRDT, then
   merge to show strong eventual consistency. Deterministic: no wall-clock,
   no unseeded randomness, no environment reads. *)

structure C = Crdt

fun intPairListStr xs =
  "[" ^ String.concatWith ","
    (List.map (fn (r, n) => "(" ^ Int.toString r ^ "," ^ Int.toString n ^ ")") xs) ^ "]"

val () = print "sml-crdt demo\n"

(* ---- GCounter: grow-only counter -------------------------------------- *)
val () = print "\nGCounter (replicas 1 and 2 increment independently):\n"
val g1 = C.GCounter.incBy 1 5 C.GCounter.empty
val g1 = C.GCounter.inc 1 g1               (* replica 1: 5 + 1 = 6 *)
val g2 = C.GCounter.incBy 2 3 C.GCounter.empty
val () = print ("  replica 1 value = " ^ Int.toString (C.GCounter.value g1) ^ "\n")
val () = print ("  replica 2 value = " ^ Int.toString (C.GCounter.value g2) ^ "\n")
val gMerged = C.GCounter.merge (g1, g2)
val () = print ("  merged value    = " ^ Int.toString (C.GCounter.value gMerged) ^ "\n")
val () = print ("  merged canonical = " ^ intPairListStr (C.GCounter.canonical gMerged) ^ "\n")

(* ---- PNCounter: increment/decrement counter ---------------------------- *)
val () = print "\nPNCounter (replica 1 increments, replica 2 decrements):\n"
val p1 = C.PNCounter.dec 1 (C.PNCounter.incBy 1 10 C.PNCounter.empty)  (* 10 - 1 = 9 *)
val p2 = C.PNCounter.decBy 2 4 C.PNCounter.empty                       (* -4 *)
val () = print ("  replica 1 value = " ^ Int.toString (C.PNCounter.value p1) ^ "\n")
val () = print ("  replica 2 value = " ^ Int.toString (C.PNCounter.value p2) ^ "\n")
val pMerged = C.PNCounter.merge (p1, p2)
val () = print ("  merged value    = " ^ Int.toString (C.PNCounter.value pMerged) ^ "\n")

(* ---- LWWRegister: last-write-wins register ------------------------------ *)
val () = print "\nLWWRegister (higher logical timestamp wins on merge):\n"
val lA = C.LWWRegister.set {ts = 1, replica = 1} "hello" C.LWWRegister.empty
val lB = C.LWWRegister.set {ts = 2, replica = 2} "world" C.LWWRegister.empty
val lMerged = C.LWWRegister.merge (lA, lB)
val () = print ("  merge ts=1 \"hello\" with ts=2 \"world\" -> "
                ^ Option.getOpt (C.LWWRegister.value lMerged, "-") ^ "\n")
val lTie1 = C.LWWRegister.set {ts = 5, replica = 1} "A" C.LWWRegister.empty
val lTie2 = C.LWWRegister.set {ts = 5, replica = 2} "B" C.LWWRegister.empty
val lTieMerged = C.LWWRegister.merge (lTie1, lTie2)
val () = print ("  tie at ts=5, replica 1 \"A\" vs replica 2 \"B\" -> "
                ^ Option.getOpt (C.LWWRegister.value lTieMerged, "-") ^ " (higher replica wins)\n")

(* ---- ORSet: observed-remove set ----------------------------------------- *)
val () = print "\nORSet (concurrent add survives a remove that never observed it):\n"
val eqInt = (op = : int * int -> bool)
val o1 = C.ORSet.add 1 42 (C.ORSet.empty eqInt)   (* replica 1 adds 42 *)
val o1removed = C.ORSet.remove 42 o1              (* replica 1 removes its own observed add *)
val o2 = C.ORSet.add 2 42 (C.ORSet.empty eqInt)   (* replica 2 concurrently adds 42, unseen by the remove *)
val oMerged = C.ORSet.merge (o1removed, o2)
val () = print ("  replica 1 after remove: member 42 = "
                ^ Bool.toString (C.ORSet.member 42 o1removed) ^ "\n")
val () = print ("  replica 2 concurrent add: member 42 = "
                ^ Bool.toString (C.ORSet.member 42 o2) ^ "\n")
val () = print ("  merged: member 42 = " ^ Bool.toString (C.ORSet.member 42 oMerged)
                ^ " (survives: the concurrent add's tag was never tombstoned)\n")

val o3 = C.ORSet.add 1 99 (C.ORSet.empty eqInt)
val o3removed = C.ORSet.remove 99 o3
val () = print ("  single-replica add then remove: member 99 = "
                ^ Bool.toString (C.ORSet.member 99 o3removed) ^ "\n")
