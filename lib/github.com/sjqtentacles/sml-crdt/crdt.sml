(* crdt.sml

   Implementation of the CvRDTs described in crdt.sig.

   Common design notes:
     - States are kept in a canonical, order-independent form so that the
       semilattice laws hold structurally and equality-by-value is well
       defined. Counters keep their per-replica maps as assoc lists sorted by
       replica id with zero entries dropped; the LWW register keeps a single
       winning entry; the OR-set keeps its live (element, tag) pairs sorted.
     - `merge` is the join: commutative, associative, idempotent. The tests
       in test/test.sml check these laws with property-based testing. *)

structure Crdt :> CRDT =
struct
  type replica = int

  (* ---- Shared helpers: replica -> int maps as sorted assoc lists --------
     We represent a per-replica count map as a list of (replica, count) pairs
     sorted strictly ascending by replica, with no zero entries. This makes
     the representation canonical (so structural equality = CRDT equality) and
     makes pointwise-max merge a simple ordered merge. *)
  structure Counts =
  struct
    type t = (replica * int) list

    val empty : t = []

    (* Look up r's count (0 if absent). *)
    fun get (m : t, r) =
      case List.find (fn (k, _) => k = r) m of
        SOME (_, v) => v
      | NONE => 0

    (* Insert/replace r's count, keeping the list sorted and zero-free. *)
    fun set (m : t, r, v) =
      let
        fun go [] = if v = 0 then [] else [(r, v)]
          | go ((k, x) :: rest) =
              if k = r then (if v = 0 then rest else (r, v) :: rest)
              else if r < k then (if v = 0 then (k, x) :: rest
                                  else (r, v) :: (k, x) :: rest)
              else (k, x) :: go rest
      in
        go m
      end

    (* Pointwise maximum of two maps -- the join. *)
    fun merge (a : t, b : t) : t =
      let
        fun go ([], ys) = ys
          | go (xs, []) = xs
          | go ((kx, vx) :: xs, (ky, vy) :: ys) =
              if kx = ky then (kx, Int.max (vx, vy)) :: go (xs, ys)
              else if kx < ky then (kx, vx) :: go (xs, (ky, vy) :: ys)
              else (ky, vy) :: go ((kx, vx) :: xs, ys)
      in
        go (a, b)
      end

    fun sum (m : t) = List.foldl (fn ((_, v), acc) => acc + v) 0 m
  end

  (* ---- GCounter ---------------------------------------------------------- *)
  structure GCounter =
  struct
    type t = Counts.t

    val empty : t = Counts.empty

    fun incBy r n m =
      if n < 0 then raise Domain
      else Counts.set (m, r, Counts.get (m, r) + n)

    fun inc r m = incBy r 1 m

    fun value m = Counts.sum m

    fun merge (a, b) = Counts.merge (a, b)

    fun canonical m = m
  end

  (* ---- PNCounter --------------------------------------------------------- *)
  structure PNCounter =
  struct
    (* p = increments, n = decrements; both grow-only. *)
    type t = { p : GCounter.t, n : GCounter.t }

    val empty : t = { p = GCounter.empty, n = GCounter.empty }

    fun incBy r k ({ p, n } : t) = { p = GCounter.incBy r k p, n = n }
    fun decBy r k ({ p, n } : t) = { p = p, n = GCounter.incBy r k n }

    fun inc r s = incBy r 1 s
    fun dec r s = decBy r 1 s

    fun value ({ p, n } : t) = GCounter.value p - GCounter.value n

    fun merge (a : t, b : t) =
      { p = GCounter.merge (#p a, #p b), n = GCounter.merge (#n a, #n b) }

    fun canonical ({ p, n } : t) = (GCounter.canonical p, GCounter.canonical n)
  end

  (* ---- LWWRegister ------------------------------------------------------- *)
  structure LWWRegister =
  struct
    (* Internally: NONE for an unwritten register, or SOME the winning write. *)
    type 'a t = { ts : int, replica : replica, value : 'a } option

    val empty : 'a t = NONE

    fun set { ts, replica } v (_ : 'a t) =
      SOME { ts = ts, replica = replica, value = v }

    fun value (NONE : 'a t) = NONE
      | value (SOME e) = SOME (#value e)

    fun entry (r : 'a t) = r

    (* Higher ts wins; equal ts broken by higher replica id. This total order
       on (ts, replica) makes the choice -- and hence merge -- commutative,
       associative and idempotent. *)
    fun beats (a : { ts : int, replica : replica, value : 'a },
               b : { ts : int, replica : replica, value : 'a }) =
      #ts a > #ts b orelse (#ts a = #ts b andalso #replica a > #replica b)

    fun merge (NONE, b) = b
      | merge (a, NONE) = a
      | merge (SOME a, SOME b) = if beats (a, b) then SOME a else SOME b
  end

  (* ---- ORSet ------------------------------------------------------------- *)
  structure ORSet =
  struct
    type tag = replica * int

    (* State:
         eq      element equality;
         adds    (element, unique tag) pairs, kept sorted by tag, deduped;
         tomb    tombstoned tags, kept sorted, deduped;
         clock   per-replica next-sequence counters (sorted assoc list).
       An element is live iff it has an add whose tag is not in `tomb`. The
       sorted/deduped lists make merge an ordered set-union and give a
       canonical form independent of operation order. *)
    type 'a t =
      { eq    : 'a * 'a -> bool
      , adds  : ('a * tag) list
      , tomb  : tag list
      , clock : (replica * int) list }

    fun empty eq : 'a t = { eq = eq, adds = [], tomb = [], clock = [] }

    (* Total order on tags for canonical ordering. *)
    fun tagLt ((r1, s1) : tag, (r2, s2) : tag) =
      r1 < r2 orelse (r1 = r2 andalso s1 < s2)

    (* Insert tag into a sorted, deduped tag list. *)
    fun insTag (t, []) = [t]
      | insTag (t, x :: xs) =
          if t = x then x :: xs
          else if tagLt (t, x) then t :: x :: xs
          else x :: insTag (t, xs)

    fun memTag (t, ts) = List.exists (fn x => x = t) ts

    (* Insert an (element, tag) add, sorted by tag, deduped by tag. *)
    fun insAdd (a as (_, t), []) = [a]
      | insAdd (a as (_, t), (b as (_, t2)) :: rest) =
          if t = t2 then b :: rest
          else if tagLt (t, t2) then a :: b :: rest
          else b :: insAdd (a, rest)

    fun nextSeq (clock, r) =
      case List.find (fn (k, _) => k = r) clock of
        SOME (_, n) => n
      | NONE => 0

    fun bumpClock (clock, r, n) =
      let
        fun go [] = [(r, n)]
          | go ((k, v) :: rest) =
              if k = r then (r, n) :: rest
              else if r < k then (r, n) :: (k, v) :: rest
              else (k, v) :: go rest
      in
        go clock
      end

    fun add r e ({ eq, adds, tomb, clock } : 'a t) : 'a t =
      let
        val s = nextSeq (clock, r)
        val tag = (r, s)
      in
        { eq = eq
        , adds = insAdd ((e, tag), adds)
        , tomb = tomb
        , clock = bumpClock (clock, r, s + 1) }
      end

    (* Tombstone every observed tag currently associated with e. *)
    fun remove e ({ eq, adds, tomb, clock } : 'a t) : 'a t =
      let
        val victims = List.filter (fn (e', _) => eq (e, e')) adds
        val tomb' = List.foldl (fn ((_, t), acc) => insTag (t, acc)) tomb victims
      in
        { eq = eq, adds = adds, tomb = tomb', clock = clock }
      end

    fun liveAdds ({ adds, tomb, ... } : 'a t) =
      List.filter (fn (_, t) => not (memTag (t, tomb))) adds

    fun member e (s as { eq, ... } : 'a t) =
      List.exists (fn (e', _) => eq (e, e')) (liveAdds s)

    (* Distinct live elements, in the order their surviving tags first appear
       (tags are sorted, so this is deterministic). *)
    fun value (s as { eq, ... } : 'a t) =
      let
        fun go ([], _) = []
          | go ((e, _) :: rest, seen) =
              if List.exists (fn x => eq (e, x)) seen then go (rest, seen)
              else e :: go (rest, e :: seen)
      in
        go (liveAdds s, [])
      end

    fun merge (a : 'a t, b : 'a t) : 'a t =
      let
        val eq = #eq a
        val adds = List.foldl insAdd (#adds a) (#adds b)
        val tomb = List.foldl insTag (#tomb a) (#tomb b)
        (* Pointwise-max of the per-replica clocks. *)
        fun joinClock (ca, cb) =
          List.foldl
            (fn ((r, n), acc) =>
               let val cur = case List.find (fn (k, _) => k = r) acc of
                               SOME (_, m) => m | NONE => 0
               in bumpClock (acc, r, Int.max (cur, n)) end)
            ca cb
      in
        { eq = eq, adds = adds, tomb = tomb
        , clock = joinClock (#clock a, #clock b) }
      end

    fun canonical ({ adds, tomb, ... } : 'a t) =
      { adds = adds, tombstones = tomb }
  end
end
