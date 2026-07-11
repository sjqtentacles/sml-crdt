(* test.sml

   Implementation of the TEST framework. See test.sig for the contract.

   The framework is dependency-free; the property-testing RNG (Prop) is a
   self-contained SplitMix64, kept here so the library pulls in nothing beyond
   the Basis. All output is plain text and deterministic across compilers. *)

structure Test :> TEST =
struct
  exception TestFailure of string

  type test = string * (unit -> unit)
  type suite = string * test list

  fun test name body = (name, body)
  fun suite name tests = (name, tests)

  fun fail msg = raise TestFailure msg

  (* Deterministic real formatting for diagnostic messages and the property
     generator's `show`. Real.toString differs between MLton and Poly/ML
     (e.g. "30" vs "30.0"), which would make failure messages and
     counterexample reports depend on the compiler. Searches Real.fmt
     FIX(0), FIX(1), ... for the first fixed-decimal rendering that
     reparses to the same real (Real.fmt is byte-identical across both
     compilers); falls back to scientific notation past 15 fixed digits. *)
  fun fmtRealDet r =
    if Real.isNan r then "nan"
    else if Real.== (r, Real.posInf) then "inf"
    else if Real.== (r, Real.negInf) then "~inf"
    else
      let
        val neg = r < 0.0
        val a = Real.abs r
        fun tryDigits n =
          if n > 15 then NONE
          else
            let val s = Real.fmt (StringCvt.FIX (SOME n)) a
            in case Real.fromString s of
                   SOME a' => if Real.== (a', a) then SOME s else tryDigits (n + 1)
                 | NONE => tryDigits (n + 1)
            end
        val body =
          case tryDigits 0 of
              SOME s => s
            | NONE => Real.fmt (StringCvt.SCI (SOME 16)) a
      in if neg then "~" ^ body else body end

  fun assert b = if b then () else fail "assertion failed"
  fun assertMsg msg b = if b then () else fail msg
  fun assertEq (expected, actual) =
    if expected = actual then () else fail "values differ"
  fun assertNeq (a, b) =
    if a <> b then () else fail "values unexpectedly equal"
  fun assertRaises thunk =
    let val raised = (ignore (thunk ()); false) handle _ => true
    in if raised then () else fail "expected an exception" end
  fun assertNear (expected, actual, epsilon) =
    if Real.isFinite (expected - actual)
       andalso Real.abs (expected - actual) <= epsilon
    then ()
    else fail (fmtRealDet expected ^ " not within " ^ fmtRealDet epsilon
               ^ " of " ^ fmtRealDet actual)

  (* ---- Property-based testing ---------------------------------------
     A self-contained SplitMix64 drives generation. A generator bundles a
     sampler, a shrinker (candidate smaller values to try on failure), and a
     printer, so `forAll` can both find and minimise a counterexample and
     display it -- all without any external RNG dependency. *)

  structure Prop =
  struct
    structure W = Word64

    type seed = W.word

    (* SplitMix64 step: an output word and the successor seed. *)
    fun nextWord (z0 : seed) : W.word * seed =
      let
        val z1 = W.+ (z0, 0wx9E3779B97F4A7C15)
        val z2 = W.* (W.xorb (z1, W.>> (z1, 0w30)), 0wxBF58476D1CE4E5B9)
        val z3 = W.* (W.xorb (z2, W.>> (z2, 0w27)), 0wx94D049BB133111EB)
        val z4 = W.xorb (z3, W.>> (z3, 0w31))
      in
        (z4, z1)
      end

    (* Split a seed into two independent seeds (for combinators). *)
    fun split (s : seed) : seed * seed =
      let val (a, s1) = nextWord s
          val (b, _)  = nextWord s1
      in (a, W.xorb (b, 0wx9E3779B97F4A7C15)) end

    (* A generator: sample from a seed, propose shrinks, and show. *)
    type 'a gen =
      { sample : seed -> 'a
      , shrink : 'a -> 'a list
      , show   : 'a -> string }

    structure Gen =
    struct
      type 'a t = 'a gen

      fun pure x = { sample = fn _ => x, shrink = fn _ => [], show = fn _ => "<?>" }

      (* map loses shrinking (no inverse) and uses a placeholder printer. *)
      fun map f (g : 'a gen) : 'b gen =
        { sample = fn s => f (#sample g s)
        , shrink = fn _ => []
        , show   = fn _ => "<mapped>" }

      fun bind (g : 'a gen) (k : 'a -> 'b gen) : 'b gen =
        { sample = fn s =>
            let val (s1, s2) = split s
                val a = #sample g s1
            in #sample (k a) s2 end
        , shrink = fn _ => []
        , show   = fn _ => "<bound>" }

      (* uniform word in [0, n) for n > 0, via rejection sampling. *)
      fun wordBelow (n : W.word) (s : seed) : W.word * seed =
        if n = 0w0 then (0w0, s)
        else
          let
            val limit = W.- (0w0, W.mod (W.- (0w0, n), n))
            fun loop s =
              let val (w, s') = nextWord s
              in if W.> (w, W.- (limit, 0w1)) then loop s'
                 else (W.mod (w, n), s') end
          in loop s end

      (* Shrink an int toward 0: halve the gap, plus 0 itself. Ordered so the
         binary search lands on the boundary value. *)
      fun shrinkInt n =
        if n = 0 then []
        else
          let
            fun halves cur acc =
              if cur = 0 then List.rev acc
              else halves (cur div 2) ((n - cur) :: acc)
            val cands = 0 :: halves n []
            (* dedupe while preserving order, drop n itself *)
            fun dedup [] _ = []
              | dedup (x :: xs) seen =
                  if x = n orelse List.exists (fn y => y = x) seen
                  then dedup xs seen
                  else x :: dedup xs (x :: seen)
          in dedup cands [] end

      fun intRange (lo, hi) : int gen =
        if lo > hi then raise Domain
        else
          { sample = fn s =>
              let val span = W.fromInt (hi - lo) + 0w1
                  val (w, _) = wordBelow span s
              in lo + W.toInt w end
          , shrink = fn n =>
              List.filter (fn m => m >= lo andalso m <= hi andalso m <> n)
                (shrinkInt n)
          , show = Int.toString }

      val int : int gen = intRange (~1000, 1000)

      val bool : bool gen =
        { sample = fn s => let val (w, _) = nextWord s in W.andb (w, 0w1) = 0w1 end
        , shrink = fn true => [false] | false => []
        , show = Bool.toString }

      val real : real gen =
        { sample = fn s =>
            let val (w, _) = nextWord s
                val top53 = W.>> (w, 0w11)
            in Real.fromLargeInt (W.toLargeInt top53) / 9007199254740992.0 end
        , shrink = fn _ => []
        , show = fmtRealDet }

      (* printable ASCII 32..126 *)
      val char : char gen =
        { sample = fn s =>
            let val (w, _) = wordBelow 0w95 s
            in Char.chr (32 + W.toInt w) end
        , shrink = fn c =>
            if c = #"a" then []
            else if Char.ord c > Char.ord #"a" then [#"a"]
            else [],
          show = fn c => "#\"" ^ Char.toString c ^ "\"" }

      fun showList (showElem : 'a -> string) (xs : 'a list) =
        "[" ^ String.concatWith "," (List.map showElem xs) ^ "]"

      (* Shrink a list: first try removing elements (toward []), then shrink
         each element in place. Removal-first yields a minimal length. *)
      fun shrinkList (shrinkElem : 'a -> 'a list) (xs : 'a list) : 'a list list =
        let
          val n = List.length xs
          (* drop the element at index i *)
          fun without i =
            List.mapPartial (fn (j, x) => if j = i then NONE else SOME x)
              (ListPair.zip (List.tabulate (n, fn k => k), xs))
          val removals = List.tabulate (n, fn i => without i)
          (* shrink element i to each of its candidates *)
          fun shrinkAt i =
            let val xi = List.nth (xs, i)
            in List.map
                 (fn x' =>
                    List.tabulate (n, fn j =>
                      if j = i then x' else List.nth (xs, j)))
                 (shrinkElem xi)
            end
          val elemShrinks = List.concat (List.tabulate (n, shrinkAt))
        in
          removals @ elemShrinks
        end

      (* short list: length in [0, 5] *)
      fun list (g : 'a gen) : 'a list gen =
        { sample = fn s =>
            let
              val (lenW, s1) = wordBelow 0w6 s
              val len = W.toInt lenW
              fun build (0, _, acc) = List.rev acc
                | build (k, s, acc) =
                    let val (s', s'') = split s
                        val x = #sample g s'
                    in build (k - 1, s'', x :: acc) end
            in build (len, s1, []) end
        , shrink = shrinkList (#shrink g)
        , show = showList (#show g) }

      val string : string gen =
        { sample = fn s =>
            let
              val (lenW, s1) = wordBelow 0w8 s
              val len = W.toInt lenW
              fun build (0, _, acc) = String.implode (List.rev acc)
                | build (k, s, acc) =
                    let val (s', s'') = split s
                        val c = #sample char s'
                    in build (k - 1, s'', c :: acc) end
            in build (len, s1, []) end
        , shrink = fn str =>
            let
              val cs = String.explode str
              val shrunk = shrinkList (#shrink char) cs
            in List.map String.implode shrunk end
        , show = fn str => "\"" ^ String.toString str ^ "\"" }

      fun tuple2 (ga : 'a gen, gb : 'b gen) : ('a * 'b) gen =
        { sample = fn s =>
            let val (s1, s2) = split s
            in (#sample ga s1, #sample gb s2) end
        , shrink = fn (a, b) =>
            List.map (fn a' => (a', b)) (#shrink ga a)
            @ List.map (fn b' => (a, b')) (#shrink gb b)
        , show = fn (a, b) => "(" ^ #show ga a ^ "," ^ #show gb b ^ ")" }

      fun oneof (gs : 'a gen list) : 'a gen =
        case gs of
            [] => raise Domain
          | _ =>
            let val k = List.length gs in
              { sample = fn s =>
                  let val (w, s1) = wordBelow (W.fromInt k) s
                      val g = List.nth (gs, W.toInt w)
                  in #sample g s1 end
              , shrink = fn _ => []
              , show = fn _ => "<oneof>" }
            end

      fun choose (xs : 'a list) : 'a gen =
        case xs of
            [] => raise Domain
          | _ =>
            let val k = List.length xs in
              { sample = fn s =>
                  let val (w, _) = wordBelow (W.fromInt k) s
                  in List.nth (xs, W.toInt w) end
              , shrink = fn _ => []
              , show = fn _ => "<choose>" }
            end
    end

    (* A property, with the element type existentially hidden: given a seed it
       samples one value, checks the predicate, and (on failure) shrinks the
       value to a minimal failing one, returning its rendering. *)
    type property =
      seed -> { failed : bool, counterexample : string, shrinks : int }

    datatype result =
        Passed of int
      | Failed of { counterexample : string
                  , seed : Word64.word
                  , shrinks : int }

    val numTests = 100
    val defaultSeed = 0wx853C49E6748FEA9B : Word64.word

    fun forAll (g : 'a gen) (pred : 'a -> bool) : property =
      fn s =>
        let
          val x = #sample g s
          fun ok v = pred v handle _ => false
          (* Greedily shrink while the value still falsifies the predicate. *)
          fun minimise (v, steps) =
            case List.find (fn c => not (ok c)) (#shrink g v) of
                NONE => (v, steps)
              | SOME c => minimise (c, steps + 1)
        in
          if ok x then
            { failed = false, counterexample = "", shrinks = 0 }
          else
            let val (v, steps) = minimise (x, 0)
            in { failed = true, counterexample = #show g v, shrinks = steps } end
        end

    (* Check a property over `numTests` cases. The per-case seed is derived
       from `seed` by stepping SplitMix64, so a given starting seed always
       produces the same sequence of cases (hence reproducible failures). *)
    fun check (seed : Word64.word) (prop : property) : result =
      let
        fun loop (0, _, _) = Passed numTests
          | loop (k, s, n) =
              let val (_, s') = nextWord s
                  val r = prop s'
              in
                if #failed r then
                  Failed { counterexample = #counterexample r
                         , seed = seed
                         , shrinks = #shrinks r }
                else loop (k - 1, s', n + 1)
              end
      in
        loop (numTests, seed, 0)
      end
  end

  fun propTest name (prop : Prop.property) : test =
    ( name
    , fn () =>
        case Prop.check Prop.defaultSeed prop of
            Prop.Passed _ => ()
          | Prop.Failed { counterexample, seed, shrinks } =>
              raise TestFailure
                ("property falsified: " ^ counterexample
                 ^ " (after " ^ Int.toString shrinks ^ " shrinks, seed "
                 ^ Word64.toString seed ^ ")") )

  (* Execute each test, catching failures per-test. Prints a TAP-style line
     per test ("ok N - name" / "not ok N - name # reason"), a "# Suite: name"
     header per suite, and a final summary. Returns true iff all passed. *)
  fun run suites =
    let
      val count  = ref 0
      val passed = ref 0
      val failed = ref 0

      fun runTest (name, body) =
        let
          val () = count := !count + 1
          val n = !count
          val outcome =
            (body (); NONE)
            handle TestFailure msg => SOME msg
                 | e => SOME ("unexpected exception: " ^ exnName e)
        in
          case outcome of
              NONE =>
                (passed := !passed + 1;
                 print ("ok " ^ Int.toString n ^ " - " ^ name ^ "\n"))
            | SOME reason =>
                (failed := !failed + 1;
                 print ("not ok " ^ Int.toString n ^ " - " ^ name
                        ^ " # " ^ reason ^ "\n"))
        end

      fun runSuite (sname, tests) =
        (print ("# Suite: " ^ sname ^ "\n");
         List.app runTest tests)
    in
      List.app runSuite suites;
      print ("\n" ^ Int.toString (!passed) ^ " passed, "
             ^ Int.toString (!failed) ^ " failed\n");
      !failed = 0
    end
end
