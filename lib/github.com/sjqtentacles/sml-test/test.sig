(* test.sig

   A small, dependency-free test framework for the sjqtentacles SML libraries.
   It supersedes the hand-rolled `Harness` that each library used to carry:
   tests are first-class values grouped into named suites, assertions raise a
   distinguished `TestFailure`, and `run` executes everything, prints
   TAP-style output, and reports a final summary.

   Output is plain text and deterministic -- identical across MLton and
   Poly/ML -- so runs under the two compilers can be diffed directly.

   The `Prop` substructure adds QuickCheck-style property testing on top of a
   self-contained, splittable SplitMix64 generator (no external RNG
   dependency). A failing property shrinks its counterexample toward a minimal
   failing case and prints the seed so failures reproduce exactly. *)

signature TEST =
sig
  (* ---- Failure signalling -------------------------------------------- *)

  (* Raised by the assertion combinators below. `run` catches it (and any
     other exception) per-test and reports the test as failed; the carried
     string is the failure detail printed in the report. *)
  exception TestFailure of string

  (* ---- Model -------------------------------------------------------- *)

  type test
  type suite

  (* A single named test. The body runs for effect and signals failure by
     raising (TestFailure for assertion failures, or anything else). *)
  val test  : string -> (unit -> unit) -> test

  (* A named group of tests. *)
  val suite : string -> test list -> suite

  (* ---- Assertions (for use inside a test body) ----------------------- *)

  (* Fail unless the condition holds. *)
  val assert    : bool -> unit
  (* As `assert`, but with a caller-supplied failure message. *)
  val assertMsg : string -> bool -> unit

  (* Equality assertions over an equality type, taking (expected, actual). *)
  val assertEq  : ''a * ''a -> unit
  val assertNeq : ''a * ''a -> unit

  (* Succeeds iff forcing the thunk raises some exception. *)
  val assertRaises : (unit -> 'a) -> unit

  (* assertNear (expected, actual, epsilon): fail unless
     |expected - actual| <= epsilon. NaNs never compare near. *)
  val assertNear : real * real * real -> unit

  (* ---- Property-based testing --------------------------------------- *)

  structure Prop :
  sig
    (* A splittable, seeded generator of 'a. Generators are pure: they map a
       seed to a value, so the same seed always yields the same value. *)
    type 'a gen

    (* A property: a generator paired with a predicate to check on each
       generated value, plus the machinery to shrink a failing value. *)
    type property

    structure Gen :
    sig
      type 'a t = 'a gen

      (* Functor / monad combinators for building generators. *)
      val map  : ('a -> 'b) -> 'a gen -> 'b gen
      val bind : 'a gen -> ('a -> 'b gen) -> 'b gen
      val pure : 'a -> 'a gen

      (* Primitive generators. *)
      val int      : int gen                    (* small ints, both signs *)
      val intRange : int * int -> int gen        (* uniform in [lo, hi] *)
      val real     : real gen                    (* in [0, 1) *)
      val bool     : bool gen
      val char     : char gen                    (* printable ASCII *)
      val string   : string gen                  (* short printable string *)

      (* Containers / composition. *)
      val list   : 'a gen -> 'a list gen          (* short list *)
      val tuple2 : 'a gen * 'b gen -> ('a * 'b) gen
      val oneof  : 'a gen list -> 'a gen          (* pick a sub-generator *)
      val choose : 'a list -> 'a gen              (* pick an element *)
    end

    (* forAll gen pred: the property "pred holds for every generated value".
       On failure the offending value is shrunk and reported. *)
    val forAll : 'a gen -> ('a -> bool) -> property

    (* The outcome of checking a property: either every case passed, or a
       (shrunk) counterexample was found, described as a string together with
       the seed that produced the original failure. *)
    datatype result =
        Passed of int                       (* number of cases checked *)
      | Failed of { counterexample : string (* shown, post-shrink *)
                  , seed : Word64.word       (* seed of the run *)
                  , shrinks : int }          (* shrink steps applied *)

    (* Run a property over `numTests` cases starting at the given seed.
       Pure and reproducible: the same seed yields the same result. *)
    val check : Word64.word -> property -> result

    (* The fixed default seed used when a property is run as a test, so runs
       are reproducible. *)
    val defaultSeed : Word64.word

    (* Number of random cases checked per property. *)
    val numTests : int
  end

  (* Wrap a property as a test. The property is checked over `Prop.numTests`
     cases starting from `Prop.defaultSeed`; on failure the shrunk
     counterexample and the seed are reported via TestFailure. *)
  val propTest : string -> Prop.property -> test

  (* ---- Running ------------------------------------------------------- *)

  (* Execute every test in every suite. Prints TAP-style "ok"/"not ok" lines
     and a final "<p> passed, <f> failed" summary. Returns true iff all
     tests passed. Catches TestFailure and any other exception per-test, so
     one failing test never aborts the run. *)
  val run : suite list -> bool
end
