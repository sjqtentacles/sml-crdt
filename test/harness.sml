(* harness.sml *)
structure Harness :>
sig
  val section : string -> unit
  val check   : string -> bool -> unit
  val checkEq : string -> (''a * ''a) -> unit
  val checkInt        : string -> int * int -> unit
  val checkBool       : string -> bool * bool -> unit
  val checkString     : string -> string * string -> unit
  val checkIntList    : string -> int list * int list -> unit
  val checkStringList : string -> string list * string list -> unit
  val checkRaises     : string -> (unit -> 'a) -> unit

  val checkReal : string -> real * real -> unit
  val checkRealTol : real -> string -> real * real -> unit
  val run    : unit -> bool
  val reset  : unit -> unit
end =
struct
  val passed = ref 0
  val failed = ref 0
  fun reset () = (passed := 0; failed := 0)
  fun section name = print (name ^ ":\n")
  fun pass name = (passed := !passed + 1; print ("  ok   - " ^ name ^ "\n"))
  fun fail name detail = (failed := !failed + 1; print ("  FAIL - " ^ name ^ (if detail = "" then "" else ": " ^ detail) ^ "\n"))
  fun check name b = if b then pass name else fail name ""
  fun checkEq name (expected, actual) = if expected = actual then pass name else fail name "values differ"
  fun checkInt name (expected, actual) = if expected = actual then pass name else fail name (Int.toString expected ^ " <> " ^ Int.toString actual)
  fun checkBool name (expected, actual) = if expected = actual then pass name else fail name (Bool.toString expected ^ " <> " ^ Bool.toString actual)
  fun checkString name (expected, actual) = if expected = actual then pass name else fail name ("\"" ^ expected ^ "\" <> \"" ^ actual ^ "\"")
  fun intListToString xs = "[" ^ String.concatWith "," (List.map Int.toString xs) ^ "]"
  fun strListToString xs = "[" ^ String.concatWith "," (List.map (fn s => "\"" ^ s ^ "\"") xs) ^ "]"
  fun checkIntList name (expected, actual) = if expected = actual then pass name else fail name (intListToString expected ^ " <> " ^ intListToString actual)
  fun checkStringList name (expected, actual) = if expected = actual then pass name else fail name (strListToString expected ^ " <> " ^ strListToString actual)

  fun checkReal name (expected, actual) =
    if Real.== (expected, actual) orelse Real.abs (expected - actual) <= 1E~9
    then pass name
    else fail name (Real.toString expected ^ " <> " ^ Real.toString actual)
  fun checkRealTol tol name (expected, actual) =
    if Real.abs (expected - actual) <= tol then pass name else fail name (Real.toString expected ^ " <> " ^ Real.toString actual)
  fun checkRaises name thunk =
    let val raised = (ignore (thunk ()); false) handle _ => true
    in if raised then pass name else fail name "expected an exception" end
  fun run () =
    let val p = !passed and f = !failed
    in print ("\n" ^ Int.toString p ^ " passed, " ^ Int.toString f ^ " failed\n"); f = 0 end
end
