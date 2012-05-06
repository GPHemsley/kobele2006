#cd "../";;

#use "chapter2.ml";;
#use "lexicons/en.ml";;

(*********)
(* Tests *)
(*********)

(*---------*)
(* Lexicon *)
(*---------*)

let _John_ = List.hd (en_get_entries_for "John");;
let _George_ = List.hd (en_get_entries_for "George");;
let _Mary_ = List.hd (en_get_entries_for "Mary");;

let _everyone_ = List.hd (en_get_entries_for "everyone");;
let _something_ = List.hd (en_get_entries_for "something");;

let _the_ = List.hd (en_get_entries_for "the");;
let _'s_ = List.hd (en_get_entries_for "'s");;

let _ointment_ = List.hd (en_get_entries_for "ointment");;

let _devour_ = List.hd (en_get_entries_for "devour");;
let _arrive_ = List.hd (en_get_entries_for "arrive");;
let _expect_ = List.hd (en_get_entries_for "expect");;

let _seem_ = List.hd (en_get_entries_for "seem");;

let _rain_ = List.nth (en_get_entries_for "rain") 0;; (* There could be a noun. *)

let _will_ = List.hd (en_get_entries_for "will");;
let __s_ = List.hd (en_get_entries_for "-s");;
let __ed_ = List.hd (en_get_entries_for "-ed");;

let _have__PERF__ = List.nth (en_get_entries_for "have") 0;; (* There could be a main verb. *)
let __en__PERF__ = List.nth (en_get_entries_for "-en") 1;;
let __PERF__ = List.nth (en_get_entries_for "") 2;;

let _be__PROG__ = List.nth (en_get_entries_for "be") 0;;
let __ing_ = List.hd (en_get_entries_for "-ing");;
let __PROG__ = List.nth (en_get_entries_for "") 3;;

let _be__PASS__ = List.nth (en_get_entries_for "be") 1;;
let __en__PASS__ = List.nth (en_get_entries_for "-en") 0;;
let __ACT__ = List.nth (en_get_entries_for "") 1;;

let _it__EXPL__ = List.nth (en_get_entries_for "it") 0;; (* There could be a non-expletive pronoun. *)
let __EXPL__ = List.nth (en_get_entries_for "") 4;;

let _to__INF__ = List.nth (en_get_entries_for "to") 0;; (* There could be a preposition. *)
let _that__COMP__ = List.nth (en_get_entries_for "that") 0;; (* There could be a determiner. *)
let __COMP__ = List.nth (en_get_entries_for "") 0;; (* Null 'that' *)

let _promise_ = List.nth (en_get_entries_for "promise") 1;;

(*-------------*)
(* Derivations *)
(*-------------*)

(*~~~~~~~~~~~*)
(* Chapter 2 *)
(*~~~~~~~~~~~*)

(** Section 1 **)

(* Page 18 *)
(* (2.1) *John will devour. *)
(*** UNGRAMMATICAL ***)

(* Page 18 *)
(* (2.2) John will devour the ointment. *)
(*** SEE PAGE 22 BELOW ***)

(* Page 19 *)
(* (2.3) John will eat. *)
(*** TO DO ***)

(* Page 19 *)
(* (2.4) John will eat the ointment. *)
(*** TO DO ***)

(* Page 20 *)
(* the ointment *)
let _the_ointment_ = merge _the_ _ointment_;;

(* Page 21 *)
(* (2.5) John's ointment *)
let _John's_ointment_ =
    let d1 = merge _'s_ _ointment_ in
    let d2 = merge _John_ d1 in
    let d3 = move d2 in
    let d4 = move d3 in
    d4
;;

(* Page 22 *)
(* (2.6) John devoured the ointment. *)
(*** SEE PAGE 57 BELOW ***)

(* Page 22 *)
(* (2.7) John will devour the ointment. *)
let _John_will_devour_the_ointment_ =
    let d1 = merge _the_ _ointment_ in
    let d2 = merge _devour_ d1 in
    let d3 = merge __ACT__ d2 in
    let d4 = move d3 in
    let d5 = merge _John_ d4 in
    let d6 = move d5 in
    let d7 = merge __PROG__ d6 in
    let d8 = merge __PERF__ d7 in
    let d9 = merge _will_ d8 in
    let d10 = move d9 in
    let d11 = move d10 in
    d11
;;

(* Page 22 *)
(* (2.8) John is devouring the ointment. *)
(*** SEE PAGE 28 BELOW ***)

(* Page 24 *)
(* (2.9) John devours the ointment. *)
let _John_devours_the_ointment_ =
    let d1 = merge _the_ _ointment_ in
    let d2 = merge _devour_ d1 in
    let d3 = merge __ACT__ d2 in
    let d4 = move d3 in
    let d5 = merge _John_ d4 in
    let d6 = move d5 in
    let d7 = merge __PROG__ d6 in
    let d8 = merge __PERF__ d7 in
    let d9 = merge __s_ d8 in
    let d10 = move d9 in
    let d11 = move d10 in
    d11
;;

(* Page 24 *)
(* (2.10) John devoured the ointment. *)
(*** SEE PAGE 57 BELOW ***)

(* Page 24 *)
(* (2.11) John will devour the ointment. *)
(*** SEE PAGE 22 ABOVE ***)

(* Page 24 *)
(* (2.12) John has devoured the ointment. *)
let _John_has_devoured_the_ointment_ =
    let d1 = merge _the_ _ointment_ in
    let d2 = merge _devour_ d1 in
    let d3 = merge __ACT__ d2 in
    let d4 = move d3 in
    let d5 = merge _John_ d4 in
    let d6 = move d5 in
    let d7 = merge __PROG__ d6 in
    let d8 = merge __en__PERF__ d7 in
    let d9 = merge _have__PERF__ d8 in
    let d10 = merge __s_ d9 in
    let d11 = move d10 in
    let d12 = move d11 in
    d12
;;

(* Page 24 *)
(* (2.13) John had devoured the ointment. *)
let _John_had_devoured_the_ointment_ =
    let d1 = merge _the_ _ointment_ in
    let d2 = merge _devour_ d1 in
    let d3 = merge __ACT__ d2 in
    let d4 = move d3 in
    let d5 = merge _John_ d4 in
    let d6 = move d5 in
    let d7 = merge __PROG__ d6 in
    let d8 = merge __en__PERF__ d7 in
    let d9 = merge _have__PERF__ d8 in
    let d10 = merge __ed_ d9 in
    let d11 = move d10 in
    let d12 = move d11 in
    d12
;;

(* Page 24 *)
(* (2.14) John will have devoured the ointment. *)
let _John_had_devoured_the_ointment_ =
    let d1 = merge _the_ _ointment_ in
    let d2 = merge _devour_ d1 in
    let d3 = merge __ACT__ d2 in
    let d4 = move d3 in
    let d5 = merge _John_ d4 in
    let d6 = move d5 in
    let d7 = merge __PROG__ d6 in
    let d8 = merge __en__PERF__ d7 in
    let d9 = merge _have__PERF__ d8 in
    let d10 = merge _will_ d9 in
    let d11 = move d10 in
    let d12 = move d11 in
    d12
;;

(* Page 24 *)
(* (2.15) John is devouring the ointment. *)
(*** SEE PAGE 28 BELOW ***)

(* Page 24 *)
(* (2.16) John was devouring the ointment. *)
let _John_was_devouring_the_ointment_ =
    let d1 = merge _the_ _ointment_ in
    let d2 = merge _devour_ d1 in
    let d3 = merge __ACT__ d2 in
    let d4 = move d3 in
    let d5 = merge _John_ d4 in
    let d6 = move d5 in
    let d7 = merge __ing_ d6 in
    let d8 = merge _be__PROG__ d7 in
    let d9 = merge __PERF__ d8 in
    let d10 = merge __s_ d9 in
    let d11 = move d10 in
    let d12 = move d11 in
    d12
;;

(* Page 24 *)
(* (2.17) John will be devouring the ointment. *)
let _John_will_be_devouring_the_ointment_ =
    let d1 = merge _the_ _ointment_ in
    let d2 = merge _devour_ d1 in
    let d3 = merge __ACT__ d2 in
    let d4 = move d3 in
    let d5 = merge _John_ d4 in
    let d6 = move d5 in
    let d7 = merge __ing_ d6 in
    let d8 = merge _be__PROG__ d7 in
    let d9 = merge __PERF__ d8 in
    let d10 = merge _will_ d9 in
    let d11 = move d10 in
    let d12 = move d11 in
    d12
;;

(* Page 24 *)
(* (2.18) John has been devouring the ointment. *)
let _John_has_been_devouring_the_ointment_ =
    let d1 = merge _the_ _ointment_ in
    let d2 = merge _devour_ d1 in
    let d3 = merge __ACT__ d2 in
    let d4 = move d3 in
    let d5 = merge _John_ d4 in
    let d6 = move d5 in
    let d7 = merge __ing_ d6 in
    let d8 = merge _be__PROG__ d7 in
    let d9 = merge __en__PERF__ d8 in
    let d10 = merge _have__PERF__ d9 in
    let d11 = merge __s_ d10 in
    let d12 = move d11 in
    let d13 = move d12 in
    d13
;;

(* Page 24 *)
(* (2.19) John had been devouring the ointment. *)
let _John_had_been_devouring_the_ointment_ =
    let d1 = merge _the_ _ointment_ in
    let d2 = merge _devour_ d1 in
    let d3 = merge __ACT__ d2 in
    let d4 = move d3 in
    let d5 = merge _John_ d4 in
    let d6 = move d5 in
    let d7 = merge __ing_ d6 in
    let d8 = merge _be__PROG__ d7 in
    let d9 = merge __en__PERF__ d8 in
    let d10 = merge _have__PERF__ d9 in
    let d11 = merge __ed_ d10 in
    let d12 = move d11 in
    let d13 = move d12 in
    d13
;;

(* Page 25 *)
(* (2.20) John will have been devouring the ointment. *)
(*** SEE PAGE 44 BELOW ***)

(* Page 28 *)
(* John is devouring the ointment. *)
let _John_is_devouring_the_ointment_ =
    let d1 = merge _the_ _ointment_ in
    let d2 = merge _devour_ d1 in
    let d3 = merge __ACT__ d2 in
    let d4 = move d3 in
    let d5 = merge _John_ d4 in
    let d6 = move d5 in
    let d7 = merge __ing_ d6 in
    let d8 = merge _be__PROG__ d7 in
    let d9 = merge __PERF__ d8 in
    let d10 = merge __s_ d9 in
    let d11 = move d10 in
    let d12 = move d11 in
    d12
;;

(* Page 32 *)
(* (2.21) A pirate ship appeared (in the distance). *)
(*** TO DO ***)

(* Page 32 *)
(* (2.22) *A pirate ship to appear (in the distance). *)
(*** UNGRAMMATICAL ***)

(* Page 32 *)
(* (2.23) A pirate ship seemed to appear (in the distance). *)
(*** TO DO ***)

(* Page 32 *)
(* (2.24) A pirate ship seemed appeared (in the distance). *)
(*** UNGRAMMATICAL ***)

(* Page 33 *)
(* (2.25) There appeared a pirate ship (in the distance). *)
(*** TO DO ***)

(* Page 33 *)
(* (2.26) *There sunk a pirate ship (in the distance). *)
(*** UNGRAMMATICAL ***)

(* Page 33 *)
(* (2.27) There seemed to appear a pirate ship (in the distance). *)
(*** TO DO ***)

(* Page 33 *)
(* (2.28) *There seemed to sink a pirate ship (in the distance). *)
(*** UNGRAMMATICAL ***)





(* Page 36 *)
(* (2.32) John has arrived. *)
let _John_has_arrived_ =
    let d1 = merge _arrive_ _John_ in
    let d2 = merge __PROG__ d1 in
    let d3 = merge __en__PERF__ d2 in
    let d4 = merge _have__PERF__ d3 in
    let d5 = merge __s_ d4 in
    let d6 = move d5 in
    let d7 = move d6 in
    d7
;;

(* Page 36 *)
(* (2.33) John seems to have arrived. *)
let _John_seems_to_have_arrived_ =
    let d1 = merge _arrive_ _John_ in
    let d2 = merge __PROG__ d1 in
    let d3 = merge __en__PERF__ d2 in
    let d4 = merge _have__PERF__ d3 in
    let d5 = merge _to__INF__ d4 in
    let d6 = merge _seem_ d5 in
    let d7 = merge __PROG__ d6 in
    let d8 = merge __PERF__ d7 in
    let d9 = merge __s_ d8 in
    let d10 = move d9 in
    let d11 = move d10 in
    d11
;;

(* Page 36 *)
(* (2.34) John seems to seem to have arrived. *)
let _John_seems_to_seem_to_have_arrived_ =
    let d1 = merge _arrive_ _John_ in
    let d2 = merge __PROG__ d1 in
    let d3 = merge __en__PERF__ d2 in
    let d4 = merge _have__PERF__ d3 in
    let d5 = merge _to__INF__ d4 in
    let d6 = merge _seem_ d5 in
    let d7 = merge __PROG__ d6 in
    let d8 = merge __PERF__ d7 in
    let d9 = merge _to__INF__ d8 in
    let d10 = merge _seem_ d9 in
    let d11 = merge __PROG__ d10 in
    let d12 = merge __PERF__ d11 in
    let d13 = merge __s_ d12 in
    let d14 = move d13 in
    let d15 = move d14 in
    d15
;;

(* Page 41 *)
(* (2.35) It seems John has arrived. *)
let _it_seems_John_has_arrived_ =
    let d1 = merge _arrive_ _John_ in
    let d2 = merge __PROG__ d1 in
    let d3 = merge __en__PERF__ d2 in
    let d4 = merge _have__PERF__ d3 in
    let d5 = merge __s_ d4 in
    let d6 = move d5 in
    let d7 = move d6 in
    let d8 = merge __COMP__ d7 in
    let d9 = merge _seem_ d8 in
    let d10 = merge __EXPL__ d9 in
    let d11 = merge _it__EXPL__ d10 in
    let d12 = merge __PROG__ d11 in
    let d13 = merge __PERF__ d12 in
    let d14 = merge __s_ d13 in
    let d15 = move d14 in
    let d16 = move d15 in
    d16
;;

(* Page 42 *)
(* (2.36) *It seems John to have arrived. *)
(*** UNGRAMMATICAL ***)

(* Page 42 *)
(* (2.37) *John seems it has arrived. *)
(*** UNGRAMMATICAL ***)

(* Page 43 *)
(* (2.38) It is raining. *)
let _it_is_raining_ =
    let d1 = merge __EXPL__ _rain_ in
    let d2 = merge _it__EXPL__ d1 in
    let d3 = merge __ing_ d2 in
    let d4 = merge _be__PROG__ d3 in
    let d5 = merge __PERF__ d4 in
    let d6 = merge __s_ d5 in
    let d7 = move d6 in
    let d8 = move d7 in
    d8
;;

(* Page 44 *)
(* (2.39) John will have been devouring the ointment. *)
let _John_will_have_been_devouring_the_ointment_ =
    let d1 = merge _the_ _ointment_ in
    let d2 = merge _devour_ d1 in
    let d3 = merge __ACT__ d2 in
    let d4 = move d3 in
    let d5 = merge _John_ d4 in
    let d6 = move d5 in
    let d7 = merge __ing_ d6 in
    let d8 = merge _be__PROG__ d7 in
    let d9 = merge __en__PERF__ d8 in
    let d10 = merge _have__PERF__ d9 in
    let d11 = merge _will_ d10 in
    let d12 = move d11 in
    let d13 = move d12 in
    d13
;;

(* Page 44 *)
(* (2.40) The ointment will have been being devoured. *)
let _the_ointment_will_have_been_being_devoured_ =
    let d1 = merge _the_ _ointment_ in
    let d2 = merge _devour_ d1 in
    let d3 = merge __en__PASS__ d2 in
    let d4 = merge _be__PASS__ d3 in
    let d5 = merge __ing_ d4 in
    let d6 = merge _be__PROG__ d5 in
    let d7 = merge __en__PERF__ d6 in
    let d8 = merge _have__PERF__ d7 in
    let d9 = merge _will_ d8 in
    let d10 = move d9 in
    let d11 = move d10 in
    d11
;;

(* Page 44 *)
(* (2.41) George expects the ointment to be devoured. *)
let _George_expects_the_ointment_to_be_devoured_ =
    let d1 = merge _the_ _ointment_ in
    let d2 = merge _devour_ d1 in
    let d3 = merge __en__PASS__ d2 in
    let d4 = merge _be__PASS__ d3 in
    let d5 = merge __PROG__ d4 in
    let d6 = merge __PERF__ d5 in
    let d7 = merge _to__INF__ d6 in
    let d8 = merge _expect_ d7 in
    let d9 = merge __ACT__ d8 in
    let d10 = move d9 in
    let d11 = merge _George_ d10 in
    let d12 = move d11 in
    let d13 = merge __PROG__ d12 in
    let d14 = merge __PERF__ d13 in
    let d15 = merge __s_ d14 in
    let d16 = move d15 in
    let d17 = move d16 in
    d17
;;

(* Page 44 *)
(* (2.42) The ointment is expected to be devoured. *)
let _the_ointment_is_expected_to_be_devoured_ =
    let d1 = merge _the_ _ointment_ in
    let d2 = merge _devour_ d1 in
    let d3 = merge __en__PASS__ d2 in
    let d4 = merge _be__PASS__ d3 in
    let d5 = merge __PROG__ d4 in
    let d6 = merge __PERF__ d5 in
    let d7 = merge _to__INF__ d6 in
    let d8 = merge _expect_ d7 in
    let d9 = merge __en__PASS__ d8 in
    let d10 = merge _be__PASS__ d9 in
    let d11 = merge __PROG__ d10 in
    let d12 = merge __PERF__ d11 in
    let d13 = merge __s_ d12 in
    let d14 = move d13 in
    let d15 = move d14 in
    d15
;;

(** Section 2 **)

(* Page 57 *)
(* John devoured the ointment. *)
let _John_devoured_the_ointment_ =
    let d1 = merge _the_ _ointment_ in
    let d2 = merge _devour_ d1 in
    let d3 = merge __ACT__ d2 in
    let d4 = move d3 in
    let d5 = merge _John_ d4 in
    let d6 = move d5 in
    let d7 = merge __PROG__ d6 in
    let d8 = merge __PERF__ d7 in
    let d9 = merge __ed_ d8 in
    let d10 = move d9 in
    let d11 = move d10 in
    d11
;;





(* Page 80 *)
(* (2.50) Something devoured everyone. *)
let _something_devoured_everyone_ =
    let d1 = merge _devour_ _everyone_ in
    let d2 = merge __ACT__ d1 in
    let d3 = move d2 in
    let d4 = merge _something_ d3 in
    let d5 = move d4 in
    let d6 = merge __PROG__ d5 in
    let d7 = merge __PERF__ d6 in
    let d8 = merge __ed_ d7 in
    let d9 = move d8 in
    let d10 = move d9 in
    d10
;;

(* Page 82 *)
(* (2.51) Something seems to be devouring everyone. *)
let _something_seems_to_be_devouring_everyone_ =
    let d1 = merge _devour_ _everyone_ in
    let d2 = merge __ACT__ d1 in
    let d3 = move d2 in
    let d4 = merge _something_ d3 in
    let d5 = move d4 in
    let d6 = merge __ing_ d5 in
    let d7 = merge _be__PROG__ d6 in
    let d8 = merge __PERF__ d7 in
    let d9 = merge _to__INF__ d8 in
    let d10 = merge _seem_ d9 in
    let d11 = merge __PROG__ d10 in
    let d12 = merge __PERF__ d11 in
    let d13 = merge __s_ d12 in
    let d14 = move d13 in
    let d15 = move d14 in
    d15
;;
