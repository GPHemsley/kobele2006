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

let _a_ = List.hd (en_get_entries_for "a");;
let _the_ = List.hd (en_get_entries_for "the");;
let _'s_ = List.hd (en_get_entries_for "'s");;
let _some_ = List.hd (en_get_entries_for "some");;
let _every_ = List.hd (en_get_entries_for "every");;

let _ointment_ = List.hd (en_get_entries_for "ointment");;
let _abbot_ = List.hd (en_get_entries_for "abbot");;
let _barber_ = List.hd (en_get_entries_for "barber");;

let _devour_ = List.hd (en_get_entries_for "devour");;
let _shave_ = List.hd (en_get_entries_for "shave");;

let _arrive_ = List.hd (en_get_entries_for "arrive");;
let _die_ = List.hd (en_get_entries_for "die");;

let _seem_ = List.hd (en_get_entries_for "seem");;

let _persuade_ = List.hd (en_get_entries_for "persuade");;

let _rain_ = List.nth (en_get_entries_for "rain") 0;; (* There could be a noun. *)

let _will_ = List.hd (en_get_entries_for "will");;
let __s_ = List.hd (en_get_entries_for "-s");;
let __ed_ = List.hd (en_get_entries_for "-ed");;

let _have__PERF__ = List.nth (en_get_entries_for "have") 0;; (* There could be a main verb. *)
let __en__PERF__ = List.nth (en_get_entries_for "-en") 1;;
let __PERF__ = List.nth (en_get_entries_for "") 3;;

let _be__PROG__ = List.nth (en_get_entries_for "be") 0;;
let __ing_ = List.hd (en_get_entries_for "-ing");;
let __PROG__ = List.nth (en_get_entries_for "") 4;;

let _be__PASS__ = List.nth (en_get_entries_for "be") 1;;
let __en__PASS__ = List.nth (en_get_entries_for "-en") 0;;
let __ACT__ = List.nth (en_get_entries_for "") 2;;

let _it__EXPL__ = List.nth (en_get_entries_for "it") 0;; (* There could be a non-expletive pronoun. *)
let __EXPL__ = List.nth (en_get_entries_for "") 5;;

let _to__INF__ = List.nth (en_get_entries_for "to") 0;; (* There could be a preposition. *)
let _that__COMP__ = List.nth (en_get_entries_for "that") 0;; (* There could be a determiner. *)
let __COMP__ = List.nth (en_get_entries_for "") 1;; (* Null 'that' *)
let __HACK__ = List.nth (en_get_entries_for "") 0;; (* Hack to remove finiteness. *)

let _expect_ = List.nth (en_get_entries_for "expect") 0;;
let _want_ = List.nth (en_get_entries_for "want") 1;;

let _hope__FIN__ = List.nth (en_get_entries_for "hope") 0;;
let _hope__NFIN__ = List.nth (en_get_entries_for "hope") 1;;

let _promise__FIN__ = List.nth (en_get_entries_for "promise") 0;;
let _promise__NFIN__ = List.nth (en_get_entries_for "promise") 1;;

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
    let d9 = merge __HACK__ d8 in
    let d10 = merge _seem_ d9 in
    let d11 = merge __EXPL__ d10 in
    let d12 = merge _it__EXPL__ d11 in
    let d13 = merge __PROG__ d12 in
    let d14 = merge __PERF__ d13 in
    let d15 = merge __s_ d14 in
    let d16 = move d15 in
    let d17 = move d16 in
    d17
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

(* Page 64 *)
(* (2.43) Exactly more than one maggot will devour more than two carcasses. *)
(*** TO DO ***)

(* Page 64 *)
(* (2.44) John will too. *)
(*** TO DO ***)

(* Page 74 *)
(* (2.45) Some abbot died. *)
let _some_abbot_died_ =
    let d1 = merge _some_ _abbot_ in
    let d2 = merge _die_ d1 in
    let d3 = merge __PROG__ d2 in
    let d4 = merge __PERF__ d3 in
    let d5 = merge __ed_ d4 in
    let d6 = move d5 in
    let d7 = move d6 in
    d7
;;

(* Page 75 *)
(* (2.46) George shaved some abbot. *)
let _George_shaved_some_abbot_ =
    let d1 = merge _some_ _abbot_ in
    let d2 = merge _shave_ d1 in
    let d3 = merge __ACT__ d2 in
    let d4 = move d3 in
    let d5 = merge _George_ d4 in
    let d6 = move d5 in
    let d7 = merge __PROG__ d6 in
    let d8 = merge __PERF__ d7 in
    let d9 = merge __ed_ d8 in
    let d10 = move d9 in
    let d11 = move d10 in
    d11
;;

(* Page 78 *)
(* (2.47) *John thinks some abbot (that) George shaved. *)
(*** UNGRAMMATICAL ***)

(* Page 78 *)
(* (2.48) John devoured George. *)
let _John_devoured_George_ =
    let d1 = merge _devour_ _George_ in
    let d2 = merge __ACT__ d1 in
    let d3 = move d2 in
    let d4 = merge _John_ d3 in
    let d5 = move d4 in
    let d6 = merge __PROG__ d5 in
    let d7 = merge __PERF__ d6 in
    let d8 = merge __ed_ d7 in
    let d9 = move d8 in
    let d10 = move d9 in
    d10
;;

(* Page 79 *)
(* (2.49) *George John devoured. *)
(*** UNGRAMMATICAL ***)

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

(* Page 85 *)
(* (2.52) Everyone was devoured. *)
let _everyone_was_devoured_ =
    let d1 = merge _devour_ _everyone_ in
    let d2 = merge __en__PASS__ d1 in
    let d3 = merge _be__PASS__ d2 in
    let d4 = merge __PROG__ d3 in
    let d5 = merge __PERF__ d4 in
    let d6 = merge __ed_ d5 in
    let d7 = move d6 in
    let d8 = move d7 in
    d8
;;

(* Page 85 *)
(* (2.53) Something devoured everyone. *)
(*** SEE PAGE 80 ABOVE ***)

(* Page 85 *)
(* (2.54) Everyone is expected to devour John. *)
let _everyone_is_expected_to_devour_John_ =
    let d1 = merge _devour_ _John_ in
    let d2 = merge __ACT__ d1 in
    let d3 = move d2 in
    let d4 = merge _everyone_ d3 in
    let d5 = move d4 in
    let d6 = merge __PROG__ d5 in
    let d7 = merge __PERF__ d6 in
    let d8 = merge _to__INF__ d7 in
    let d9 = merge _expect_ d8 in
    let d10 = merge __en__PASS__ d9 in
    let d11 = merge _be__PASS__ d10 in
    let d12 = merge __PROG__ d11 in
    let d13 = merge __PERF__ d12 in
    let d14 = merge __s_ d13 in
    let d15 = move d14 in
    let d16 = move d15 in
    d16
;;

(* Page 86 *)
(* (2.55) John seemed to shave an abbot. *)
let _John_seemed_to_shave_an_abbot_ =
    let d1 = merge _a_ _abbot_ in
    let d2 = merge _shave_ d1 in
    let d3 = merge __ACT__ d2 in
    let d4 = move d3 in
    let d5 = merge _John_ d4 in
    let d6 = move d5 in
    let d7 = merge __PROG__ d6 in
    let d8 = merge __PERF__ d7 in
    let d9 = merge _to__INF__ d8 in
    let d10 = merge _seem_ d9 in
    let d11 = merge __PROG__ d10 in
    let d12 = merge __PERF__ d11 in
    let d13 = merge __ed_ d12 in
    let d14 = move d13 in
    let d15 = move d14 in
    d15
;;

(* Page 86 *)
(* (2.56) John wanted to shave an abbot. *)
let _John_wanted_to_shave_an_abbot_ =
    let d1 = merge _a_ _abbot_ in
    let d2 = merge _shave_ d1 in
    let d3 = merge __ACT__ d2 in
    let d4 = move d3 in
    let d5 = cmerge _John_ d4 in
    let d6 = move d5 in
    let d7 = merge __PROG__ d6 in
    let d8 = merge __PERF__ d7 in
    let d9 = merge _to__INF__ d8 in
    let d10 = merge _want_ d9 in
    let d11 = cmove1 d10 in
    let d12 = merge __PROG__ d11 in
    let d13 = merge __PERF__ d12 in
    let d14 = merge __ed_ d13 in
    let d15 = move d14 in
    let d16 = move d15 in
    d16
;;

(* Page 86 *)
(* (2.57) George expected John to shave an abbot. *)
let _George_expected_John_to_shave_an_abbot_ =
    let d1 = merge _a_ _abbot_ in
    let d2 = merge _shave_ d1 in
    let d3 = merge __ACT__ d2 in
    let d4 = move d3 in
    let d5 = merge _John_ d4 in
    let d6 = move d5 in
    let d7 = merge __PROG__ d6 in
    let d8 = merge __PERF__ d7 in
    let d9 = merge _to__INF__ d8 in
    let d10 = merge _expect_ d9 in
    let d11 = merge __ACT__ d10 in
    let d12 = move d11 in
    let d13 = merge _George_ d12 in
    let d14 = move d13 in
    let d15 = merge __PROG__ d14 in
    let d16 = merge __PERF__ d15 in
    let d17 = merge __ed_ d16 in
    let d18 = move d17 in
    let d19 = move d18 in
    d19
;;

(* Page 86 *)
(* (2.58) George persuaded John to shave an abbot. *)
let _George_persuaded_John_to_shave_an_abbot_ =
    let d1 = merge _a_ _abbot_ in
    let d2 = merge _shave_ d1 in
    let d3 = merge __ACT__ d2 in
    let d4 = move d3 in
    let d5 = cmerge _John_ d4 in
    let d6 = move d5 in
    let d7 = merge __PROG__ d6 in
    let d8 = merge __PERF__ d7 in
    let d9 = merge _to__INF__ d8 in
    let d10 = merge _persuade_ d9 in
    let d11 = cmove1 d10 in
    let d12 = merge __ACT__ d11 in
    let d13 = move d12 in
    let d14 = merge _George_ d13 in
    let d15 = move d14 in
    let d16 = merge __PROG__ d15 in
    let d17 = merge __PERF__ d16 in
    let d18 = merge __ed_ d17 in
    let d19 = move d18 in
    let d20 = move d19 in
    d20
;;





(* Page 86 *)
(* (2.63) It seemed to be raining. *)
let _it_seemed_to_be_raining_ =
    let d1 = merge __ing_ _rain_ in
    let d2 = merge _be__PROG__ d1 in
    let d3 = merge __PERF__ d2 in
    let d4 = merge _to__INF__ d3 in
    let d5 = merge _seem_ d4 in
    let d6 = merge __EXPL__ d5 in
    let d7 = merge _it__EXPL__ d6 in
    let d8 = merge __PROG__ d7 in
    let d9 = merge __PERF__ d8 in
    let d10 = merge __ed_ d9 in
    let d11 = move d10 in
    let d12 = move d11 in
    d12
;;

(* Page 86 *)
(* (2.64) *It wanted to be raining. *)
(*** UNGRAMMATICAL ***)

(* Page 86 *)
(* (2.65) George expected it to be raining. *)
let _George_expected_it_to_be_raining_ =
    let d1 = merge __EXPL__ _rain_ in
    let d2 = merge _it__EXPL__ d1 in
    let d3 = merge __ing_ d2 in
    let d4 = merge _be__PROG__ d3 in
    let d5 = merge __PERF__ d4 in
    let d6 = merge _to__INF__ d5 in
    let d7 = merge _expect_ d6 in
    let d8 = merge __ACT__ d7 in
    let d9 = move d8 in
    let d10 = merge _George_ d9 in
    let d11 = move d10 in
    let d12 = merge __PROG__ d11 in
    let d13 = merge __PERF__ d12 in
    let d14 = merge __ed_ d13 in
    let d15 = move d14 in
    let d16 = move d15 in
    d16
;;

(* Page 86 *)
(* (2.66) *George persuaded it to be raining. *)
(*** UNGRAMMATICAL ***)

(* Page 88 *)
(* (2.67) George persuaded John to want to shave an abbot. *)
let _George_persuaded_John_to_want_to_shave_an_abbot_ =
    let d1 = merge _a_ _abbot_ in
    let d2 = merge _shave_ d1 in
    let d3 = merge __ACT__ d2 in
    let d4 = move d3 in
    let d5 = cmerge _John_ d4 in
    let d6 = move d5 in
    let d7 = merge __PROG__ d6 in
    let d8 = merge __PERF__ d7 in
    let d9 = merge _to__INF__ d8 in
    let d10 = merge _want_ d9 in
    let d11 = cmove2 d10 in
    let d12 = merge __PROG__ d11 in
    let d13 = merge __PERF__ d12 in
    let d14 = merge _to__INF__ d13 in
    let d15 = merge _persuade_ d14 in
    let d16 = cmove1 d15 in
    let d17 = merge __ACT__ d16 in
    let d18 = move d17 in
    let d19 = merge _George_ d18 in
    let d20 = move d19 in
    let d21 = merge __PROG__ d20 in
    let d22 = merge __PERF__ d21 in
    let d23 = merge __ed_ d22 in
    let d24 = move d23 in
    let d25 = move d24 in
    d25
;;

(* Page 89 *)
(* (2.68) Every barber promised George to be persuaded to shave an abbot. *)
let _every_barber_promised_George_to_be_persuaded_to_shave_an_abbot_ =
    let d1 = merge _a_ _abbot_ in
    let d2 = merge _shave_ d1 in
    let d3 = merge __ACT__ d2 in
    let d4 = move d3 in
    let d5 = merge _every_ _barber_ in
    let d6 = cmerge d5 d4 in
    let d7 = move d6 in
    let d8 = merge __PROG__ d7 in
    let d9 = merge __PERF__ d8 in
    let d10 = merge _to__INF__ d9 in
    let d11 = merge _persuade_ d10 in
    let d12 = cmove2 d11 in
    let d13 = merge __en__PASS__ d12 in
    let d14 = merge _be__PASS__ d13 in
    let d15 = merge __PROG__ d14 in
    let d16 = merge __PERF__ d15 in
    let d17 = merge _to__INF__ d16 in
    let d18 = merge _promise__NFIN__ _George_ in
    let d19 = move d18 in
    let d20 = merge d19 d17 in
    let d21 = cmove1 d20 in
    let d22 = move d21 in
    let d23 = merge __PROG__ d22 in
    let d24 = merge __PERF__ d23 in
    let d25 = merge __ed_ d24 in
    let d26 = move d25 in
    let d27 = move d26 in
    d27
;;

(* Page 96 *)
(* (2.69) George promised John to arrive on time. *)
(*** TO DO ***)

(* Page 96 *)
(* (2.70) *John was promised to arrive on time. *)
(*** UNGRAMMATICAL ***)

(* Page 96 *)
(* (2.71) Mary promised John that George would arrive on time. *)
(*** TO DO ***)

(* Page 96 *)
(* (2.72) John was promised that George would arrive on time. *)
(*** TO DO ***)

(* Page 99 *)
(* (2.73) Every barber wanted to arrive. *)
let _every_barber_wanted_to_arrive_ =
    let d1 = merge _every_ _barber_ in
    let d2 = cmerge _arrive_ d1 in
    let d3 = merge __PROG__ d2 in
    let d4 = merge __PERF__ d3 in
    let d5 = merge _to__INF__ d4 in
    let d6 = merge _want_ d5 in
    let d7 = cmove1 d6 in
    let d8 = merge __PROG__ d7 in
    let d9 = merge __PERF__ d8 in
    let d10 = merge __ed_ d9 in
    let d11 = move d10 in
    let d12 = move d11 in
    d12
;;

(* Page 99 *)
(* (2.74) *Every barber wanted (that) George will be promised to arrive. *)
(*** UNGRAMMATICAL ***)

(* Page 100 *)
(* (2.75) to arrive every barber *)
let _to_arrive_every_barber__raising =
    let d1 = merge _every_ _barber_ in
    let d2 = merge _arrive_ d1 in
    let d3 = merge __PROG__ d2 in
    let d4 = merge __PERF__ d3 in
    let d5 = merge _to__INF__ d4 in
    d5
;;

let _to_arrive_every_barber__control =
    let d1 = merge _every_ _barber_ in
    let d2 = cmerge _arrive_ d1 in
    let d3 = merge __PROG__ d2 in
    let d4 = merge __PERF__ d3 in
    let d5 = merge _to__INF__ d4 in
    d5
;;

(* Page 100 *)
(* (2.76) promise some abbot *)
let _promise_some_abbot__merge =
    let d1 = merge _some_ _abbot_ in
    let d2 = merge _promise__NFIN__ d1 in
    d2
;;

let _promise_some_abbot__cmerge =
    let d1 = merge _some_ _abbot_ in
    let d2 = cmerge _promise__NFIN__ d1 in
    d2
;;

(* Page 101 *)
(* (2.77) *Some abbot promised every barber to arrive. *)
(*** UNGRAMMATICAL ***)

(* Page 101 *)
(* (2.78) Some abbot promised himself that every barber would arrive. *)
(*** TO DO ***)

(*
let d1 = merge _persuade_ ( (Derivation.make_subtree [] [ M("to") ] [ M("arrive") ] [ Categorial("t") ]), [ (Derivation.make_subtree [] [ M("every") ] [ M("barber") ] [ Licensee("k"); Licensee("q") ]) ] ) in
let d2 = ( (Derivation.make_subtree [] [ M("some") ] [ M("abbot") ] [ Categorial'("d"); Licensee("k"); Licensee("q") ]), [] ) in
let d3 = cmerge d1 d2 in
d3;;
*)

(* Page 105 *)
(* (2.79) John promised George to shave an abbot. *)
let _John_promised_George_to_shave_an_abbot_ =
    let d1 = merge _a_ _abbot_ in
    let d2 = merge _shave_ d1 in
    let d3 = merge __ACT__ d2 in
    let d4 = move d3 in
    let d5 = cmerge _John_ d4 in
    let d6 = move d5 in
    let d7 = merge __PROG__ d6 in
    let d8 = merge __PERF__ d7 in
    let d9 = merge _to__INF__ d8 in
    let d10 = merge _promise__NFIN__ _George_ in
    let d11 = move d10 in
    let d12 = merge d11 d9 in
    let d13 = cmove1 d12 in
    let d14 = move d13 in
    let d15 = merge __PROG__ d14 in
    let d16 = merge __PERF__ d15 in
    let d17 = merge __ed_ d16 in
    let d18 = move d17 in
    let d19 = move d18 in
    d19
;;

(* Page 105 *)
(* (2.80) *George was promised to shave an abbot. *)
(*** UNGRAMMATICAL ***)

(* Page 105 *)
(* (2.81) John persuaded George to shave an abbot. *)
let _John_persuaded_George_to_shave_an_abbot_ =
    let d1 = merge _a_ _abbot_ in
    let d2 = merge _shave_ d1 in
    let d3 = merge __ACT__ d2 in
    let d4 = move d3 in
    let d5 = cmerge _George_ d4 in
    let d6 = move d5 in
    let d7 = merge __PROG__ d6 in
    let d8 = merge __PERF__ d7 in
    let d9 = merge _to__INF__ d8 in
    let d10 = merge _persuade_ d9 in
    let d11 = cmove1 d10 in
    let d12 = merge __ACT__ d11 in
    let d13 = move d12 in
    let d14 = merge _John_ d13 in
    let d15 = move d14 in
    let d16 = merge __PROG__ d15 in
    let d17 = merge __PERF__ d16 in
    let d18 = merge __ed_ d17 in
    let d19 = move d18 in
    let d20 = move d19 in
    d20
;;

(* Page 105 *)
(* (2.82) George was persuaded to shave an abbot. *)
let _George_was_persuaded_to_shave_an_abbot_ =
    let d1 = merge _a_ _abbot_ in
    let d2 = merge _shave_ d1 in
    let d3 = merge __ACT__ d2 in
    let d4 = move d3 in
    let d5 = cmerge _George_ d4 in
    let d6 = move d5 in
    let d7 = merge __PROG__ d6 in
    let d8 = merge __PERF__ d7 in
    let d9 = merge _to__INF__ d8 in
    let d10 = merge _persuade_ d9 in
    let d11 = cmove1 d10 in
    let d12 = merge __en__PASS__ d11 in
    let d13 = merge _be__PASS__ d12 in
    let d14 = merge __PROG__ d13 in
    let d15 = merge __PERF__ d14 in
    let d16 = merge __ed_ d15 in
    let d17 = move d16 in
    let d18 = move d17 in
    d18
;;

(* Page 105 *)
(* (2.83) John promised George that every barber had shaved an abbot. *)
let _John_promised_George_that_every_barber_had_shaved_an_abbot_ =
    let d1 = merge _a_ _abbot_ in
    let d2 = merge _shave_ d1 in
    let d3 = merge __ACT__ d2 in
    let d4 = move d3 in
    let d5 = merge _every_ _barber_ in
    let d6 = merge d5 d4 in
    let d7 = move d6 in
    let d8 = merge __PROG__ d7 in
    let d9 = merge __en__PERF__ d8 in
    let d10 = merge _have__PERF__ d9 in
    let d11 = merge __ed_ d10 in
    let d12 = move d11 in
    let d13 = move d12 in
    let d14 = merge _that__COMP__ d13 in
    let d15 = merge _promise__FIN__ _George_ in
    let d16 = merge d15 d14 in
    let d17 = merge __ACT__ d16 in
    let d18 = move d17 in
    let d19 = merge _John_ d18 in
    let d20 = move d19 in
    let d21 = merge __PROG__ d20 in
    let d22 = merge __PERF__ d21 in
    let d23 = merge __ed_ d22 in
    let d24 = move d23 in
    let d25 = move d24 in
    d25
;;

(* Page 106 *)
(* (2.84) George was promised that every barber had shaved an abbot. *)
let _George_was_promised_that_every_barber_had_shaved_an_abbot_ =
    let d1 = merge _a_ _abbot_ in
    let d2 = merge _shave_ d1 in
    let d3 = merge __ACT__ d2 in
    let d4 = move d3 in
    let d5 = merge _every_ _barber_ in
    let d6 = merge d5 d4 in
    let d7 = move d6 in
    let d8 = merge __PROG__ d7 in
    let d9 = merge __en__PERF__ d8 in
    let d10 = merge _have__PERF__ d9 in
    let d11 = merge __ed_ d10 in
    let d12 = move d11 in
    let d13 = move d12 in
    let d14 = merge _that__COMP__ d13 in
    let d15 = merge _promise__FIN__ _George_ in
    let d16 = merge d15 d14 in
    let d17 = merge __en__PASS__ d16 in
    let d18 = merge _be__PASS__ d17 in
    let d19 = merge __PROG__ d18 in
    let d20 = merge __PERF__ d19 in
    let d21 = merge __ed_ d20 in
    let d22 = move d21 in
    let d23 = move d22 in
    d23
;;

(* Page 106 *)
(* (2.85) George hoped to shave an abbot. *)
(*** TO DO ***)

(* Page 106 *)
(* (2.86) *George was hoped to shave an abbot. *)
(*** UNGRAMMATICAL ***)

(* Page 106 *)
(* (2.87) George hoped that every barber had shaved an abbot. *)
(*** TO DO ***)

(* Page 106 *)
(* (2.88) It was hoped that every barber had shaved an abbot. *)
(*** TO DO ***)

(* Page 106 *)
(* (fn35-1) It rained. *)
let _it_rained_ =
    let d1 = merge __EXPL__ _rain_ in
    let d2 = merge _it__EXPL__ d1 in
    let d3 = merge __PROG__ d2 in
    let d4 = merge __PERF__ d3 in
    let d5 = merge __ed_ d4 in
    let d6 = move d5 in
    let d7 = move d6 in
    d7
;;

(* Page 106 *)
(* (fn35-2) *To rain. *)
(*** UNGRAMMATICAL ***)
