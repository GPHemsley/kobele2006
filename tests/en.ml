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

let _everyone_ = List.hd (en_get_entries_for "everyone");;
let _something_ = List.hd (en_get_entries_for "something");;

let _the_ = List.hd (en_get_entries_for "the");;
let _'s_ = List.hd (en_get_entries_for "'s");;

let _ointment_ = List.hd (en_get_entries_for "ointment");;

let _devour_ = List.hd (en_get_entries_for "devour");;
let _arrive_ = List.hd (en_get_entries_for "arrive");;

let _seem_ = List.hd (en_get_entries_for "seem");;

let _will_ = List.hd (en_get_entries_for "will");;
let __s_ = List.hd (en_get_entries_for "-s");;
let __ed_ = List.hd (en_get_entries_for "-ed");;

let _have__PERF__ = List.nth (en_get_entries_for "have") 0;;
let __en__PERF__ = List.nth (en_get_entries_for "-en") 1;;
let __PERF__ = List.nth (en_get_entries_for "") 1;;

let _be__PROG__ = List.nth (en_get_entries_for "be") 0;;
let __ing_ = List.hd (en_get_entries_for "-ing");;
let __PROG__ = List.nth (en_get_entries_for "") 2;;

let _be__PASS__ = List.nth (en_get_entries_for "be") 1;;
let __en__PASS__ = List.nth (en_get_entries_for "-en") 0;;
let __ACT__ = List.nth (en_get_entries_for "") 0;;

let _to__INF__ = List.nth (en_get_entries_for "to") 0;;
let _that__COMP__ = List.nth (en_get_entries_for "that") 0;;

let _promise_ = List.nth (en_get_entries_for "promise") 1;;

(*-------------*)
(* Derivations *)
(*-------------*)

(* Page 20 *)
(* the ointment *)
let _the_ointment_ = merge _the_ _ointment_;;

(* Page 21 *)
(* John's ointment *)
let _John's_ointment_ =
    let d1 = merge _'s_ _ointment_ in
    let d2 = merge _John_ d1 in
    let d3 = move d2 in
    let d4 = move d3 in
    d4
;;

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

(* Page 22, 28 *)
(* (2.8) John is devouring the ointment. *)
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

(* Page 22, 57 *)
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
