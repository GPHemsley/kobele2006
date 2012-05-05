(*
type licensor = Licensor of string;;
type licensee = Licensee of string;;
type selector = Selection of string | Raising of string | Lowering of string;;
type selectee = Categorial of string | Categorial' of string;;
*)

(* Lexicon *)
type feature =
    (* Selection *)
      Categorial of string  (* f *)
    | Categorial' of string (* *f *)
    | Selection of string   (* =f *)
    (* Affixes *)
    | Raising of string     (* =>f *)
    | Lowering of string    (* f=> *)
    (* Movement *)
    | Licensee of string    (* -x *)
    | Licensor of string    (* +x *)
;;

type 'a derivation = Tree of int * 'a derivation * 'a derivation | Lexeme of string * feature list | Trace;;

type entry = Entry of string * feature list;;

module DerivationOrder =
    struct
        type t = entry
        let compare = compare
    end
;;

module Lexicon = Set.Make( DerivationOrder );;

exception InvalidEntry;;

(* val lexeme_of_entry : entry -> 'a derivation = <fun> *)
let lexeme_of_entry entry =
    match entry with
          Entry( l, f ) ->  Lexeme( l, f )
        | _             ->  raise InvalidEntry
;;

(* val add_lexeme : Lexicon.t -> Lexicon.elt -> Lexicon.t = <fun> *)
let add_lexeme lexicon entry = Lexicon.add entry lexicon;;

(* val add_lexemes : Lexicon.t -> Lexicon.elt list -> Lexicon.t = <fun> *)
let rec add_lexemes lexicon entries =
    match entries with
          []    ->  lexicon
        | h::t  ->  add_lexemes (add_lexeme lexicon h) t
;;

exception InvalidDerivation of string;;

(* val get_head : 'a derivation -> string * feature list = <fun> *)
let rec get_head derivation =
    match derivation with
          Lexeme( l, f )    ->  ( l, f )
        | Tree( 1, d, _ )
        | Tree( 2, _, d )   ->  get_head d
        | _                 ->  raise (InvalidDerivation "get_head")
;;

(* val pop_feature : 'a derivation -> 'a derivation = <fun> *)
let rec pop_feature derivation =
    match derivation with
          Lexeme( l, f )    ->  Lexeme( l, (List.tl f) )
        | Tree( 1, d1, d2 ) ->  Tree( 1, (pop_feature d1), d2 )
        | Tree( 2, d1, d2 ) ->  Tree( 2, d1, (pop_feature d2) )
        | _                 ->  raise (InvalidDerivation "pop_feature")
;;

(* val change_head : string -> 'a derivation -> 'a derivation = <fun> *)
let rec change_head label derivation =
    match derivation with
          Lexeme( l, f )    ->  Lexeme( label, f )
        | Tree( 1, d1, d2 ) ->  Tree( 1, (change_head label d1), d2 )
        | Tree( 2, d1, d2 ) ->  Tree( 2, d1, (change_head label d2) )
        | _                 ->  raise (InvalidDerivation "change_head")
;;

(* val add_head : string -> 'a derivation -> 'a derivation = <fun> *)
let add_head label derivation =
    change_head ( label ^ (fst (get_head derivation)) ) derivation
;;

(* val move_head : 'a derivation -> 'b derivation -> 'b derivation = <fun> *)
let move_head dfrom dto =
    add_head (fst (get_head dfrom)) dto
;;

(* val null_head : 'a derivation -> 'a derivation = <fun> *)
let rec null_head derivation =
    change_head "" derivation
;;

(* val merge : 'a derivation -> 'a derivation -> 'a derivation = <fun> *)
let merge d1 d2 =
    let h1 = List.hd (snd (get_head d1)) in
    let h2 = List.hd (snd (get_head d2)) in
    match ( h1, h2 ) with
          ( Selection(s), Categorial(c) ) when s = c    ->  Tree( 1, (pop_feature d1), (pop_feature d2) )
        | ( Categorial(c), Selection(s) ) when s = c    ->  Tree( 2, (pop_feature d1), (pop_feature d2) )
        | ( Raising(r), Categorial(c) ) when r = c      ->  Tree( 1, (pop_feature (move_head d2 d1)), (pop_feature (null_head d2)) )
        | ( Categorial(c), Raising(r) ) when r = c      ->  Tree( 2, (pop_feature (null_head d1)), (pop_feature (move_head d1 d2)) )
        | ( Lowering(l), Categorial(c) ) when l = c     ->  Tree( 1, (pop_feature (null_head d1)), (pop_feature (move_head d1 d2)) )
        | ( Categorial(c), Lowering(l) ) when l = c     ->  Tree( 2, (pop_feature (move_head d1 d2)), (pop_feature (null_head d2)) )

        (* These might need special handling at some point. *)
        | ( Selection(s), Categorial'(c) ) when s = c   ->  Tree( 1, (pop_feature d1), (pop_feature d2) )
        | ( Categorial'(c), Selection(s) ) when s = c   ->  Tree( 2, (pop_feature d1), (pop_feature d2) )
        | ( Raising(r), Categorial'(c) ) when r = c      ->  Tree( 1, (pop_feature (move_head d2 d1)), (pop_feature (null_head d2)) )
        | ( Categorial'(c), Raising(r) ) when r = c      ->  Tree( 2, (pop_feature (null_head d1)), (pop_feature (move_head d1 d2)) )
        | ( Lowering(l), Categorial'(c) ) when l = c     ->  Tree( 1, (pop_feature (null_head d1)), (pop_feature (move_head d1 d2)) )
        | ( Categorial'(c), Lowering(l) ) when l = c     ->  Tree( 2, (pop_feature (move_head d1 d2)), (pop_feature (null_head d2)) )

        | _                                             ->  raise (InvalidDerivation "merge")
;;

let rec find_licensee feature derivation =
    match derivation with
          Lexeme( l, f )->
            if ( (List.hd f) = Licensee(feature) )
            then
        | Tree( 1, d1, d2 )->
        | Tree( 2, d1, d2 )->
        | _->raise (InvalidDerivation "find_licensee")
;;

let move derivation =
    let head = (get_head derivation) in
    match (List.hd (snd head)) with
          Licensor(f)->
;;

(* val get_entries_that_match : Lexicon.t -> (Lexicon.elt -> bool) -> Lexicon.elt list = <fun> *)
let get_entries_that_match lexicon condition =
    Lexicon.elements (Lexicon.filter condition lexicon)
;;

(* val get_entries_for : Lexicon.t -> string -> Lexicon.elt list = <fun> *)
let get_entries_for lexicon lexeme =
    get_entries_that_match lexicon (function | x -> ( ((function | Entry( y, _ ) -> y ) x) = lexeme ))
;;

(* English *)
let lex_en =
    add_lexemes Lexicon.empty [
        (* Tense and Aspect *)
        Entry( "will", [ Selection("perf"); Licensor("k"); Licensor("q"); Categorial("s") ] );
        Entry( "-s", [ Lowering("perf"); Licensor("k"); Licensor("q"); Categorial("s") ] );
        Entry( "-ed", [ Lowering("perf"); Licensor("k"); Licensor("q"); Categorial("s") ] );

        Entry( "have", [ Selection("en"); Categorial("perf") ] );
        Entry( "-en", [ Raising("prog"); Categorial("en") ] );
        Entry( "", [ Raising("prog"); Categorial("perf") ] );

        Entry( "be", [ Selection("ing"); Categorial("prog") ] );
        Entry( "-ing", [ Raising("v"); Categorial("ing") ] );
        Entry( "", [ Raising("v"); Categorial("prog") ] );

        (* Non-finite, Embedded Clause, and Passive *)
        Entry( "to", [ Selection("perf"); Categorial("t") ] );
        Entry( "that", [ Selection("s"); Categorial("t") ] );
        Entry( "be", [ Selection("pass"); Categorial("v") ] );
        Entry( "-en", [ Raising("V"); Categorial("pass") ] );
        Entry( "", [ Raising("V"); Licensor("k"); Selection("d"); Licensor("q"); Categorial("v") ] );

        (* Verbs *)
        Entry( "arrive", [ Selection("d"); Categorial("v") ] );

        Entry( "devour", [ Selection("d"); Categorial("V") ] );
        Entry( "shave", [ Selection("d"); Categorial("V") ] );

        Entry( "seem", [ Selection("t"); Categorial("v") ] );

        Entry( "expect", [ Selection("t"); Categorial("V") ] );
        Entry( "want", [ Selection("t"); Categorial("V") ] );
        Entry( "hope", [ Selection("s"); Categorial("V") ] );

        Entry( "expect", [ Selection("t"); Selection("d"); Categorial("v") ] );
        Entry( "want", [ Selection("t"); Selection("d"); Categorial("v") ] );
        Entry( "hope", [ Selection("t"); Selection("d"); Categorial("v") ] );

        Entry( "persuade", [ Selection("t"); Selection("d"); Categorial("V") ] );

        Entry( "promise", [ Selection("d"); Selection("s"); Categorial("V") ] );

        Entry( "promise", [ Selection("d"); Licensor("k"); Selection("t"); Selection("d"); Licensor("q"); Categorial("v") ] );

        Entry( "", [ Raising("v"); Selection("z"); Categorial("v") ] );

        Entry( "it", [ Categorial("z"); Licensee("k"); Licensee("q") ] );

        (* Nominals *)
        Entry( "George", [ Categorial'("d"); Licensee("k"); Licensee("q") ] );
        Entry( "John", [ Categorial'("d"); Licensee("k"); Licensee("q") ] );
        Entry( "Mary", [ Categorial'("d"); Licensee("k"); Licensee("q") ] );

        Entry( "'s", [ Selection("n"); Selection("d"); Categorial'("d"); Licensee("k"); Licensee("q") ] );

        Entry( "the", [ Selection("n"); Categorial'("d"); Licensee("k"); Licensee("q") ] );
        Entry( "every", [ Selection("n"); Categorial'("d"); Licensee("k"); Licensee("q") ] );
        Entry( "some", [ Selection("n"); Categorial'("d"); Licensee("k"); Licensee("q") ] );

        Entry( "ointment", [ Categorial("n") ] );
        Entry( "abbot", [ Categorial("n") ] );
        Entry( "barber", [ Categorial("n") ] );
    ]
;;

(* val en_get_entries_for : string -> Lexicon.elt list = <fun> *)
let en_get_entries_for = get_entries_for lex_en;;

(* Lexeme ("the", [Selection "n"; Categorial' "d"; Licensee "k"; Licensee "q"]) *)
let _the_ = lexeme_of_entry (List.hd (en_get_entries_for "the"));;
let _ointment_ = lexeme_of_entry (List.hd (en_get_entries_for "ointment"));;
let _'s_ = lexeme_of_entry (List.hd (en_get_entries_for "'s"));;
let _John_ = lexeme_of_entry (List.hd (en_get_entries_for "John"));;
let _devour_ = lexeme_of_entry (List.hd (en_get_entries_for "devour"));;
(* Lexeme ("will", [Selection "perf"; Licensor "k"; Licensor "q"; Categorial "s"]) *)
let _will_ = lexeme_of_entry (List.hd (en_get_entries_for "will"));;
let __ed_ = lexeme_of_entry (List.hd (en_get_entries_for "-ed"));;
(* Lexeme ("-ing", [Raising "v"; Categorial "ing"]) *)
let __ing_ = lexeme_of_entry (List.hd (en_get_entries_for "-ing"));;

(* Lexeme ("", [Raising "V"; Licensor "k"; Selection "d"; Licensor "q"; Categorial "v"]) *)
let __pass__ = lexeme_of_entry (List.nth (en_get_entries_for "") 0);;

(* Tree (1, Lexeme ("the", [Categorial' "d"; Licensee "k"; Licensee "q"]), Lexeme ("ointment", [])) *)
let _the_ointment_ = merge _the_ _ointment_;;

let _'s_ointment_ = merge _'s_ _ointment_;;
let _John's_ointment_ = merge _John_ _'s_ointment_;;

(* Tree (1, Lexeme ("devour", [Categorial "V"]), Tree (1, Lexeme ("the", [Licensee "k"; Licensee "q"]), Lexeme ("ointment", []))) *)
let _devour_the_ointment_ = merge _devour_ _the_ointment_;;
(*
let _will_devour_the_ointment_ = merge _will_ _devour_;;
*)

(*
let __ing_devour_the_ointment_ = merge __ing_ _devour_the_ointment_;;
*)

let __pass__devour_the_ointment_ = merge __pass__ _devour_the_ointment_;;

let _arrive_ = lexeme_of_entry (List.hd (en_get_entries_for "arrive"));;

let _promise_ = lexeme_of_entry (List.nth (en_get_entries_for "promise") 1);;

let _arrive_the_ointment_ = merge _arrive_ _the_ointment_;;
let _promise_the_ointment_ = merge _promise_ _the_ointment_;;
let _the_ointment_promise_ = move _promise_the_ointment_;;

