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

type morpheme = M of string | Trace;;
type subtree = { spec: morpheme list; head: morpheme list; comp: morpheme list; features: feature list };;

module Derivation =
    struct
        type t = subtree * subtree list;;
        let compare = compare;;

        (* make_subtree : morpheme list -> morpheme list -> morpheme list -> feature list -> subtree *)
        let make_subtree spec head comp features =
            { spec = spec; head = head; comp = comp; features = features }
        ;;
    end
;;

module Lexicon = Set.Make( Derivation );;

(* val make_lexeme : string -> feature list -> subtree * subtree list = <fun> *)
let make_lexeme head features =
    ( (Derivation.make_subtree [] [ M( head ) ] [] features), [] )
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

(* val merge_selection : subtree * subtree list -> subtree * subtree list -> subtree * subtree list = <fun> *)
let merge_selection ( stree : subtree * subtree list ) ( ctree : subtree * subtree list ) =
    let st_merge = fst stree in
    let ct_merge = fst ctree in
    let st_move = snd stree in
    let ct_move = snd ctree in
    let st_f = List.tl (st_merge.features) in
    let ct_f = List.tl (ct_merge.features) in
    match ct_f with
          []    ->print_endline "no more features";
            (
                (Derivation.make_subtree [] st_merge.head ( ct_merge.spec @ ct_merge.head @ ct_merge.comp ) st_f),
                ct_move
            )
        | _::_  ->print_endline "licensing features";
            (
                (Derivation.make_subtree st_merge.spec st_merge.head st_merge.comp st_f),
                ct_move @ [ (Derivation.make_subtree ct_merge.spec ct_merge.head ct_merge.comp ct_f) ]
            )
;;

(* val merge_raising : subtree * subtree list -> subtree * subtree list -> subtree * subtree list = <fun> *)
let merge_raising stree ctree =
    let st_merge = fst stree in
    let ct_merge = fst ctree in
    let st_move = snd stree in
    let ct_move = snd ctree in
    let stree =
        (
            (Derivation.make_subtree st_merge.spec ( ct_merge.head @ st_merge.head ) st_merge.comp st_merge.features),
            (List.rev (List.rev_map (function x -> Derivation.make_subtree x.spec x.head x.comp x.features) st_move))
        )
    in
    let ctree =
        (
            (Derivation.make_subtree ct_merge.spec [] ct_merge.comp ct_merge.features),
            (List.rev (List.rev_map (function x -> Derivation.make_subtree x.spec x.head x.comp x.features) ct_move))
        )
    in
    merge_selection stree ctree
;;

(* val merge_lowering : subtree * subtree list -> subtree * subtree list -> subtree * subtree list = <fun> *)
let merge_lowering stree ctree =
    let st_merge = fst stree in
    let ct_merge = fst ctree in
    let st_move = snd stree in
    let ct_move = snd ctree in
    let stree =
        (
            (Derivation.make_subtree st_merge.spec [] st_merge.comp st_merge.features),
            (List.rev (List.rev_map (function x -> Derivation.make_subtree x.spec x.head x.comp x.features) st_move))
        )
    in
    let ctree =
        (
            (Derivation.make_subtree ct_merge.spec ( st_merge.head @ ct_merge.head ) ct_merge.comp ct_merge.features),
            (List.rev (List.rev_map (function x -> Derivation.make_subtree x.spec x.head x.comp x.features) ct_move))
        )
    in
    merge_selection stree ctree
;;

(* val merge_test : subtree * subtree -> subtree * subtree -> subtree * subtree = <fun> *)
let merge tree1 tree2 =
    let f1 = List.hd (fst tree1).features in
    let f2 = List.hd (fst tree2).features in
    match ( f1, f2 ) with
          ( Selection(s), Categorial(c) ) when s = c    ->  merge_selection tree1 tree2
        | ( Categorial(c), Selection(s) ) when s = c    ->  merge_selection tree2 tree1
        | ( Raising(r), Categorial(c) ) when r = c      ->  merge_raising tree1 tree2
        | ( Categorial(c), Raising(r) ) when r = c      ->  merge_raising tree2 tree1
        | ( Lowering(l), Categorial(c) ) when l = c     ->  merge_lowering tree1 tree2
        | ( Categorial(c), Lowering(l) ) when l = c     ->  merge_lowering tree2 tree1

        (* These might need special handling at some point. *)
        | ( Selection(s), Categorial'(c) ) when s = c   ->  merge_selection tree1 tree2
        | ( Categorial'(c), Selection(s) ) when s = c   ->  merge_selection tree2 tree1
        | ( Raising(r), Categorial'(c) ) when r = c     ->  merge_raising tree1 tree2
        | ( Categorial'(c), Raising(r) ) when r = c     ->  merge_raising tree2 tree1
        | ( Lowering(l), Categorial'(c) ) when l = c    ->  merge_lowering tree1 tree2
        | ( Categorial'(c), Lowering(l) ) when l = c    ->  merge_lowering tree2 tree1

        | _                                             ->  raise (InvalidDerivation "merge")
;;

(* val check_licensee : subtree -> subtree list -> subtree * subtree list = <fun> *)
let check_licensee licensor licensee =
    let ee1 = List.hd licensee in
    let f_or = List.tl licensor.features in
    let f_ee = List.tl ee1.features in
    match f_ee with
          []    ->print_endline "no more licensing features";
            (
                (Derivation.make_subtree ( ee1.spec @ ee1.head @ ee1.comp ) licensor.head licensor.comp f_or),
                []
            )
        | _::_  ->print_endline "more licensing features";
            (
                (Derivation.make_subtree licensor.spec licensor.head licensor.comp f_or),
                (Derivation.make_subtree ee1.spec ee1.head ee1.comp f_ee)::(List.tl licensee)
            )
;;

(* val move : subtree * subtree list -> subtree * subtree list = <fun> *)
let move tree =
    let t_merge = fst tree in
    let t_move = snd tree in
    let f_merge = List.hd t_merge.features in
    let f_move = List.hd (List.hd t_move).features in
    match ( f_merge, f_move ) with
          ( Licensor( o ), Licensee( e ) ) when e = o   ->  check_licensee t_merge t_move
        | _                                             ->  raise (InvalidDerivation "move")
;;

(* val get_entries_that_match : Lexicon.t -> (Lexicon.elt -> bool) -> Lexicon.elt list = <fun> *)
let get_entries_that_match lexicon condition =
    Lexicon.elements (Lexicon.filter condition lexicon)
;;

(* val get_entries_for : Lexicon.t -> string -> Lexicon.elt list = <fun> *)
let get_entries_for lexicon lexeme =
    let match_lexeme entry =
        let get_lexeme entry =
            List.hd (fst entry).head
        in
        ( (get_lexeme entry) = M( lexeme ) )
    in
    get_entries_that_match lexicon match_lexeme
;;

(* English *)
let lex_en =
    add_lexemes Lexicon.empty [
        (* Tense and Aspect *)
        make_lexeme "will" [ Selection("perf"); Licensor("k"); Licensor("q"); Categorial("s") ];
        make_lexeme "-s" [ Lowering("perf"); Licensor("k"); Licensor("q"); Categorial("s") ];
        make_lexeme "-ed" [ Lowering("perf"); Licensor("k"); Licensor("q"); Categorial("s") ];

        make_lexeme "have" [ Selection("en"); Categorial("perf") ];
        make_lexeme "-en" [ Raising("prog"); Categorial("en") ];
        make_lexeme "" [ Raising("prog"); Categorial("perf") ];

        make_lexeme "be" [ Selection("ing"); Categorial("prog") ];
        make_lexeme "-ing" [ Raising("v"); Categorial("ing") ];
        make_lexeme "" [ Raising("v"); Categorial("prog") ];

        (* Non-finite, Embedded Clause, and Passive *)
        make_lexeme "to" [ Selection("perf"); Categorial("t") ];
        make_lexeme "that" [ Selection("s"); Categorial("t") ];
        make_lexeme "be" [ Selection("pass"); Categorial("v") ];
        make_lexeme "-en" [ Raising("V"); Categorial("pass") ];
        make_lexeme "" [ Raising("V"); Licensor("k"); Selection("d"); Licensor("q"); Categorial("v") ];

        (* Verbs *)
        make_lexeme "arrive" [ Selection("d"); Categorial("v") ];

        make_lexeme "devour" [ Selection("d"); Categorial("V") ];
        make_lexeme "shave" [ Selection("d"); Categorial("V") ];

        make_lexeme "seem" [ Selection("t"); Categorial("v") ];

        make_lexeme "expect" [ Selection("t"); Categorial("V") ];
        make_lexeme "want" [ Selection("t"); Categorial("V") ];
        make_lexeme "hope" [ Selection("s"); Categorial("V") ];

        make_lexeme "expect" [ Selection("t"); Selection("d"); Categorial("v") ];
        make_lexeme "want" [ Selection("t"); Selection("d"); Categorial("v") ];
        make_lexeme "hope" [ Selection("t"); Selection("d"); Categorial("v") ];

        make_lexeme "persuade" [ Selection("t"); Selection("d"); Categorial("V") ];

        make_lexeme "promise" [ Selection("d"); Selection("s"); Categorial("V") ];

        make_lexeme "promise" [ Selection("d"); Licensor("k"); Selection("t"); Selection("d"); Licensor("q"); Categorial("v") ];

        make_lexeme "" [ Raising("v"); Selection("z"); Categorial("v") ];

        make_lexeme "it" [ Categorial("z"); Licensee("k"); Licensee("q") ];

        (* Nominals *)
        make_lexeme "George" [ Categorial'("d"); Licensee("k"); Licensee("q") ];
        make_lexeme "John" [ Categorial'("d"); Licensee("k"); Licensee("q") ];
        make_lexeme "Mary" [ Categorial'("d"); Licensee("k"); Licensee("q") ];

        make_lexeme "the" [ Selection("n"); Categorial'("d"); Licensee("k"); Licensee("q") ];
        make_lexeme "every" [ Selection("n"); Categorial'("d"); Licensee("k"); Licensee("q") ];
        make_lexeme "some" [ Selection("n"); Categorial'("d"); Licensee("k"); Licensee("q") ];

        make_lexeme "ointment" [ Categorial("n") ];
        make_lexeme "abbot" [ Categorial("n") ];
        make_lexeme "barber" [ Categorial("n") ];

        (* *** *)

        make_lexeme "a" [ Selection("n"); Categorial'("d"); Licensee("k"); Licensee("q") ];

        make_lexeme "'s" [ Selection("d"); Licensor("k"); Licensor("q"); Selection("n"); Categorial'("d"); Licensee("k"); Licensee("q") ];

        make_lexeme "everyone" [ Categorial("d"); Licensee("k"); Licensee("q") ];
        make_lexeme "something" [ Categorial("d"); Licensee("k"); Licensee("q") ];
    ]
;;

(* val en_get_entries_for : string -> Lexicon.elt list = <fun> *)
let en_get_entries_for = get_entries_for lex_en;;

(*********)
(* Tests *)
(*********)

(*---------*)
(* Lexicon *)
(*---------*)

let _the_ = List.hd (en_get_entries_for "the");;
let _ointment_ = List.hd (en_get_entries_for "ointment");;
let _'s_ = List.hd (en_get_entries_for "'s");;
let _John_ = List.hd (en_get_entries_for "John");;
let _devour_ = List.hd (en_get_entries_for "devour");;
let _will_ = List.hd (en_get_entries_for "will");;
let __ed_ = List.hd (en_get_entries_for "-ed");;
let __ing_ = List.hd (en_get_entries_for "-ing");;
let _everyone_ = List.hd (en_get_entries_for "everyone");;
let _something_ = List.hd (en_get_entries_for "something");;

let __act__ = List.nth (en_get_entries_for "") 0;;
let __perf__ = List.nth (en_get_entries_for "") 1;;
let __prog__ = List.nth (en_get_entries_for "") 2;;

(*-------------*)
(* Derivations *)
(*-------------*)

let _the_ointment_ = merge _the_ _ointment_;;

let _John's_ointment_ =
    let d1 = merge _'s_ _John_ in
    let d2 = move d1 in
    let d3 = move d2 in
    let d4 = merge _ointment_ d3 in
    d4
;;

let _devour_the_ointment_ = merge _devour_ _the_ointment_;;
(*
let _will_devour_the_ointment_ = merge _will_ _devour_;;
*)

(*
let __ing_devour_the_ointment_ = merge __ing_ _devour_the_ointment_;;
*)

let __act__devour_the_ointment_ = merge __act__ _devour_the_ointment_;;
let _the_ointment__act__devour_ = move __act__devour_the_ointment_;;
(*
merge _John_ _the_ointment__act__devour_;;
*)

let _arrive_ = List.hd (en_get_entries_for "arrive");;

let _promise_ = List.nth (en_get_entries_for "promise") 1;;

let _arrive_the_ointment_ = merge _arrive_ _the_ointment_;;
let _promise_the_ointment_ = merge _promise_ _the_ointment_;;
let _the_ointment_promise_ = move _promise_the_ointment_;;

(* Page 80 *)
(* (2.50) Something devoured everyone. *)
let _something_devoured_everyone_ =
    let d1 = merge _devour_ _everyone_ in
    let d2 = merge __act__ d1 in
    let d3 = move d2 in
    let d4 = merge d3 _something_ in
    let d5 = move d4 in
    let d6 = merge __prog__ d5 in
    let d7 = merge __perf__ d6 in
    let d8 = merge __ed_ d7 in
    let d9 = move d8 in
    let d10 = move d9 in
    d10
;;
