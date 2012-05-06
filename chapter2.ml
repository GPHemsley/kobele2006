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
          []    ->(*print_endline "no more features";*)
            (
                (Derivation.make_subtree [] st_merge.head ( ct_merge.spec @ ct_merge.head @ ct_merge.comp ) st_f),
                ct_move
            )
        | _::_  ->(*print_endline "licensing features";*)
            (
                (Derivation.make_subtree st_merge.spec st_merge.head st_merge.comp st_f),
                st_move @ [ (Derivation.make_subtree ct_merge.spec ct_merge.head ct_merge.comp ct_f) ]
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
            (Derivation.make_subtree ct_merge.spec ( ct_merge.head @ st_merge.head ) ct_merge.comp ct_merge.features),
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
          []    ->(*print_endline "no more licensing features";*)
            (
                (Derivation.make_subtree ( ee1.spec @ ee1.head @ ee1.comp ) licensor.head licensor.comp f_or),
                (List.tl licensee)
            )
        | _::_  ->(*print_endline "more licensing features";*)
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
