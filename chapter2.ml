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

type morpheme = M of string;;
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

(* val replace_head : subtree -> morpheme list -> subtree = <fun> *)
let replace_head subtree head =
    Derivation.make_subtree subtree.spec head subtree.comp subtree.features
;;

(* val replace_features : subtree -> feature list -> subtree = <fun> *)
let replace_features subtree features =
    Derivation.make_subtree subtree.spec subtree.head subtree.comp features
;;

(* val get_first_feature : subtree -> feature = <fun> *)
let rec get_first_feature subexp =
    List.hd subexp.features
;;

(* val get_first_licensee : subtree -> feature = <fun> *)
let rec get_first_licensee subexp =
    try
        List.find (function | Licensee(_) -> true | _ -> false) subexp.features
    with
        Not_found -> raise (InvalidDerivation "get_first_licensee")
;;

(* val get_first_features : subtree list -> feature list = <fun> *)
let rec get_first_features subexps =
    match subexps with
          []    ->  []
        | h::t  ->  (get_first_feature h)::(get_first_features t)
;;

(* val get_first_licensees : subtree list -> feature list = <fun> *)
let rec get_first_licensees subexps =
    match subexps with
          []    ->  []
        | h::t  ->  (get_first_licensee h)::(get_first_licensees t)
;;

(* val merge : subtree * subtree -> subtree * subtree -> subtree * subtree = <fun> *)
let merge tree1 tree2 =
    let merge_selection st_merge st_move ct_merge ct_move =
        let st_f = List.tl (st_merge.features) in
        let ct_f = List.tl (ct_merge.features) in
        match ct_f with
              []    ->
                (
                    (Derivation.make_subtree [] st_merge.head ( ct_merge.spec @ ct_merge.head @ ct_merge.comp ) st_f),
                    st_move @ ct_move
                )
            | _::_  ->
                let ct_move_new = replace_features ct_merge ct_f in
                if ( List.mem (get_first_feature ct_move_new) (get_first_features st_move) ) || ( List.mem (get_first_licensee ct_move_new) (get_first_licensees st_move) )
                then raise (InvalidDerivation "IMM")
                else
                    (
                        (replace_features st_merge st_f),
                        st_move @ ct_move @ [ ct_move_new ]
                    )
    in
    let merge_raising st_merge st_move ct_merge ct_move =
        merge_selection (replace_head st_merge ( ct_merge.head @ st_merge.head )) st_move (replace_head ct_merge []) ct_move
    in
    let merge_lowering st_merge st_move ct_merge ct_move =
        merge_selection (replace_head st_merge []) st_move (replace_head ct_merge ( ct_merge.head @ st_merge.head )) ct_move
    in
    let t1_merge = fst tree1 in
    let t2_merge = fst tree2 in
    let t1_move = snd tree1 in
    let t2_move = snd tree2 in
    let f1 = List.hd t1_merge.features in
    let f2 = List.hd t2_merge.features in
    match ( f1, f2 ) with
          ( Selection(s), Categorial(c) )
        | ( Selection(s), Categorial'(c) ) when s = c   ->  merge_selection t1_merge t1_move t2_merge t2_move
        | ( Categorial(c), Selection(s) )
        | ( Categorial'(c), Selection(s) ) when s = c   ->  merge_selection t2_merge t2_move t1_merge t1_move

        | ( Raising(r), Categorial(c) )
        | ( Raising(r), Categorial'(c) ) when r = c     ->  merge_raising t1_merge t1_move t2_merge t2_move
        | ( Categorial(c), Raising(r) )
        | ( Categorial'(c), Raising(r) ) when r = c     ->  merge_raising t2_merge t2_move t1_merge t1_move

        | ( Lowering(l), Categorial(c) )
        | ( Lowering(l), Categorial'(c) ) when l = c    ->  merge_lowering t1_merge t1_move t2_merge t2_move
        | ( Categorial(c), Lowering(l) )
        | ( Categorial'(c), Lowering(l) ) when l = c    ->  merge_lowering t2_merge t2_move t1_merge t1_move

        | _                                             ->  raise (InvalidDerivation "merge")
;;

(* XXX: There's gotta be a way to combine this with merge to reduce code duplication! *)
(* val cmerge : subtree * subtree list -> subtree * subtree list -> subtree * subtree list = <fun> *)
let cmerge tree1 tree2 =
    let cmerge_selection st_merge st_move ct_merge ct_move =
        let st_f = List.tl (st_merge.features) in
        let ct_f = ct_merge.features in
        match ct_f with
              []    ->
                (
                    (Derivation.make_subtree [] st_merge.head ( ct_merge.spec @ ct_merge.head @ ct_merge.comp ) st_f),
                    st_move @ ct_move
                )
            | _::_  ->
                let ct_move_new = replace_features ct_merge ct_f in
                if ( List.mem (get_first_feature ct_move_new) (get_first_features st_move) ) || ( List.mem (get_first_licensee ct_move_new) (get_first_licensees st_move) )
                then raise (InvalidDerivation "IMM")
                else
                    (
                        (replace_features st_merge st_f),
                        st_move @ ct_move @ [ ct_move_new ]
                    )
    in
    let cmerge_raising st_merge st_move ct_merge ct_move =
        cmerge_selection (replace_head st_merge ( ct_merge.head @ st_merge.head )) st_move (replace_head ct_merge []) ct_move
    in
    let cmerge_lowering st_merge st_move ct_merge ct_move =
        cmerge_selection (replace_head st_merge []) st_move (replace_head ct_merge ( ct_merge.head @ st_merge.head )) ct_move
    in
    let t1_merge = fst tree1 in
    let t2_merge = fst tree2 in
    let t1_move = snd tree1 in
    let t2_move = snd tree2 in
    let f1 = List.hd t1_merge.features in
    let f2 = List.hd t2_merge.features in
    match ( f1, f2 ) with
        | ( Selection(s), Categorial'(c) ) when s = c   ->  cmerge_selection t1_merge t1_move t2_merge t2_move
        | ( Categorial'(c), Selection(s) ) when s = c   ->  cmerge_selection t2_merge t2_move t1_merge t1_move

        | ( Raising(r), Categorial'(c) ) when r = c     ->  cmerge_raising t1_merge t1_move t2_merge t2_move
        | ( Categorial'(c), Raising(r) ) when r = c     ->  cmerge_raising t2_merge t2_move t1_merge t1_move

        | ( Lowering(l), Categorial'(c) ) when l = c    ->  cmerge_lowering t1_merge t1_move t2_merge t2_move
        | ( Categorial'(c), Lowering(l) ) when l = c    ->  cmerge_lowering t2_merge t2_move t1_merge t1_move

        | _                                             ->  raise (InvalidDerivation "merge")
;;

(* val get_movable_subexp : subtree -> subtree list -> subtree = <fun> *)
let get_movable_subexp t_merge t_move =
    let f_merge = List.hd t_merge.features in
    let match_feature f =
        match ( f_merge, (get_first_feature f) ) with
              ( Licensor(o), Licensee(e) )
            | ( Raising(o), Categorial'(e) )
            | ( Lowering(o), Categorial'(e) )
            | ( Selection(o), Categorial'(e) ) when e = o -> true
            | _ -> false
    in
    try
        List.find match_feature t_move
    with
        Not_found -> raise (InvalidDerivation "get_movable_subexp")
;;

(* val symmetric_check : subtree -> subtree list -> subtree * subtree list = <fun> *)
let symmetric_check checkor checkee =
    let ee_h = get_movable_subexp checkor checkee in
    let ee_t = List.filter ((!=) ee_h) checkee in
    let f_or = List.tl checkor.features in
    let f_ee = List.tl ee_h.features in
    match f_ee with
          []    ->(*print_endline "no more licensing features";*)
            (
                (Derivation.make_subtree ( ee_h.spec @ ee_h.head @ ee_h.comp ) checkor.head checkor.comp f_or),
                ee_t
            )
        | _::_  ->(*print_endline "more licensing features";*)
            (
                (Derivation.make_subtree checkor.spec checkor.head checkor.comp f_or),
                (Derivation.make_subtree ee_h.spec ee_h.head ee_h.comp f_ee)::ee_t (* XXX: This changes the order of the list. Does that matter? *)
            )
;;

(* val asymmetric_check : subtree -> subtree list -> subtree * subtree list = <fun> *)
let asymmetric_check checkor checkee =
    let ee_h = get_movable_subexp checkor checkee in
    let ee_t = List.filter ((!=) ee_h) checkee in
    let f_or = List.tl checkor.features in
    let f_ee = ee_h.features in
    match f_ee with
          []    ->(*print_endline "no more licensing features";*)
            (
                (Derivation.make_subtree ( ee_h.spec @ ee_h.head @ ee_h.comp ) checkor.head checkor.comp f_or),
                ee_t
            )
        | _::_  ->(*print_endline "more licensing features";*)
            (
                (Derivation.make_subtree checkor.spec checkor.head checkor.comp f_or),
                (Derivation.make_subtree ee_h.spec ee_h.head ee_h.comp f_ee)::ee_t (* XXX: This changes the order of the list. Does that matter? *)
            )
;;

(* val move : subtree * subtree list -> subtree * subtree list = <fun> *)
let move tree =
    let t_merge = fst tree in
    let t_move = snd tree in
    let f_merge = List.hd t_merge.features in
    let f_move = List.hd (get_movable_subexp t_merge t_move).features in
    match ( f_merge, f_move ) with
          ( Licensor( o ), Licensee( e ) ) when e = o   ->  symmetric_check t_merge t_move
        | _                                             ->  raise (InvalidDerivation "move")
;;

(* val cmove1 : subtree * subtree list -> subtree * subtree list = <fun> *)
let cmove1 tree =
    let t_merge = fst tree in
    let t_move = snd tree in
    let f_merge = List.hd t_merge.features in
    let f_move = List.hd (get_movable_subexp t_merge t_move).features in
    match ( f_merge, f_move ) with
          ( Raising( s ), Categorial'( c ) )
        | ( Lowering( s ), Categorial'( c ) )
        | ( Selection( s ), Categorial'( c ) ) when c = s   ->  symmetric_check t_merge t_move
        | _                                                 ->  raise (InvalidDerivation "cmove1")
;;

(* val cmove2 : subtree * subtree list -> subtree * subtree list = <fun> *)
let cmove2 tree =
    let t_merge = fst tree in
    let t_move = snd tree in
    let f_merge = List.hd t_merge.features in
    let f_move = List.hd (get_movable_subexp t_merge t_move).features in
    match ( f_merge, f_move ) with
          ( Raising( s ), Categorial'( c ) )
        | ( Lowering( s ), Categorial'( c ) )
        | ( Selection( s ), Categorial'( c ) ) when c = s   ->  asymmetric_check t_merge t_move
        | _                                                 ->  raise (InvalidDerivation "cmove2")
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
