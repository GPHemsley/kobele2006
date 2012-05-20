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
        make_lexeme "that" [ Selection("s"); Categorial("f") ]; (* p.c. *)
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
        make_lexeme "hope" [ Selection("f"); Categorial("V") ]; (* p.c. *)

        make_lexeme "expect" [ Selection("t"); Selection("d"); Categorial("v") ];
        make_lexeme "want" [ Selection("t"); Selection("d"); Categorial("v") ];
        make_lexeme "hope" [ Selection("t"); Selection("d"); Categorial("v") ];

        make_lexeme "persuade" [ Selection("t"); Selection("d"); Categorial("V") ];

        make_lexeme "promise" [ Selection("d"); Selection("f"); Categorial("V") ]; (* p.c. *)

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

        make_lexeme "a" [ Selection("n"); Categorial'("d"); Licensee("k"); Licensee("q") ]; (* Page 90 *)

        make_lexeme "'s" [ Selection("n"); Selection("d"); Licensor("k"); Licensor("q"); Categorial'("d"); Licensee("k"); Licensee("q") ]; (* o.r. *)

        make_lexeme "everyone" [ Categorial'("d"); Licensee("k"); Licensee("q") ]; (* Page 80 + 88 *)
        make_lexeme "something" [ Categorial'("d"); Licensee("k"); Licensee("q") ]; (* Page 80 + 88 *)

        make_lexeme "" [ Selection("s"); Categorial("f") ]; (* covert 'that' *) (* p.c. *)
        make_lexeme "" [ Selection("f"); Categorial("t") ]; (* p.c. *)

        make_lexeme "eat" [ Selection("d"); Categorial("V") ]; (* o.r. *)

        make_lexeme "rain" [ Categorial("v") ]; (* Page 43 *)

        make_lexeme "die" [ Selection("d"); Categorial("v") ]; (* Page 74 *)
    ]
;;

(* val en_get_entries_for : string -> Lexicon.elt list = <fun> *)
let en_get_entries_for = get_entries_for lex_en;;
