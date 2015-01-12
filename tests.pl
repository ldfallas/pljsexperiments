:- use_module(parseexperiment).

test_lex_var_decl :-
   js_lex_string("var x;",
		 [ tok(keyword, "var", _,_),
		   tok(id, "x", _,_),
		   tok(punctuator, ";",_, _) ]).

test_lex_id_with_keyword_prefix_decl :-
   js_lex_string("varx",
		 [ tok(id, "varx", _,_)]).

test_lex_numeric_literal :-
   js_lex_string("234",
		 [ tok(number, "234", _,_)]).

test_lex_string_literal :-
   js_lex_string("\"hola\"",
		 [ tok(string, "\"hola\"", _,_)]).



test_lex_var_decl_with_init_with_comments :-
   js_lex_string("var//uno
x = 20;",
		 [ tok(keyword, "var", _,_),
		   tok(id, "x", _, [line_comment("uno",_)]),
		   tok(punctuator, "=",_,_),
		   tok(number,"20",_,_),
		   tok(punctuator, ";",_, _) ]).

test_lex_var_decl_with_init :-
   js_lex_string("var x = 20;",
		 [ tok(keyword, "var", _,_),
		   tok(id, "x", _,_),
		   tok(punctuator, "=",_,_),
		   tok(number,"20",_,_),
		   tok(punctuator, ";",_, _) ]).

test_lex_simple_obj_literal :-
   js_lex_string("{
        \"on_keyup\"      : function(event) {
            // this is the first line of a comment,
            // just a noter comment,
            return true
        }
    }",

    [ tok(punctuator, "{", _, _),

      tok(string, "\"on_keyup\"",_,_),
      tok(punctuator, ":", _, _),
      tok(keyword, "function",_,_),
      tok(punctuator, "(", _, _),
      tok(id, "event",_,_),
      tok(punctuator, ")", _, _),
      tok(punctuator, "{", _, _),
      tok(keyword, "return",_,_),
      tok(keyword, "true",_,_),
      tok(punctuator, "}", _, _),

      tok(punctuator, "}", _, _)
    ]).



run_test(Test) :-
        functor(Test, Name, _),
        writef("Running "),
        writef(Name),
        (call(Test) -> writef(" PASS") ;
         (\+ call(Test)) -> writef(" FAILED")),
        writef("\n").


run_tests :-
        run_test(test_lex_var_decl),
        run_test(test_lex_id_with_keyword_prefix_decl ),
        run_test(test_lex_numeric_literal),
        run_test(test_lex_string_literal),
        run_test(test_lex_var_decl_with_init),
        run_test(test_lex_var_decl_with_init_with_comments),
        run_test(test_lex_simple_obj_literal).
run_tests.

member([X|Rest],X).
member([_|Rest],X) :- member(Rest,X).
