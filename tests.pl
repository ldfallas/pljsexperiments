:- use_module(parseexperiment).

test_lex_var_decl :-
   js_lex_string("var x;",
		 [ tok(keyword, "var", _,_,_),
		   tok(id, "x", _,_,_),
		   tok(punctuator, ";",_,_,_) ]).

test_lex_id_with_keyword_prefix_decl :-
   js_lex_string("varx",
		 [ tok(id, "varx", _,_,_)]).

test_lex_id1 :-
   js_lex_string("foo",
		 [ tok(id, "foo", _,_,_)]).
test_lex_id2 :-
   js_lex_string("foo1",
		 [ tok(id, "foo1", _,_,_)]).
test_lex_id3 :-
   js_lex_string("$foo1",
		 [ tok(id, "$foo1", _,_,_)]).
test_lex_id4 :-
   js_lex_string("$",
		 [ tok(id, "$", _,_,_)]).
test_lex_id5 :-
   js_lex_string("an_id_for",
		 [ tok(id, "an_id_for", _,_,_)]).
test_lex_two_char_op :-
   js_lex_string("+=",
		 [ tok(punctuator, "+=", _,_,_)]).

test_lex_floating_point_number_1 :-
   js_lex_string("34.45",
		 [ tok(number, "34.45", _,_,_)]).

test_lex_dotted_id :-
   js_lex_string("a.b",
		 [ tok(id, "a", _,_,_),
                   tok(punctuator, ".", _,_,_),
                   tok(id, "b", _,_,_) ]).

test_lex_numeric_literal :-
   js_lex_string("234",
		 [ tok(number, "234", _,_,_)]).

test_lex_string_literal :-
   js_lex_string("\"hola\"",
		 [ tok(string, "\"hola\"", _,_,_)]).

test_lex_string_literal_single_quote :-
   js_lex_string("\'hola\'",
		 [ tok(string, "\'hola\'", _,_,_)]).

test_lex_operator_literal :-
   js_lex_string("1+2*3/4-5",
		 [ 
                 tok(number, "1", _,_,_),
                 tok(punctuator, "+", _,_,_),
                 tok(number, "2", _,_,_),
                 tok(punctuator, "*", _,_,_),
                 tok(number, "3", _,_,_),
                 tok(punctuator, "/", _,_,_),
                 tok(number, "4", _,_,_),
                 tok(punctuator, "-", _,_,_),
                 tok(number, "5", _,_,_)
                 ]).


test_lex_var_decl_with_init_with_comments :-
   js_lex_string("var//uno
x = 20;",
		 [ tok(keyword, "var", _,_,_),
		   tok(id, "x", _,_, [line_comment("uno",_)]),
		   tok(punctuator, "=",_,_,_),
		   tok(number,"20",_,_,_),
		   tok(punctuator, ";",_,_,_) ]).

test_lex_var_decl_with_init :-
   js_lex_string("var x = 20;",
		 [ tok(keyword, "var", _,_,_),
		   tok(id, "x", _,_,_),
		   tok(punctuator, "=",_,_,_),
		   tok(number,"20",_,_,_),
		   tok(punctuator, ";",_,_,_) ]).

test_lex_simple_obj_literal :-
   js_lex_string("{
        \"on_keyup\"      : function(event) {
            // this is the first line of a comment,
            // just a noter comment,
            return true
        }
    }",

    [ tok(punctuator, "{", _,_,_),

      tok(string, "\"on_keyup\"",_,_,_),
      tok(punctuator, ":", _,_,_),
      tok(keyword, "function",_,_,_),
      tok(punctuator, "(", _,_,_),
      tok(id, "event",_,_,_),
      tok(punctuator, ")", _,_,_),
      tok(punctuator, "{", _,_,_),
      tok(keyword, "return",_,_,_),
      tok(keyword, "true",_,_,_),
      tok(punctuator, "}", _,_,_),

      tok(punctuator, "}", _,_,_)
    ]).

test_lex_simple_block_comment :-
   js_lex_string("{
        \"on_keyup\"      : function(event) { /* c
             this is the first line of a block comment,
             just a noter comment,
            */ return true
        }
    }",

    [ tok(punctuator, "{", _,_,_),

      tok(string, "\"on_keyup\"",_,_,_),
      tok(punctuator, ":", _,_,_),
      tok(keyword, "function",_,_,_),
      tok(punctuator, "(", _,_,_),
      tok(id, "event",_,_,_),
      tok(punctuator, ")", _,_,_),
      tok(punctuator, "{", _,_,_),
      tok(keyword, "return",_,_,_),
      tok(keyword, "true",_,_,_),
      tok(punctuator, "}", _,_,_),

      tok(punctuator, "}", _,_,_)
    ]).


test_lex_line_numbers1 :-
   js_lex_string("[1,
   2,
   
   3]",
		 [ tok(punctuator, "[", _,1,_),
                   tok(number, "1", _,1,_),
                   tok(punctuator, ",", _,1,_),
                   tok(number, "2", _,2,_),
                   tok(punctuator, ",", _,2,_),
                   tok(number, "3", _,4,_),
                   tok(punctuator, "]", _,4,_)
                    ]).

test_lex_line_numbers2 :-
   js_lex_string("[1, /*
      comment
   */ 
   3]",
		 [ tok(punctuator, "[", _,1,_),
                   tok(number, "1", _,1,_),
                   tok(punctuator, ",", _,1,_),
                   tok(number, "3", _,4,_),
                   tok(punctuator, "]", _,4,_)
                    ]).



run_test(Test) :-
        functor(Test, Name, _),
        (call(Test) -> writef(" PASS") ;
         (\+ call(Test)) -> writef(" FAILED")),
        writef("\t\t"),
        writef(Name),
        writef("\n").


run_tests :-
        run_test(test_lex_var_decl),
        run_test(test_lex_id_with_keyword_prefix_decl ),
        run_test(test_lex_id1),
        run_test(test_lex_id2),
        run_test(test_lex_id3),
        run_test(test_lex_id4),
        run_test(test_lex_id5),
        run_test(test_lex_numeric_literal),
        run_test(test_lex_string_literal),
        run_test(test_lex_string_literal_single_quote),
        run_test(test_lex_var_decl_with_init),
        run_test(test_lex_var_decl_with_init_with_comments),
        run_test(test_lex_simple_obj_literal),
        run_test(test_lex_simple_block_comment),
        run_test(test_lex_operator_literal),
        run_test(test_lex_dotted_id),
        run_test(test_lex_floating_point_number_1),
        run_test(test_lex_line_numbers1),
        run_test(test_lex_line_numbers2),
        run_test(test_lex_two_char_op).

run_tests.

member([X|_],X).
member([_|Rest],X) :- member(Rest,X).
