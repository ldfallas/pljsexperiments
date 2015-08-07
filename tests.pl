:- use_module(jslexer).
:- use_module(jsparser).

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

test_lex_regex1 :-
   js_lex_string("/a/",
		 [ tok(regex, regex_literal("a",""), _,_,_)]).


test_lex_two_char_op :-
   js_lex_string("+=",
		 [ tok(punctuator, "+=", _,_,_)]).

test_lex_two_char_op2 :-
   js_lex_string("++",
		 [ tok(punctuator, "++", _,_,_)]).


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

test_parse_basic_literal :-
   parse_js_expression_string("'hola'", js_literal(string, "'hola'",_)).
test_parse_basic_id :-
   parse_js_expression_string("$foo", js_identifier("$foo",_)).
test_parse_parenthesized:-
   parse_js_expression_string("((a))", js_par(js_par(js_identifier("a",_),_),_)).

test_parse_newExpr1:-
   parse_js_expression_string("new Foo()",js_new( js_identifier("Foo",_), js_arguments([],_) , _) ).

test_parse_newExpr2:-
   parse_js_expression_string("new Foo",js_new( js_identifier("Foo",_),  _) ).

test_parse_newExpr3:-
   parse_js_expression_string("new Foo().x",
      js_dotted_access(
          js_new( js_identifier("Foo",_),js_arguments([],_),  _),
          js_identifier("x", _),_)
           ).

test_parse_array_literal1 :-
   parse_js_expression_string("[]",js_array_literal( [ ],  _) ).
test_parse_array_literal2 :-
   parse_js_expression_string("[1]",js_array_literal( [ js_literal(number, "1",_) ],  _) ).
test_parse_array_literal3 :-
   parse_js_expression_string("[a,]",js_array_literal( [ js_identifier( "a",_) ],  _) ).
test_parse_array_literal4 :-
   parse_js_expression_string("[a,b,c]",
         js_array_literal( [ js_identifier( "a",_),
                             js_identifier("b",_),
                             js_identifier("c",_) ],  _) ).
test_parse_array_literal5 :-
   parse_js_expression_string("[,]",
                     js_array_literal( [ js_implicit_undefined ],  _) ).
test_parse_array_literal6 :-
   parse_js_expression_string("[a,,b]",
                     js_array_literal( [ js_identifier( "a",_),
                                         js_implicit_undefined, 
                                         js_identifier( "b",_) ],  _) ).

test_parse_array_literal7 :-
   parse_js_expression_string("[a,[c,d],b]",
                     js_array_literal( [ js_identifier( "a",_),
                                         js_array_literal( 
                                              [ js_identifier( "c",_),
                                                js_identifier( "d",_) ],  _),
                                         js_identifier( "b",_) ],  _) ).

test_parse_object_literal1 :-
   parse_js_expression_string("{}",js_object( [ ],  _) ).

test_parse_object_literal2 :-
   parse_js_expression_string(
       "{ a : 1}",
       js_object( [ 
          js_property_assignment("a", js_literal(number, "1",_))
       ],  _) ).

test_parse_object_literal3 :-
   parse_js_expression_string(
       "{ 'b' : 1}",
       js_object( [ 
          js_property_assignment("'b'", js_literal(number, "1",_))
       ],  _) ).


test_parse_array_access1 :-
   parse_js_expression_string(
       "a[1]",
        js_array_access( 
            js_identifier("a",_),
            js_literal(number, "1",_),  _) ).

test_parse_array_access2 :-
   parse_js_expression_string(
       "a[1][2]",
        js_array_access( 
           js_array_access( 
              js_identifier("a",_),
              js_literal(number, "1",_),  _),
           js_literal(number, "2",_),
           _ )).

test_parse_array_access3 :-
   parse_js_expression_string(
       "a[b[c]]",
        js_array_access(
           js_identifier("a",_),
           js_array_access( 
               js_identifier("b",_),
               js_identifier( "c",_),  _),
           _ )).

test_parse_array_access4 :-
   parse_js_expression_string(
       "a.b[c]",
        js_array_access( 
           js_dotted_access( 
               js_identifier("a",_),
               js_identifier("b",_),  _),
           js_identifier("c",_),_)).


test_parse_dotted_access1 :-
   parse_js_expression_string(
       "a.b",
        js_dotted_access( 
            js_identifier("a",_),
            js_identifier("b",_),  _)).

test_parse_dotted_access2 :-
   parse_js_expression_string(
       "a.b.c",
        js_dotted_access( 
           js_dotted_access( 
               js_identifier("a",_),
               js_identifier("b",_),  _),
           js_identifier("c",_),_)).

test_parse_dotted_access3 :-
   parse_js_expression_string(
       "a[b].c",
        js_dotted_access( 
           js_array_access( 
               js_identifier("a",_),
               js_identifier("b",_),  _),
           js_identifier("c",_),_)).

test_parse_dotted_access4 :-
   parse_js_expression_string(
      "a[b].c.e[x].y.j",
      js_dotted_access(
         js_dotted_access(
            js_array_access( 
               js_dotted_access(
                  js_dotted_access( 
                     js_array_access( 
                        js_identifier("a",_),
                        js_identifier("b",_),  _),
                     js_identifier("c",_),_),
                  js_identifier("e",_),_),
               js_identifier("x",_),_),
            js_identifier("y",_),_),
         js_identifier("j",_),_)).



test_parse_function_expression1 :-
   parse_js_expression_string(
       "function(){}",
        js_function_expression( 
            [],
            [],  _)).

test_parse_call_expression1 :-
   parse_js_expression_string(
       "foo()",
        js_call_expression( 
            js_identifier("foo", _),
            js_arguments([],_),  _)).

test_parse_call_expression2 :-
   parse_js_expression_string(
       "foo().x",
       js_dotted_access(
        js_call_expression( 
            js_identifier("foo", _),
            js_arguments([], _),  _),
         js_identifier("x",_), _)).

test_parse_call_expression3 :-
   parse_js_expression_string(
       "foo()[x]",
       js_array_access(
        js_call_expression( 
            js_identifier("foo", _),
            js_arguments([],_),  _),
         js_identifier("x",_), _)).


test_parse_call_expression_with_args1 :-
   parse_js_expression_string(
       "foo(x)",
        js_call_expression( 
            js_identifier("foo", _),
            js_arguments([js_identifier("x", _)], _),  _)).

test_parse_call_expression_with_args2 :-
   parse_js_expression_string(
       "foo(x, y)",
        js_call_expression( 
            js_identifier("foo", _),
            js_arguments(
            [ js_identifier("x", _),
              js_identifier("y", _)
              ],_),  _)).

test_parse_call_expression_with_args3 :-
   parse_js_expression_string(
       "foo(x, goo())",
        js_call_expression( 
            js_identifier("foo", _),
            js_arguments(
              [js_identifier("x", _),
               js_call_expression( 
                  js_identifier("goo", _),
                  js_arguments([],_),  _)
              ],_),  _)).

test_parse_postfix_expression1 :-
   parse_js_expression_string(
       "x++",
        js_postfix_expression( 
            js_identifier("x", _)
            ,  _)).

test_parse_delete_expression :-
   parse_js_expression_string(
       "delete a.b",
        js_delete_expression( 
            js_dotted_access( 
               js_identifier("a",_),
               js_identifier("b",_), _)
            ,  _)).

test_parse_multiplication1:-
   parse_js_expression_string(
       "x * y",
        js_binary_operation( 
            "*",
            js_identifier("x", _),
            js_identifier("y", _)
            ,  _)).

test_parse_multiplication2:-
   parse_js_expression_string(
       "x * y * z",
        js_binary_operation( 
            "*",
            js_binary_operation( 
               "*",
               js_identifier("x", _),
               js_identifier("y", _),
               _),
            js_identifier("z", _)
            ,  _)).

test_parse_multiplication3:-
   parse_js_expression_string(
       "x % y / z",
        js_binary_operation( 
            "/",
            js_binary_operation( 
               "%",
               js_identifier("x", _),
               js_identifier("y", _),
               _),
            js_identifier("z", _)
            ,  _)).

test_parse_additive1 :-
   parse_js_expression_string(
       "x + y",
       js_binary_operation( 
          "+",
          js_identifier("x", _),
          js_identifier("y", _),
          _)).

test_parse_additive2 :-
   parse_js_expression_string(
       "x + y * z",
        js_binary_operation( 
            "+",
            js_identifier("x", _),
            js_binary_operation( 
               "*",
               js_identifier("y", _),
               js_identifier("z", _),
               _) ,
           _)).

test_parse_shift1 :-
   parse_js_expression_string(
       "x >> y",
       js_binary_operation( 
          ">>",
          js_identifier("x", _),
          js_identifier("y", _),
          _)).

test_parse_shift2 :-
   parse_js_expression_string(
       "x << y",
       js_binary_operation( 
          "<<",
          js_identifier("x", _),
          js_identifier("y", _),
          _)).

test_parse_shift3 :-
   parse_js_expression_string(
       "x >>> y",
       js_binary_operation( 
          ">>>",
          js_identifier("x", _),
          js_identifier("y", _),
          _)).


test_parse_division1 :-
   parse_js_expression_string(
       "x / y",
        js_binary_operation( 
            "/",
            js_identifier("x", _),
            js_identifier("y", _)
            ,  _)).


test_parse_division2:-
   parse_js_expression_string(
       "x / y / z",
        js_binary_operation( 
            "/",
            js_binary_operation( 
               "/",
               js_identifier("x", _),
               js_identifier("y", _),
               _),
            js_identifier("z", _)
            ,  _)).

test_parse_equality1 :-
   parse_js_expression_string(
       "x == y",
        js_binary_operation( 
            "==",
            js_identifier("x", _),
            js_identifier("y", _)
            ,  _)).

test_parse_equality2 :-
   parse_js_expression_string(
       "1 + 2 == 1 + 1 + 1",
        js_binary_operation( 
            "==", 
            js_binary_operation("+", 
               js_literal(number,"1", _), 
               js_literal(number,"2", _), 
               _) ,
            js_binary_operation("+", 
               _, 
               _, 
               _)
            /*js_identifier("x", _),
            js_identifier("y", _)*/
            ,  _)).


run_test(Test) :-
        functor(Test, Name, _),
        (call(Test) -> writef(" PASS") ;
         writef(" FAILED")),
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
        run_test(test_lex_two_char_op),
        run_test(test_lex_two_char_op2),
        run_test(test_lex_regex1),

        run_test(test_parse_basic_literal),
        run_test(test_parse_basic_literal ),
        run_test(test_parse_basic_id ),
        run_test(test_parse_parenthesized),
        run_test(test_parse_newExpr1),

        run_test(test_parse_newExpr2),
        run_test(test_parse_newExpr3),

        run_test(test_parse_array_literal1),
        run_test(test_parse_array_literal2),
        run_test(test_parse_array_literal3),
        run_test(test_parse_array_literal4),
        run_test(test_parse_array_literal5),
        run_test(test_parse_array_literal6),
        run_test(test_parse_array_literal7),
        run_test(test_parse_object_literal1),
        run_test(test_parse_object_literal2),
        run_test(test_parse_object_literal3),

        run_test(test_parse_array_access1),
        run_test(test_parse_array_access2),
        run_test(test_parse_array_access3),
        run_test(test_parse_array_access4),

        run_test(test_parse_dotted_access1),
        run_test(test_parse_dotted_access2),
        run_test(test_parse_dotted_access3),
        run_test(test_parse_dotted_access4 ),
        
        run_test(test_parse_call_expression1),
        run_test(test_parse_call_expression2),
        run_test(test_parse_call_expression3),
        run_test(test_parse_call_expression_with_args1),
        run_test(test_parse_call_expression_with_args2),
        run_test(test_parse_call_expression_with_args3),

        run_test(test_parse_postfix_expression1),
        run_test(test_parse_delete_expression),
        run_test(test_parse_multiplication1),
        run_test(test_parse_multiplication2),
        run_test(test_parse_multiplication3),
        run_test(test_parse_division1),
        run_test(test_parse_division2),
        run_test(test_parse_additive1),
        run_test(test_parse_additive2),
        run_test(test_parse_shift1),
        run_test(test_parse_shift2),
        run_test(test_parse_shift3),
        run_test(test_parse_equality1),
        run_test(test_parse_equality2).

        /*,

        run_test(test_parse_function_expression1)*/



/*member([X|_],X).
member([_|Rest],X) :- member(Rest,X).*/




test(Pairs) :-
   generate_test_operations([['*','/'],['+','-'],

['<<','>>','>>>'],['>','<','>=','<=']], ['x','y'], Pairs).

testStr(Str) :-
   test(Pairs),
   member(Pair, Pairs),
   test_nodes_to_string(Pair, Str).

test_parse_stat(Str) :-
   test(Pairs),
   member(Pair, Pairs),
   test_nodes_to_string(Pair, Str),
   writef(Str),
   test_nodes_to_jsast(Pair, JsAst),
   ( ( (\+ parse_js_expression_string(Str,JsAst)), writef(" FAIL"))
   /*  ; (writef(" FAIL") */
     )).

test_nodes_to_string(Var,Str) :-
   atom(Var),
   atom_to_chars(Var, Str).

test_nodes_to_string(op(Left, Operator , Right), Result) :-
  atom_to_chars(Operator, OperatorStr), 
  test_nodes_to_string(Left, LeftStr),
  append(LeftStr, OperatorStr, TempLeft),
  test_nodes_to_string(Right, RightStr),
  append(TempLeft,RightStr, Result).

test_nodes_to_jsast(Var, Ast) :-
   atom(Var),
   atom_to_chars(Var, Str),
   Ast = js_identifier(Str, _).

test_nodes_to_jsast(op(Left, Operator , Right), 
       js_binary_operation( 
            OperatorStr,
            LeftAst,
            RightAst,  _)) :-
   atom_codes(Operator, OperatorStr),
   test_nodes_to_jsast(Left, LeftAst),
   test_nodes_to_jsast(Right, RightAst).


generate_test_operations([],_,[]).
generate_test_operations([Ops1|OpsRest],Vars, Pairs) :-
  findall(Tuple, generating_test_operations(Ops1,Vars,Tuple),Pairs1), 
  append(Vars,Pairs1,NewVars),
  generate_test_operations(OpsRest, NewVars, Pairs2),
  append(Pairs1, Pairs2, Pairs).

generating_test_operations(Ops,[Var1|[Var2|Rest]],Pair1) :-
  member(Op, Ops),
  \+ Var1 = Var2, 
  (Pair1 = op(Var1,Op,Var2))
   ; generating_test_operations(Ops, Rest,Pair1). 
	

