:- use_module(jslexer).
:- use_module(jsparser).
/*:- use_module(tests).*/


test(Pairs) :-
   generate_test_operations([ ['*','/'],
                              ['+','-'],
                              ['<<','>>','>>>'],
                              ['>','<','>=','<='],
                              ['==', '!=', '===','!=='],
                              ['&'],
                              ['^'],
                              ['|'],
                              ['&&'],
                              ['||'],
                              ['=']/*,
                              ['*=', '/=','%=','+=','-=','<<=',
                               '>>=', '>>>=', '&=', '^=', '!=']*/
                            ], 
                            ['x','y'], Pairs).

testStr(Str) :-
   test(Pairs),
   member(Pair, Pairs),
   test_nodes_to_string(Pair, Str).

test_parse_stat(Str) :-
   test(Pairs),
   member(Pair, Pairs),
   test_nodes_to_string(Pair, Str),
   test_nodes_to_jsast(Pair, JsAst),
   try_alternative(Str, JsAst).
   /*((parse_js_expression_string(Str,JsAst) , !) ; writef("FAIL")).*/

try_alternative(Str,JsAst) :-
   (\+ parse_js_expression_string(Str,JsAst))
      /*
      ,
   format("FAIL ~s~n",[Str])*/.
   /*
   ((parse_js_expression_string(Str,JsAst) , !) 
     ; format("FAIL ~s~n",[Str])).
     */

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
	
