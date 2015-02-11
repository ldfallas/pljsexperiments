:- module(jsparser, [parse_js_expression_string/2 ]).

:- use_module(jslexer).

parse_js_expression_string(CodeString, Ast) :- 
   js_lex_string(CodeString, Toks), 
   !, /* just one  chance to tokenize the string */
   phrase(js_expression(Ast), Toks).

js_expression(Ast) -->
    /* js_primary_expression(Ast). */
    js_new_expression(Ast).
   

js_primary_expression(Ast) -->
   js_literal_expression(Ast) 
   ; js_identifier_expression(Ast)
   ; js_par_expression(Ast) 
   ; js_array_literal(Ast)
   ; js_object_literal(Ast).

js_literal_expression(
   js_literal(string, 
              StringValue,
              lex_info(Line, PreTokenWhitespace))) -->
   [tok(string, StringValue, _, Line, PreTokenWhitespace)] .

js_literal_expression(
   js_literal(number, 
              NumericStringValue,
              lex_info(Line, PreTokenWhitespace))) -->
   [tok(number, NumericStringValue, _, Line, PreTokenWhitespace)] .


js_identifier_expression(
   js_identifier(IdName,
                 lex_info(Line, PreTokenWhitespace))) -->
   [tok(id, IdName, _, Line, PreTokenWhitespace)] .


js_par_expression(js_par(Expr, lex_info(Line, PreTokenWhitespace))) -->
   [tok(punctuator, "(", _, Line, PreTokenWhitespace)],
   js_expression(Expr),
   [tok(punctuator, ")", _, Line, PreTokenWhitespace)].

js_member_expression(Ast) -->
   js_primary_expression(Ast) 
   ; js_function_expression(Ast)
   ; js_array_access_expression(Ast) 
   ; js_dotted_access_expression(Ast) 
   ; js_new_object_expression_args(Ast)
    .


js_function_expression(js_function_expression(Params, Body, lex_info(Line, PreTokenWhitespace))) -->
   [tok(keyword, "function", _, Line, PreTokenWhitespace)],
   [tok(punctuator, "(", _, Line1, PreTokenWhitespace1)],
   js_formal_parameter_list(Params),
   [tok(punctuator, ")", _, Line2, PreTokenWhitespace2)],
   js_statement_block(Body).

js_formal_parameter_list([Param | Rest]) --> 
   js_identifier_expression(Param),
   [tok(punctuator, ",", _, _, _)],
   js_formal_parameter_list(Rest).
   
js_formal_parameter_list([Param]) --> 
   js_identifier_expression(Param).

js_statement_block([]) -->
  [tok(punctuator, "{", _, Line, PreTokenWhitespace)],
  [tok(punctuator, "}", _, Line2, PreTokenWhitespace2)].

js_dotted_access_expression(js_dotted_access(Expr, Identifier,lex_info(Line,PreTokenWhitespace))) -->
  js_member_expression(Expr),
  [tok(punctuator, ".", _, Line, PreTokenWhitespace)],
  js_identifier_expression(Identifier).

js_array_access_expression(js_array_access(MemberExpr, IndexExpr, lex_info(Line,PreTokenWhitespace))) -->
  js_member_expression(MemberExpr),
  [tok(punctuator, "[", _, Line, PreTokenWhitespace)],
  js_expression(IndexExpr),
  [tok(punctuator, "]", _, Line2, PreTokenWhitespace2)].

js_new_expression(Ast) -->
   js_member_expression(Ast)
   ; js_new_object_expression(Ast).

js_new_object_expression(js_new(Expr, lex_info(Line, PreTokenWhitespace))) -->
   [tok(keyword, "new", _, Line, PreTokenWhitespace)],
   js_new_expression(Expr).

js_new_object_expression_args(js_new(Expr, Args , lex_info(Line, PreTokenWhitespace))) -->
   [tok(keyword, "new", _, Line, PreTokenWhitespace)],
   js_new_expression(Expr),
   js_arguments(Args).

js_arguments(js_arguments(Args, lex_info(Line1, PreTokenWhitespace1))) -->
   [tok(punctuator, "(", _, Line1, PreTokenWhitespace1)],
   js_argument_list(Args),
   [tok(punctuator, ")", _, Line2, PreTokenWhitespace2)].

js_argument_list([Ast|Rest]) -->
   js_expression(Ast),
   [tok(punctuator, ",", _, Line, PreTokenWhitespace)],
   js_argument_list(Rest).
js_argument_list([Ast]) -->
   js_expression(Ast).
js_argument_list([]) --> [].

js_array_literal(js_array_literal(Exprs, lex_info(Line, PreTokenWhitespace))) -->
   [tok(punctuator, "[", _, Line, PreTokenWhitespace)],
   (([], {Exprs = []})
    ; (js_elision(Exprs))
    ; (js_element_list(Exprs))), 
   [tok(punctuator, "]", _, Line2, PreTokenWhitespace2)].

js_elision([js_implicit_undefined|Rest]) -->
   [tok(punctuator, ",", _, _, _)],
   js_elision(Rest).
js_elision([js_implicit_undefined]) -->
   [tok(punctuator, ",", _, _, _)].

js_element_list(List) --> 
   js_elision(Elision),
   js_assignment_expression(Expr),
   ((
      [tok(punctuator, ",", _, _, _)],
      js_element_list(Rest))
      ;([] , { Rest = []})),
      { append(Elision, [Expr| Rest], List) }.

js_element_list([Expr| Rest]) --> 
   js_assignment_expression(Expr),
   ((
      [tok(punctuator, ",", _, _, _)],
      js_element_list(Rest))
      ;([] , { Rest = []})).

js_element_list([Expr]) --> 
   js_assignment_expression(Expr),
   ( [tok(punctuator, ",", _, _, _)]
     ; []).

js_assignment_expression(Expr) --> js_expression(Expr).

js_object_literal(js_object(ObjectProperties, lex_info(Line, PreTokenWhitespace))) -->
   [tok(punctuator, "{", _, Line, PreTokenWhitespace)],
   js_property_and_name_list(ObjectProperties),
   [tok(punctuator, "}", _, Line2, PreTokenWhitespace2 )].

js_property_and_name_list([]) --> [].
js_property_and_name_list(Elements) --> js_property_and_name_list_seq(Elements).

js_property_and_name_list_seq([Prop|Rest]) -->
   js_property_assignment(Prop),
   [tok(punctuator, ",", _, _, _)],
   js_property_and_name_list_continuation(Rest).

js_property_and_name_list_seq([Prop]) --> js_property_assignment(Prop).

js_property_assignment(js_property_assignment(Name, Expression)) -->
    js_property_name(Name),
    [tok(punctuator, ":", _, _, _)],
    js_assignment_expression(Expression) .

js_property_name(Name) -->
   [tok(id, Name, _, Line, PreTokenWhitespace)] .

js_property_name(Name) -->
   [tok(string, Name, _, Line, PreTokenWhitespace)] .
