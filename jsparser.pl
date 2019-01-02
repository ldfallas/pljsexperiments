:- module(jsparser, [parse_js_expression_string/2,parse_js_stat_string/2 ]).

:- use_module(jslexer).

parse_js_file(FileName, Ast) :-
   read_file_to_codes(FileName, Codes,[]),
   js_lex_string(Codes, Toks), !,
   phrase(js_statement_sequence(Ast), Toks).

lex_js_file(FileName, Toks) :-
   read_file_to_codes(FileName, Codes,[]),
   js_lex_string(Codes, Toks).

parse_js_stat_string(CodeString, Ast) :- 
   string_to_list(CodeString, CodeStringLst),
   js_lex_string(CodeStringLst, Toks), 
   !, /* just one  chance to tokenize the string */
   phrase(js_statement(Ast), Toks).

parse_js_expression_string(CodeString, Ast) :- 
   string_to_list(CodeString, CodeStringLst),
   js_lex_string(CodeStringLst, Toks), 
   !, /* just one  chance to tokenize the string */
   phrase(js_expression(Ast), Toks).

trace_parse_js_expression_string(CodeString, Ast) :- 
   notrace,
   js_lex_string(CodeString, Toks), 
   !, /* just one  chance to tokenize the string */
   trace, 
   phrase(js_expression(Ast), Toks),
   notrace.


js_expression(Ast) -->
    /* js_equality_expression(Ast).*/
%    js_conditional_expression(Ast).
    js_assignment_expression(Ast).
     /*js_binary_or_expression(Ast).*/

js_unary_expression(Ast) -->
   ([tok(keyword, `delete`, _, Line, PreTokenWhitespace)],
    js_unary_expression(InnerAst),
    { Ast = js_delete_expression(InnerAst, lex_info(Line, PreTokenWhitespace))  }
   ) ;
    js_postfix_expression(Ast).

js_multiplicative_expression(Ast) -->
   js_unary_expression(UnaryExpr),
   ( js_multiplicative_sequence(UnaryExpr, Ast)
    ; { Ast = UnaryExpr }).


js_multiplicative_operator(Op, Line, PreTokenWhitespace ) -->
  [tok(punctuator, Op, _, Line, PreTokenWhitespace)],
  { member(Op, [`*`, `%`, `/`]) }. 
    
js_multiplicative_sequence(Left, Ast) -->
   js_multiplicative_operator(Op,Line,PreTokenWhitespace), 
   js_unary_expression(Right),
   { ResultAst = js_binary_operation(Op, Left, Right, lex_info(Line, PreTokenWhitespace)) },
   js_multiplicative_sequence(ResultAst, Ast).
   
js_multiplicative_sequence(Left, Ast) --> { Ast = Left }.

js_additive_expression(Ast) -->
   js_binary_operator_expression(js_multiplicative_expression, [`+`,`-`],Ast).

js_shift_expression(Ast) -->
   js_binary_operator_expression(js_additive_expression, [`<<`, `>>`, `>>>`],Ast).

js_relational_expression(Ast) -->
   js_binary_operator_expression(js_shift_expression, 
         [`<`, `>`, `<=`,`>=`, `instanceof`, `in`],Ast).

js_equality_expression(Ast) -->
   js_binary_operator_expression(js_relational_expression, 
         [`==`, `!=`, `===`,`!==`],Ast).

js_binary_and_expression(Ast) -->
   js_binary_operator_expression(js_equality_expression, 
         [`&`],Ast).

js_binary_xor_expression(Ast) -->
   js_binary_operator_expression(js_binary_and_expression, 
         [`^`],Ast).

js_binary_or_expression(Ast) -->
   js_binary_operator_expression(js_binary_xor_expression, 
         [`|`],Ast).

js_and_expression(Ast) -->
   js_binary_operator_expression(js_binary_or_expression, 
         [`&&`],Ast).

js_or_expression(Ast) -->
   js_binary_operator_expression(js_and_expression, 
         [`||`],Ast).

js_conditional_expression(Ast) --> 
    js_or_expression(Expr),
    (([tok(punctuator, `?`, _, Line, PreTokenWhitespace)],
      js_assignment_expression(ThenExpr),
       [tok(punctuator, `:`, _, Line2, PreTokenWhitespace2)],
       js_assignment_expression(ElseExpr),
       { Ast = js_conditional(Expr,ThenExpr, ElseExpr, PreTokenWhitespace)})
    ; { Ast = Expr}).

js_assignment_expression(Ast) -->
    (js_left_hand_side_expression(Left),
     [tok(punctuator, Op, _, Line, PreTokenWhitespace)],
     { member(Op, [`=`, `*=`, `/=`, `%=`,
                   `+=`, `-=`, `<<=`, `>>=`,
                   `>>>=`, `&=`, `^=`, `|=`]) },!,
     js_assignment_expression(Right),
     { Ast = js_assign(Left, Right, lex_info(Line, PreTokenWhitespace)) })
    ;  js_conditional_expression(Ast).
    
%   js_binary_operator_expression(js_conditional_expression,
%                                 [`=`, `*=`, `/=`, `%=`,
%                                  `+=`, `-=`, `<<=`, `>>=`
%                                 `>>>=`, `&=`, `^=`, `|=`],Ast).


js_binary_operator_expression(ExpressionPredicate, Operators, Ast) -->
   call_operator_dcg(ExpressionPredicate, Expr),
   ( js_binary_operator_sequence(ExpressionPredicate, Operators, Expr, Ast), !
    ;  { Ast = Expr } ).

js_binary_operator(Choices, Op, Line, PreTokenWhitespace ) -->
  [tok(TokenKind, Op, _, Line, PreTokenWhitespace)],
  { member(Op, Choices), 
    ( (TokenKind = punctuator,!) ; (TokenKind = keyword,!) ) }. 
    
js_binary_operator_sequence(ExpressionPredicate,Operators, Left, Ast) -->
   js_binary_operator(Operators, Op,Line,PreTokenWhitespace), 
   call_operator_dcg(ExpressionPredicate, Right), !,
   { ResultAst = js_binary_operation(Op, Left, Right, lex_info(Line, PreTokenWhitespace)) },
   js_binary_operator_sequence(ExpressionPredicate,Operators, ResultAst, Ast).

js_binary_operator_sequence(_,_, Left, Ast) --> { Ast = Left }.

call_operator_dcg(OperatorDcgBody, Ast, State0, StateN) :- 
   call(OperatorDcgBody, Ast, State0, StateN).


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

js_literal_expression(
   js_literal(boolean, 
              Value,
              lex_info(Line, PreTokenWhitespace))) -->
    (([tok(keyword, `false`, _, Line, PreTokenWhitespace)], { Value = `false` })
     ; ([tok(keyword, `true`, _, Line, PreTokenWhitespace)],  { Value = `true` })).


js_literal_expression(
   js_literal(regex, 
              Regex,
              lex_info(Line, PreTokenWhitespace))) -->
   [tok(regex, Regex, _, Line, PreTokenWhitespace)] .


js_identifier_expression(
   js_identifier(IdName,
                 lex_info(Line, PreTokenWhitespace))) -->
   [tok(id, IdName, _, Line, PreTokenWhitespace)] .


js_par_expression(js_par(Expr, lex_info(Line, PreTokenWhitespace1))) -->
   [tok(punctuator, `(`, _, Line, PreTokenWhitespace1)],
   js_expression(Expr),
   [tok(punctuator, `)`, _, Line, PreTokenWhitespace2)].

js_member_expression(Ast) -->
    js_simple_member_expression(LeftExprAst),
    js_member_access_expression(LeftExprAst,Ast). 

js_member_access_expression(LeftExprAst, Ast) -->
   (js_array_access_expression(LeftExprAst, Ast), !) ;
    (js_dotted_access_expression(LeftExprAst, Ast), !) ;
    {Ast = LeftExprAst}.
   

js_simple_member_expression(Ast) -->
   js_primary_expression(Ast) 
   ; js_function_expression(Ast)
  /* ; js_array_access_expression(Ast) 
   ; js_dotted_access_expression(Ast) */
   ; js_new_object_expression_args(Ast)
    .

js_function_declaration(js_function_decl(Name, Params, Body, lex_info(Line, PreTokenWhitespace))) -->
    [tok(keyword, `function`, _, Line, PreTokenWhitespace)],
    (
        ([tok(id, Name, _, _, _)],
         [tok(punctuator, `(`, _, Line1, PreTokenWhitespace1)], 
         js_formal_parameter_list(Params), 
         [tok(punctuator, `)`, _, Line2, PreTokenWhitespace2)],
         js_statement_block(Body), !)
        ; throw(malformed_function_declaration(line(Line)))).


js_function_expression(js_function_expression(Params, Body, lex_info(Line, PreTokenWhitespace))) -->
    [tok(keyword, `function`, _, Line, PreTokenWhitespace)],
    (
        ([tok(punctuator, `(`, _, Line1, PreTokenWhitespace1)], 
         js_formal_parameter_list(Params), 
         [tok(punctuator, `)`, _, Line2, PreTokenWhitespace2)],
         js_statement_block(Body), !)
        ; throw(malformed_function_expression(line(Line)))).

js_formal_parameter_list([Param | Rest]) --> 
   js_identifier_expression(Param),
   [tok(punctuator, `,`, _, _, _)], !,
   js_formal_parameter_list(Rest).
   
js_formal_parameter_list([Param]) --> 
    js_identifier_expression(Param), !.
js_formal_parameter_list(Result) --> {Result = []}.


js_statement_block(js_block(Stats, lex_info(Line,PreTokenWhitespace ))) -->
    [tok(punctuator, `{`, _, Line, PreTokenWhitespace)],
    js_statement_sequence(Stats),  
    [tok(punctuator, `}`, _, Line2, PreTokenWhitespace2)].

js_statement_sequence([First|Rest]) -->
    js_statement(First),
    !,
    js_statement_sequence(Rest).
js_statement_sequence(R) --> {R = []}.

js_dotted_access_expression(Expr,  FinalResult) -->
  /*js_member_expression(Expr),*/
  [tok(punctuator, `.`, _, Line, PreTokenWhitespace)],
  js_identifier_expression(Identifier),
  {
     Result = js_dotted_access(Expr, Identifier,lex_info(Line,PreTokenWhitespace))
  },js_member_access_expression(Result, FinalResult) .

js_array_access_expression(MemberExpr, FinalResult /*js_array_access(MemberExpr, IndexExpr, lex_info(Line,PreTokenWhitespace))*/) -->
  /*js_member_expression(MemberExpr),*/
  [tok(punctuator, `[`, _, Line, PreTokenWhitespace)],
  js_expression(IndexExpr),
  [tok(punctuator, `]`, _, Line2, PreTokenWhitespace2)],
  { Result = js_array_access(MemberExpr, IndexExpr, lex_info(Line,PreTokenWhitespace)) },
  (   js_member_access_expression(Result, FinalResult); { FinalResult = Result } ).

js_new_expression(Ast) -->
   js_member_expression(Ast)
   ; js_new_object_expression(Ast).

js_new_object_expression(js_new(Expr, lex_info(Line, PreTokenWhitespace))) -->
   [tok(keyword, `new`, _, Line, PreTokenWhitespace)],
   js_new_expression(Expr).

js_new_object_expression_args(js_new(Expr, Args , lex_info(Line, PreTokenWhitespace))) -->
   [tok(keyword, `new`, _, Line, PreTokenWhitespace)],
   js_new_expression(Expr),
   js_arguments(Args).

js_arguments(js_arguments(Args, lex_info(Line1, PreTokenWhitespace1))) -->
   [tok(punctuator, `(`, _, Line1, PreTokenWhitespace1)],
   js_argument_list(Args),
   [tok(punctuator, `)`, _, Line2, PreTokenWhitespace2)].

js_argument_list([Ast|Rest]) -->
   js_expression(Ast),
   [tok(punctuator, `,`, _, Line, PreTokenWhitespace)],
   js_argument_list(Rest).
js_argument_list([Ast]) -->
   js_expression(Ast).
js_argument_list([]) --> [].

js_call_expression(FinalResult) -->
   js_member_expression(Function),
   js_arguments(Arguments),
   { Result = js_call_expression(Function, Arguments, null) },
   js_member_access_expression(Result, FinalResult).

js_array_literal(js_array_literal(Exprs, lex_info(Line, PreTokenWhitespace))) -->
   [tok(punctuator, `[`, _, Line, PreTokenWhitespace)],
   (([], {Exprs = []})
    ; (js_elision(Exprs))
    ; (js_element_list(Exprs))), 
   [tok(punctuator, `]`, _, Line2, PreTokenWhitespace2)].

js_elision([js_implicit_undefined|Rest]) -->
   [tok(punctuator, `,`, _, _, _)],
   js_elision(Rest).
js_elision([js_implicit_undefined]) -->
   [tok(punctuator, `,`, _, _, _)].

js_element_list(List) --> 
   js_elision(Elision),
   js_assignment_expression(Expr),
   ((
      [tok(punctuator, `,`, _, _, _)],
      js_element_list(Rest))
      ;([] , { Rest = []})),
      { append(Elision, [Expr| Rest], List) }.

js_element_list([Expr| Rest]) --> 
   js_assignment_expression(Expr),
   ((
      [tok(punctuator, `,`, _, _, _)],
      js_element_list(Rest))
      ;([] , { Rest = []})).

js_element_list([Expr]) --> 
   js_assignment_expression(Expr),
   ( [tok(punctuator, `,`, _, _, _)]
     ; []).

%js_assignment_expression(Expr) --> js_expression(Expr).

js_object_literal(js_object(ObjectProperties, lex_info(Line, PreTokenWhitespace))) -->
   [tok(punctuator, `{`, _, Line, PreTokenWhitespace)],
   js_property_and_name_list(ObjectProperties),
   [tok(punctuator, `}`, _, Line2, PreTokenWhitespace2 )].

js_property_and_name_list([]) --> [].
js_property_and_name_list(Elements) --> js_property_and_name_list_seq(Elements).

js_property_and_name_list_seq([Prop|Rest]) -->
   js_property_assignment(Prop),
   [tok(punctuator, `,`, _, _, _)],
   js_property_and_name_list_continuation(Rest).

js_property_and_name_list_seq([Prop]) --> js_property_assignment(Prop).

js_property_assignment(js_property_assignment(Name, Expression)) -->
    js_property_name(Name),
    [tok(punctuator, `:`, _, _, _)],
    js_assignment_expression(Expression) .

js_property_name(Name) -->
   [tok(id, Name, _, Line, PreTokenWhitespace)] .

js_property_name(Name) -->
   [tok(string, Name, _, Line, PreTokenWhitespace)] .

js_left_hand_side_expression(Expr) --> js_call_expression(Expr).
js_left_hand_side_expression(Expr) --> js_new_expression(Expr).


js_postfix_expression(FinalExpr) -->
   js_left_hand_side_expression(Expr),
   js_postfix_increment_expression(Expr, FinalExpr).

js_postfix_increment_expression(Expr, FinalExpr) -->
  (
   [tok(punctuator, `++`, _, _, Whitespace_elements)],
   { (\+ member(ws(_, true), Whitespace_elements), !),
     FinalExpr = js_postfix_expression(Expr, lex_info(1))  }) ;
   { FinalExpr = Expr }.


js_statement(Ast) -->
    js_return_statement(Ast)
    ; js_if_statement(Ast)
    ; js_var_statement(Ast)
    ; js_function_declaration(Ast)
    ; js_expr_statement(Ast)
    ; js_statement_block(Ast).

js_expr_statement(js_expr_stat(Call)) -->
    js_call_expression(Call),
    [tok(punctuator, `;`, _, _, _)].

js_expr_statement(js_expr_stat(Asg)) -->
    js_assignment_expression(Asg),
    [tok(punctuator, `;`, _, _, _)].


js_return_statement(js_return(lex_info(Line1,PreTokenWhitespace1 ))) -->
   [tok(keyword, `return`, _, Line1, PreTokenWhitespace1)],
   [tok(punctuator, `;`, _, Line2, PreTokenWhitespace2)].

js_return_statement(js_return(Expr, lex_info(Line1,PreTokenWhitespace1 ))) -->
   [tok(keyword, `return`, _, Line1, PreTokenWhitespace1)],
   js_expression(Expr),
   [tok(punctuator, `;`, _, Line2, PreTokenWhitespace2)].

js_var_statement(js_var_stat(Vars, lex_info(Line1, PreTokenWhitespace))) -->
    [tok(keyword,`var`, _, Line1, PreTokenWhitespace)],
    (
      (js_var_decl_sequence(Vars),
       [tok(punctuator, `;`, _, _, _)], !)
      ; throw(unexpected_element(line(Line1)))).

js_var_decl(js_var_decl(IdName, Init, lex_info(Line, PreTokenWhitespace))) -->
    [tok(id, IdName, _, Line, PreTokenWhitespace)],
    [tok(punctuator, `=`, _, _, _)], !,
    js_expression(Init).

js_var_decl(js_var_decl(IdName, lex_info(Line, PreTokenWhitespace))) -->
    [tok(id, IdName, _, Line, PreTokenWhitespace)].


js_var_decl_sequence([First|Rest]) -->
    js_var_decl(First),
    [tok(punctuator, `,`, _, _, _)], !,
    js_var_decl_sequence(Rest).

js_var_decl_sequence([First]) -->
    js_var_decl(First).


js_if_statement(Ast) -->
   [tok(keyword, `if`, _, Line1, PreTokenWhitespace1)],
   [tok(punctuator, `(`, _, Line2, PreTokenWhitespace2)],

   js_expression(Condition),
   [tok(punctuator, `)`, _, Line3, PreTokenWhitespace3)],   
   js_statement(ThenStat),
   ((
      [tok(keyword, `else`, _, Line4, PreTokenWhitespace4)],
       js_statement(ElseStat),
       { Ast = js_if(Condition, ThenStat, ElseStat, lex_info(Line1, PreTokenWhitespace1))})
   ; ( { Ast = js_if(Condition, ThenStat, lex_info(Line1, PreTokenWhitespace1  )) }) ).


test_pp(js_binary_operation(Op, Left, Right, _), Result) :- 
   test_pp(Left, LeftStr),
   append(LeftStr, Op,Tmp1),
   test_pp(Right, RightStr),
   append(Tmp1, RightStr,Result).

test_pp(js_identifier(Result, _), Result).



