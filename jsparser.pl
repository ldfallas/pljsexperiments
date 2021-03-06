:- module(jsparser, [parse_js_expression_string/2,parse_js_stat_string/2 , parse_js_file/2]).

:- use_module(jslexer).

parse_js_file(FileName, Ast) :-
   read_file_to_codes(FileName, Codes,[]),
   js_lex_string(Codes, Toks), !,
   phrase(js_statement_sequence(Ast), Toks).

lex_js_file(FileName, Toks) :-
   read_file_to_codes(FileName, Codes,[]),
   js_lex_string(Codes, Toks).

lex_js_string(Codestring, Toks) :-
   string_to_list(Codestring, Codes),
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
    js_assignment_expression(TmpAst),
    (( [tok(punctuator, comma, _, _, _)],!,
      js_expression(Rest),
      { Ast = js_comma(TmpAst, Rest) } );
    {Ast = TmpAst})
    .
     /*js_binary_or_expression(Ast).*/

js_unary_expression(Ast) -->
   ([tok(keyword, delete_kw, _, Line, PreTokenWhitespace)],
    js_unary_expression(InnerAst),
    { Ast = js_delete_expression(InnerAst, lex_info(Line, PreTokenWhitespace))  }
   ) ;
   ([tok(keyword, typeof_kw, _, Line, PreTokenWhitespace)],
    js_unary_expression(InnerAst),
    { Ast = js_typeof(InnerAst, lex_info(Line, PreTokenWhitespace))  }
   ) ;   
   ([tok(punctuator, not_op, _, Line, PreTokenWhitespace)],
    js_unary_expression(InnerAst),
    { Ast = js_not(InnerAst, lex_info(Line, PreTokenWhitespace))  }
   ) ;
   ([tok(punctuator, plusp_op, _, Line, PreTokenWhitespace)],
    js_unary_expression(InnerAst),
    { Ast = js_prefix_expr(InnerAst,plusp_op, lex_info(Line, PreTokenWhitespace))  }
   ) ;
   ([tok(punctuator, minusm_op, _, Line, PreTokenWhitespace)],
    js_unary_expression(InnerAst),
    { Ast = js_prefix_expr(InnerAst,plusp_op, lex_info(Line, PreTokenWhitespace))  }
   ) ;   
   ([tok(punctuator, bit_not_op, _, Line, PreTokenWhitespace)],
    js_unary_expression(InnerAst),
    { Ast = js_bit_not(InnerAst, lex_info(Line, PreTokenWhitespace))  }
   ) ;   

   ([tok(punctuator, minus_op, _, Line, PreTokenWhitespace)],
    js_unary_expression(InnerAst),
    { Ast = js_negate(InnerAst, lex_info(Line, PreTokenWhitespace))  }
   ) ;
   ([tok(punctuator, plus_op, _, Line, PreTokenWhitespace)],
    js_unary_expression(InnerAst),
    { Ast = js_positive(InnerAst, lex_info(Line, PreTokenWhitespace))  }
   ) ;
    js_postfix_expression(Ast).

js_multiplicative_expression(Ast) -->
   js_unary_expression(UnaryExpr),
   ( js_multiplicative_sequence(UnaryExpr, Ast)
    ; { Ast = UnaryExpr }).


multiplicative_op(times_op).
multiplicative_op(mod_op).
multiplicative_op(div_op).

js_multiplicative_operator(Op, Line, PreTokenWhitespace ) -->
  [tok(punctuator, Op, _, Line, PreTokenWhitespace)],
  { multiplicative_op(Op) }. 
    
js_multiplicative_sequence(Left, Ast) -->
   js_multiplicative_operator(Op,Line,PreTokenWhitespace), 
   js_unary_expression(Right),
   { ResultAst = js_binary_operation(Op, Left, Right, lex_info(Line, PreTokenWhitespace)) },
   js_multiplicative_sequence(ResultAst, Ast).
   
js_multiplicative_sequence(Left, Ast) --> { Ast = Left }.

additive_operator(plus_op).
additive_operator(minus_op).

js_additive_expression(Ast) -->
    js_binary_operator_expression(
        js_multiplicative_expression,
        additive_operator,
        Ast).

shift_operator(shift_left_op).
shift_operator(shift_right_op).
shift_operator(shift_right_1_op).

js_shift_expression(Ast) -->
   js_binary_operator_expression(js_additive_expression, shift_operator, Ast).

relational_expression(lt_op).
relational_expression(gt_op).
relational_expression(lt_eq_op).
relational_expression(gt_eq_op).
relational_expression(instanceof_kw).
relational_expression(in_kw).

js_relational_expression(Ast) -->
   js_binary_operator_expression(js_shift_expression, 
         relational_expression, Ast).

equality_expression(equals_op).
equality_expression(not_equals_op).
equality_expression(exact_equal_op).
equality_expression(not_exact_equal_op).

js_equality_expression(Ast) -->
   js_binary_operator_expression(js_relational_expression, 
         equality_expression, Ast).

js_binary_and_expression(Ast) -->
   js_binary_operator_expression(js_equality_expression, 
         binary_and_op, Ast).

binary_xor_op(bit_xor_op).

js_binary_xor_expression(Ast) -->
   js_binary_operator_expression(js_binary_and_expression, 
         binary_xor_op, Ast).



js_binary_or_expression(Ast) -->
   js_binary_operator_expression(js_binary_xor_expression, 
         binary_or_op, Ast).

binary_and_op(bit_and_op).
binary_and_op(and_op).

js_and_expression(Ast) -->
   js_binary_operator_expression(js_binary_or_expression, 
         binary_and_op, Ast).

binary_or_op(bit_or_op).
binary_or_op(or_op).

js_or_expression(Ast) -->
   js_binary_operator_expression(js_and_expression, 
         binary_or_op, Ast).

js_conditional_expression(Ast) --> 
    js_or_expression(Expr),
    (([tok(punctuator, question, _, Line, PreTokenWhitespace)],!,
      js_assignment_expression(ThenExpr),
       [tok(punctuator, colon, _, _, _)],!,
       js_assignment_expression(ElseExpr),
       { Ast = js_conditional(Expr, ThenExpr, ElseExpr,
                              lex_info(Line, PreTokenWhitespace)) })
    ; { Ast = Expr}).

assignment_op(assign).
assignment_op(times_eq_op).
assignment_op(div_eq_op).
assignment_op(mod_eq_op).
assignment_op(plus_eq_op).
assignment_op(minus_eq_op).
assignment_op(shift_left_eql_op).
assignment_op(shift_right_eql_op).
assignment_op(bin_and_eq_op).
assignment_op(bit_xor_eq_op).
assignment_op(bit_and_eq_op).
assignment_op(bit_or_eq_op).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% MAIN PERFORMANCE IMPROVEMENT OPORTUNITY/
js_assignment_expression(Ast) -->
    (js_left_hand_side_expression(Left),
     [tok(punctuator, Op, _, Line, PreTokenWhitespace)],
     { assignment_op(Op) },!,
     js_assignment_expression(Right),
     { Ast = js_assign(Left, Right, lex_info(Line, PreTokenWhitespace)) })
    ;  js_conditional_expression(Ast).

    


js_binary_operator_expression(ExpressionPredicate, Operators, Ast) -->
   call_operator_dcg(ExpressionPredicate, Expr),
   ( js_binary_operator_sequence(ExpressionPredicate, Operators, Expr, Ast), !
    ;  { Ast = Expr } ).

js_binary_operator(ChoicePred, Op, Line, PreTokenWhitespace ) -->
  [tok(TokenKind, Op, _, Line, PreTokenWhitespace)],
  { call(ChoicePred, Op), 
    ( (TokenKind = punctuator,!) ; (TokenKind = keyword,!) ) }. 
    
js_binary_operator_sequence(ExpressionPredicate, Operators, Left, Ast) -->
   js_binary_operator(Operators, Op, Line,PreTokenWhitespace), !,
   call_operator_dcg(ExpressionPredicate, Right), !,
   { ResultAst = js_binary_operation(Op, Left, Right, lex_info(Line, PreTokenWhitespace)) },
   js_binary_operator_sequence(ExpressionPredicate, Operators, ResultAst, Ast).

js_binary_operator_sequence(_, _, Left, Ast) --> { Ast = Left }.

call_operator_dcg(OperatorDcgBody, Ast, State0, StateN) :- 
   call(OperatorDcgBody, Ast, State0, StateN).


js_primary_expression(Ast) -->
    js_identifier_expression(Ast)
   ; js_literal_expression(Ast) 
   ; js_par_expression(Ast) 
   ; js_array_literal(Ast)
   ; js_object_literal(Ast).

js_literal_expression(
   js_literal(Kind, 
              Value,
              lex_info(Line, PreTokenWhitespace))) -->
    [tok(TokKind, TokValue, _, Line, PreTokenWhitespace)],
    {(TokKind = string,!, Kind = string, Value = TokValue) ;
     (TokKind = number,!, Kind = number, Value = TokValue) ;
     (TokKind = number,!, Kind = number, Value = TokValue) ;
     (TokKind = keyword, TokValue = false_kw,!, Kind = boolean, Value = `false`) ;
     (TokKind = keyword, TokValue = true_kw,!, Kind = boolean, Value = `true`) ;
     (TokKind = regex,!, Kind = regex, Kind = regex, Value = TokValue)
    }.


js_identifier_name(
   js_id_name(IdName,
                 lex_info(Line, PreTokenWhitespace))) -->
   [tok(id, IdName, _, Line, PreTokenWhitespace)] .
js_identifier_name(
    js_id_name(`catch`,
               lex_info(Line, PreTokenWhitespace))) -->
   [tok(keyword, catch_kw, _, Line, PreTokenWhitespace)] .

js_identifier_expression(
   js_identifier(IdName,
                 lex_info(Line, PreTokenWhitespace))) -->
   [tok(id, IdName, _, Line, PreTokenWhitespace)] .


js_par_expression(js_par(Expr, lex_info(Line, PreTokenWhitespace1))) -->
   [tok(punctuator, left_par, _, Line, PreTokenWhitespace1)],
   js_expression(Expr),
   [tok(punctuator, right_par, _, _,_)].

js_member_expression(Ast) -->
    js_simple_member_expression(LeftExprAst),
    js_member_access_expression(LeftExprAst,Ast). 

call_member_dcg(Pred, Expr, Ast, State0, StateN) :- 
   call(Pred, Expr, Ast, State0, StateN).

js_member_access_expression(LeftExprAst, Ast) -->
   (js_array_access_expression(LeftExprAst, js_member_access_expression, Ast), !) ;
    (js_dotted_access_expression(LeftExprAst,  js_member_access_expression, Ast), !) ;
    {Ast = LeftExprAst}.

js_call_access_expression(Expr,  FinalResult) -->
  js_arguments(Args),
  {
     Result = js_call(Expr, Args, null)
  },js_call_member_access_expression(Result, FinalResult) .


js_call_member_access_expression(LeftExprAst, Ast) -->
   (js_array_access_expression(LeftExprAst,js_call_member_access_expression, Ast), !) ;
    (js_dotted_access_expression(LeftExprAst,js_call_member_access_expression, Ast), !) ;
    (js_call_access_expression(LeftExprAst, Ast), !) ;   
    {Ast = LeftExprAst}.
   

js_simple_member_expression(Ast) -->
   js_primary_expression(Ast) 
   ; js_function_expression(Ast)
  /* ; js_array_access_expression(Ast) 
   ; js_dotted_access_expression(Ast) */
   ; js_new_object_expression_args(Ast)
    .

js_function_declaration(js_function_decl(Name, Params, Body, lex_info(Line, PreTokenWhitespace))) -->
    [tok(keyword, function_kw, _, Line, PreTokenWhitespace)],
    (
        ([tok(id, Name, _, _, _)],
         [tok(punctuator, left_par, _, _, _)], 
         js_formal_parameter_list(Params), 
         [tok(punctuator, right_par, _, _, _)],
         js_statement_block(Body), !)
        ; throw(parser_exception(malformed_function_declaration, line(Line)))).


js_function_expression(js_function_expression(Name,Params, Body, lex_info(Line, PreTokenWhitespace))) -->
    [tok(keyword, function_kw, _, Line, PreTokenWhitespace)],
    (
        ((([tok(id, FName, _, _, _)] , { Name = name(FName) }) ;
           { Name = name() }),
         [tok(punctuator, left_par, _, _, _)], 
         js_formal_parameter_list(Params), 
         [tok(punctuator, right_par, _, _, _)],
         js_statement_block(Body), !)
        ; throw(parser_exception(malformed_function_expression, line(Line)))).

js_formal_parameter_list([Param | Rest]) --> 
   js_identifier_expression(Param),
   [tok(punctuator, comma, _, _, _)], !,
   js_formal_parameter_list(Rest).
   
js_formal_parameter_list([Param]) --> 
    js_identifier_expression(Param), !.
js_formal_parameter_list(Result) --> {Result = []}.

js_peek, [tok(A,B,C,Line,D)] --> [tok(A,B,C,Line,D)]
%                                 , { writeln("")       }
.

js_statement_block(js_block(Stats, lex_info(Line,PreTokenWhitespace ))) -->
    [tok(punctuator, left_bracket, _, Line, PreTokenWhitespace)],!, js_peek,
     js_statement_sequence(Stats),
     (
         [tok(punctuator, right_bracket, _, _, _)]
     ; ( [tok(_, _, _, CurrLine, _)],
         { throw(parser_exception(expectingValidStatement, line(CurrLine))) })).

js_statement_sequence([First|Rest]) -->
    js_statement(First),
    !,
    js_statement_sequence(Rest).
js_statement_sequence(R) --> {R = []}.

js_dotted_access_expression(Expr, ContPred, FinalResult) -->
  [tok(punctuator, dot, _, Line, PreTokenWhitespace)],
  js_identifier_name(Identifier),
  {
     Result = js_dotted_access(Expr, Identifier,lex_info(Line,PreTokenWhitespace))
  }, 
  call_member_dcg(ContPred, Result, FinalResult).

js_array_access_expression(MemberExpr, ContPred, FinalResult) -->
  [tok(punctuator, left_sbracket, _, Line, PreTokenWhitespace)],
  js_expression(IndexExpr),
  [tok(punctuator, right_sbracket, _, _, _)],
  { Result = js_array_access(MemberExpr, IndexExpr, lex_info(Line,PreTokenWhitespace)) },
    (   call_member_dcg(ContPred, Result, FinalResult); { FinalResult = Result } ).

js_new_expression(Ast) -->
   js_member_expression(Ast)
   ; js_new_object_expression(Ast).

js_new_object_expression(js_new(Expr, lex_info(Line, PreTokenWhitespace))) -->
   [tok(keyword, new_kw, _, Line, PreTokenWhitespace)],
   js_new_expression(Expr).

js_new_object_expression_args(js_new(Expr, Args , lex_info(Line, PreTokenWhitespace))) -->
   [tok(keyword, new_kw, _, Line, PreTokenWhitespace)],
   js_new_expression(Expr),
   (js_arguments(Args) ; {Args = no_args()}).

js_arguments(js_arguments(Args, lex_info(Line1, PreTokenWhitespace1,
                                         Line2, PreTokenWhitespace2))) -->
   [tok(punctuator, left_par, _, Line1, PreTokenWhitespace1)],
   js_argument_list(Args),
   [tok(punctuator, right_par, _, Line2, PreTokenWhitespace2)].

js_argument_list([Ast|Rest]) -->
   js_assignment_expression(Ast),
   (([tok(punctuator, comma, _, _, _)],!,
     js_argument_list(Rest)) ;
    { Rest = []}).
js_argument_list([]) --> [].

js_call_expression(FinalResult) -->
   js_member_expression(Function),
   ((js_arguments(Arguments),!,
    { Result = js_call(Function, Arguments, null) },
    js_call_member_access_expression(Result, FinalResult))
    ; {FinalResult = Function }).

js_array_literal(js_array_literal(Exprs, lex_info(Line, PreTokenWhitespace))) -->
   [tok(punctuator, left_sbracket, _, Line, PreTokenWhitespace)],
   (([], {Exprs = []})
    ; (js_elision(Exprs))
    ; (js_element_list(Exprs))), 
   [tok(punctuator, right_sbracket, _, _, _)].

js_elision([js_implicit_undefined|Rest]) -->
   [tok(punctuator, comma, _, _, _)],
   js_elision(Rest).
js_elision([js_implicit_undefined]) -->
   [tok(punctuator, comma, _, _, _)].

js_element_list(List) --> 
   js_elision(Elision),
   js_assignment_expression(Expr),
   ((
      [tok(punctuator, comma, _, _, _)],
      js_element_list(Rest))
      ;([] , { Rest = []})),
      { append(Elision, [Expr| Rest], List) }.

js_element_list([Expr| Rest]) --> 
   js_assignment_expression(Expr),
   ((
      [tok(punctuator, comma, _, _, _)],
      js_element_list(Rest))
      ;([] , { Rest = []})).

js_element_list([Expr]) --> 
   js_assignment_expression(Expr),
   ( [tok(punctuator, comma, _, _, _)]
     ; []).


js_object_literal(js_object(ObjectProperties, lex_info(Line, PreTokenWhitespace))) -->
    
    [tok(punctuator, left_bracket, _, Line, PreTokenWhitespace)],
    !,
    
   js_property_and_name_list_seq(ObjectProperties),
   ((
           [tok(punctuator, right_bracket, _, _, _ )],!)
    ; ([tok(_,T,_,L,_)],{ throw(parser_exception(malformed_object_literal(L,T),-1)) })).

js_property_and_name_list_seq([Prop|Rest]) -->
   js_property_assignment(Prop),
   (([tok(punctuator, comma, _, _, _)],
     js_property_and_name_list_seq(Rest),!)
    ;  {Rest = []}).

js_property_and_name_list_seq([]) --> [].

js_property_assignment(js_property_assignment(Name, Expression)) -->
    js_property_name(Name),
    [tok(punctuator, colon, _, Line, _)],!,
    (
        js_assignment_expression(Expression) ;
        { throw(parser_exception(malformed_assignment, Line)) }).

js_property_name(Name) -->
    [tok(Kind, Name, _, _, _)],
    {  Kind = id; Kind = string ; Kind = number }.


js_left_hand_side_expression(Expr) --> js_call_expression(Expr).
%js_left_hand_side_expression(Expr) --> js_new_expression(Expr).


js_postfix_expression(FinalExpr) -->
   js_left_hand_side_expression(Expr),
   js_postfix_increment_expression(Expr, FinalExpr).

js_postfix_increment_expression(Expr, FinalExpr) -->
  (
  [tok(punctuator, Op, _, _, Whitespace_elements)],
  {  (Op = plusp_op; Op = minusm_op),!,
     (\+ member(ws(_, true), Whitespace_elements), !),
     FinalExpr = js_postfix_expression(Expr,Op, lex_info(1))  } );
   { FinalExpr = Expr }.


js_statement(Ast) -->
    js_return_statement(Ast)
    ; js_if_statement(Ast)
    ; js_var_statement(Ast)
    ; js_function_declaration(Ast)
    ; js_while_statement(Ast)
    ; js_do_while_statement(Ast)
    ; js_for_statement(Ast)    
    ; js_statement_block(Ast)
    ; js_break_statement(Ast)
    ; js_switch_statement(Ast)
    ; js_try(Ast)
    ; js_throw_statement(Ast)
    ; js_empty_statement(Ast)
    ; js_expr_statement(Ast).


js_next_token_is_valid, [tok(Kind,Value,Pos,Line,Ws)] -->
    [tok(Kind,Value,Pos,Line,Ws)], {\+ ( Value =  function_kw ;
                                         Value =  left_bracket)}.

js_automatic_semicolon_insertion(_), [tok(Kind,Value,C,Line,D)] -->
    [tok(Kind, Value, C, Line, D)],
    { Kind  = punctuator, Value = right_bracket }, !.

js_automatic_semicolon_insertion(js_call( _, js_arguments(_, lex_info( _, _, LastLine, _)), _)), [tok(Kind, Value, Pos, NewLine, Ws)] -->
    [tok(Kind,Value,Pos,NewLine,Ws)], { NewLine > LastLine }.

js_expr_statement(js_expr_stat(Expr)) -->
    js_next_token_is_valid,
    js_expression(Expr),
    (([tok(punctuator, semicolon, _, _, _)], !)
     ; (js_automatic_semicolon_insertion(Expr))).


js_empty_statement(js_empty_stat(lex_info(Line, PreTokenWhitespace ))) -->
   [tok(punctuator, semicolon, _, Line, PreTokenWhitespace)].

js_break_statement(js_break(lex_info(Line1,PreTokenWhitespace1 ))) -->
   [tok(keyword, break_kw, _, Line1, PreTokenWhitespace1)],
   [tok(punctuator, semicolon, _, _, _)].


js_return_statement(js_return(lex_info(Line1,PreTokenWhitespace1 ))) -->
   [tok(keyword, return_kw, _, Line1, PreTokenWhitespace1)], 
   [tok(punctuator, semicolon, _, _, _)].

js_return_statement(js_return(Expr, lex_info(Line1,PreTokenWhitespace1 ))) -->
   [tok(keyword, return_kw, _, Line1, PreTokenWhitespace1)],
   js_expression(Expr),
   (([tok(punctuator, semicolon, _, _, _)], !)
     ; (js_automatic_semicolon_insertion(Expr))).


js_throw_statement(js_throw(Expr, lex_info(Line1,PreTokenWhitespace1 ))) -->
   [tok(keyword, throw_kw, _, Line1, PreTokenWhitespace1)],
   js_expression(Expr),
   [tok(punctuator, semicolon, _, _, _)].


js_var_statement(js_var_stat(Vars, lex_info(Line1, PreTokenWhitespace))) -->
    [tok(keyword,var_kw, _, Line1, PreTokenWhitespace)],
    (
        (js_var_decl_sequence(Vars),!,
       [tok(punctuator, semicolon, _, _, _)])
      ; throw(parser_exception(unexpected_element, line(Line1)))).

js_var_decl(js_var_decl(IdName, Init, lex_info(Line, PreTokenWhitespace))) -->
    [tok(id, IdName, _, Line, PreTokenWhitespace)],
    [tok(punctuator, assign, _, _, _)], !,
    js_assignment_expression(Init).

js_var_decl(js_var_decl(IdName, lex_info(Line, PreTokenWhitespace))) -->
    [tok(id, IdName, _, Line, PreTokenWhitespace)].


js_var_decl_sequence([First|Rest]) -->
    js_var_decl(First),
    ((
    [tok(punctuator, comma, _, _, _)], !,
    js_var_decl_sequence(Rest))
     ; {Rest = []}).
%js_var_decl_sequence([]) --> [].


js_if_statement(Ast) -->
   [tok(keyword, if_kw, _, Line1, PreTokenWhitespace1)],
   [tok(punctuator, left_par, _, _, _)],

   js_expression(Condition),
   [tok(punctuator, right_par, _, _, _)],   
   js_statement(ThenStat),
   ((
      [tok(keyword, else_kw, _, _, _)],
       js_statement(ElseStat),
       { Ast = js_if(Condition, ThenStat, ElseStat, lex_info(Line1, PreTokenWhitespace1))})
   ; ( { Ast = js_if(Condition, ThenStat, lex_info(Line1, PreTokenWhitespace1  )) }) ).

js_while_statement(Ast) -->
    [tok(keyword, while_kw, _, Line1, PreTokenWhitespace1)], !,
    ((
        [tok(punctuator, left_par, _, _, _)], 
        js_expression(Condition),
        [tok(punctuator, right_par, _, _, _)],   !,
        js_statement(Body),
        { Ast = js_while(Condition, Body, lex_info(Line1, PreTokenWhitespace1  )) } )
    ; throw(parser_excepton(malformedWhile, line(Line1)))).

js_do_while_statement(Ast) -->
    [tok(keyword, do_kw, _, Line1, PreTokenWhitespace1)], !,
    ((
        js_statement(Body),
        [tok(keyword, while_kw, _, _, _)],
        [tok(punctuator, left_par, _, _, _)], 
        js_expression(Condition),
        [tok(punctuator, right_par, _, _, _)],
        [tok(punctuator, semicolon, _, _, _)],!,
        { Ast = js_do_while( Body, Condition, lex_info(Line1, PreTokenWhitespace1  )) } )
    ; throw(parser_exception(malformedDoWhile, line(Line1)))).

js_case(Ast) -->
    [tok(keyword, case_kw, _, Line1, PreTokenWhitespace1)], !,
    ((
        js_expression(LabelValue),
        [tok(punctuator, colon, _, _, _)],   !,
        js_statement_sequence(Sequence),
        { Ast = js_case(LabelValue, Sequence, lex_info(Line1, PreTokenWhitespace1  )) } )
    ; throw(parser_exception(malformedCase, line(Line1)))).

js_case(Ast) -->
    [tok(keyword, default_kw, _, Line1, PreTokenWhitespace1)], !,
    ((
        [tok(punctuator, colon, _, _, _)],   !,
        js_statement_sequence(Sequence),
        { Ast = js_default(Sequence, lex_info(Line1, PreTokenWhitespace1  )) } )
    ; throw(parser_excepton(malformedCase, line(Line1)))).

js_case_sequence([First|Rest]) -->
    js_case(First),
    js_case_sequence(Rest).

js_case_sequence(R) --> {R = []}.

js_switch_statement(Ast) -->
    [tok(keyword, switch_kw, _, Line1, PreTokenWhitespace1)], !,
    ((
        [tok(punctuator, left_par, _, _, _)], 
        js_expression(Value),
        [tok(punctuator, right_par, _, _, _)],   !,
        [tok(punctuator, left_bracket, _, _, _)], 
        js_case_sequence(Sequence),
        [tok(punctuator, right_bracket, _, _, _)], 

        { Ast = js_switch(Value, Sequence, lex_info(Line1, PreTokenWhitespace1  )) } )
    ; throw(parser_exception(malformedSwitch, line(Line1)))).

js_catch(Catch) -->
    [tok(keyword, catch_kw, _, Line1, PreTokenWhitespace1)], !,
    ( (
         [tok(punctuator, left_par, _, _, _)],
         [tok(id, IdName, _, _, _)],
         [tok(punctuator, right_par, _, _, _)],!,
         js_statement_block(Block),
         { Catch = js_catch(IdName, Block,
                           lex_info(Line1, PreTokenWhitespace1  )) }
        )
    ; throw(parser_exception(unexpected_element, line(Line1)))).

js_finally(Finally) -->
    [tok(keyword, finally_kw, _, Line1, PreTokenWhitespace1)], !,
    ( (
         js_statement_block(Block),!,
         { Finally = js_finally(Block,
                              lex_info(Line1, PreTokenWhitespace1  ) ) }
        )
      ; throw(parser_exception(unexpected_element, line(Line1)))).


js_try(Ast) -->
    [tok(keyword, try_kw, _, Line1, PreTokenWhitespace1)], !,
    ((
        js_statement_block(Block),
        ((js_catch(CatchSection), !,
          { CatchBlock = js_catch_section(CatchSection) } )
          ; { CatchBlock = js_catch_section()}),
        ((js_finally(FinallySection), !,
          { FinallyBlock = js_finally_section(FinallySection) } )
         ; { FinallyBlock = js_finally_section()}),
        { Ast = js_try(
                    Block,
                    CatchBlock,
                    FinallyBlock,
                    lex_info(Line1, PreTokenWhitespace1  )) } )
    ; throw(parser_exception(malformedTry, line(Line1)))).



js_for_init(js_for_init(var, Vars, lex_info(Line1, PreTokenWhitespace))) -->
    [tok(keyword,var_kw, _, Line1, PreTokenWhitespace)],
    js_var_decl_sequence(Vars), !.

js_for_init(js_for_init( Vars, lex_info(null, null))) -->
      js_var_decl_sequence(Vars), !.

js_for_init(js_for_init()) --> [].

js_for_increment(js_for_increment(Ast)) --> js_expression(Ast), !.
js_for_increment(js_for_increment()) --> [].

js_for_statement(Ast) -->
    [tok(keyword, for_kw, _, Line1, PreTokenWhitespace1)], !,
    ((
         [tok(punctuator, left_par, _, _, _)], 
         js_for_init(Init),
         (([tok(keyword, in_kw, _,_,_)],
           js_expression(CollectionAst),
           [tok(punctuator, right_par, _, _, _)],
           js_statement(Body),
           { Ast = js_for_in( 
                       Init,
                       CollectionAst,
                       Body,
                       lex_info(Line1, PreTokenWhitespace1  )) }
           ) ;
         (
         [tok(punctuator, semicolon, _, _, _)],
         js_expression(Condition),
         [tok(punctuator, semicolon, _, _, _)],
         js_for_increment(Increment),
         [tok(punctuator, right_par, _, _, _)],   !,
        
         js_statement(Body),
         { Ast = js_for( 
           Init,
           js_for_condition(Condition),
           Increment,
           Body,
           lex_info(Line1, PreTokenWhitespace1  )) } )))
    ; throw(parser_exception(malformedFor, line(Line1)))).    
           


test_pp(js_binary_operation(Op, Left, Right, _), Result) :- 
   test_pp(Left, LeftStr),
   append(LeftStr, Op,Tmp1),
   test_pp(Right, RightStr),
   append(Tmp1, RightStr,Result).

test_pp(js_identifier(Result, _), Result).



