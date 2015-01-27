:- module(jsparser, [parse_js_expression_string/2 ]).

:- use_module(parseexperiment).

parse_js_expression_string(CodeString, Ast) :- 
   js_lex_string(CodeString, Toks), 
   !, /* just one  chance to tokenize the string */
   phrase(js_expression(Ast), Toks).

js_expression(Ast) -->
    js_primary_expression(Ast).
   

js_primary_expression(Ast) -->
   js_literal_expression(Ast) 
   ; js_identifier_expression(Ast)
   ; js_par_expression(Ast) .

js_literal_expression(
   js_literal(string, 
              StringValue,
              lex_info(Line, PreTokenWhitespace))) -->
   [tok(string, StringValue, _, Line, PreTokenWhitespace)] .


js_identifier_expression(
   js_identifier(IdName,
                 lex_info(Line, PreTokenWhitespace))) -->
   [tok(id, IdName, _, Line, PreTokenWhitespace)] .


js_par_expression(js_par(Expr, lex_info(Line, PreTokenWhitespace))) -->
   [tok(punctuator, "(", _, Line, PreTokenWhitespace)],
   js_expression(Expr),
   [tok(punctuator, ")", _, Line, PreTokenWhitespace)].
