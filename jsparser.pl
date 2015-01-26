:- module(jsparser, [parse_js_expression_string/2 ]).

:- use_module(parseexperiment).

parse_js_expression_string(CodeString, Ast) :- 
   js_lex_string(CodeString, Toks),
   phrase(js_expression(Ast), Toks).

js_expression(Ast) -->
    js_primary_expression(Ast).
   

js_primary_expression(Ast) -->
   js_literal_expression(Ast).

js_literal_expression(js_literal(string, StringValue,lex_info(Line, PreTokenWhitespace))) -->
   [tok(string, StringValue, _, Line, PreTokenWhitespace)] .

