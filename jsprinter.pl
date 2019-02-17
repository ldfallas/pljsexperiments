:- module(js_ast_printing, [print_js_ast/2]).
:- use_module(jsparser).

use_module(library(memfile)).



print_js_ast_to_string(Ast, Result) :-
    new_memory_file(Handle),
    
    open_memory_file(Handle, write, Stream),!,
    print_js_ast(Ast, Stream),!,
    close(Stream),
    memory_file_to_string(Handle, Result),
    free_memory_file(Handle).

print_js_ast(Ast, Stream) :-
    printing_js_ast(Ast, Stream, 0).


printing_js_ast(js_if(Condition, ThenStat, ElseStat, _),
                Stream,
                Level) :-
    write(Stream, "if ("),
    printing_js_ast(Condition, Stream, Level),
    write(Stream, ") \n"),
    NewLevel is Level + 1,
    print_indentation(NewLevel, Stream),
    printing_js_ast(ThenStat, Stream, NewLevel),
    write(Stream, "\n"),
    print_indentation(Level, Stream),
    write(Stream, "else\n"),
    print_indentation(NewLevel, Stream),    
    printing_js_ast(ElseStat, Stream, NewLevel), !.

printing_js_ast(js_if(Condition, ThenStat, _),
                Stream,
                Level) :-
    write(Stream, "if ("),
    printing_js_ast(Condition, Stream, Level),
    write(Stream, ") \n"),
    NewLevel is Level + 1,
    print_indentation(NewLevel, Stream),
    printing_js_ast(ThenStat, Stream, NewLevel),
    write(Stream, "\n"),
    print_indentation(Level, Stream), !.

printing_js_ast(js_for_in(Init, CollectionAst, Body, _),
             Stream,
             Level) :-
    write(Stream, "for ("),
    printing_js_ast(Init, Stream, Level),
    write(Stream, ";"),
    printing_js_ast(CollectionAst, Stream, Level),
    write(Stream, ")\n"),
    NewLevel is Level + 1,    
    print_indentation(NewLevel, Stream),
    printing_js_ast(Body).

printing_js_ast(js_switch(Expr, Cases, _ ),
                Stream,
                Level) :-
    write(Stream, "switch("),
    printing_js_ast(Expr, Stream, Level),
    write(Stream, ") {\n"),
    NewLevel is Level + 1,
    printing_js_ast_elements(Cases, Stream, NewLevel),
    print_indentation(Level, Stream),
    write(Stream, "}\n").

printing_js_ast(js_case(Case, Instructions, _),
                Stream,
                Level) :-
    write(Stream, "case "),
    printing_js_ast(Case, Stream, Level),
    write(Stream, ":\n"),
    NewLevel is Level + 1,
    printing_js_ast_elements(Instructions, Stream, NewLevel).

printing_js_ast(js_default( Instructions, _),
                Stream,
                Level) :-
    write(Stream, "default:\n "),
    NewLevel is Level + 1,
    printing_js_ast_elements(Instructions, Stream, NewLevel).

printing_js_ast(js_do_while(Condition, Body, _),
                         Stream,
                         Level) :-
    write(Stream, "do\n"),
    NewLevel is Level + 1,
    print_indentation(NewLevel, Stream),
    printing_js_ast(Body, Stream, NewLevel),
    write(Stream, "while("),
    print_js_ast(Condition, Stream, Level),
    write(Stream, ");").

printing_js_ast(js_while(Condition, Body, _),
                         Stream,
                         Level) :-
    write(Stream, "while("),
    print_js_ast(Condition, Stream, Level),
    write(Stream, ")\n"),
    NewLevel is Level + 1,
    print_indentation(NewLevel, Stream),    
    printing_js_ast(Body, Stream, NewLevel).

printing_js_ast(js_identifier(Id, _), Stream, _) :-
    string_codes(Str, Id),
    write(Stream, Str).

printing_js_ast(js_literal(_, LiteralTxt, _), Stream, _) :-
    string_codes(Str, LiteralTxt),
    write(Stream, Str).

printing_js_ast(js_call(Method, Arguments, _), Stream, Level) :-
    printing_js_ast(Method, Stream, Level),
    printing_js_ast(Arguments, Stream, Level).

printing_js_ast(js_expr_stat(Expr, _), Stream, Level) :-
    printing_js_ast(Expr, Stream, Level),
    write(Stream, ";\n").

printing_js_ast(js_break(_), Stream, _) :-
    write(Stream, "break;\n").

printing_js_ast(js_return(_), Stream, _) :-
    write(Stream, "return;\n").

printing_js_ast(js_var_stat(Decls, _), Stream, _) :-
    write(Stream, "var ").

printing_js_ast(Ast, Stream, _) :-
    write(Stream, "Could not print: "),
    write(Stream, Ast).


printing_js_ast_elements([], _, _).
printing_js_ast_elements([E|Rest], Stream, Level) :-
    print_indentation(Level, Stream),
    printing_js_ast(E, Stream, Level),
    printing_js_ast_elements(Rest, Stream, Level).

printing_js_ast_elements_separator([],_ , _, _).
printing_js_ast_elements_separator([E|Rest],
                                   Separator,
                                   Stream,
                                   Level) :-
    print_indentation(Level, Stream),
    printing_js_ast(E, Stream, Level),
    ( Rest = [] , 
    printing_js_ast_elements(Rest, Stream, Level)).


print_indentation(0, _) :- !.
print_indentation(Level, Stream) :-
    write(Stream, " "),
    NewLevel is Level - 1,
    print_indentation(NewLevel, Stream), !.

switch_if_cases(_, [js_default(Body,_)],
                js_block(Body, _)).

switch_if_cases(Variable,
                [js_case(Value, Body, _)|Rest],
                js_if(js_binary_operation(
                          equals_op,
                          Variable,
                          Value,
                          _),
                      js_block(Body, _),
                      RestIf,
                      _)) :-
     switch_if_cases(Variable, Rest, RestIf).
                      

switch_if(js_switch(Variable,
                    Cases,
                   _),
          IfStat) :-
       switch_if_cases(Variable, Cases, IfStat).
