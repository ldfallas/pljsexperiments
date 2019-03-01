:- module(js_ast_printing, [print_js_ast/2]).
:- use_module(jsparser).

use_module(library(memfile)).

binary_operator_string(gt_op, ">").
binary_operator_string(gt_eq_op, ">=").
binary_operator_string(lt_op, "<").
binary_operator_string(lt_eq_op, "<=").
binary_operator_string(equals_op, "==").


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

    ((ThenStat = js_block(_, _),
      write(Stream, ") "),
      printing_js_ast(ThenStat, Stream, Level),
      write(Stream, " "),
      !) ;
     (write(Stream, ") \n"),
      NewLevel is Level + 1,
      print_indentation(NewLevel, Stream),
      printing_js_ast(ThenStat, Stream, NewLevel))),
    print_indentation(Level, Stream),
    printing_js_ast(js_tmp_else(ElseStat), Stream, Level),
    !.

printing_js_ast(js_tmp_else(js_block(Instructions, Lex)),
                Stream,
                Level) :-
    write(Stream, "else "),
    printing_js_ast(js_block(Instructions, Lex), Stream, Level).

printing_js_ast(js_tmp_else(Stat),
                Stream,
                _) :-
    write(Stream, "else "),
    print_indentation(NewLevel, Stream),
    printing_js_ast(Stat, Stream, NewLevel).


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
    printing_js_ast(Condition, Stream, Level),
    write(Stream, ")\n"),
    NewLevel is Level + 1,
    print_indentation(NewLevel, Stream),    
    printing_js_ast(Body, Stream, NewLevel).

printing_js_ast(js_binary_operation(Operator,
                                    LeftExpr,
                                    RightExpr,
                                    _),
                Stream,
                Level) :-
    printing_js_ast(LeftExpr, Stream, Level),
    write(Stream, " "),
    binary_operator_string(Operator, OperatorString),
    write(Stream, OperatorString),
    write(Stream, " "),    
    printing_js_ast(RightExpr, Stream, Level).

printing_js_ast(js_identifier(Id, _), Stream, _) :-
    string_codes(Str, Id),
    write(Stream, Str).

printing_js_ast(js_literal(_, LiteralTxt, _), Stream, _) :-
    string_codes(Str, LiteralTxt),
    write(Stream, Str).

printing_js_ast(js_assign(Left, Right, _), Stream, _) :-
    printing_js_ast(Left, Stream, Left),
    write(Stream, " = "),
    printing_js_ast(Left, Stream, Right).

printing_js_ast(js_call(Method, Arguments, _), Stream, Level) :-
    printing_js_ast(Method, Stream, Level),
    printing_js_ast(Arguments, Stream, Level).

printing_js_ast(js_arguments(Args, _), Stream, Level) :-
    write(Stream, "("),
    printing_js_ast_elements_separator_noindent(Args, ", ", Stream, Level),
    write(Stream, ")").

printing_js_ast(js_expr_stat(Expr), Stream, Level) :-
    printing_js_ast(Expr, Stream, Level),
    write(Stream, ";\n").

printing_js_ast(js_break(_), Stream, _) :-
    write(Stream, "break;\n").

printing_js_ast(js_return(_), Stream, _) :-
    write(Stream, "return;\n").

printing_js_ast(js_return(Expr, _), Stream, Level) :-
    write(Stream, "return "),
    printing_js_ast(Expr, Stream, Level),
    write(Stream, ";\n").


printing_js_ast(js_var_stat(Decls, _), Stream, Level) :-
    write(Stream, "var "),
    printing_js_ast_elements_separator_noindent(
        Decls,
        ", ",
        Stream,
        Level),
    !.

printing_js_ast(js_var_decl(Name, InitValue,_), Stream, Level) :-
    write(Stream, Name),
    write(Stream, " = "),
    printing_js_ast(InitValue, Stream, Level).

printing_js_ast(js_var_decl(Name, _), Stream, _) :-
    write(Stream, Name).

printing_js_ast(js_block(Instructions, _), Stream, Level) :-
    write(Stream, "{\n"),
    NewLevel is Level + 1,
    printing_js_ast_elements_separator(
        Instructions,
        "\n",
        Stream,
        NewLevel),
    print_indentation(Level, Stream),
    write(Stream, "}").


printing_js_ast(Ast, Stream, _) :-
    write(Stream, "Could not print: "),
    write(Stream, Ast).

printing_js_ast_elements([], _, _).
printing_js_ast_elements([E|Rest], Stream, Level) :-
    print_indentation(Level, Stream),
    printing_js_ast(E, Stream, Level),!,
    printing_js_ast_elements(Rest, Stream, Level).

printing_js_ast_elements_separator([],_ , _, _).
printing_js_ast_elements_separator([E|Rest],
                                   Separator,
                                   Stream,
                                   Level) :-
    print_indentation(Level, Stream),
    printing_js_ast(E, Stream, Level), !,
    ( Rest = [],! ;
      ( write(Stream, Separator),
        printing_js_ast_elements_separator(
            Rest,
            Separator,
            Stream,
            Level))).

printing_js_ast_elements_separator_noindent([],_ , _, _).
printing_js_ast_elements_separator_noindent([E|Rest],
                                   Separator,
                                   Stream,
                                   Level) :-
    printing_js_ast(E, Stream, Level),
    ( Rest = [] ,
      (write(Stream, Separator),
       printing_js_ast_elements(Rest, Stream, Level))).



print_indentation(0, _) :- !.
print_indentation(Level, Stream) :-
    write(Stream, " "),
    NewLevel is Level - 1,
    print_indentation(NewLevel, Stream), !.


