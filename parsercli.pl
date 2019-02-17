:- use_module(jsparser).

try_run_parser(FileName) :-
    write("Parsing : "),
    writeln(FileName),
    parse_js_file(FileName, _), !.

main :-
    current_prolog_flag(argv, [FileName]),
    catch(try_run_parser(FileName),
          parser_exception(Error, Line),
          writeln([Error, Line])).





