:- module(parserexperiment, [js_lex_string/2]).



use_module(library(pure_io)).
use_module(library(charsio)).

/* Added for SWI Prolog 7 compatibility */
:- set_prolog_flag(double_quotes, codes).

string_tok(tok(string, [QuotesChar|Chars], CurrentPosition, PreTokenWhitespace)), [NewPosition, []] -->
	[CurrentPosition, PreTokenWhitespace],
        "\"", {[QuotesChar|_] = "\""},
        string_literal_chars(Chars),
        "\"",
        {
           length(Chars, ContentLength),
           NewPosition is CurrentPosition + ContentLength + 1
        }.

string_literal_char(Char) -->
        [Char],
        { \+ ([QuotesChar|_] = "\"", Char = QuotesChar) }.

string_literal_chars([Char|Chars]) -->
   string_literal_char(Char),
   string_literal_chars(Chars).

string_literal_chars("\""),"\"" --> "\"".



number(tok(number, [Digit|Digits], CurrentPosition, PreTokenWhitespace)), [NewPosition, []] -->
	[CurrentPosition, PreTokenWhitespace],
	digit(Digit),
	digits(Digits),
	{  /*number_codes(Num, [Digit|Digits]),*/
	   length(Digits, NCount),
	   Count is NCount + 1,
	   NewPosition is Count + CurrentPosition

	}.

digit(Digit) --> [Digit],
	{ code_type(Digit, digit) }.

digits([Digit|Digits]) -->
	digit(Digit),
	digits(Digits).
digits([Digit]) --> digit(Digit).

word(tok(id,[Letter|Letters], CurrentPosition, PreTokenWs )), [NewPosition, []] -->
	[CurrentPosition, PreTokenWs],
	letter(Letter),
	letters(Letters),
	{  Word = [Letter|Letters],
	   length(Word, WordLength),
	   NewPosition is (CurrentPosition + WordLength) }.

letter(L) --> [L], { code_type(L, alpha) }.
letters([First|Rest]) -->
	letter(First),
	letters(Rest).
letters([]) --> [].

line_comment(line_comment(Content, CurrentPosition)), [NewPosition, Lex] -->
        [CurrentPosition, Lex],
	"//",
	not_end_of_line_chars(Content),
	end_of_line(EndOfLineSize),
	{ length(Content, ContentSize),
	  Size is 2 + ContentSize + EndOfLineSize,
	  NewPosition is CurrentPosition + Size
	}.

end_of_line(2) --> "\r\n".
end_of_line(1) --> "\n".
end_of_line(0) --> [].

not_end_of_line_chars([Char|Chars]) -->
	[Char],
	not_end_of_line_chars(Chars),
	{ \+ code_type(Char,end_of_line) }.

not_end_of_line_chars([]) --> [].

tok(Tok) -->
	word(Word),
	{
	    js_keyword(Word, Tok),! ;
	    (
		/*tok(id, Text, _,_) = Word,
		\+ is_jskeyword(Text),*/
	        Tok = Word, !
	    )
	}.

tok(tok(punctuator, Value, CurrentPosition, PreTokenWs)), [NewPosition, []] -->
	[CurrentPosition, PreTokenWs],
	js_punctuator(Value),
	{
	    NewPosition is CurrentPosition + 1
	}.

tok(Number) -->
	number(Number).

tok(String) -->
	string_tok(String).

js_punctuator([Value]) -->
	[Value],
	{
	    is_js_punctuator([Value])
	}.

is_js_punctuator(";").
is_js_punctuator("=").
is_js_punctuator("(").
is_js_punctuator(")").
is_js_punctuator("{").
is_js_punctuator("}").
is_js_punctuator(":").
is_js_punctuator(",").

js_keyword(tok(id, Text, Position, Lex), Token) :-
	is_jskeyword(Text),
	Token = tok(keyword, Text, Position,Lex).

is_jskeyword("var").
is_jskeyword("function").
is_jskeyword("return").
is_jskeyword("true").
is_jskeyword("false").

toks([Tok|[Sep|Rest]]) -->
	tok(Tok),
	whitespace(Sep),
	toks(Rest).
toks([Tok]) -->
	tok(Tok).
toks([]) --> [].


toks2([Tok|Rest]) -->
	lexical_whitespace,
	tok(Tok),
	toks2(Rest).
toks2([]) --> [].

lexical_whitespace, [NewPosition, NewWhitespace] -->
	lex_whitespace_elements(NewWhitespace),
	[NewPosition, _].

lex_whitespace_elements([WhiteSpaceElement|Rest]) -->
	(whitespace(WhiteSpaceElement)
	 ; line_comment(WhiteSpaceElement)),
	lex_whitespace_elements(Rest).

lex_whitespace_elements([]) --> [].




whitespace(ws(CurrentPosition)), [NewPosition, Lex] -->
	[CurrentPosition, Lex],
	ws,
	wss(Subcount),
        { Count is Subcount + 1,
	  NewPosition is CurrentPosition + Count
	}.
ws --> [X],{ code_type(X, space) }.
wss(Count) -->
	ws,
	wss(Subcount),
        { 
          Count is Subcount + 1, !
	}.
wss(0) --> [].

without_ws([ws(_)|Rest],Other) :-
	without_ws(Rest,Other).
without_ws([X|Rest1],[X|Rest2]) :-
	\+ (X = ws(_)),
	without_ws(Rest1,Rest2).
without_ws([],[]).

js_lex_string(Str, Toks) :-
	phrase(toks2(Toks), [0|[[]|Str]], [_,_]).

js_lex_string2(Str, Toks) :-
	length(Str,Length),
	phrase(toks2(Toks), [0|[[]|Str]], [Length,[]]).















