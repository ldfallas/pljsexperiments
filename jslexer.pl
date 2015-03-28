:- module(parserexperiment, [js_lex_string/2, js_regex/3]).

use_module(library(pure_io)).
use_module(library(charsio)).

/* Added for SWI Prolog 7 compatibility */
:- set_prolog_flag(double_quotes, codes).

string_tok(tok(string, [QuotesChar|Chars], CurrentPosition, Line, PreTokenWhitespace)), 
          [NewPosition, Line, []] -->
	[CurrentPosition, Line, PreTokenWhitespace],
         (("\"", {[QuotesChar|_] = "\""}, !) ;
          ("\'", {[QuotesChar|_] = "'"}, !) ),
        string_literal_chars(Chars, QuotesChar),
        [QuotesChar],
        {
           length(Chars, ContentLength),
           NewPosition is CurrentPosition + ContentLength + 1
        }.

string_literal_char(Char, QuotesChar) -->
        [Char],
        { \+ (Char = QuotesChar) }.

string_literal_chars([Char|Chars], QuotesChar) -->
   string_literal_char(Char, QuotesChar),
   string_literal_chars(Chars, QuotesChar).

string_literal_chars([QuotesChar], QuotesChar),[QuotesChar] --> [QuotesChar].


number(tok(number, [Digit|Digits], CurrentPosition, Line, PreTokenWhitespace)), 
       [NewPosition, Line, []] -->
	[CurrentPosition, Line, PreTokenWhitespace],
	digit(Digit),
	digits(IntDigits),
        ((".", digits(Decimals), { append(".", Decimals, PointAndDecimals),
                                   append(IntDigits, PointAndDecimals, Digits) })
          ; ( [], { Digits = IntDigits})),
	{  
	   length(Digits, NCount),
	   Count is NCount + 1,
	   NewPosition is Count + CurrentPosition
	}.

digit(Digit) --> [Digit],
	{ code_type(Digit, digit) }.

digits([Digit|Digits]) -->
	digit(Digit),
	digits(Digits).
digits([]) --> [].

/*

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
*/


/****/
identifier(tok(id,[InitialCharacter|Letters], CurrentPosition, Line, PreTokenWs )), 
           [NewPosition, Line, []] -->
	[CurrentPosition, Line, PreTokenWs],
	initialIdCharacter(InitialCharacter),
	letters(Letters),
	{  Word = [InitialCharacter|Letters],
	   length(Word, WordLength),
	   NewPosition is (CurrentPosition + WordLength) }.

initialIdCharacter(L) --> [L], { code_type(L, alpha)  }.
initialIdCharacter(L) --> "$", { "$" = [L] }.
initialIdCharacter(L) --> "_", { "_" = [L] }.
letters([First|Rest]) -->
	(initialIdCharacter(First) ; digit(First)),
	letters(Rest).
letters([]) --> [].
/***/


line_comment(CurrentPosition,Line, Lex, line_comment(Content, CurrentPosition)), [NewPosition, NewLine, Lex] -->
	"//",
	not_end_of_line_chars(Content),
	end_of_line(EndOfLineSize),
	{ length(Content, ContentSize),
	  Size is 2 + ContentSize + EndOfLineSize,
	  NewPosition is CurrentPosition + Size,
          NewLine is Line + 1
	}.

end_of_line(2) --> "\r\n".
end_of_line(1) --> "\n".
end_of_line(0) --> [].

not_end_of_line_chars([Char|Chars]) -->
	[Char],
	{ \+ code_type(Char,end_of_line) },
	not_end_of_line_chars(Chars).

not_end_of_line_chars([]) --> [].

block_comment(CurrentPosition,Line, Lex, block_comment(Content, CurrentPosition)), [NewPosition, NewLine, Lex] -->
    "/*",
    not_end_block_comment_chars(Content, Lines),
    "*/", {
          length(Content, ContentSize),
	  Size is 4 + ContentSize ,
	  NewPosition is CurrentPosition + Size,
          NewLine is Line + Lines
    }.

not_end_block_comment_chars([], 0),"*/" --> "*/", { ! }.
not_end_block_comment_chars([Char|Chars],Lines) -->
   [Char], 
   not_end_block_comment_chars(Chars, NextLines),
   {  ( (code_type(Char,newline), CurrentLine = 1, !);
         CurrentLine = 0 ) , Lines is NextLines + CurrentLine  }.
not_end_block_comment_chars([],0) --> [].

tok(Tok, _) -->
	identifier(Word),
	{
	    js_keyword(Word, Tok),! ;
	    (
		/*tok(id, Text, _,_) = Word,
		\+ is_jskeyword(Text),*/
	        Tok = Word, !
	    )
	}.

/*tok(tok(punctuator, Value, CurrentPosition, Line, PreTokenWs), PreviousToken), */
tok(ResultTok, PreviousToken), 
    [NewPosition, Line, []] -->
	[CurrentPosition, Line, PreTokenWs],
        ((peek_slash_char,  
           {  regexPossible(PreviousToken) }, 
           js_regex(Result), {
	       (NewPosition is CurrentPosition + 2),
               ResultTok = tok(regex, Result, CurrentPosition, Line, PreTokenWs)
           }, !) ;

	(js_punctuator(Value, Length),
	{
	    (NewPosition is CurrentPosition + Length),
            ResultTok = tok(punctuator, Value, CurrentPosition, Line, PreTokenWs)
	})).

regexPossible(null).
regexPossible(tok(punctuator, Punc, _, _, _)) :- \+ Punc = ")".

peek_slash_char, [Char] -->
   [Char],
   { Char = 47 /* 47 == '/' */ }.

js_regex(regex_literal(Body, Options)) -->
   "/",
   js_regular_expression_body(Body),
   "/",
   js_regular_expression_flags(Options).

js_regular_expression_flags([Char|Rest]) -->
   [Char],
   { code_type(Char, alpha) },
   js_regular_expression_flags(Rest).

js_regular_expression_flags([]) --> [].

js_regular_expression_body(Chrs) -->
   js_regular_expression_first_valid_char(First),! ,
   js_regular_expression_class_chars(Rest),!,
   { append( First, Rest, Chrs) }.


js_regular_expression_first_char(First) --> 
  js_regular_expression_first_char(First)
  ; js_regular_expression_backslash_sequence(First).

js_regular_expression_char(First) --> 
  js_regular_expression_common_char(First)
  ; js_regular_expression_backslash_sequence(First).


js_regular_expression_backslash_sequence(BackslashSequence) -->
   "\\",
   js_regular_expression_non_terminator(Chr),
   { append("\\", Chr, BackslashSequence)  }.

js_regular_expression_class_char -->
   "[",
    js_regular_expression_class_chars(Chars),
   "]",
   { append("[",Chars, C1)  }.

js_regular_expression_class_char(Char) -->
   js_regular_expression_common_char(Char).

js_regular_expression_class_char(BackslashSequence) -->
   js_regular_expression_backslash_sequence(BackslashSequence).
  
js_regular_expression_class_chars(Chars) --> 

    js_regular_expression_class_char(First), !,
    js_regular_expression_class_chars(Rest), !,
    { append(First, Rest, Chars)  }.
   
js_regular_expression_class_chars([]) --> [].
   

js_regular_expression_non_terminator([C]) --> 
   [C], !, { \+ code_type(C, newline)  }. 

js_regular_expression_first_valid_char([C]) --> 
   [C], !, { \+ code_type(C, newline), \+ member(C,"*\\/")  }. 

js_regular_expression_common_char([C]) --> 
   [C], !, { \+ code_type(C, newline), \+ member(C,"\\/")  }. 

tok(Number, _) -->
	number(Number).

tok(String, _) -->
	string_tok(String).

js_punctuator(Value, 3) -->
      [Value1, Value2, Value3], 
      { 
        Value = [Value1, Value2, Value3], 
        three_char_punctuator(Value)
      }.


js_punctuator(Value, 2) -->
      [Value1, Value2], 
      { 
        Value = [Value1, Value2], 
        two_char_punctuator(Value)
      }.

js_punctuator([Value], 1) -->
	[Value],
	{
	    is_js_punctuator([Value])
	}.

three_char_punctuator("===").
three_char_punctuator("!==").
three_char_punctuator(">>>").
three_char_punctuator(">>=").
three_char_punctuator("<<=").

two_char_punctuator("+=").
two_char_punctuator("-=").
two_char_punctuator("/=").
two_char_punctuator("*=").
two_char_punctuator("<=").
two_char_punctuator(">=").
two_char_punctuator("||").
two_char_punctuator("&&").
two_char_punctuator("++").



is_js_punctuator(";").
is_js_punctuator("=").
is_js_punctuator("(").
is_js_punctuator(")").
is_js_punctuator("[").
is_js_punctuator("]").
is_js_punctuator("{").
is_js_punctuator("}").
is_js_punctuator(":").
is_js_punctuator(",").
is_js_punctuator(".").
is_js_punctuator("+").
is_js_punctuator("-").
is_js_punctuator("*").
is_js_punctuator("/").
is_js_punctuator("%").
is_js_punctuator(">").
is_js_punctuator("<").
is_js_punctuator("!").
is_js_punctuator("?").
is_js_punctuator("&").
is_js_punctuator("|").
is_js_punctuator("^").

js_keyword(tok(id, Text, Position, Line, Lex), Token) :-
	is_jskeyword(Text),
	Token = tok(keyword, Text, Position, Line,Lex).

is_jskeyword("var").
is_jskeyword("function").
is_jskeyword("return").
is_jskeyword("true").
is_jskeyword("false").
is_jskeyword("new").
is_jskeyword("delete").

toks([Tok|[Sep|Rest]]) -->
	tok(Tok),
	whitespace(Sep),
	toks(Rest).
toks([Tok]) -->
	tok(Tok).
toks([]) --> [].


toks2([Tok|Rest], PreviousToken) -->
	lexical_whitespace,
	tok(Tok, PreviousToken),
	toks2(Rest, Tok).
toks2([], _),[X,Y,Z] --> lexical_whitespace,[X,Y,Z], \+ [_].
toks2([], _) -->[X,Line,Z], [A,B,C], {  throw(unexpectedInput(line(Line), [A,B,C])) }.
toks2([], _) -->[X,Line,Z], { throw(unexpectedInput(line(Line))) }.

lexical_whitespace, [NewPosition, Line, NewWhitespace] -->
	lex_whitespace_elements(NewWhitespace),
	[NewPosition, Line, _].

lex_whitespace_elements([WhiteSpaceElement|Rest]) -->
	[CurrentPosition,Line, Lex],
        peek_whitespace_start,
	(whitespace(CurrentPosition,Line, Lex, WhiteSpaceElement), !
	 ; line_comment(CurrentPosition,Line, Lex, WhiteSpaceElement), !
         ; block_comment(CurrentPosition,Line, Lex, WhiteSpaceElement)),
	lex_whitespace_elements(Rest).

lex_whitespace_elements([]) --> [].

peek_whitespace_start , [Char] -->
   [Char],
   {code_type(Char,space); Char = 47 /* 47 == '/' */ }.
   

whitespace(CurrentPosition,Line, Lex, ws(CurrentPosition, WithNewLine)), [NewPosition, NewLine, Lex] -->
	ws(AddedLines), 
        wss(Subcount,NewLines),
        { Count is Subcount + 1,
	  NewPosition is CurrentPosition + Count,
          NewLine is Line + NewLines + AddedLines,
          writef(AddedLines),
          ((AddedLines + NewLines > 0, WithNewLine = true,!) ;
             WithNewLine = false) 
	}.
ws(1) --> [X],{ code_type(X, newline), ! }.
ws(0) --> [X],{ code_type(X, space) }.
wss(Count, Lines) -->
	ws(AddedLines),
	wss(Subcount, NewLines),
        { 
          Lines is  NewLines + AddedLines, 
          Count is Subcount + 1, !
	}.
wss(0, 0) --> [].

without_ws([ws(_,_)|Rest],Other) :-
	without_ws(Rest,Other).
without_ws([X|Rest1],[X|Rest2]) :-
	\+ (X = ws(_,_)),
	without_ws(Rest1,Rest2).
without_ws([],[]).

js_lex_string(Str, Toks) :-
	phrase(toks2(Toks, null), [0|[1|[[]|Str]]], [_,_,_]).

js_lex_string2(Str, Toks) :-
	length(Str,Length),
	phrase(toks2(Toks, null), [0|[1|[[]|Str]]], [Length,_,[]]).















