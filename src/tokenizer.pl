% Tokenize program from .tts files to a list of tokens
tokenize_file(Filename, Tokens) :-
    file_name_extension(_Base, tts, Filename),  % Check for .tts extension
    open(Filename, read, Stream),
    read_string(Stream, _, Content),           % Read File
    close(Stream),
    tokenize(Content, Tokens).

% Tokenize predicate taking program as a string giving a list of tokens
tokenize(Input, Tokens) :-
    string_chars(Input, Chars),
    tokenize_chars(Chars, Tokens).

% Base Case
tokenize_chars([], []).

% Char tokenizer to handle numbers
tokenize_chars([Char | RestChars], [Number | Tokens]) :-
    char_type(Char, digit),
    !,
    collect_number_chars([Char | RestChars], NumChars, RemainingChars),
    number_chars(Number, NumChars),
    tokenize_chars(RemainingChars, Tokens).

% Char tokenizer to handle strings
tokenize_chars(['"' | RestChars], [QuotedString | Tokens]) :-
    collect_string(RestChars, StringChars, RemainingChars),
    append(['"'], StringChars, TempQuoted),
    append(TempQuoted, ['"'], QuotedStringChars),
    atom_chars(QuotedString, QuotedStringChars),
    tokenize_chars(RemainingChars, Tokens).

% Handling operators with 2 characters like -> <- -- ++
tokenize_chars([Char1, Char2 | RestChars], [Token | Tokens]) :-
    tokenize_double(Char1, Char2, RestChars, Token, RemainingChars),
    !,
    tokenize_chars(RemainingChars, Tokens).

% Hangling single character operators
tokenize_chars([Char | RestChars], [Token | Tokens]) :-
    tokenize_single(Char, RestChars, Token, RemainingChars),
    tokenize_chars(RemainingChars, Tokens).

% Whitespace and new line handling
tokenize_chars([' ' | RestChars], Tokens) :-
    tokenize_chars(RestChars, Tokens).
tokenize_chars(['\n' | RestChars], Tokens) :-
    tokenize_chars(RestChars, Tokens).
tokenize_chars(['\t' | RestChars], Tokens) :-
    tokenize_chars(RestChars, Tokens).

% Token continuous numbers
collect_number_chars([], [], []).
collect_number_chars([Char | Rest], [Char | NumChars], Remaining) :-
    char_type(Char, digit),
    !,
    collect_number_chars(Rest, NumChars, Remaining).
collect_number_chars([Char | Rest], [], [Char | Rest]) :-
    \+ char_type(Char, digit).

% String tokeninzation : Collect until double quotes uncounter
collect_string(['"' | Rest], [], Rest) :- !.
collect_string([Char | RestChars], [Char | StringChars], Remaining) :-
    collect_string(RestChars, StringChars, Remaining).

% Two character operators
tokenize_double(Char1, Char2, RestChars, Token, RestChars) :-
    member([Char1, Char2], [['<', '-'], ['-', '>'], ['+', '+'], ['-', '-'], 
                            ['<','='],['>','='],['!','='],['=','=']]),
    atom_chars(Token, [Char1, Char2]).

% Single Character operators
tokenize_single(Char, RestChars, Token, RestChars) :-
    member(Char, ['=', '+', '-', '*', '/', ',', '.', ';', ':', '!', '?', 
                 '(', ')', '[', ']', '{', '}', '<', '>', '"', '&', '|']),
    atom_chars(Token, [Char]).

% Identifiers and Keywords handling
tokenize_single(Char, RestChars, Word, RemainingChars) :-
    is_alnum(Char),
    collect_alnum([Char | RestChars], AlnumChars, RemainingChars),
    atom_chars(Word, AlnumChars).

collect_alnum([Char | RestChars], [Char | AlnumChars], RemainingChars) :-
    is_alnum(Char),
    !,
    collect_alnum(RestChars, AlnumChars, RemainingChars).
collect_alnum(RemainingChars, [], RemainingChars).

is_alnum(Char) :-
    char_type(Char, alnum).
