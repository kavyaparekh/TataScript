%:- use_rendering('svgtree').
% Tabling condition predicate
:- table condition/3.
:- table add_and_sub_expr/3.
:- table mul_and_div_expr/3.

% Start point of the program, program is a block encapusated between semicolons(;)
program(program(X)) --> [;], main_block(X), [;].

% Program block is bunch of statements
main_block(main_block(X)) --> statements(X).

% Statements can be individual statement or a chain of statements.
statements(statement_block(X, Y)) --> statement(X), statements(Y).
statements(statement(X)) --> statement(X).

% Individual statements
statement(X) --> assignment(X), [.].
statement(X) --> display(X), [.].
statement(X) --> if_statement(X).
statement(X) --> for_loop(X).
statement(X) --> while_loop(X).
statement(X) --> increment(X), [.].

% Assignment can be with or without datatype
assignment(assign(D, V, E)) --> data_type(D), variable(V), [=], expression(E).
assignment(assign(V, E)) --> variable(V), [=], expression(E).

expression(X) --> chainedassign(X).

expression(X) --> ternary_statement(X).
expression(X) --> boolean_op(X).

%--------------------------------------------------------------------------------------------
%--------------------------------------------------------------------------------------------
%--------------------------------------------------------------------------------------------
% to implement precedence in arithematic operators
% addition and subtraction are grouped because they have the same precedence
chainedassign(assign(X,Y)) --> variable(X), [=], expression(Y), !.
chainedassign(X)--> add_and_sub_expr(X).
add_and_sub_expr(add(X, Y)) --> add_and_sub_expr(X), [+], mul_and_div_expr(Y).
add_and_sub_expr(sub(X, Y)) --> add_and_sub_expr(X), [-], mul_and_div_expr(Y).
add_and_sub_expr(X) --> mul_and_div_expr(X).

% multiplication and division are grouped because they have the same precedence
mul_and_div_expr(mul(X, Y)) --> mul_and_div_expr(X), [*], remaining_expression(Y).
mul_and_div_expr(divide(X, Y)) --> mul_and_div_expr(X), [/], remaining_expression(Y).
mul_and_div_expr(X) --> remaining_expression(X).
remaining_expression(assign(X, Y)) --> variable(X), [=], expression(Y).
% handling parenthesis, it has higher precedence than the operators above
remaining_expression(parenthesis(X)) --> ['('], expression(X), [')'].
remaining_expression(X) --> variable(X).
remaining_expression(X) --> number(X).
% remaining_expression(bool_val(X) --> 
remaining_expression(X) --> string_literal(X).
% handling assignments, which are chained


% the expression can finally be an identifier and number


%--------------------------------------------------------------------------------------------
%--------------------------------------------------------------------------------------------
%--------------------------------------------------------------------------------------------
% Display is used to output to the screen
output(X) --> string_literal(X).
output(X) --> number(X).
output(X) --> variable(X).    
display(display(X)) --> [display], [->], output(X).


% Condtional Statements
% If statement with else if chain followed by a final else block. If is followed by a ->
if_statement(if(C, B, EI, E)) --> [if, ->], condition(C), block(B), elseif_blocks(EI), else_block(E).

% Else-if and else blocks
% Optional blocks. Else-if is <--> and else is <-
elseif_blocks(elseif_block(X, Y)) --> elseif_block(X), elseif_blocks(Y).
elseif_blocks([]) --> [].

elseif_block(elseIf(C, B)) --> [<-, ->], condition(C), block(B).

else_block(else(B)) --> [<-], block(B).
else_block([]) --> [].

% Ternary Statements
ternary_statement(ternary(C, E1, E2)) --> condition(C), [->], ternary_expression(E1), [<-], ternary_expression(E2).
ternary_expression(X) --> expression(X).
ternary_expression(X) --> statements(X).

% For loop is the keyword for followed by ->, and init, condition and increment separated by :
for_loop(for(I, C, In, B)) --> [for, ->], init(I), [:], condition(C), [:], increment(In), block(B).

% Initialization :  for "for" loop
init(I) --> assignment(I).

% Increment : for "for" loop
increment(incr(X, Op)) --> variable(X), increment_op(Op).

% While loop is the keyword while followed by -> and a condition
while_loop(while(C, B)) --> [while, ->], condition(C), block(B).

% Code blocks are statements encapusulated within {} curly braces
block(block(B)) --> ['{'], statements(B), ['}'].
block([]) --> ['{'], ['}'].

% Expressions
%expression(X) --> term(X).
%expression(exp(X, Op, Y)) --> term(X), arithmetic_op(Op), expression(Y).
%expression(X) --> increment(X).
%expression(X) --> ternary_statement(X).
%expression(X) --> boolean_op(X).

% Terms
%term(X) --> variable(X).
%term(X) --> number(X).
%term(X) --> string_literal(X).

% Conditions
condition(cond(X, Op, Y)) --> expression(X), relational_op(Op), expression(Y).
condition(cond(X, Op, Y)) --> condition(X), logical_op(Op), condition(Y).

% Operators
arithmetic_op(+) --> [+].
arithmetic_op(-) --> [-].
arithmetic_op(*) --> [*].
arithmetic_op(/) --> [/].

relational_op(<) --> [<].
relational_op(>) --> [>].
relational_op(==) --> [==].
relational_op(<=) --> [<=].
relational_op(>=) --> [>=].
relational_op(!) --> [!].

boolean_op(true) --> [true].
boolean_op(false) --> [false].

increment_op(++) --> [++].
increment_op(--) --> [--].

logical_op(and) --> [&].
logical_op(or) --> ['|'].

% Data types can be int, string and bool
data_type(data_type(int)) --> [int].
data_type(data_type(string)) --> [string].
data_type(data_type(bool)) --> [bool].

% Variable names
variable(var(Var)) --> [Var], { atom(Var), atom_chars(Var, [FirstChar | Rest]), char_type(FirstChar, alpha), all_alnum_or_underscore(Rest) }.

% Numbers and string literals
number(num(Num)) --> [Num], { number(Num) }.
% Negative Number Handling
number(num(Num)) --> [-], [Num1], { number(Num1), Num is -1 * Num1 }.

string_literal(string(Str)) --> [Str], { atom(Str), atom_concat('"', _, Str), atom_concat(_, '"', Str) }.

% Helper for checking valid variable names
all_alnum_or_underscore([]).
all_alnum_or_underscore([Char | Rest]) :-
    (char_type(Char, alnum) ; Char == '_'),
    all_alnum_or_underscore(Rest).