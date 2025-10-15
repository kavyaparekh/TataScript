% Tabling condition predicate
:- table condition/2.
:- table add_and_sub_expr/2.
:- table mul_and_div_expr/2.
% Start point of the program, program is a block encapusated between semicolons(;)
program --> [;], main_block, [;].

% Program block is bunch of statements
main_block --> statements.

% Statements can be individual statement or a chain of statements.
statements --> statement, statements.
statements --> statement.

% Individual statements
statement --> assignment, [.].
statement --> display, [.].
statement --> if_statement.
statement --> for_loop.
statement --> while_loop.
statement --> increment, [.].

% Assignment can be with or without datatype
assignment --> data_type, variable, [=], expression.
assignment --> variable, [=], expression.

% this deals with the expressions
expression --> chainedassign.
expression --> ternary_statement.
expression --> boolean_op.

% to implement precedence in arithematic operators
chainedassign --> variable, [=], expression.
chainedassign--> add_and_sub_expr.
add_and_sub_expr--> add_and_sub_expr, [+], mul_and_div_expr.
add_and_sub_expr --> add_and_sub_expr, [-], mul_and_div_expr.
add_and_sub_expr --> mul_and_div_expr.

% multiplication and division are grouped because they have the same precedence
mul_and_div_expr --> mul_and_div_expr, [*], remaining_expression.
mul_and_div_expr --> mul_and_div_expr, [/], remaining_expression.
mul_and_div_expr --> remaining_expression.

% handling parenthesis, it has higher precedence than the operators above
remaining_expression --> ['('], expression, [')'].

% the expression can finally be an identifier and number
remaining_expression --> variable.
remaining_expression --> number.
remaining_expression --> string_literal.
%remaining_expression --> variable, [=], expression.

% Display is used to output to the screen
display --> [display], [->], output.
output --> string_literal | variable | number.

% Condtional Statements
% If statement with else if chain followed by a final else block. If is followed by a ->
if_statement --> [if, ->], condition, block, elseif_blocks, else_block.

% Else-if and else blocks
% Optional blocks. Else-if is <--> and else is <-
elseif_blocks --> elseif_block, elseif_blocks.
elseif_blocks --> [].
elseif_block --> [<-, ->], condition, block.
else_block --> [<-], block.
else_block --> [].

% Ternary Statements
ternary_statement --> condition, [->], ternary_expression, [<-], ternary_expression.
ternary_expression --> expression | statements.

% For loop is the keyword for followed by ->, and init, condition and increment separated by :
for_loop --> [for, ->], init, [:], condition, [:], increment, block.

% Initialization :  for "for" loop
init --> assignment.

% Increment : for "for" loop
increment --> variable, increment_op.

% While loop is the keyword while followed by -> and a condition
while_loop --> [while, ->], condition, block.

% Code blocks are statements encapusulated within {} curly braces
block --> ['{'], statements, ['}'].
block --> ['{'],['}'].

% Expressions
%expression --> term.
%expression --> term, arithmetic_op, expression.
%expression --> arithmetic_op, parenthesis, arithmetic_op
%expression --> arithmetic_op, parenthesis, arithmetic_op
%expression --> increment.
%expression --> ternary_statement.
%expression --> boolean_op.

% Terms
%term --> variable.
%term --> number.
%term --> string_literal.

% Conditions
condition --> expression, relational_op, expression.
condition --> expression, boolean_op, expression.
condition --> condition, logical_op, condition.

% Operators
%arithmetic_op --> [+] | [-] | [*] | [/].
relational_op --> [<] | [>] | [==] | [<=] | [>=] | [!].
%parenthesis --> ['('], expression, [')'].
boolean_op --> [true] | [false].
increment_op --> [++] | [--].
logical_op --> [&] | ['|'].

% Data types can be int, string and bool
data_type --> [int] | [string] | [bool].

% Variable names
variable --> [Var], { atom(Var), atom_chars(Var, [FirstChar | Rest]), char_type(FirstChar, alpha), all_alnum_or_underscore(Rest) }.

% Numbers and string literals
number --> [Num], { number(Num) }.
string_literal --> [Str], { atom(Str), atom_concat('"', _, Str), atom_concat(_, '"', Str) }.

% Helper for checking valid variable names
all_alnum_or_underscore([]).
all_alnum_or_underscore([Char | Rest]) :-
    (char_type(Char, alnum) ; Char == '_'),
    all_alnum_or_underscore(Rest).