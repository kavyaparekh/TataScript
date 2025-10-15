% Lookup will check the variable in the env
lookup(Var, [Var=Value | _], Value).
lookup(Var, [_ | Rest], Value) :- lookup(Var, Rest, Value).

% Update env will update the env variables with the value
update_env([], Var, Value, [Var=Value]).
update_env([Var=_ | Rest], Var, Value, [Var=Value | Rest]).
update_env([Other | Rest], Var, Value, [Other | UpdatedRest]) :-
    update_env(Rest, Var, Value, UpdatedRest).

% Program Evaluation is the evaluation of the main block which is enclosed in semicolons
program_eval(program(MainBlock), FinalEnv) :-
    main_block_eval(MainBlock, [], FinalEnv).

% Main block evaluation is the evaluation of statements
main_block_eval(main_block(Statements), Env, FinalEnv) :-
    statements_eval(Statements, Env, FinalEnv).

% Statements Evaluation
statements_eval(statement_block(Stmt, Rest), Env, FinalEnv) :-
    statement_eval(Stmt, Env, TempEnv),
    statements_eval(Rest, TempEnv, FinalEnv).
statements_eval(statement(Stmt), Env, FinalEnv) :-
    statement_eval(Stmt, Env, FinalEnv).

% Individual Statement Evaluation
% Statement evaluations for assignment
statement_eval(assign(data_type(_Type), var(Var), Expr), Env, FinalEnv) :-
    expression_eval(Expr, Env, Value),
    update_env(Env, Var, Value, FinalEnv).

statement_eval(assign(var(Var), Expr), Env, FinalEnv) :-
    expression_eval(Expr, Env, Value),
    update_env(Env, Var, Value, FinalEnv).

% Statement evaluations for display statements
statement_eval(display(Output), Env, Env) :-
    output_eval(Output, Env, Str),
	strip_quotes(Str, Stripped),
    writeln(Stripped).

% If condition Evaluations
% If the first condition is true evaluate the block 
statement_eval(if(Cond, ThenBlock, _ElseIf, _Else), Env, FinalEnv) :-
    condition_eval(Cond, Env, true),
    block_eval(ThenBlock, Env, FinalEnv), !. 

% If the first condition is false evaluate the chain of elseif and finally else statement blocks
statement_eval(if(Cond, _ThenBlock, ElseIf, Else), Env, FinalEnv) :-
    condition_eval(Cond, Env, false),
    elseif_blocks_eval(ElseIf, Env, Else, FinalEnv).

% For loop evaluation
statement_eval(for(Init, Cond, Incr, Block), Env, FinalEnv) :-
    statement_eval(Init, Env, InitEnv),
    for_loop_eval(Cond, Incr, Block, InitEnv, FinalEnv).

% While loop evaluation
statement_eval(while(Cond, Block), Env, FinalEnv) :-
    while_loop_eval(Cond, Block, Env, FinalEnv).

% Increment evaluator ++ 
statement_eval(incr(var(Var), ++), Env, FinalEnv) :-
    lookup(Var, Env, Value),
    NewValue is Value + 1,
    update_env(Env, Var, NewValue, FinalEnv).

% Decrement evaluator --
statement_eval(incr(var(Var), --), Env, FinalEnv) :-
    lookup(Var, Env, Value),
    NewValue is Value - 1,
    update_env(Env, Var, NewValue, FinalEnv).

% Ternary Condition evaluator with assignment
% When true
statement_eval(assign(data_type(_Type), var(Var), ternary(Cond, TrueExpr, _FalseExpr)), Env, FinalEnv) :-
    condition_eval(Cond, Env, true),
    expression_eval(TrueExpr, Env, Result),
    update_env(Env, Var, Result, FinalEnv).

% When false
statement_eval(assign(data_type(_Type), var(Var), ternary(Cond, _TrueExpr, FalseExpr)), Env, FinalEnv) :-
    condition_eval(Cond, Env, false),
    expression_eval(FalseExpr, Env, Result),
    update_env(Env, Var, Result, FinalEnv).

% Ternary Assignment without Data Type
% When true
statement_eval(assign(var(Var), ternary(Cond, TrueExpr, _FalseExpr)), Env, FinalEnv) :-
    condition_eval(Cond, Env, true),
    expression_eval(TrueExpr, Env, Result),
    update_env(Env, Var, Result, FinalEnv).

% When false
statement_eval(assign(var(Var), ternary(Cond, _TrueExpr, FalseExpr)), Env, FinalEnv) :-
    condition_eval(Cond, Env, false),
    expression_eval(FalseExpr, Env, Result),
    update_env(Env, Var, Result, FinalEnv).

% Display output without quotes
strip_quotes(Str, Stripped) :-
    atom_chars(Str, Chars),
    strip_quotes_helper(Chars, StrippedChars),
    atom_chars(Stripped, StrippedChars).

strip_quotes_helper(['"' | Rest], Stripped) :-  % Remove leading quote
    append(Stripped, ['"'], Rest), !.           % Remove trailing quote
strip_quotes_helper(Chars, Chars). 

substitute_variables(Str, Env, FinalStr) :-
    atom_chars(Str, Chars),
    substitute_variables_in_chars(Chars, Env, SubstitutedChars),
    atom_chars(FinalStr, SubstitutedChars).

% Handling vars in side display statements
substitute_variables_in_chars([], _, []).

substitute_variables_in_chars(['\\', '\'' | Rest], Env, ['\'' | SubstitutedRest]) :-
    substitute_variables_in_chars(Rest, Env, SubstitutedRest).

substitute_variables_in_chars(['\'' | Rest], Env, Result) :-
    extract_variable(Rest, VarChars, AfterVar),
    atom_chars(Var, VarChars),
    lookup(Var, Env, Value),         
    atom_chars(Value, ValueChars), 
    append(ValueChars, SubstitutedRest, Result),
    substitute_variables_in_chars(AfterVar, Env, SubstitutedRest).

substitute_variables_in_chars([Char | Rest], Env, [Char | SubstitutedRest]) :-
    Char \= '\\', 
    substitute_variables_in_chars(Rest, Env, SubstitutedRest).

extract_variable(['\'' | Rest], [], Rest).
extract_variable([Char | Rest], [Char | VarChars], AfterVar) :-
    extract_variable(Rest, VarChars, AfterVar).

% Block Evaluation
block_eval([], Env, Env).
block_eval(block(B), Env, FinalEnv) :-
    statements_eval(B, Env, FinalEnv).
check_non_zero(RightVal) :-
    RightVal \= 0.
check_non_zero(_) :-
    write("ERROR : Divide by Zero Exception"),
    halt.
% Expression Evaluation
expression_eval(add(L, R), Env, Value) :-
    expression_eval(L, Env, LeftVal),
    expression_eval(R, Env, RightVal),
    Value is LeftVal + RightVal.
expression_eval(sub(L, R), Env, Value) :-
    expression_eval(L, Env, LeftVal),
    expression_eval(R, Env, RightVal),
    Value is LeftVal - RightVal.
expression_eval(mul(L, R), Env, Value) :-
    expression_eval(L, Env, LeftVal),
    expression_eval(R, Env, RightVal),
    Value is LeftVal * RightVal.
expression_eval(divide(L, R), Env, Value) :-
    expression_eval(L, Env, LeftVal),
    expression_eval(R, Env, RightVal),
    check_non_zero(RightVal),
    Value is LeftVal // RightVal.
expression_eval(parenthesis(Expr), Env, Value) :-
    expression_eval(Expr, Env, Value).
expression_eval(num(Value), _, Value).
expression_eval(var(Var), Env, Value) :-
    lookup(Var, Env, Value).
expression_eval(string(Str), _, Str).

% Ternary Expression Evaluation
expression_eval(ternary(Cond, TrueExpr, _FalseExpr), Env, Value) :-
    condition_eval(Cond, Env, true), % If condition evaluates to true
    expression_eval(TrueExpr, Env, Value).

expression_eval(ternary(Cond, _TrueExpr, FalseExpr), Env, Value) :-
    condition_eval(Cond, Env, false), % If condition evaluates to false
    expression_eval(FalseExpr, Env, Value).



% Output Evaluation
output_eval(string(Str), Env, FinalStr) :-
	substitute_variables(Str, Env, FinalStr), !.
output_eval(var(Var), Env, Value) :-
    lookup(Var, Env, Value).
output_eval(num(Num), _, Num).

% Condition Evaluation
condition_eval(cond(L, Op, R), Env, true) :-
    expression_eval(L, Env, LeftVal),
    expression_eval(R, Env, RightVal),
    relational_eval(Op, LeftVal, RightVal, true).
condition_eval(cond(L, Op, R), Env, false) :-
    expression_eval(L, Env, LeftVal),
    expression_eval(R, Env, RightVal),
    relational_eval(Op, LeftVal, RightVal, false).
condition_eval(cond(X, and, Y), Env, true) :-
    condition_eval(X, Env, true),
	condition_eval(Y, Env, true).

condition_eval(cond(X, and, Y), Env, false) :-
    condition_eval(X, Env, false),
	condition_eval(Y, Env, true).

condition_eval(cond(X, and, Y), Env, false) :-
    condition_eval(X, Env, true),
	condition_eval(Y, Env, false).

condition_eval(cond(X, and, Y), Env, false) :-
    condition_eval(X, Env, false),
	condition_eval(Y, Env, false).

condition_eval(cond(X, or, Y), Env, true) :-
    condition_eval(X, Env, true),
	condition_eval(Y, Env, true).

condition_eval(cond(X, or, Y), Env, true) :-
    condition_eval(X, Env, false),
	condition_eval(Y, Env, true).

condition_eval(cond(X, or, Y), Env, true) :-
    condition_eval(X, Env, true),
	condition_eval(Y, Env, false).

condition_eval(cond(X, or, Y), Env, false) :-
    condition_eval(X, Env, false),
	condition_eval(Y, Env, false).


% Relational Operators
relational_eval(<, L, R, true) :- L < R.
relational_eval(>, L, R, true) :- L > R.
relational_eval(==, L, R, true) :- L =:= R.
relational_eval(<=, L, R, true) :- L =< R.
relational_eval(>=, L, R, true) :- L >= R.
relational_eval(!, L, R, true) :- L \= R.
relational_eval(_, _, _, false).

% Loops Evaluation
% For loop evaluations
for_loop_eval(Cond, Incr, Block, Env, FinalEnv) :-
    (condition_eval(Cond, Env, true) ->
        block_eval(Block, Env, TempEnv),
        statement_eval(Incr, TempEnv, NextEnv),
        for_loop_eval(Cond, Incr, Block, NextEnv, FinalEnv)
    ; FinalEnv = Env).

% while loop evaluations
while_loop_eval(Cond, Block, Env, FinalEnv) :-
    condition_eval(Cond, Env, true),
    block_eval(Block, Env, TempEnv),
    while_loop_eval(Cond, Block, TempEnv, FinalEnv).

while_loop_eval(Cond, _Block, Env, Env) :-
    condition_eval(Cond, Env, false).

% elseif chain evaluations
elseif_blocks_eval(elseif_block(X, _Y), Env, _Else, FinalEnv) :-
	elseif_block_eval(X, Env, FinalEnv, true).

elseif_blocks_eval(elseif_block(X, Y), Env, Else, FinalEnv) :-
	elseif_block_eval(X, Env, _Env2, false),
	elseif_blocks_eval(Y, Env, Else, FinalEnv).

elseif_blocks_eval([], Env, Else, FinalEnv) :-
	else_block_eval(Else, Env, FinalEnv).

elseif_block_eval(elseIf(C, B), Env, FinalEnv, true) :-
	condition_eval(C, Env, true),
	block_eval(B, Env, FinalEnv).

elseif_block_eval(elseIf(C, _B), Env, Env, false) :-
	condition_eval(C, Env, false).

else_block_eval(else(B), Env, FinalEnv) :-
    block_eval(B, Env, FinalEnv).

else_block_eval([], Env, Env).