% for statements

evaluate([assign(var(x), num(5)), if(gt(var(x), num(0)), [assign(var(y), num(10))], [assign(var(y), num(-10))])], Env, NewEnv).

% for blocks

evaluate([block([assign(var(x), num(1)), assign(var(y), var(x))])], Env, NewEnv).


% for strings

evaluate([assign(var(s), string("hello")), assign(var(t), string_concat(var(s), string(" world")))], Env, NewEnv).


% for variable validation

evaluate([assign(var(x), num(10)), assign(var(y), var(z))], [], NewEnv).


% test logic for assignment operator

:- use_module(evaluator).  % Ensure evaluator is loaded

test_assignment :-
    Tokens = [int, x, =, 10, .],
    phrase(program(ParsedProgram), Tokens),  % Parse the tokens
    evaluate_program(ParsedProgram, Env),    % Evaluate the parsed program
    writeln(Env).                            % Check the resulting environment


% test logic for multiple assignments

test_multiple_assignments :-
    Tokens = [int, x, =, 10, ., int, y, =, x, +, 5, .],
    phrase(program(ParsedProgram), Tokens),  
    evaluate_program(ParsedProgram, Env),    
    writeln(Env).                            


% test logic for if-else statements

test_simple_if :-
    Tokens = [int, x, =, 10, ., if, x, ==, 10, {, display, "x is 10", ;, }, .],
    phrase(program(ParsedProgram), Tokens),  
    evaluate_program(ParsedProgram, Env),    
    writeln(Env).                            


    test_if_else :-
        Tokens = [int, x, =, 10, ., if, x, ==, 5, {, display, "x is 5", ;, }, else, {, display, "x is not 5", ;, }, .],
        phrase(program(ParsedProgram), Tokens),  
        evaluate_program(ParsedProgram, Env),    
        writeln(Env).                            


        test_nested_if_else :-
            Tokens = [int, x, =, 10, ., int, y, =, 6, ., if, x, ==, 10, {, 
                      if, y, >, 5, {, display, "x is 10 and y is greater than 5", ;, }, 
                      else, {, display, "x is 10 and y is not greater than 5", ;, }, 
                      }, 
                      else, {, display, "x is not 10", ;, }, .],
            phrase(program(ParsedProgram), Tokens),  
            evaluate_program(ParsedProgram, Env),    
            writeln(Env).                            
        