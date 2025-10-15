% Include tokenizer and parsedtree pl files
:- ['tokenizer'].  
:- ['tatascript_compiler']. 
:- ['tatascript_interpreter']. 

run :-
    current_prolog_flag(argv, [TTS_File]),
    process_file(TTS_File).
    
% Tokenize .tts file, generating list of tokens
process_file(File) :-
    tokenize_file(File, ListOfTokens),
    % Generate ParsedTree from the generated list of tokens
    program(ParsedTree, ListOfTokens, []), 
    % Evaluate the program
    program_eval(ParsedTree, _).