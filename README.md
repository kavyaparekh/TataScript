#TataScript
TataScript (.tts)  

**[YouTube Link](https://youtu.be/4EGQRHPF1k8)**

    The language supports an intermediate compilation process where code is compiled and then interpreted. We are planning on developing a loosely object-oriented programming paradigm. Support for classes and encapsulated functions is future scope.

    Language Features

        The program starts and ends with a semicolon (;)

        We are using the following delimiters :
                full stop (.),
                curly braces ({}),
                forward arrow (->), 
                backward arrow (<-)
        
        Each statement is terminated with a full stop (.). 

        Code blocks are defined using curly braces ({}).
  
        Conditional Statement: 
            Syntax: if -> condition -> {} <- -> {} <- -> {} <- {}.
            else (<-), else if (<- –>)

        For Loop:
        Syntax: for -> <initialization>: <condition>: <increment> -> {}.

        While Loop:
        Syntax: while -> condition -> {}.

        Variables may consist of:
          All letters (starting with a letter)
          Underscores (_)
          Digits (0-9)
            Examples of valid variables: a, a_1, aPPle
            Examples of invalid variables: _abc, ab*, 12abc
    
        Assignment Operator: Variables are assigned values using the = operator.
  
        Relational Operators: <, >, ==, <=, >=, ! (not)
  
        Boolean Operators: TRUE, FALSE, and logical operations with variables

        Arithmetic Operators: +, -, *, /
  
        Increment/Decrement Operators: ++ (increment), -- (decrement)
  
        Ternary Operator:
            Syntax: a = condition -> c <- d.

        Logical Operators: & (and), | (or)

        "display" keyword is used for printing purposes.

        Double quotes are used for literal strings, while single quotes are used for variables.
            Example: a = "hello world."
            display -> "this is going to be 'a'".
            Output: This is going to be Hello World.

        The language is strongly typed and currently supports the following data types: int, string, and bool

Tools Used: DCG

Platform Support: Windows, Mac


--------------------------------------------------------------------------------------------------------------------------

STEPS to execute the tatascript file

    1. Download this repository and unzip.
    2. Using Terminal, go to the root folder.
        2.1 cd SER502-TataScript-Team13
    3. Write the program and save it with the extension (.tts). Examples given in the data folder.
    4. Run the file using the following commands
        4.1 Mac OS / Linux: bash tata.sh <path to .tts file>
                                OR
                            ./tata.sh <path to .tts file> 
            Example: bash tata.sh data/assignment.tts OR ./tata.sh data/assignment.tts
        4.2 Windows : tata.bat <path to .tts file>
            Example: tata.bat data/assignment.tts
    5. The output has a Token List and a corresponding Parsed Tree.


