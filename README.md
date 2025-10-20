# Interpreter for a New Programming Language

This project was developed for the **Design of Programming Languages** course at **Sharif University of Technology**.

We designed a simple statically typed programming language and implemented its interpreter in **Racket**.  
The project included both a **context-free grammar (CFG)** for the language and an **interpreter** that executes programs based on it.

## Language Features
- Variable types: `number`, `string`, `array`
- Variable declaration and scoping
- Static type checking
- Functions and recursive functions
- Function calls (call-by-value)
- If–else conditions and loops
- Logical and numerical operations
- Lazy evaluation

## Implementation Details
- The interpreter parses programs using the defined CFG.
- A **static type checker** verifies type correctness before execution.
- Variables are stored using **implicit references** in memory.
- Evaluation uses **lazy semantics**, ensuring expressions are computed only when needed.

## How to Run
1. Write your program in a file.
2. Open the `runner.rkt` file.
3. Change `path` variable to your program's path.
4. Run the interpreter by executing `racket src/runner.rkt`. The output will be printed to the console.  

## Project Structure
```
newlang/
├── CFG.txt                         # Context-free grammar specification for the language
└── src/
    ├── compiled/drracket/errortrace/   
    ├── examples/                       # Example programs written in the new language
    ├── boo-lang.rkt                    # Main entry point for the interpreter
    ├── datatypes.rkt                   # Definitions of language data types 
    ├── environment.rkt                 # Implements variable scopes and environments
    ├── lexer.rkt                       # Lexical analyzer (tokenizer) for the language
    ├── memory.rkt                      # Memory model — manages implicit references and variable storage
    ├── parser.rkt                      # Parser based on the context-free grammar 
    ├── runner.rkt                      # Runs programs: connects parser, type checker, and interpreter
    └── type-check.rkt                  # Static type checker for ensuring type safety before interpretationng 
```
