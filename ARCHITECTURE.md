# The interpreter consists of several stages
* Lexing
* Parsing
* Compiling
* Interpreting (+GC)

# Lexer
The lexer is handwritten and implemented as an Iterator. Lexing errors are passed on using 
`Error` tokens. The lexer already allocates identifiers and string literals into the GC.

# Parser
The parser is handwritten using recursive descent. It calls the `next` method on the lexer to get the next token,
but the iterator could be any Iterator and doesn't have to be a lexer.

The AST is allocated using a bump-allocator with the lifetime `'ast`.

# Compiler
The compiler takes the AST and compiles it down to bytecode. The full instruction set my change and can be found in the code.

The bytecode is allocated using a bump-allocator with the lifetime `'bc`

# Interpreter (VM)
The VM executes the bytecode. It uses the GC for its allocations.

# GC
The garbage-collector is work-in-progress.