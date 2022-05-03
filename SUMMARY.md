# SUMMARY

## INIT

- Started with Makefile and Readme.
- Made a basic hello world example in assembly.

## Reverse Polish

- Improved the features of the given reverse polish compiler.
- Changed the Concrete syntax for introducing division/multiplicaiton operations and also added parenthesis in the lex and grm file.
- Changed Abstract syntax in the ast.sml file by introducing new constructors for division and multiplication . 

## MIPS, Lexing and Parsing
- Used SPIM's mips documentation to create a MIPS structure in sml which captures MIPS's AST.
- Made datatype for capturing registers, labels, statements and instructions.
- Made functions to pretty print it.
- Made the Tiger AST as a datatype which was built upon the reverse polish's AST.
- Used the lexer and parser of the given reverse polish to built the Tiger's lexer and parser.
- Added new productions in the grammar and keywords to capture Tiger AST.
- Added productions to implement statements with side effects, for loop (do...done), variable assignments in parser.
  
## Register Allocation, IR, Blocks and Graphs
- Greedy Method for Register allocation (infinite registers).
- Another Register allocation method ( temp % (total temp registers) ) (removed).
- Implemented structure for Tree IR with basic functions .
- Current working version is using MIPS as the IR and compiling it.
- Uses AtomMap to store variables as temporary registers.
- Created an Environment feature for local scope variables (Using AtomMap for each scope).
- Added temporary label feature.
- Created functor for Basic Blocks.
- Created structure for extracting control flow blocks from a MIPS program.
- Made a graph structure using AtomTable's hash_table for a mutable data structure requirement.

## TODO
- Implement Canonisation in Tree IR.
- Use Tree IR as IR for compiling.
- Pretty Print Basic Blocks for debuggin and control flow visualization
- Implement Register Allocation using Graph Coloring Algorithm and connect it to compiler. 
- Add features of Tiger Programming Langauge (while loops, classes ...).