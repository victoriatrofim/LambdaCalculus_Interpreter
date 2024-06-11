# LambdaCalculus_Interpreter

The link for all the tasks: https://ocw.cs.pub.ro/ppcarte/doku.php?id=pp:2024:tema3

The project centers around the creation of a lambda calculus interpreter built in Haskell, a functional programming language well-suited for such mathematical computations. The primary objectives of the interpreter are to evaluate lambda expressions, parse string representations of these expressions into usable data structures, and simplify expressions to their normal forms using beta-reduction, avoiding variable capture issues through auxiliary functions.

The interpreter begins by handling the parsing of lambda expressions using a custom Parser data type. This parser is designed to interpret strings representing lambda expressions and convert them into the interpreter's internal representation, adhering strictly to a defined grammar that supports variables, applications, and abstractions.

Once parsed, the expressions undergo evaluation where the main challenge is reducing expressions to their normal forms while managing variable scopes and avoiding naming collisions—a common issue in lambda calculus known as variable capture. The project implements two strategies for reducing expressions: normal order and applicative order, each selecting redexes (reducible expressions) in different manners to optimize computation paths.

An extension to basic lambda calculus, this project introduces macros—named, reusable expressions—that simplify writing and managing complex lambda expressions. This is achieved through a computational context that acts like a dictionary, linking macro names to their corresponding expressions. This context is crucial when evaluating macros, as it substitutes macro calls with their actual expressions seamlessly.

Furthermore, the interpreter includes a 'Default Library' filled with commonly used lambda expressions for Boolean operations, pair manipulations, and basic arithmetic, demonstrating the interpreter's practical application. These predefined macros help users quickly implement complex logical and numerical operations without manually defining them each time.

The project culminates in a REPL (Read-Eval-Print Loop), providing a user-friendly interface where users can interactively enter lambda expressions, evaluate them, define new macros, and view or reset the computational context. This interactive environment is crucial for testing, debugging, and demonstrating the functionality of the lambda calculus interpreter in a dynamic and responsive manner.

Overall, this project not only serves as an educational tool for understanding lambda calculus but also as a practical implementation of a functional programming language interpreter, demonstrating advanced concepts in computer science and programming language theory.

