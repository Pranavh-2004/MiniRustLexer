# MiniRustLexer

**RustLexer** is a custom lexer and parser built with Python's [PLY (Python Lex-Yacc)](https://www.dabeaz.com/ply/) library to interpret a subset of Rust syntax. The project demonstrates how to tokenize and parse basic Rust constructs like variable declarations, mutable variables, function definitions, conditional statements, loops, and print statements in a Rust-inspired format.

### Features

- **Tokenizing and Parsing**:
- **Variable Declarations**: Supports both mutable (`let mut`) and immutable (`let`) variable declarations.
- **Function Definitions**: Recognizes simple function definitions with the `fn` keyword.
- **Control Flow**: Parses basic control flow structures including `if-else` statements, `while` loops, and `for` loops.
- **Print Statements**: Supports the `println!` macro for basic print functionality with expressions.
- **Expressions**: Recognizes integers, floats, and string literals, with basic assignment operations and comparisons.

### Project Structure

The project contains:

- **Lexer Rules**: Rules to recognize Rust-like keywords, operators, symbols, and literals.
- **Parser Rules**: Rules to parse and validate Rust-like statements, including conditional statements, loops, and function structures.
- **Test Cases**: A set of Rust-like example statements to test the lexer and parser.

### Usage

#### Installation

1. Clone this repository:

   ```bash
   git clone https://github.com/your-username/RustLexer.git
   ```

2. Navigate to the project directory:

   ```bash
   cd RustLexer
   ```

3. Ensure you have Python and the PLY library installed. Install PLY if needed:
   ```bash
   pip install ply
   ```

### Running the Lexer and Parser

1. Run the `RustLexer.py` file to tokenize and parse the example Rust-like code:

   ```bash
   python RustLexer.py
   ```

2. You will see output indicating whether each test case is syntactically correct or not.

#### Example Code Snippet

Here’s an example of the Rust-like syntax that this lexer and parser can handle:

```rust
let x = 10;
let mut y = 20;
while (x > 5) { let z = 20; }
if (x == 15) { let z = 30; } else { let z = 40; }
for (let i = 0; i < 10; i = i + 1) { let j = i; }
fn main() { println!("Hello, world!"); }
```

Each line in the example will be tokenized and parsed, with output indicating correct syntax or any syntax errors detected in the structure.

### Project Roadmap

Currently, **RustLexer** focuses on a simplified subset of Rust syntax. Future enhancements could include:

- **Support for Advanced Rust Features**: Expanding the lexer and parser to handle more complex Rust syntax, such as pattern matching, ownership and borrowing, traits, and custom data types.
- **Detailed Error Reporting**: Improving the error-handling capabilities for more informative syntax error messages.
- **Testing Suite**: Adding a suite of automated tests to validate the lexer and parser against a variety of Rust code examples.

### License

This project is licensed under the MIT License.
