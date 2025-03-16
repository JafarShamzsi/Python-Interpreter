# Build Your Own Interpreter

[![progress-banner](https://backend.codecrafters.io/progress/interpreter/52b0ad07-967a-49bc-84ec-cbf60b6337fd)](https://app.codecrafters.io/users/codecrafters-bot?r=2qF)

This is a Python implementation of the Lox interpreter following the ["Build your own Interpreter" Challenge](https://app.codecrafters.io/courses/interpreter/overview) and the book [Crafting Interpreters](https://craftinginterpreters.com/) by Robert Nystrom.

## Implementation Status

The interpreter currently supports:

- **Lexical Analysis**: Tokenization of Lox source code
- **Parsing**: Converting tokens into an Abstract Syntax Tree
- **Expressions**: Literals, binary operations, grouping, unary operations, variable references
- **Statements**: Print, variable declarations, blocks, if statements, while loops
- **Control Flow**: If/else conditions and while loops
- **Functions**: Declaration, calls, return statements, closures
- **Classes**: Declaration, instantiation, methods, properties
- **OOP Features**: 
  - The `this` keyword for accessing instance properties
  - Constructors via `init()` method
  - Method inheritance

## Language Features

This interpreter implements Lox, a dynamically-typed scripting language with the following features:

```lox
// Variables
var x = 10;
var greeting = "Hello, world!";

// Control flow
if (x > 5) {
  print "x is greater than 5";
} else {
  print "x is not greater than 5";
}

// Loops
var i = 0;
while (i < 5) {
  print i;
  i = i + 1;
}

// Functions
fun add(a, b) {
  return a + b;
}
print add(5, 3);

// Closures
fun makeCounter() {
  var count = 0;
  fun counter() {
    count = count + 1;
    return count;
  }
  return counter;
}
var counter = makeCounter();
print counter(); // 1
print counter(); // 2

// Classes and OOP
class Person {
  init(name) {
    this.name = name;
  }

  sayHello() {
    print "Hello, I'm " + this.name;
  }
}

var person = Person("Alice");
person.sayHello();
```

## Getting Started

### Prerequisites

- Python 3.12 or later

### Running the Interpreter

To run a Lox script:

```sh
./your_program.sh run path/to/script.lox
```

## Development

1. Make changes to main.py
2. Test your changes locally:
   ```sh
   ./your_program.sh run examples/hello.lox
   ```
3. Commit and push to submit your solution:
   ```sh
   git commit -am "Implement feature X"
   git push origin master
   ```

## Testing

The CodeCrafters platform provides tests for each stage of the implementation. You can also write your own Lox scripts to test specific features.

## Project Structure

- main.py: The main interpreter implementation
- `your_program.sh`: Shell script to run the interpreter

## Implementation Notes

- The interpreter follows a tree-walk approach, directly interpreting the AST
- Environment nesting is used to implement variable scoping
- Functions are first-class values with proper closures
- Classes support instance methods and properties
- Constructor methods (`init()`) properly initialize instances and handle returns specially

## Resources

- [Crafting Interpreters Book](https://craftinginterpreters.com/)
- [The Lox Language](https://craftinginterpreters.com/the-lox-language.html)
- [CodeCrafters Interpreter Challenge](https://app.codecrafters.io/courses/interpreter/overview)

// Variables
var x = 10;
var greeting = "Hello, world!";

// Control flow
if (x > 5) {
  print "x is greater than 5";
} else {
  print "x is not greater than 5";
}

// Loops
var i = 0;
while (i < 5) {
  print i;
  i = i + 1;
}

// Functions
fun add(a, b) {
  return a + b;
}
print add(5, 3);

// Closures
fun makeCounter() {
  var count = 0;
  fun counter() {
    count = count + 1;
    return count;
  }
  return counter;
}
var counter = makeCounter();
print counter(); // 1
print counter(); // 2

// Classes and OOP
class Person {
  init(name) {
    this.name = name;
  }

  sayHello() {
    print "Hello, I'm " + this.name;
  }
}

var person = Person("Alice");
person.sayHello();

