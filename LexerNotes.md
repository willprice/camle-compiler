# Lexer definition
The lexer's job is to tokenize the input from a string into a list of
tokens. We need to tokenize the following aspects of the language:
## Reserved words
* `while`
* `do`
* `if`
* `then`
* `else`
* `read`
* `write`
* `writeln`
* `skip`
* `true`
* `false`

## Syntax
* End of line terminator `;`
* Boolean negation: `!`
* Boolean comparisons: `<=`, `=`
* Arithmetic operators: `+`, `-`, `*`
* Assignment: `:=`
* Brackets: `(`, `)`

## Comments
Defined as `{[^}]*}`, discard them as they are read.

## String, Identifiers, Integer constants
We have our more variable input types such as strings, identifiers and
constants all of which vary considerably, but can be encoded quite
simply.

* Identifier: `[a-zA-Z][a-zA-0-9]{0,7}`
* Integer constant: `[0-9]+`
* Strings: `'([^']|'')'`, `'` is escaped by itself inside a string
  literal...

Encode the reserved strings as lists which will later make part of the
lexer definition, a data structure that holds all the data about the
grammar. 
