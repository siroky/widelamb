# Widelamb (Functional Programming Semestral project)

A parser, a Hindley-Milner type checker and an interpreter of a simple functional language.

## Assignment

Create a Hindley-Milner type-checker (60 points) and a simple interpreter (30 points) for a simple functional language. Write the Euclid's algorithm for searching the greatest common divisor in the language (10 points). 
A term of the language is one of: 

- A variable - any sequence of letters (except one of the keywords).
- A lambda application - two terms separated by a white space.
- A lambda abstraction - written as ```\variable . term```
- A let expression - written as ```let variable = term1 in term2```, where the variable doesn't occur in term1.
- A fix expression - written as ```fix variable . term```, where the variable may occur in the term. Its semantic is given by ```fix x . M --> M[x := fix x . M]```
- A parenthesised expression - written as ```(term)```. Allows expressions like ```(x z) (y z)```.
- Integer (decimal) constant.

The type checker should compute the most general type for a given context and a program in the language. 

The language contains a primitive (built-in) type N for natural numbers, and several primitive functions: 

- ```ifzero : Λα . N → α → α → α``` if the first argument is zero, return the second argument, otherwise the third.
- ```plus : N → N → N``` addition.
- ```minus : N → N → N``` subtraction.
- ```mul : N → N → N``` multiplication.
- ```div : N → N → N``` integral division.

These functions with their types are always present in the default context when the type checker starts. 

If a program is successfully type checked and its type is N, the interpreter is run on it and the result is printed. The interpreter doesn't have to be efficient at all, but it must be correct. You must be sure that if the interpreter is run on a program whose type is N, the interpreter will not crash and will return the correct integer result (or loop forever, if the program doesn't terminate). You can also implement the interpreter by converting programs into some standard suitable language and running the result. 

### Example program 

```(fix fact . \x . ifzero x 1 (mul x (fact (minus x 1)))) 10```

should yield ```3628800``` as the result.
