# HAL

A lisp interpreter in Haskell based on [Chez-Scheme](https://github.com/cisco/chezscheme). This was a thrid year project at Epitech.


## Supported features
 - Ints, Floats, Bools, Symbols & Strings
 - Cons (aka lists)
 - Procedures (via a define or a lambda)
 - [REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop) (Read, Evaluate, Print & Loop)
 - Strange shenanigans of Chez-Scheme (for example `'(quote 3)` yields `'3`)

## Builtins
 - define, let, lambda
 - quote
 - cons, car, cdr
 - eq?, atom?
 - +, -, *, div, mod, <

## Usage
``./hal [-i] [FILES]``

-i: to use REPL feature

[FILES]: list of files to run. If -i is specified, the files are run before prompting the user

## Authors
 - [Zoe Roux](https://github.com/AnonymusRaccoon)

