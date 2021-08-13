<!-- Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com -->
<!-- Licensed as Public Domain work, see https://en.wikipedia.org/wiki/Public_domain. -->

|| [next >>>](1_organization.md "next >>>")

libcrochess developer documentation                         {#mainpage}
===================================

libcrochess is a main library facilitating everything needed to play any
of Croatian Chess variants:
- chessboard and tags storage and transformations
- formatting output for chessboard, and algebraic notation
- parsing user input, and algebraic notation [*]
- enforcing rules, timings [*]
- list of moves [*]
- save and load game [*]
- export into extended PGN format files [*]
- list, start bot(s) [*]

[*] currently not implemented

libcrochess does not implement bots on its own, those will be implemented
as separate libraries, to allow for different bots to play against each other,
or a human.

libcrochess is aimed at developers whishing to support Croatian Chess chess
variants, one needs to do little more than to capture user inputs (e.g. in GUI
application), and show responses from the library. There is an accompanying
console application, crochess, which demonstrates just that.

libcrochess is designed to be as portable as possible, so it uses only standard
C library. As such, there is no networking support. It is possible to redirect
stdin, stdout and stderr into and from crochess application so that other
application, library takes over networking.

Here are recommended reads (book and related pages), before diving into detailed reference:
- [**Croatian Chess and other variants**][the book]
- [Naming and organization](1_organization.md "Naming and organization")
- [Memory management](2_memory.md "Memory management")

### Abbreviations

Abbreviations used in documentation.

| Abbreviation |            Meaning |
| -----------: | -----------------: |
|           AN | algebraic notation |

|| [next >>>](1_organization.md "next >>>")

[The Book]: https://github.com/mmlacak/crochess/raw/master/crochess.pdf "Croatian Chess and other variants"
