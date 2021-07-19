<!-- Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com -->
<!-- Licensed as Public Domain work, see https://en.wikipedia.org/wiki/Public_domain. -->

Naming and organization                         {#organization}
=======================

To avoid name collisions with other code, libraries, all entities are prefixed with
`cc_`, `CC_`, or `Cc`, depending if they are functions and modules, `#define`s and `const`ants,
or `enum`s and `struct`s, respectively.

Functions and modules names are lowercase words, separated by `_` (underscore),
e.g. `cc_ply.h`, `cc_ply_append_new()`.

`#define`s and `const`ants names are uppercase words, separated by `_` (underscore),
e.g. `CC_OFF_BOARD_COORD`, `CC_SETUP_BOARD_CLASSICAL_CHESS`.

`enum`s and `struct`s names are capitalized words.
`enum` names end in `Enum`, while there is no comparable name end for `struct`s.
Examples, `CcPlyLinkEnum`, `CcPly`.

`enum` items have constant-like library prefix (i.e. `CC_`), combined with abbreviated `enum` name
(e.g. `PLE_`) and capitalized words for item name.
For instance, `CC_PLE_FailedTeleportationOblation` would be one of items in `CcPlyLinkEnum`.

Items (pieces, tags, variants, moves, plies, steps) introduced in [the book] are represented
in the samely named files, `cc_piece`, `cc_tag`, `cc_variant`, `cc_move`, `cc_ply`, `cc_step`.

Board (as a holder of pieces) and tags are combined in a `cc_chessboard` files. Initial setup
for those can be found in `cc_setup_board` and `cc_setup_tags` files.

Moves are applied as a transformation to a chessboard in `cc_do_moves` files.
Functions in this module assume that argument(s) are correct, so there are only a few sanity checks.

Moves are formatted into algebraic notation in a `cc_format_moves` files.
These functions too assume that argument(s) are correct, so there are only a few sanity checks.

Parser messages (and other generic parser stuff) are located in `cc_parser` files.
Move parsing module (`cc_parse_move` files) is not finished yet.

House-keeping utilities are comprised of the rest of modules:
- `cc_define.h` contains `#define`s used accross all modules
- `cc_str_utils` files, various string related functions
- `cc_tokenizer` files, primitive tokenizer for CLI commands issued by user
- `cc_version` files, library version

Library version defined in `cc_version.c` file is a reference version for the whole project,
versions of other applications (`crochess`, `tests`) and libraries (implementing bots) are
required to list `libcrochess` library version as its own.

In turn, `libcrochess` library version is updated every time source code anywhere in the project is updated,
regardless if code is in the library itself, or is in application, or in other dependant libraries.

Versioning scheme used is
[**Natural Versioning 1.2**](https://croatian-chess.blogspot.com/p/natver.html "Natural Versioning 1.2").

Finally, a word of caution. Function reference documentation include "Referenced-by" and
"Referencing" fields, in hope  that it will be useful, which, for the most part, it is.
However, doxygen C parser consistently confuses local variable(s) `i`, `r`, `s`, ... as
referencing static one(s) in `cc_setup_board.c`, `cc_setup_tags.c` files, e.g. in the
description of `cc_chessboard_as_string_new()`.
Please, ignore.


[The Book]: https://github.com/mmlacak/crochess/raw/master/crochess.pdf "Croatian Chess and other variants"

[Naming convention]: https://en.wikipedia.org/wiki/Naming_convention_(programming)#Examples_of_multiple-word_identifier_formats "Examples of multiple-word identifier formats"
