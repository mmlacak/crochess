.. Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
   Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See LICENSING, COPYING files for details.

.. include:: defines.rst

.. _lbl-organization-naming:

Naming and organization
=======================

Describes where to find, and what.

.. _lbl-organization-naming-conventions:

Naming conventions
------------------

To avoid name collisions with other code, libraries, all entities are prefixed with:

.. _lbl-organization-naming-conventions-prefixes:

.. list-table:: Prefixes table
   :header-rows: 1
   :align: left
   :widths: 10, 40, 65

   * - Prefix
     - Entities
     - Examples
   * - ``cc_``
     - functions, modules
     - :c:`cc_ply.h`, :c:`cc_ply_append_new()`
   * - ``CC_``
     - :c:`#define`\s, :c:`const`\ants, :c:`enum` items
     - :c:`CC_OFF_BOARD_COORD`, :c:`CC_PPLE_FailedTeleportationOblation`
   * - ``Cc``
     - :c:`enum`\s, :c:`struct`\s, :c:`union`\s
     - :c:`CcParsedPlyLinkEnum`, :c:`CcParsedPly`

No particular
`naming convention <https://en.wikipedia.org/wiki/Naming_convention_(programming)#Examples_of_multiple-word_identifier_formats>`_
is used.

Functions and modules names are lowercase words, separated by ``_`` (underscore),
e.g. :c:`cc_ply.h`, :c:`cc_ply_append_new()`.

:c:`#define` and :c:`const`\ant names are uppercase words, separated by ``_``
(underscore), e.g. :c:`CC_OFF_BOARD_COORD`, :c:`CC_SETUP_BOARD_CLASSICAL_CHESS`.

:c:`enum`\s, :c:`struct`\s and :c:`union`\s names are capitalized words. :c:`enum`
names end in ``Enum``, while there is no comparable name end for :c:`struct`\s,
:c:`union`\s. Examples, :c:`CcParsedPlyLinkEnum`, :c:`CcParsedPly`.

:c:`enum` items have constant-like library prefix (i.e. ``CC_``), combined with
abbreviated :c:`enum` name (e.g. ``PPLE_``) and capitalized words for item name.
For instance, :c:`CC_PPLE_FailedTeleportationOblation` would be one of items in
:c:`CcParsedPlyLinkEnum`.

.. _lbl-organization-naming-namingentities:

Naming entities
---------------

.. todo::

    Naming entities

.. _lbl-organization-naming-linkedlists:

Linked lists
------------

.. todo::

    Linked lists

.. _lbl-organization-naming-organization:

Organization
------------

Pieces, tags, variants, chessboards introduced in
`the book <https://github.com/mmlacak/crochess/raw/master/crochess.pdf>`_ are in
similarly named (source and header) files, i.e. :c:`cc_piece`, :c:`cc_tag`,
:c:`cc_variant`, :c:`cc_chessboard`.

Each chessboard holds a board for pieces and the other one for tags.
Initial setups for those are in :c:`cc_setup_board` and :c:`cc_setup_tags` files.



Parsers for user notation are in :c:`cc_parse_move`, :c:`cc_parse_ply`, :c:`cc_parse_step`,
:c:`cc_parse_side_effect`.
Parsed notation is stored in :c:`cc_parsed_move`, :c:`cc_parsed_ply`, :c:`cc_parsed_step`,
:c:`cc_parsed_side_effect`.
Parser messages are located in :c:`cc_parse_msg` files.

Game (i.e. its status, current chessboard, and list of moves played so far) can
be found in :c:`game` files.

House-keepers, conveniences are in:

- ``cc_define.h`` contains :c:`#define`\s used across modules
- ``cc_str_utils`` various string related functions
- ``cc_tokenizer`` primitive tokenizer for CLI commands issued by user
- ``cc_version`` library version

.. _lbl-organization-naming-versioning:

Versioning
----------

Library version defined in ``cc_version.c`` file is a reference version for the
whole project.
Currently, other applications (``crochess``, ``tests``) are all syncing with
``libcrochess`` library version as their own.

In turn, ``libcrochess`` library version is updated every time source code anywhere
in the project is updated, regardless if updated code is actually in the library, or
elsewhere.

Versioning scheme used is
`Natural Versioning 1.2 <https://croatian-chess.blogspot.com/p/natver.html>`_.
