.. Copyright (c) 2021, 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.

.. include:: ../defines.rst

.. _lbl-libcc-organization-naming:

Naming and organization
=======================

Describes where to find, and what.

.. _lbl-libcc-organization-naming-conventions:

Naming conventions
------------------

To avoid name collisions with other code, libraries, every :term:`entity` is prefixed with:

.. _lbl-libcc-organization-naming-conventions-prefixes:

.. list-table:: Prefixes table
   :header-rows: 1
   :align: left
   :widths: 10, 40, 65

   * - Prefix
     - Entities
     - Examples
   * - ``cc_``
     - functions, modules
     - :c:`cc_ply.h`, :c:func:`cc_ply_append_new()`
   * - ``CC_``
     - :c:`#define`\s, :c:`const`\ants, :c:`enum` items
     - :c:`CC_OFF_BOARD_COORD`, :c:enumerator:`CC_PLTE_FailedTeleportationOblation`
   * - ``Cc``
     - :c:`enum`\s, :c:`struct`\s, :c:`union`\s
     - :c:enum:`CcPlyLinkTypeEnum`, :c:struct:`CcPly`

No particular
`naming convention <https://en.wikipedia.org/wiki/Naming_convention_(programming)#Examples_of_multiple-word_identifier_formats>`_
is used.

Functions and modules names are lowercase words, separated by ``_`` (underscore),
e.g. :c:`cc_ply.h`, :c:func:`cc_ply_append_new()`.

:c:`#define` and :c:`const`\ant names are uppercase words, separated by ``_``
(underscore), e.g. :c:`CC_OFF_BOARD_COORD`, :c:`CC_SETUP_BOARD_CLASSICAL_CHESS`.

:c:`enum`\s, :c:`struct`\s and :c:`union`\s names are capitalized words. :c:`enum`
names end in ``Enum``, while there is no comparable name end for :c:`struct`\s,
:c:`union`\s. Examples, :c:enum:`CcPlyLinkTypeEnum`, :c:struct:`CcPly`.

:c:`enum` items have constant-like library prefix (i.e. ``CC_``), combined with
abbreviated :c:`enum` name (e.g. ``PPLE_``) and capitalized words for item name.
For instance, :c:enumerator:`CC_PLTE_FailedTeleportationOblation` would be one of items in
:c:enum:`CcPlyLinkTypeEnum`.

.. _lbl-libcc-organization-naming-linkedlists:

Linked lists
------------

Linked lists are described in concepts :ref:`lbl-libcc-concepts-linkedlists`.

:term:`Link`\s have ``Link`` appended to the name, usually of a base entity,
and also :c:`next` member, which points to next item in sequence.

For instance, for a simple :c:`struct CcPos;`, link would be:

.. code-block:: C
    :force:

    struct CcPosLink {
        CcPos pos; // Base entity.
        struct CcPosLink * next; // Next link in a linked list.
    }

Common functions linked lists have:

    - :c:`<linked_list>__new()`, allocates new link initialized with a given base item
    - :c:`<linked_list>_append()`, appends a single base item to linked list
    - :c:`<linked_list>_extend()`, extends linked list with another of the same type
    - :c:`<linked_list>_free_all()`, :c:func:`free()`\s all links in a linked list, and all other contained storage
    - :c:`<linked_list>_duplicate_all__new()`, allocates new, deep copy of a given linked list
    - :c:`<linked_list>_len()`, returns length of a given linked list

.. note::

    :c:func:`_append()` and :c:func:`_extend()` functions also work on empty linked list,
    i.e. even if pointer to linked list is :c:data:`NULL`.

.. note::

    :c:func:`_extend()` transfers ownership of the other linked list to the first given.

In our :c:struct:`CcPosLink` example, common definitions would be:

    - :c:`CcPosLink * cc_pos_link__new( CcPos pos );`
    - :c:`CcPosLink * cc_pos_link_append( CcPosLink ** list__iod_a, CcPos pos );`
    - :c:`CcPosLink * cc_pos_link_extend( CcPosLink ** list__iod_a, CcPosLink ** other__n );`
    - :c:`bool cc_pos_link_free_all( CcPosLink ** list__f );`
    - :c:`CcPosLink * cc_pos_link_duplicate_all__new( CcPosLink * list );`
    - :c:`size_t cc_pos_link_len( CcPosLink * list );`

.. seealso::

    :ref:`lbl-libcc-memory-management-parameters-transfer`

.. _lbl-libcc-organization-naming-organization:

Organization
------------

Pieces, tags, variants, chessboards introduced in
`the book <https://github.com/mmlacak/crochess/raw/master/crochess.pdf>`_ are in
similarly named :term:`module`\s, i.e. ``CcPieceType``, ``CcTagType``, ``cc_variant``,
``cc_chessboard``.

Each chessboard holds a board for pieces and the other one for tags.
Initial setups for those are in ``cc_setup_board`` and ``cc_setup_tags``,
misc setup function are in ``cc_setup_misc`` :term:`module`.

Various :term:`position`\s, :term:`step`\s and linked lists based on those are in
``cc_pos``, :term:`step` definitions are in ``cc_pos_defs``, position generators
are in ``cc_pos_gens``, and position utilities are in ``cc_pos_utils`` :term:`module`.

Parsers for user notation are in ``cc_parse_move``, ``cc_parse_ply``, ``cc_parse_step``,
``cc_parse_side_effect``, and helpers are in ``cc_parse_defs``, ``cc_parse_utils``
:term:`module`\s.

Parsed notation is stored in ``cc_move``, ``cc_parsed_ply``, ``cc_parsed_step``,
``cc_parsed_side_effect`` :term:`module`\s.

Parser messages are located in ``cc_parse_msg`` :term:`module`.

Game (i.e. its status, current chessboard, and list of moves played so far) can
be found in ``game`` :term:`module`.

House-keepers, conveniences are in:

- ``cc_define.h`` contains :c:`#define`\s used across :term:`module`\s
- ``cc_str_utils`` various string related functions
- ``cc_token`` primitive tokenizer for CLI commands issued by user
- ``cc_math`` math functions
- ``cc_version`` library version

Currently not used, or not finished:

- ``cc_strings`` linked list of strings, not used
- ``cc_rules`` not finished
- ``cc_rules_path`` not finished
- ``cc_rules_misc`` not finished

.. _lbl-libcc-organization-naming-auxiliaries:

Naming auxiliaries
------------------

:term:`Method` in this text is a function provided in the same :term:`module` as
:c:`enum`, :c:`struct`, :c:`union`, or other user-defined data type on which it
operates.

:term:`Method`\s are named after their :term:`entity`, so that they look like
:c:`cc_<entity>_<method_name>()`.

For instance, some :c:struct:`CcPos` methods are:

- :c:`bool cc_pos_is_congruent( CcPos pos_1, CcPos pos_2 );`
- :c:`CcPos cc_pos_add( CcPos pos, CcPos step, int count );`
- etc.

The same applies to macros defined for an entity, only their name is in upper-case.

In our :c:struct:`CcPos` example, some macros are:

- :c:`#define CC_POS_IS_VALID(pos) ...`
- :c:`#define CC_POS_IS_STATIC_STEP(pos) ...`
- :c:`#define CC_POS_IS_EQUAL(pos_1,pos_2) ...`
- etc.

.. note::

    Functions which require additional dependencies (beside what was used to
    define :term:`entity`) are not :term:`method`\s.

.. TODO

.. todo::

    Rewrite, example are now obsolete.

For instance, in ``cc_pos`` :term:`module` there is :c:`struct CcPathLink;`
linked list defined, which just chains :c:`struct CcPosDesc pd;`.

One of :term:`method`\s linked list has is

.. code-block:: C
    :force:

    CcPathLink * cc_path_link_append( CcPathLink ** pd_link__iod_a,
                                             CcPosDesc pd );

Note, that ``append()`` :term:`method` depends only on :term:`entity` used to
define said linked list, i.e. :c:struct:`CcPosDesc`.

This is very different from similarly named function defined in ``cc_pos_utils``:

.. code-block:: C
    :force:

    bool cc_append_pos_to_pos_desc_link( CcChessboard * cb,
                                         CcPos destination,
                                         cc_uint_t momentum,
                                         CcPathLink ** pdl__iod_a );

which depends on bunch of other stuff instead of linked list base :term:`entity`.

.. _lbl-libcc-organization-naming-versioning:

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
