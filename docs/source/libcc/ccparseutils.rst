.. Copyright (c) 2021, 2022, 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.
   Included source code files are licensed under GNU GPL v3+ license. See LICENSING, COPYING files in root folder of the project for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccparseutils:

Parse utils
===========

Documents ``cc_parse_utils.h`` and ``cc_parse_utils.c`` files, which contain
various definitions and functions.

.. _lbl-libcc-ccparseutils-data:

Parse utils data
----------------

Constants, and macros to check ply gather :c:`char`\s, step separator, or
piece symbols.

Plies are gathered between :c:`[` and :c:`]` chars.

Step separators are :c:`.` and :c:`-` chars.

Piece symbol is uppercase :c:`char`, as used in :term:`AN`.

.. c:macro:: CC_MAX_LEN_STEP_POS_AN

    Maximum length (count of :c:`char`\s) step (position) in :term:`AN` can have;
    equal to ``3``.

    Positions have `file` (lowercase :c:`char`) and `rank` (maximum two digits),
    e.g. ``r24``.

.. c:macro:: CC_MAX_LEN_DISAMBIGUATION

    Maximum length (count of :c:`char`\s) disambiguation in :term:`AN` can have;
    equal to ``3``.

    Disambiguations can have the same composition as steps (positions),
    e.g. in ``r24s25`` first part (``r24``) is disambiguation, the rest is
    position (``s25``).

.. c:macro:: CC_MAX_LEN_DISAMBIGUATION_STEP

    Maximum length (count of :c:`char`\s) disambiguation followed by a step can
    have  in :term:`AN`; equal to ``6``.

.. c:macro:: CC_CHAR_IS_PLY_GATHER_START(char_c)

    Macro to check if :c:`char` is ply gather start.

    :param char_c: A :c:`char`.
    :returns: :c:data:`true` if given :c:`char` is :c:`[`, :c:data:`false` otherwise.

.. c:macro:: CC_CHAR_IS_PLY_GATHER_END(char_c)

    Macro to check if :c:`char` is ply gather end.

    :param char_c: A :c:`char`.
    :returns: :c:data:`true` if given :c:`char` is :c:`]`, :c:data:`false` otherwise.

.. c:macro:: CC_CHAR_IS_PLY_GATHER(char_c)

    Macro to check if :c:`char` is ply gather.

    :param char_c: A :c:`char`.
    :returns: :c:data:`true` if given :c:`char` is either :c:`[` or :c:`]`,
              :c:data:`false` otherwise.

.. c:macro:: CC_CHAR_IS_STEP_SEPARATOR(char_c)

    Macro to check if :c:`char` is step separator.

    :param char_c: A :c:`char`.
    :returns: :c:data:`true` if given :c:`char` is either :c:`.` or :c:`-`,
              :c:data:`false` otherwise.

.. c:macro:: CC_CHAR_IS_PIECE_SYMBOL(char_c)

    Macro to check if :c:`char` is piece symbol.

    :param char_c: A :c:`char`.
    :returns: :c:data:`true` if given :c:`char` is piece symbol, :c:data:`false` otherwise.

.. _lbl-libcc-ccparseutils-functions:

Parse utils functions
---------------------

.. c:function:: bool cc_parse_ply_link( char const * an_str, CcParsedPlyLinkEnum * ple__o )

    Function returns ply link from notation.

    :param an_str: Notation; zero-terminated, :term:`AN` string.
    :param ple__o: _Output_, ply link, i.e. :c:enum:`CcParsedPlyLinkEnum` value.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. c:function:: size_t cc_ply_link_len( CcParsedPlyLinkEnum ple )

    Function returns length in :c:`char`\s of a ply link as used in notation.

    :param ple: Ply link, i.e. :c:enum:`CcParsedPlyLinkEnum` value.
    :returns: Length if given ply link was valid, ``0`` otherwise.

.. c:function:: char const * cc_next_ply_link( char const * an_str )

    Function returns pointer to next ply link in a given notation.

    If there is no next ply link, function returns pointer to first found ``'\0'``,
    i.e. zero-terminating :c:`char` of a given string.

    :param an_str: Notation; zero-terminated, :term:`AN` string.
    :returns: Pointer to next ply link if successful, :c:data:`NULL` otherwise.

.. c:function:: bool cc_iter_ply( char const * an_str, char const ** start__io, char const ** end__io )

    Function iterates over plies in a given notation string.

    .. note::

        Both inner pointers, i.e. :c:`*start__io` and :c:`*end__io` **must** be
        reset to :c:data:`NULL` before iterating plies.

    After each call, function returns :c:data:`true` if next ply is found, and
    sets start, end pointers to it.

    Once all plies are exhausted, function returns :c:data:`false`, and resets
    start, end pointers to :c:data:`NULL`.

    Typical usage:

    .. code-block:: C
        :force:

        // Start, end of ply algebraic notation.
        char const * ply_start_an = NULL;
        char const * ply_end_an = NULL;
        // Note: both pointers must be set to NULL before iterating plies.

        // move_an here is just a zero-terminated string, containing complete user move notation.
        while ( cc_iter_ply( move_an, &ply_start_an, &ply_end_an ) ) {
            // Do stuff with found ply, pointed by ply_start_an and ply_end_an ...
        }

    :param an_str: Notation; zero-terminated, :term:`AN` string.
    :param start__io: *Input/output*; start of a found ply.
    :param end__io: *Input/output*; end of a found ply.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.



.. _lbl-libcc-ccparseutils-sourcecodeheader:

Parse utils source code header
------------------------------

Included source code file is ``cc_parse_utils.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_parse_utils.h
    :language: C
    :linenos:

.. _lbl-libcc-ccparseutils-sourcecodefile:

Parse utils source code file
----------------------------

Included source code file is ``cc_parse_utils.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_parse_utils.c
    :language: C
    :linenos:
