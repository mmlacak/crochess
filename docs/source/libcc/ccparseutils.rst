.. Copyright (c) 2021, 2022, 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.
   Included source code files are licensed under GNU GPL v3+ license. See LICENSING, COPYING files in root folder of the project for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccparseutils:

Parse utilities
===============

Documents ``cc_parse_utils.h`` and ``cc_parse_utils.c`` files, which contain
various definitions and functions.

.. _lbl-libcc-ccparseutils-data:

Data
----

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

.. _lbl-libcc-ccparseutils-functions:

Functions
---------

.. c:function:: bool cc_parse_ply_link( char const * an_str, CcPlyLinkTypeEnum * ple__o )

    Function returns ply link from ply notation.

    :param ply_an_str: Ply notation, points at ply link;
        zero-terminated, :term:`AN` string.
    :param ple__o: _Output_, ply link, i.e. :c:enum:`CcPlyLinkTypeEnum` value.
    :returns: Valid :c:enum:`CcPlyLinkEnum` value if ply link found,
        :c:enumerator:`CC_PLTE_StartingPly` if not,
        :c:enumerator:`CC_PLTE_None` in case of error.

.. c:function:: size_t cc_ply_link_len( CcPlyLinkTypeEnum plte )

    Function returns length in :c:`char`\s of a ply link as used in notation.

    :param plte: Ply link, i.e. :c:enum:`CcPlyLinkTypeEnum` value.
    :returns: Length if given ply link was valid, ``0`` otherwise.

.. c:function:: bool cc_is_ply_link_char( char const c )

    Function checks if given :c:`char` is used for ply link notation.

    :param c: A :c:`char`\acter.
    :returns: :c:data:`true` if ply link :c:`char`, :c:data:`false` otherwise.

.. c:function:: char const * cc_next_ply_link( char const * ply_an_str )

    Function returns pointer to next ply link in a given ply notation.

    If there is no next ply link, function returns pointer to first found ``'\0'``,
    i.e. zero-terminating :c:`char` of a given string.

    :param ply_an_str: Ply notation, points at ply link;
        zero-terminated, :term:`AN` string.
    :returns: Pointer to next ply link if successful, :c:data:`NULL` otherwise.

.. c:function:: bool cc_iter_ply( char const * move_an_str, char const ** start__io, char const ** end__io )

    Function iterates over plies in a given move notation string.

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

    :param move_an_str: Move notation, point at the vert start;
        zero-terminated, :term:`AN` string.
    :param start__io: *Input/output*; start of a found ply.
    :param end__io: *Input/output*; end of a found ply.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. c:function:: bool cc_fetch_piece_symbol( char const * piece_an, char * piece_symbol__o, bool default_to_pawn, bool return_validity )

    Function checks piece symbol in given notation, and outputs findings via
    *output* parameter.

    If there is no piece symbol (upper-case :c:`char`\acter), depending on
    :c:`default_to_pawn` flag, function outputs ``'P'``, or ``' '``.

    Depending on :c:`return_validity` flag, function also returns if found
    piece symbol is valid.

    :param piece_an: Notation, points at piece symbol;
        zero-terminated, :term:`AN` string.
    :param piece_symbol__o: *Output*; pointer to piece symbol :c:`char`.
    :param default_to_pawn: Flag, if there is no upper-case :c:`char`, should
        this be interpreted as Pawn.
    :param return_validity: Flag, if function also checks and returns validity
        of found piece symbol.
    :returns: :c:data:`true` if successful (and/or found piece symbol valid),
        :c:data:`false` otherwise.

.. c:function:: CcLosingTagType cc_parse_losing_tag( char const * lt_an_str )

    Function returns losing tag found in a given notation.

    :param lt_an_str: Notation, points at losing tag;
        zero-terminated, :term:`AN` string.
    :returns: Losing tag, :c:type:`CcLosingTagType` value.

.. c:function:: size_t cc_losing_tag_len( CcLosingTagType ltt )

    Function returns length in :c:`char`\s of a losing tag as used in notation.

    :param ltt: Losing tag, i.e. :c:type:`CcLosingTagType` value.
    :returns: Length if given losing tag was valid, ``0`` otherwise.

.. c:function:: bool cc_convert_coords( char const * pos_an_str, int * file__o, int * rank__o )

    Function returns coordinates parsed from a positional notation via
    *output* parameters.

    :param pos_an_str: Notation, points at positional notation;
        zero-terminated, :term:`AN` string.
    :param file__o: *Output*; pointer to horizontal coordinate storage.
    :param rank__o: *Output*; pointer to vertical coordinate storage.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. c:function:: bool cc_convert_pos( char const * pos_an_str, CcPos * pos__o )

    Convenience wrapper for :c:func:`cc_convert_coords()`; function returns
    coordinates parsed from a positional notation via *output* parameter.

    :param pos_an_str: Notation, points at positional notation;
        zero-terminated, :term:`AN` string.
    :param pos__o: *Output*; pointer to coordinates storage.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. c:function:: bool cc_parse_pos( char const * an_str, CcPos * pos__o, char const ** pos_end__o )

    Function returns coordinates parsed from a positional notation, and pointer
    to end of positional notation, via *output* parameters.

    End of positional notation is the first :c:`char` after the positional
    notation, i.e. the one that does not belong to the positional notation.

    .. note::

        Inner pointer :c:`*pos_end__o` **must** be reset to :c:data:`NULL`
        before parsing coordinates.

    :param pos_an_str: Notation, points at positional notation;
        zero-terminated, :term:`AN` string.
    :param pos__o: *Output*; pointer to coordinates storage.
    :param pos_end__o: *Output*; pointer to the end of positional notation.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. c:function:: char const * cc_skip_disambiguation( char const * pos_an_str )

    Function returns pointer to end of disambiguation, i.e. pointer to first
    :c:`char` which does not belong to the disambiguation.

    Disambiguation includes both partial and complete initial positions.

    :param pos_an_str: Notation, points at positional notation;
        zero-terminated, :term:`AN` string.
    :returns: Valid pointer if disambiguation has been skipped,
        :c:data:`NULL` otherwise.

.. c:function:: bool cc_ply_has_separated_steps( char const * ply_an_str, char const * ply_end, bool check_intermediate_steps, bool check_destination_step )

    Function checks if ply contains step separators, either ``'.'``, or ``'-'``.

    At least one of flags :c:`check_intermediate_steps`,
    :c:`check_destination_step` has to be :c:data:`true` before searching for
    step separators.

    :param ply_an_str: Ply notation, points at the start of the ply;
        zero-terminated, :term:`AN` string.
    :param ply_end: Ply notation, points at the end of the ply;
        zero-terminated, :term:`AN` string.
    :param check_intermediate_steps: Flag, to check if ply contains ``'.'``.
    :param check_destination_step: Flag, to check if ply contains ``'-'``.
    :returns: :c:data:`true` if ply contains step separators,
        :c:data:`false` otherwise.

.. c:function:: bool cc_parse_step_link( char const * step_an_str, char const * ply_end, CcStepLinkTypeEnum * sle__o )

    Function returns parsed step link via *output* parameter.

    :param step_an_str: Step notation, points at the very start;
        zero-terminated, :term:`AN` string.
    :param ply_end: Ply notation, points at the very end of the ply;
        zero-terminated, :term:`AN` string.
    :param sle__o: *Output*; pointer to step link storage.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. c:function:: size_t cc_step_link_len( CcStepLinkTypeEnum sle )

    Function returns length in :c:`char`\s of a step link as used in notation.

    :param sle: Step link, i.e. :c:enum:`CcStepLinkTypeEnum` value.
    :returns: Length if given step link was valid, ``0`` otherwise.

.. c:function:: char const * cc_next_step_link( char const * step_an_str, char const * ply_end )

    Function returns pointer to next step link found in notation.

    If there is no next step link, function returns :c:`ply_end` pointer.

    :param step_an_str: Step notation, points at the start;
        zero-terminated, :term:`AN` string.
    :param ply_end: Ply notation, points at the end of the ply;
        zero-terminated, :term:`AN` string.
    :returns: Pointer to next step link if successful, :c:data:`NULL` otherwise.

.. c:function:: bool cc_iter_step( char const * ply_an_str, char const * ply_end, char const ** start__io, char const ** end__io )

    Function iterates over steps in a given ply notation string.

    .. note::

        Both inner pointers, i.e. :c:`*start__io` and :c:`*end__io` **must** be
        reset to :c:data:`NULL` before iterating steps.

    After each call, function returns :c:data:`true` if next step is found, and
    sets start, end pointers to it.

    Once all steps are exhausted, function returns :c:data:`false`, and resets
    start, end pointers to :c:data:`NULL`.

    Typical usage:

    .. code-block:: C
        :force:

        // Start, end of step algebraic notation.
        char const * step_start_an = NULL;
        char const * step_end_an = NULL;
        // Note: both pointers must be set to NULL before iterating steps.

        // ply_an, ply_end here are just zero-terminated strings, delimiting ply notation.
        while ( cc_iter_step( ply_an, ply_end, &step_start_an, &step_end_an ) ) {
            // Do stuff with found step, pointed by step_start_an and step_end_an ...
        }

    :param ply_an_str: Ply notation, point at the vert start;
        zero-terminated, :term:`AN` string.
    :param ply_end: Ply notation, points at the end of the ply;
        zero-terminated, :term:`AN` string.
    :param start__io: *Input/output*; start of a found ply.
    :param end__io: *Input/output*; end of a found ply.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. c:function:: CcSideEffectTypeEnum cc_parse_side_effect_type( char const * step_an_str, bool * has_promotion_sign__o )

    Function returns side-effect type found in a given step notation.

    :param step_an_str: Step notation, points at side-effect;
        zero-terminated, :term:`AN` string.
    :param has_promotion_sign__o: *Output*; pointer to promotion sign flag storage.
    :returns: Side-effect type, :c:enum:`CcSideEffectTypeEnum` value.

.. c:function:: size_t cc_side_effect_type_len( CcSideEffectTypeEnum see, bool has_promotion_sign )

    Function returns length in :c:`char`\s of a side-effect type as used in notation.

    :param see: Side-effect type, :c:enum:`CcSideEffectTypeEnum` value.
    :param has_promotion_sign: Flag, whether promotion sign was present in notation.
    :returns: Length if given side-effect type was valid, ``0`` otherwise.

.. _lbl-libcc-ccparseutils-sourcecodeheader:

Header file
-----------

Included source header file is ``cc_parse_utils.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_parse_utils.h
    :language: C
    :linenos:

.. _lbl-libcc-ccparseutils-sourcecodefile:

Source code file
----------------

Included source code file is ``cc_parse_utils.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_parse_utils.c
    :language: C
    :linenos:
