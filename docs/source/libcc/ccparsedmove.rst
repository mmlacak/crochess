.. Copyright (c) 2021, 2022, 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.
   Included source code files are licensed under GNU GPL v3+ license. See LICENSING, COPYING files in root folder of the project for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccparsedmove:

Parsed move
===========

Documents ``cc_parsed_move.h`` and ``cc_parsed_move.c`` files, which contain various
parsed move definitions and functions.

.. _lbl-libcc-ccparsedmove-data:

Data
----

.. c:enum:: CcMoveStatusEnum

    Move status enumeration, after a valid movement.

    .. c:enumerator:: CC_MSE_None

        No status.

    .. c:enumerator:: CC_MSE_DrawOffer

        Player offered a draw.

    .. c:enumerator:: CC_MSE_DrawOffer_Revoked

        Player took back draw offer.


    .. c:enumerator:: CC_MSE_Check

        Checking opponent.

    .. c:enumerator:: CC_MSE_Check_DrawOffer

        Checking opponent, player offered a draw.

    .. c:enumerator:: CC_MSE_Check_DrawOffer_Revoked

        Checking opponent, player took back draw offer.


    .. c:enumerator:: CC_MSE_Checkmate

        Opponent checkmated.

    .. c:enumerator:: CC_MSE_SelfCheckmate

        Opponent checkmated self, game ended.


    .. c:enumerator:: CC_MSE_Resign

        Player resigned, game ended.

    .. c:enumerator:: CC_MSE_DrawAccepted

        Player accepted draw offer, game ended.

    .. c:enumerator:: CC_MSE_DrawByRules

        Game was drawn by rules, game ended.

    :c:`enum` is tagged with the same :c:enum:`CcMoveStatusEnum` name.

.. c:struct:: CcMove

    Parsed move :c:`struct`\ure, queue.

    .. c:member:: char * notation

        Original notation, before parsing. Usually, from user input.

    .. c:member:: CcParsedPly * plies

        Plies.

    .. c:member:: CcMoveStatusEnum status

        Status.

    .. c:member:: struct CcMove * prev__w

        Weak pointer to previous move.

    .. c:member:: struct CcMove * next

        Next move in a queue.

    :c:`struct` is tagged with the same :c:struct:`CcMove` name.

.. _lbl-libcc-ccparsedmove-functions:

Functions
---------

.. c:function:: CcMove * cc_move__new( char const * notation, size_t max_len__d, CcParsedPly ** plies__d_n, CcMoveStatusEnum status )

    Returns newly allocated move.

    Takes ownership of :c:`plies__d_n`, inner pointer will be set to :c:data:`NULL`,
    if valid move is produced.

    If no valid move is produced, :c:`plies__d_n` is still valid,
    and accessible.

    :param notation: Original notation, as received from a user.
    :param max_len__d: *Optional*, maximum length of :c:`notation` to copy, can be :c:macro:`CC_MAX_LEN_ZERO_TERMINATED`.
    :param plies__n: **Ownership transfer**, *optional*; plies linked list, inner pointer can be :c:data:`NULL`.
    :param status: Move status.
    :returns: A newly allocated move if successful, :c:data:`NULL` otherwise.

.. c:function:: CcMove * cc_move_append( CcMove ** moves__iod_a, char const * notation, size_t max_len__d, CcParsedPly ** plies__d_n, CcMoveStatusEnum status )

    Appends a newly allocated move to a given queue.

    Takes ownership of :c:`plies__d_n`, inner pointer will be set to :c:data:`NULL`,
    if valid move is produced.

    If no valid move is produced, :c:`plies__d_n` is still valid,
    and accessible.

    :param moves__iod_a: **Ownership**, *optional* *input/output* parameter; queue of moves,
                         to which a new move is appended, inner pointer can be :c:data:`NULL`.
    :param notation: Original notation, as received from a user.
    :param max_len__d: *Optional*, maximum length of :c:`notation` to copy, can be :c:macro:`CC_MAX_LEN_ZERO_TERMINATED`.
    :param plies__n: **Ownership transfer**, *optional*; plies linked list, inner pointer can be :c:data:`NULL`.
    :param status: Move status.
    :returns: Weak pointer to a newly allocated move if successful, :c:data:`NULL` otherwise.

.. c:function:: CcMove * cc_move_duplicate_all__new( CcMove * moves )

    Duplicates a given :c:`moves` queue, and all accompanying resources,
    into a newly allocated queue.

    :param moves: A queue to duplicate.
    :returns: A newly allocated duplicate of :c:`moves` if successful, :c:data:`NULL` otherwise.

.. c:function:: bool cc_move_free_all( CcMove ** moves__f )

    Frees all moves in a queue, and all associated entities.

    :param moves__f: A queue to :c:func:`free()`.
    :returns: A newly allocated duplicate of :c:`moves` if successful, :c:data:`NULL` otherwise.

.. c:function:: size_t cc_move_plies_count( CcMove * move )

    Function returns count of plies owned by a given move.

    :param move: A move.
    :returns: Count of plies if successful, ``0`` otherwise.

.. _lbl-libcc-ccparsedmove-notations:

Notations
---------

Game score consists of numbered list of :term:`cycle`\s, each :term:`cycle` in one
line with its own index, light player move followed by dark player move, like so:

    .. code-block:: text
        :force:

        1. <light player move #1> <dark player move #1>
        2. <light player move #2> <dark player move #2>
        3. <light player move #3> ...

Notations list is a simple dump of all notations from parsed moves, alternating
between light player and dark player moves, where each move is placed verbatim on
its own line, like so:

    .. code-block:: text
        :force:

        <light player move #1>
        <dark player move #1>
        <light player move #2>
        <dark player move #2>
        <light player move #3>

.. c:function:: size_t cc_move_all_notations_size( CcMove * move, bool is_score )

    Function returns size of all notation strings taken together, with optional
    formatting, if game score flag was given.

    :param move: A move.
    :param is_score: Flag, whether notation should be formatted as game score (if
        :c:data:`true`), or as a simple move list (if :c:data:`false`).
    :returns: Formatted string size if successful, ``0`` otherwise.

.. c:function:: char * cc_move_as_string__new( CcMove * move, bool is_score )

    Function returns a newly allocated string, containing notations from all
    parsed moves in a given linked list, with optional formatting.

    :param move: A move.
    :param is_score: Flag, whether notation should be formatted as game score (if
        :c:data:`true`), or as a simple move list (if :c:data:`false`).
    :returns: Valid pointer to newly allocated string if successful, :c:data:`NULL`
        otherwise.

.. _lbl-libcc-ccparsedmove-sourcecodeheader:

Header file
-----------

Included source header file is ``cc_parsed_move.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_parsed_move.h
    :language: C
    :linenos:

.. _lbl-libcc-ccparsedmove-sourcecodefile:

Source code file
----------------

Included source code file is ``cc_parsed_move.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_parsed_move.c
    :language: C
    :linenos:
