.. Copyright (c) 2023, 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.
   Included source code files are licensed under GNU GPL v3+ license. See LICENSING, COPYING files in root folder of the project for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccchecks:

Checks
======

Documents ``cc_checks.h`` and ``cc_checks.c`` files, which contain various
checks.

.. _lbl-libcc-ccchecks-data:

Data
----

.. c:macro:: CC_CHECK_STEPS_NO_LIMIT

    Macro constant for check steps function(s), represents no limit on how many
    steps to check; equals to ``0``.

    Calculated positions still has to be on a chessboard.

.. _lbl-libcc-ccchecks-functions:

Functions
---------

.. c:function:: bool cc_check_valid_draw_offer_exists( CcMove * moves, CcGameStatusEnum gse )

    Function checks if valid draw offer exists.

    Function searches for last draw offer by an opponent, which is valid if it's still pending
    (i.e. not cancelled).

    :param moves: Moves played so far, a queue.
    :param gse: Game status, :c:type:`CcGameStatusEnum` value.
    :returns: :c:data:`true` if valid draw offer exists, :c:data:`false` otherwise.

.. _lbl-libcc-ccchecks-functions-piecechecks:

Piece checks
^^^^^^^^^^^^

.. c:function:: bool cc_check_piece_can_lose_tag( CcPieceTagType piece, CcLosingTagType ltt )

    Function checks if a piece can lose given tag.

    :param piece: A piece.
    :param ltt: :c:type:`CcLosingTagType` value.
    :returns: :c:data:`true` if piece can lose given tag, :c:data:`false` otherwise.

.. c:function:: bool cc_check_piece_is_blocked( CcPieceTagType moving, CcPieceTagType encounter, cc_uint_t momentum )

    Function checks if moving piece is blocked by encountered piece,
    and can't move any further.

    .. todo::

        * TODO :: DOCS
        * returned :c:data:`false` does not mean transparency can be used,
          since it's also used for invalid enums
        * add section in Concepts, describe single answer (bool) vs.
          full answer (maybe bool)

    :param moving: A moving piece.
    :param encounter: An encountered piece.
    :param momentum: Momentum a moving piece had when another piece was encountered.
    :returns: :c:data:`true` if encountered piece is blocking,
        :c:data:`false` otherwise, or in case of error (invalid data).

.. c:function:: bool cc_check_piece_can_step_over( CcPieceTagType moving, CcPieceTagType encounter, cc_uint_t momentum )

    Function checks if moving piece can step over encountered piece.

    .. todo::

        * TODO :: DOCS
        * returned :c:data:`false` does not mean moving piece is blocked,
          since it's also used for invalid enums
        * add section in Concepts, describe single answer (bool) vs.
          full answer (maybe bool)

    :param moving: A moving piece.
    :param encounter: An encountered piece.
    :param momentum: Momentum a moving piece had when another piece was encountered.
    :returns: :c:data:`true` if encountered piece can be step over,
        :c:data:`false` otherwise, or in case of error (invalid data).

.. c:function:: bool cc_check_piece_can_capture( CcPieceTagType moving, CcPieceTagType encounter )

    Function checks if a moving piece can capture encountered piece.

    All pieces that can capture also use momentum for movement.
    Momentum is not checked because capture can be done with no momentum.

    :param moving: A moving piece.
    :param encounter: An encountered piece.
    :returns: :c:data:`true` if encountered piece can be captured, :c:data:`false` otherwise.

.. c:function:: bool cc_check_piece_can_activate( CcPieceTagType moving, CcPieceTagType encounter, cc_uint_t momentum, CcStepTypeEnum step_type )

    Function checks if moving piece can activate stationary one, given
    :c:var:`momentum` and :c:var:`step_type` arguments.

    :param moving: A moving piece.
    :param encounter: A static, encountered piece.
    :param momentum: Momentum.
    :param step_type: Type of an activation step, e.g. to differentiate between capture-step and just movement.
    :returns: :c:data:`true` if moving piece can activate encountered one, :c:data:`false` otherwise.

.. _lbl-libcc-ccchecks-functions-positionalchecks:

Positional checks
^^^^^^^^^^^^^^^^^

.. c:function:: bool cc_check_piece_is_blocked_at( CcChessboard * cb, CcPieceTagType moving, CcActivationDesc act_desc, bool is_first_ply, CcPos pos )

    Function checks if piece is blocked at given position.

    :param cb: A chessboard.
    :param moving: A moving piece.
    :param act_desc: An activation descriptor.
    :param is_first_ply: Flag, if current ply is first in a cascade.
    :param pos: A position.
    :returns: :c:data:`true` if piece is blocked at given position, :c:data:`false` otherwise.
    :seealso: :c:func:`cc_check_piece_is_blocked()`

.. c:function:: bool cc_check_piece_can_capture_at( CcChessboard * cb, CcPieceTagType moving, CcPos pos )

    Function checks if a piece can capture at given position.

    All pieces that can capture also use momentum for movement.
    Momentum is not checked because capture can be done with no momentum.

    :param cb: A chessboard.
    :param moving: A moving piece.
    :param pos: A position.
    :returns: :c:data:`true` if a piece can capture at given position, :c:data:`false` otherwise.
    :seealso: :c:func:`cc_check_piece_can_capture()`

.. c:function:: bool cc_check_piece_can_activate_at( CcChessboard * cb, CcPieceTagType moving, CcActivationDesc act_desc, bool is_first_ply, CcPos destination, CcStepTypeEnum step_type )

    Function checks if moving piece can activate piece encountered at :c:var:`destination`.

    :param cb: Current chessboard.
    :param moving: A moving piece.
    :param act_desc: An activation descriptor.
    :param is_first_ply: Flag, if current ply is first in a cascade.
    :param destination: Destination field.
    :param step_type: Type of an activation step, e.g. to differentiate between capture-step and just movement.
    :returns: :c:data:`true` if moving piece can activate encountered piece, :c:data:`false` otherwise.
    :seealso: :c:func:`cc_check_piece_can_activate()`

.. c:function:: bool cc_check_piece_can_diverge_at( CcChessboard * cb, CcPieceTagType moving, cc_uint_t momentum, CcPieceTagType activator, CcPos pos )

    Function checks if a piece can diverge from given position.

    :param cb: A chessboard.
    :param moving: A moving piece.
    :param momentum: Momentum.
    :param activator: An :term:`activator`.
    :param pos: A position.
    :returns: :c:data:`true` if a piece can diverge from given position, :c:data:`false` otherwise.

.. c:function:: bool cc_check_castling_step_fields( CcChessboard * cb, CcPos king_start, CcPos king_dest, CcPos rook_start, CcPos rook_dest )

    Function checks if pieces can castle from their given positions.

    :param cb: A chessboard.
    :param king_start: King's initial position.
    :param king_dest: King's destination field after castling.
    :param rook_start: Rook's initial position.
    :param rook_dest: Rook's destination field after castling.
    :returns: :c:data:`true` if pieces can castle from given position, :c:data:`false` otherwise.

.. c:function:: bool cc_find_en_passant_target( CcChessboard * cb, CcPieceTagType capturing, CcActivationDesc act_desc, bool is_first_ply, CcPos destination, CcPosDesc * target__o )

    Function finds a private to be captured by en passant, its location and tag.

    .. note::

        Function assumes that it's called when :c:var:`capturing` private is "in-the-air",
        but all other pieces are still in their original positions, including encountered piece
        at :c:var:`destination` capture-field, and target (i.e. to be captured) private.

    :param cb: Current chessboard.
    :param capturing: A moving private, capturing en passant.
    :param act_desc: An activation descriptor.
    :param is_first_ply: Flag, if current ply is first in a cascade.
    :param destination: Destination of a :c:var:`private`, where activation takes place.
    :param target__o: An *output*; target private to be captured en passant, its position and tag; if found.
    :returns: :c:data:`true` if target private was found, :c:data:`false` otherwise.
    :seealso: :c:func:`cc_check_piece_can_activate_at()`

.. c:function:: bool cc_find_first_piece( CcChessboard * cb, CcPieceTagType piece, CcPos start, CcPos step, bool ignore_tags, CcPosDesc * found__o )

    Function searches for a given :c:var:`piece`, starting from :c:var:`start` position,
    and advancing in :c:var:`step` direction.

    Starting position is not checked.

    Piece and its position, if found, is returned via *output* parameter :c:var:`found__o`.

    Given and encountered pieces are compared exactly (tags are significant) if :c:var:`ignore_tags`
    is :c:data:`false`, otherwise tags are stripped and only base piece types are compared.

    For instance, function will find :c:enumerator:`CC_PTE_LightKing_CanCastle` for a given
    :c:enumerator:`CC_PTE_LightKing` only if :c:var:`ignore_tags` was :c:data:`true`.

    :param cb: A chessboard.
    :param piece: A piece to find.
    :param start: Search starting position.
    :param step: A step.
    :param ignore_tags: A flag, whether tags are compared, or stripped.
    :param found__o: An *output*; piece and its position, if found.
    :returns: :c:data:`true` if piece was found, :c:data:`false` otherwise.

.. _lbl-libcc-ccchecks-sourcecodeheader:

Header file
-----------

Included source header file is ``cc_checks.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_checks.h
    :language: C
    :linenos:

.. _lbl-libcc-ccchecks-sourcecodefile:

Source code file
----------------

Included source code file is ``cc_checks.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_checks.c
    :language: C
    :linenos:
