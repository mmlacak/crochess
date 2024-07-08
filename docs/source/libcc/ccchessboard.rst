.. Copyright (c) 2021, 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.
   Included source code files are licensed under GNU GPL v3+ license. See LICENSING, COPYING files in root folder of the project for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccchessboard:

Chessboard
==========

Documents ``cc_chessboard.h`` and ``cc_chessboard.c`` files, which contain
chessboard definitions and functions.

.. _lbl-libcc-ccchessboard-data:

Chessboard data
---------------

.. c:type:: char const CC_CHESSBOARD_SEPARATORS_SETUP_FROM_STRING[]

    Separators constant, used to tokenize chessboard setup string.

.. _lbl-libcc-ccchessboard-types:

Chessboard types
----------------

.. c:struct:: CcChessboard

    Chessboard :c:`struct`\ure, used for all variants.

    .. c:member:: CcVariantEnum type

        Chess variant to play.

    .. c:member:: uint size

        Actual size of a board used for a given variant.

    .. c:member:: CcPieceEnum board[ CC_VARIANT_BOARD_SIZE_MAXIMUM ][ CC_VARIANT_BOARD_SIZE_MAXIMUM ]

        Holds pieces.

    .. c:member:: CcTagEnum tags[ CC_VARIANT_BOARD_SIZE_MAXIMUM ][ CC_VARIANT_BOARD_SIZE_MAXIMUM ]

        Holds tags for pieces at their respective position.

    :c:`struct` is tagged with the same :c:expr:`CcChessboard` name.

.. _lbl-libcc-ccchessboard-functions:

Chessboard functions
--------------------

.. c:function:: CcChessboard * cc_chessboard__new( CcVariantEnum ve, bool do_setup )

    Function returns a newly allocated chessboard, optionally initialized
    for a given variant.

    :param ve: Variant to play.
    :param do_setup: Whether to set-up pieces to their initial positions.
                     If :c:`false`, chessboard returned is empty.
    :returns: A newly allocated chessboard if successful, :c:`NULL` otherwise.

.. c:function:: bool cc_chessboard_init( CcChessboard * cb__io, CcVariantEnum ve, bool do_setup )

    (Re-)initializes a chessboard with initial setup, for a given variant.

    :param cb__io: *Input/output* parameter, chessboard to (re-)initialize.
    :param ve: Variant to play.
    :param do_setup: Whether to set-up pieces to their initial positions.
                     If :c:`false`, chessboard returned is empty.
    :returns: :c:`true` if chessboard is successfully (re-)initialized,
              :c:`false` otherwise.

.. c:function:: bool cc_chessboard_is_size_valid( CcChessboard * cb )

    Checks if chessboard size is valid, i.e. the same as defined for
    variant being played.

    Actual board size cannot be tested, 2D array is allocated only once,
    at the size of the largest variant.

    Chessboard size just limits which portion of board is used for a
    particular variant.

    :param cb: Chessboard to check.
    :returns: :c:`true` if chessboard size is valid, :c:`false` otherwise.

.. c:function:: bool cc_chessboard_clear( CcChessboard * cb__io )

    Clears a chessboard of all pieces.

    :param cb__io: *Input/output* parameter, chessboard to clear.
    :returns: :c:`true` if chessboard is successfully cleared,
              :c:`false` otherwise.

.. c:function:: bool cc_chessboard_setup( CcChessboard * cb__io )

    Sets up pieces on a chessboard to their initial positions.

    :param cb__io: *Input/output* parameter, chessboard to set up.
    :returns: :c:`true` if chessboard is successfully set up,
              :c:`false` otherwise.

.. c:function:: bool cc_chessboard_copy( CcChessboard * into__io, CcChessboard * from )

    Copies a chessboard to another one.

    :param into__io: *Input/output* parameter, chessboard to copy into.
    :param from: Chessboard to copy from.
    :returns: :c:`true` if chessboard is successfully copied,
              :c:`false` otherwise.

.. c:function:: CcChessboard * cc_chessboard_duplicate__new( CcChessboard * from )

    Function returns a newly allocated chessboard, with all data copied
    from a given chessboard.

    :param from: Chessboard to copy from.
    :returns: A newly allocated chessboard if successful, :c:`NULL` otherwise.

.. c:function:: bool cc_chessboard_free_all( CcChessboard ** cb__f )

    Deallocates chessboard, and all used resources.

    :param cb__f: Chessboard to deallocate.
    :returns: :c:`true` if successful, :c:`false` otherwise.

.. c:function:: bool cc_chessboard_is_coord_on_board( CcChessboard * cb, int coord )

    Function returning if given coordinate is on board, for a current variant.

    All board for all variants are squares, so files and ranks always have the same range.

    :param cb: A chessboard.
    :param coord: A coordinate to check.
    :returns: :c:`true` if coordinate is on-board, :c:`false` otherwise.

.. c:function:: bool cc_chessboard_is_pos_on_board( CcChessboard * cb, int i, int j )

    Function returning if given position belongs to a board.

    :param cb: A chessboard.
    :param i: File, position along horizontal axis.
    :param j: Rank, position along vertical axis.
    :returns: :c:`true` if position is on-board, :c:`false` otherwise.

.. c:function:: bool cc_chessboard_is_disambiguation_on_board( CcChessboard * cb, int i, int j )

    Function returning if at least one coordinate of given disambiguation
    belongs to a board.

    Disambiguation is a position which can have one invalid coordinate,
    i.e. equal to :c:expr:`CC_INVALID_COORD`.

    :param cb: A chessboard.
    :param i: File, position along horizontal axis.
    :param j: Rank, position along vertical axis.
    :returns: :c:`true` if disambiguation is on-board, :c:`false` otherwise.

.. c:function:: bool cc_chessboard_is_coord_safe_off_board( CcChessboard * cb, int coord )

    Function checks if given coordinate might be within safe off-board boundaries.

    Coordinate is safely off-board if there could be a movement (e.g. trance-journey),
    which would place piece back on-board.

    :param cb: A chessboard.
    :param coord: A coordinate to check.
    :returns: :c:`true` if coordinate might be safely off-board, :c:`false` otherwise.

.. c:function:: bool cc_chessboard_is_pos_safe_off_board( CcChessboard * cb, int i, int j )

    Function returning if given position might be within safe off-board boundaries.

    Position is safely off-board if there could be a movement (e.g. trance-journey),
    which would place piece back on-board.

    :param cb: A chessboard.
    :param i: File, position along horizontal axis.
    :param j: Rank, position along vertical axis.
    :returns: :c:`true` if position might be safely off-board, :c:`false` otherwise.

.. c:function:: bool cc_chessboard_is_disambiguation_safe_off_board( CcChessboard * cb, int i, int j )

    Function returning if at least one coordinate of given disambiguation might be
    within safe off-board boundaries.

    Position is safely off-board if there could be a movement (e.g. trance-journey),
    which would place piece back on-board.

    :param cb: A chessboard.
    :param i: File, position along horizontal axis.
    :param j: Rank, position along vertical axis.
    :returns: :c:`true` if disambiguation might be safely off-board, :c:`false` otherwise.

.. c:function:: bool cc_chessboard_is_field_on_light_side( CcChessboard * cb, int j )

    Function returning if given position is on the light side of a chessboard.

    :param cb: A chessboard.
    :param j: Rank, position along vertical axis.
    :returns: :c:`true` if position is on the light side, :c:`false` otherwise.

.. c:function:: bool cc_chessboard_is_field_on_dark_side( CcChessboard * cb, int j )

    Function returning if given position is on the dark side of a chessboard.

    :param cb: A chessboard.
    :param j: Rank, position along vertical axis.
    :returns: :c:`true` if position is on the dark side, :c:`false` otherwise.

.. c:function:: int cc_chessboard_promoting_rank( CcChessboard * cb, bool is_light )

    Function returns rank of a promoting row.

    :param cb: A chessboard.
    :param is_light: Flag, whether it is for light or dark player.
    :returns: Rank of a promoting row if successful,
              :c:expr:`CC_INVALID_COORD` otherwise.

.. c:function:: int cc_chessboard_figure_rank( CcChessboard * cb, bool is_light )

    Function returns rank of a figure row.

    :param cb: A chessboard.
    :param is_light: Flag, whether it is for light or dark player.
    :returns: Rank of a figure row if successful,
              :c:expr:`CC_INVALID_COORD` otherwise.

.. c:function:: CcPieceEnum cc_chessboard_get_piece( CcChessboard * cb, int i, int j )

    Function returning piece at a given position.

    :param cb: A chessboard.
    :param i: File, position along horizontal axis.
    :param j: Rank, position along vertical axis.
    :returns: Piece if position is on-board, :c:`CC_PE_None` otherwise.

.. c:function:: CcTagEnum cc_chessboard_get_tag( CcChessboard * cb, int i, int j )

    Function returning tag at a given position.

    :param cb: A chessboard.
    :param i: File, position along horizontal axis.
    :param j: Rank, position along vertical axis.
    :returns: Tag if position is on-board, :c:`CC_TE_None` otherwise.

.. c:function:: bool cc_chessboard_set_piece_tag( CcChessboard * cb__io, int i, int j, CcPieceEnum pe, CcTagEnum ct )

    Function sets piece and tag at a given position.

    :param cb__io: *Input/output*, a chessboard.
    :param i: File, position along horizontal axis.
    :param j: Rank, position along vertical axis.
    :param pe: Piece to set.
    :param ct: Tag to set.
    :returns: :c:`true` if successful, :c:`false` otherwise.

.. c:function:: bool cc_chessboard_set_piece( CcChessboard * cb__io, int i, int j, CcPieceEnum pe )

    Function sets piece at a given position.

    .. note::

        Function resets tag on the same position to :c:`CC_TE_None`.

    :param cb__io: *Input/output*, a chessboard.
    :param i: File, position along horizontal axis.
    :param j: Rank, position along vertical axis.
    :param pe: Piece to set.
    :returns: :c:`true` if successful, :c:`false` otherwise.

.. c:function:: bool cc_chessboard_set_tag( CcChessboard * cb__io, int i, int j, CcTagEnum tt )

    Function sets tag at a given position.

    .. note::

        Function does not alter piece at the same position.

    :param cb__io: *Input/output*, a chessboard.
    :param i: File, position along horizontal axis.
    :param j: Rank, position along vertical axis.
    :param tt: Tag to set.
    :returns: :c:`true` if successful, :c:`false` otherwise.

.. c:function:: bool cc_chessboard_is_equal( CcChessboard * cb, CcChessboard * cb_2 )

    Compares two given chessboards field-by-field, if they are for
    the same variant.

    :param cb: A chessboard.
    :param cb_2: The other chessboard.
    :returns: :c:`true` if chessboards are equal, :c:`false` otherwise.


.. c:function:: char * cc_chessboard_as_string__new( CcChessboard * cb, bool is_board_or_tag )

    Formats a newly allocated string to represent piece, tag positions
    on a given chessboard.

    :param cb: Chessboard to display.
    :param is_board_or_tag: Whether pieces are displayed (if :c:`true`),
                            or tags (if :c:`false`).
    :returns: A newly allocated string if successful, :c:`NULL` otherwise.

.. c:function:: bool cc_chessboard_print( CcChessboard * cb, bool is_board_or_tag )

    TODO :: move out

.. c:function:: CcChessboard * cc_chessboard_clear_from_string__new( CcChessboard * cb, char const * setup )

    TODO


.. _lbl-libcc-ccchessboard-sourcecodeheader:

Chessboard source code header
-----------------------------

Included source code file is ``cc_chessboard.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_chessboard.h
    :language: C
    :linenos:

.. _lbl-libcc-ccchessboard-sourcecodefile:

Chessboard source code file
---------------------------

Included source code file is ``cc_chessboard.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_chessboard.c
    :language: C
    :linenos:
