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

Data
----

.. c:type:: char const CC_CHESSBOARD_SEPARATORS_SETUP_FROM_STRING[]

    Separators constant, used to tokenize chessboard setup string.

.. _lbl-libcc-ccchessboard-types:

Types
-----

.. c:struct:: CcChessboard

    Chessboard :c:`struct`\ure, used for all variants.

    .. c:member:: CcVariantType type

        Chess variant to play.

    .. c:member:: cc_uint_t size

        Actual size of a board used for a given variant.

    .. c:member:: CcPieceTagType board[ CC_VARIANT_BOARD_SIZE_MAXIMUM ][ CC_VARIANT_BOARD_SIZE_MAXIMUM ]

        Holds pieces, and their tags.

    :c:`struct` is tagged with the same :c:struct:`CcChessboard` name.

.. _lbl-libcc-ccchessboard-functions:

Functions
---------

.. c:function:: CcChessboard * cc_chessboard__new( CcVariantType ve, bool do_setup )

    Function returns a newly allocated chessboard, optionally initialized
    for a given variant.

    :param ve: Variant to play.
    :param do_setup: Whether to set-up pieces to their initial positions.
                     If :c:data:`false`, chessboard returned is empty.
    :returns: A newly allocated chessboard if successful, :c:data:`NULL` otherwise.

.. c:function:: bool cc_chessboard_init( CcChessboard * cb__io, CcVariantType ve, bool do_setup )

    (Re-)initializes a chessboard with initial setup, for a given variant.

    :param cb__io: *Input/output* parameter, chessboard to (re-)initialize.
    :param ve: Variant to play.
    :param do_setup: Whether to set-up pieces to their initial positions.
                     If :c:data:`false`, chessboard returned is empty.
    :returns: :c:data:`true` if chessboard is successfully (re-)initialized,
              :c:data:`false` otherwise.

.. c:function:: bool cc_chessboard_clear( CcChessboard * cb__io )

    Clears a chessboard of all pieces.

    :param cb__io: *Input/output* parameter, chessboard to clear.
    :returns: :c:data:`true` if chessboard is successfully cleared,
              :c:data:`false` otherwise.

.. c:function:: bool cc_chessboard_setup( CcChessboard * cb__io )

    Sets up pieces on a chessboard to their initial positions.

    :param cb__io: *Input/output* parameter, chessboard to set up.
    :returns: :c:data:`true` if chessboard is successfully set up,
              :c:data:`false` otherwise.

.. c:function:: cc_uint_t cc_chessboard_get_size( CcChessboard * cb )

    Function returns the size of a given chessboard.

    :param cb: A chessboard.
    :returns: Size of a given chessboard is successful, ``0`` otherwise.
    :seealso: :c:func:`cc_variant_board_size()`

.. c:function:: bool cc_chessboard_copy( CcChessboard * into__io, CcChessboard * from )

    Copies a chessboard to another one.

    :param into__io: *Input/output* parameter, chessboard to copy into.
    :param from: Chessboard to copy from.
    :returns: :c:data:`true` if chessboard is successfully copied,
              :c:data:`false` otherwise.

.. c:function:: CcChessboard * cc_chessboard_duplicate__new( CcChessboard * from )

    Function returns a newly allocated chessboard, with all data copied
    from a given chessboard.

    :param from: Chessboard to copy from.
    :returns: A newly allocated chessboard if successful, :c:data:`NULL` otherwise.

.. c:function:: bool cc_chessboard_free_all( CcChessboard ** cb__f )

    Deallocates chessboard, and all used resources.

    :param cb__f: Chessboard to deallocate.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. c:function:: bool cc_chessboard_is_coord_on_board( CcChessboard * cb, int coord )

    Function returning if given coordinate is on board, for a current variant.

    All board for all variants are squares, so files and ranks always have the same range.

    :param cb: A chessboard.
    :param coord: A coordinate to check.
    :returns: :c:data:`true` if coordinate is on-board, :c:data:`false` otherwise.

.. c:function:: bool cc_chessboard_is_pos_on_board( CcChessboard * cb, int i, int j )

    Function returning if given position belongs to a board.

    :param cb: A chessboard.
    :param i: File, position along horizontal axis.
    :param j: Rank, position along vertical axis.
    :returns: :c:data:`true` if position is on-board, :c:data:`false` otherwise.

.. c:function:: bool cc_chessboard_is_disambiguation_on_board( CcChessboard * cb, int i, int j )

    Function returning if at least one coordinate of given disambiguation
    belongs to a board.

    Disambiguation is a position which can have one invalid coordinate,
    i.e. equal to :c:macro:`CC_INVALID_COORD`.

    :param cb: A chessboard.
    :param i: File, position along horizontal axis.
    :param j: Rank, position along vertical axis.
    :returns: :c:data:`true` if disambiguation is on-board, :c:data:`false` otherwise.

.. c:function:: bool cc_chessboard_is_coord_safe_off_board( CcChessboard * cb, int coord )

    Function checks if given coordinate might be within safe off-board boundaries.

    Coordinate is safely off-board if there could be a movement (e.g. trance-journey),
    which would place piece back on-board.

    :param cb: A chessboard.
    :param coord: A coordinate to check.
    :returns: :c:data:`true` if coordinate might be safely off-board, :c:data:`false` otherwise.

.. c:function:: bool cc_chessboard_is_pos_safe_off_board( CcChessboard * cb, int i, int j )

    Function returning if given position might be within safe off-board boundaries.

    Position is safely off-board if there could be a movement (e.g. trance-journey),
    which would place piece back on-board.

    :param cb: A chessboard.
    :param i: File, position along horizontal axis.
    :param j: Rank, position along vertical axis.
    :returns: :c:data:`true` if position might be safely off-board, :c:data:`false` otherwise.

.. c:function:: bool cc_chessboard_is_disambiguation_safe_off_board( CcChessboard * cb, int i, int j )

    Function returning if at least one coordinate of given disambiguation might be
    within safe off-board boundaries.

    Position is safely off-board if there could be a movement (e.g. trance-journey),
    which would place piece back on-board.

    :param cb: A chessboard.
    :param i: File, position along horizontal axis.
    :param j: Rank, position along vertical axis.
    :returns: :c:data:`true` if disambiguation might be safely off-board, :c:data:`false` otherwise.

.. c:function:: bool cc_chessboard_is_field_on_light_side( CcChessboard * cb, int j )

    Function returning if given position is on the light side of a chessboard.

    :param cb: A chessboard.
    :param j: Rank, position along vertical axis.
    :returns: :c:data:`true` if position is on the light side, :c:data:`false` otherwise.

.. c:function:: bool cc_chessboard_is_field_on_dark_side( CcChessboard * cb, int j )

    Function returning if given position is on the dark side of a chessboard.

    :param cb: A chessboard.
    :param j: Rank, position along vertical axis.
    :returns: :c:data:`true` if position is on the dark side, :c:data:`false` otherwise.

.. c:function:: CcPieceTagType cc_chessboard_get_piece( CcChessboard * cb, int i, int j )

    Function returning piece and its tag at a given position.

    :param cb: A chessboard.
    :param i: File, position along horizontal axis.
    :param j: Rank, position along vertical axis.
    :returns: Piece if position is on-board, :c:enumerator:`CC_PTE_None` otherwise.

.. c:function:: bool cc_chessboard_set_piece( CcChessboard * cb__io, int i, int j, CcPieceTagType pe )

    Function sets piece and its tag at a given position.

    :param cb__io: *Input/output*, a chessboard.
    :param i: File, position along horizontal axis.
    :param j: Rank, position along vertical axis.
    :param pe: Piece to set.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. c:function:: bool cc_chessboard_is_equal( CcChessboard * cb, CcChessboard * cb_2 )

    Compares two given chessboards field-by-field, if they are for
    the same variant.

    :param cb: A chessboard.
    :param cb_2: The other chessboard.
    :returns: :c:data:`true` if chessboards are equal, :c:data:`false` otherwise.


.. c:function:: char * cc_chessboard_as_string__new( CcChessboard * cb, bool is_board_or_tag )

    Formats a newly allocated string to represent piece, tag positions
    on a given chessboard.

    :param cb: Chessboard to display.
    :param is_board_or_tag: Whether pieces are displayed (if :c:data:`true`),
                            or tags (if :c:data:`false`).
    :returns: A newly allocated string if successful, :c:data:`NULL` otherwise.

.. c:function:: bool cc_chessboard_print( CcChessboard * cb, bool is_board_or_tag )

    Prints chessboard, either pieces or tags.

    :param cb: Chessboard to print.
    :param is_board_or_tag: Whether pieces are printed (if :c:data:`true`),
                            or tags (if :c:data:`false`).
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. c:function:: CcChessboard * cc_chessboard_clear_from_string__new( CcChessboard * cb, char const * setup )

    .. todo::

        DOCS


.. _lbl-libcc-ccchessboard-sourcecodeheader:

Header file
-----------

Included source header file is ``cc_chessboard.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_chessboard.h
    :language: C
    :linenos:

.. _lbl-libcc-ccchessboard-sourcecodefile:

Source code file
----------------

Included source code file is ``cc_chessboard.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_chessboard.c
    :language: C
    :linenos:
