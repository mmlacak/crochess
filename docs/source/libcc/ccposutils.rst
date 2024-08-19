.. Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.
   Included source code files are licensed under GNU GPL v3+ license. See LICENSING, COPYING files in root folder of the project for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccposutils:

Position utilities
==================

Documents ``cc_pos_utils.h`` and ``cc_pos_utils.c`` files, which contain various
position utilities.

.. _lbl-libcc-ccposutils-functions:

Functions
---------

.. c:function:: CcPosDesc cc_convert_pos_to_pos_desc( CcChessboard * cb, CcPos pos, cc_uint_t momentum )

    Function converts position to position descriptor (i.e. the one
    containing piece, and tag at that location on a chessboard).

    If chessboard is not given, piece and tag members are not updated,
    returned value still contains a given position.

    :param cb: A chessboard.
    :param pos: A position.
    :param momentum: Momentum.
    :returns: Position descriptor.

.. c:function:: bool cc_calc_checked_momentum( cc_uint_t * momentum__io, bool accumulating )

    Function calculates next momentum value, given and then returned via
    *input/output* argument.

    Function checks if momentum calculation will over- or under-flow before
    actual calculation takes place.

    :param momentum__io: *Input/output*; momentum.
    :param accumulating: Flag, whether momentum is being accumulated
        (if :c:data:`true`), or used (if :c:data:`false`).
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. c:function:: CcPosDescLink * cc_apply_steps_to_position__new( CcChessboard * cb, CcPos pos, cc_uint_t momentum, bool accumulating, CcTypedStepLink * steps )

    Function returns a newly allocated linked list of position descriptors,
    which was produced by applying steps from starting position and momentum.

    :param cb: A chessboard.
    :param pos: Starting position.
    :param momentum: Starting momentum.
    :param accumulating: Flag, whether momentum is being accumulated
        (if :c:data:`true`), or used (if :c:data:`false`).
    :param steps: Steps to perform.
    :returns: Valid pointer to newly allocated linked list of position
        descriptors if successful, :c:data:`NULL` otherwise.

.. c:function:: bool cc_append_pos_to_pos_desc_link( CcChessboard * cb, CcPos pos, cc_uint_t momentum, CcPosDescLink ** pdl__iod_a )

    Function appends position descriptor to a given linked list, based on a position
    on a chessboard and a given momentum.

    :param cb: A chessboard.
    :param pos: A position.
    :param momentum: Momentum.
    :param pdl__iod_a: **Ownership**, *input/output*, *optional*; a position
        descriptor linked list.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. c:function:: bool cc_validate_pos_desc_link( CcChessboard * cb, CcPosDescLink * pd_link )

    Function validates all position descriptors in a given linked list.

    Validation is done by checking piece and tag found on a chessboard position
    matches descriptor's piece and tag.

    .. todo::

        Add check: momentum forms strict increasing / decreasing sequence.

    Lastly, steps are checked if there are any after momentum reaches ``0`` for
    a piece which uses that momentum for movement.

    :param cb: A chessboard.
    :param pd_link: A position descriptor linked list.
    :returns: :c:data:`true` if position descriptors are all valid,
        :c:data:`false` otherwise.

.. c:function:: bool cc_update_pos_desc_link( CcChessboard * cb, CcPosDescLink * pd_link__io )

    Function updates all position descriptors in a given linked list.

    Function updates piece and tag as found on a chessboard, at a position fetched
    from descriptor; momentum is not changed.

    :param cb: A chessboard.
    :param pd_link__io: *Input/output*; a position descriptor linked list.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.


.. todo::

    Document all other functions.

.. _lbl-libcc-ccposutils-sourcecodeheader:

Header file
-----------

Included source header file is ``cc_pos_utils.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_pos_utils.h
    :language: C
    :linenos:

.. _lbl-libcc-ccposutils-sourcecodefile:

Source code file
----------------

Included source code file is ``cc_pos_utils.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_pos_utils.c
    :language: C
    :linenos:
