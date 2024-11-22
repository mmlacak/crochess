.. Copyright (c) 2021, 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.
   Included source code files are licensed under GNU GPL v3+ license. See LICENSING, COPYING files in root folder of the project for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccpos:

Position
========

Documents ``cc_pos.h`` and ``cc_pos.c`` files, which contain various
position definitions, linked lists and functions.

.. _lbl-libcc-ccpos-data:

Data
----

.. c:macro:: CC_POS_INVALID

    Invalid position value, both coordinates are equal to :c:macro:`CC_INVALID_COORD`.

.. c:macro:: CC_POS_STATIC_STEP

    Static position value, i.e. no-movement step; both coordinates are equal to ``0``.

.. c:macro:: CC_POS_ORIGIN_FIELD

    Origin field, i.e. coordinate system start; both coordinates are equal to ``0``.

.. c:struct:: CcPos

    Position :c:`struct`, either absolute or relative,
    i.e. either a location or a step.

    .. c:member:: int i

        File, horizontal coordinate.

    .. c:member:: int j

        Rank, vertical coordinate.

    :c:`struct` is tagged with the same :c:struct:`CcPos` name.

.. c:macro:: CC_POS_CAST_INVALID

    Casted invalid position value, i.e. :c:macro:`CC_POS_INVALID`.

.. c:macro:: CC_POS_CAST_STATIC_STEP

    Casted static position value, i.e. :c:macro:`CC_POS_STATIC_STEP`.

.. c:macro:: CC_POS_CAST_ORIGIN_FIELD

    Casted origin field, i.e. :c:macro:`CC_POS_ORIGIN_FIELD`.

.. c:macro:: CC_POS(int_i,int_j)

    Macro definition for instantiating position, i.e. :c:struct:`CcPos` :c:`struct`\ure.

    :param int_i: File, horizontal coordinate.
    :param int_j: Rank, vertical coordinate.
    :returns: Position with given coordinates.

.. c:macro:: CC_POS_CAST(int_i,int_j)

    Macro definition for a casted position.

    :param int_i: File, horizontal coordinate.
    :param int_j: Rank, vertical coordinate.
    :returns: Casted position with given coordinates.

.. c:macro:: CC_POS_IS_VALID(pos)

    Macro to check if given position is valid, i.e. if both
    coordinates are valid.

    Coordinate is valid if it's not :c:macro:`CC_INVALID_COORD`.

    :param pos: A position, i.e. :c:struct:`CcPos` value.
    :returns: :c:data:`true` if valid position, :c:data:`false` otherwise.

.. c:macro:: CC_POS_IS_STATIC_STEP(pos)

    Macro to check if given position is static step.

    :param pos: A position, i.e. :c:struct:`CcPos` value.
    :returns: :c:data:`true` if static step, :c:data:`false` otherwise.

.. c:macro:: CC_POS_IS_DISAMBIGUATION(pos)

    Macro to check if given position is disambiguation.

    .. todo::

        move to design.rst

        Disambiguation is any position with at least one valid coordinate.

    :param pos: A position, i.e. :c:struct:`CcPos` value.
    :returns: :c:data:`true` if disambiguation, :c:data:`false` otherwise.

.. c:macro:: CC_POS_IS_PARTIAL(pos)

    Macro to check if given position is partial.

    .. todo::

        move to design.rst

        Partial position is any which have exactly one valid coordinate.

    :param pos: A position, i.e. :c:struct:`CcPos` value.
    :returns: :c:data:`true` if partial position, :c:data:`false` otherwise.

.. c:macro:: CC_POS_IS_EQUAL(pos_1,pos_2)

    Macro to check if given positions are equal.

    :param pos_1: A position, i.e. :c:struct:`CcPos` value.
    :param pos_2: Another position, i.e. :c:struct:`CcPos` value.
    :returns: :c:data:`true` if equal, :c:data:`false` otherwise.

.. _lbl-libcc-ccpos-functions:

Functions
---------

.. c:function:: bool cc_pos_is_congruent( CcPos pos_1, CcPos pos_2 )

    Function checks if two positions are congruent.

    For positions to be congruent, at least one set of coordinates (files,
    or ranks) from both positions has to be valid, and the same.

    :param pos_1: A position.
    :param pos_2: Another position.
    :returns: :c:data:`true` if positions are congruent, :c:data:`false` otherwise.

.. c:function:: CcPos cc_pos_add( CcPos pos, CcPos step, int count )

    Function adds step to position.

    Function adds valid coordinates, if both :c:`pos` and :c:`step`
    arguments are one of:

    * valid positions
    * file disambiguations
    * rank disambiguations.

    If :c:`pos` and :c:`step` have no common valid coordinates,
    result is invalid position, e.g. if a rank disambiguation is
    added to a file disambiguation.

    :param pos: A position to add to.
    :param step: A step to be added.
    :param count: Count of steps to be added.
    :returns: A position with added step(s) if successful,
              :c:macro:`CC_POS_INVALID` otherwise.

.. c:function:: CcPos cc_pos_difference( CcPos start, CcPos destination )

    Function returns difference between :c:`destination` and :c:`start` positions.

    Function subtracts valid coordinates, if both positions are one of:

    * valid positions
    * file disambiguations
    * rank disambiguations.

    If given positions have no common valid coordinates, result is
    invalid position, e.g. if a rank disambiguation is subtracted
    from a file disambiguation.

    :param start: Starting position.
    :param destination: Destination field.
    :returns: A position difference if successful,
              :c:macro:`CC_POS_INVALID` otherwise.

.. c:function:: CcPos cc_pos_calc_step( CcPos start, CcPos destination )

    Function calculates step from :c:`start` to :c:`destination`.

    Returned step might not be legal step for any piece, it's just the
    shortest possible step between the two given positions.

    :param start: Starting position.
    :param destination: Destination field.
    :returns: A valid step if successful, :c:macro:`CC_POS_INVALID` otherwise.

.. c:function:: bool cc_pos_to_string( CcPos pos, cc_char_8 * pos_str__o )

    Function converts position into a user-readable :c:`<file char><rank number>` notation.

    Coordinates outside chessboard are converted into short integers, if possible.

    If outside of 2 decimal places, coordinate is represented as asterisk.

    :param pos: A position.
    :param pos_str__o: *Output*, pointer to short string array.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. _lbl-libcc-ccpos-positiondescriptor:

Position descriptor
-------------------

.. c:macro:: CC_POS_DESC_INVALID

    Invalid position descriptor value.

.. c:macro:: CC_POS_DESC_STATIC_STEP

    Static position descriptor value, i.e. no-movement position.

.. c:struct:: CcPosDesc

    Position descriptor; holding a position, and a piece and a tag
    found at it.

    .. c:member:: CcPos pos

        A position.

    .. c:member:: CcPieceType piece

        Piece found at position.

    .. c:member:: CcTagType tag

        Tag found at position.

    :c:`struct` is tagged with the same :c:struct:`CcPosDesc` name.

.. c:macro:: CC_POS_DESC_CAST_INVALID

    Casted invalid position descriptor value.

.. c:macro:: CC_POS_DESC_CAST_STATIC_STEP

    Casted static position descriptor value, i.e. no-movement step.

.. c:macro:: CC_POS_DESC(int_i,int_j,piece_enum,tag_enum)

    Macro which constructs position descriptor struct.

    :param int_i: File, horizontal coordinate; integer.
    :param int_j: Rank, vertical coordinate; integer.
    :param piece_enum: A piece; :c:type:`CcPieceType` value.
    :param tag_enum: A tag; :c:type:`CcTagType` value.
    :returns: Position descriptor value.
    :seealso: :c:struct:`CcPosDesc`

.. c:macro:: CC_POS_DESC_CAST(int_i,int_j,piece_enum,tag_enum)

    Macro which casts position descriptor macro.

    :param int_i: File, horizontal coordinate; integer.
    :param int_j: Rank, vertical coordinate; integer.
    :param piece_enum: A piece; :c:type:`CcPieceType` value.
    :param tag_enum: A tag; :c:type:`CcTagType` value.
    :returns: Casted position descriptor value.
    :seealso: :c:macro:`CC_POS_DESC`

.. c:macro:: CC_POS_DESC_IS_VALID(pd)

    Macro to check if given position descriptor is valid.

    :param pd: A position descriptor; :c:struct:`CcPosDesc` value.
    :returns: Casted position descriptor value.
    :seealso: :c:data:`true` if valid position descriptor, :c:data:`false` otherwise.

.. c:macro:: CC_POS_DESC_IS_EQUAL(pd_1,pd_2)

    Macro to check if two given position descriptors are equal.

    :param pd_1: A position descriptor; :c:struct:`CcPosDesc` value.
    :param pd_2: Another position descriptor; :c:struct:`CcPosDesc` value.
    :returns: Casted position descriptor value.
    :seealso: :c:data:`true` if equal, :c:data:`false` otherwise.

.. c:function:: bool cc_pos_desc_is_congruent( CcPosDesc pd_1, CcPosDesc pd_2 )

    Function checks if two position descriptor values are congruent.

    For positions to be congruent, at least one set of coordinates
    (files, or ranks) from both positions has to be valid, and the
    same.

    For pieces to be congruent, they have to be valid, and the same
    type, e.g two Rooks, not necessarily in the same color.

    Tags are not checked.

    :param pd_1: A position descriptor.
    :param pd_2: Another position descriptor.
    :returns: :c:data:`true` if position descriptors are congruent,
              :c:data:`false` otherwise.

.. c:function:: bool cc_pos_desc_to_string( CcPosDesc pd, cc_char_16 * pd_str__o )

    Function converts position descriptor value into a user-readable
    ``<file char><rank number><piece>`` notation.

    Coordinates outside chessboard are converted into short integers,
    if possible, otherwise into asterisk.

    :param pd: A position descriptor.
    :param pd_str__o: *Output*, pointer to short string array.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. _lbl-libcc-ccpos-sourcecodeheader:

Header file
-----------

Included source header file is ``cc_pos.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_pos.h
    :language: C
    :linenos:

.. _lbl-libcc-ccpos-sourcecodefile:

Source code file
----------------

Included source code file is ``cc_pos.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_pos.c
    :language: C
    :linenos:
