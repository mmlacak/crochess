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

Position data
-------------

.. c:macro:: CC_POS_INVALID

    Invalid position value, both coordinates are equal to :c:expr:`CC_INVALID_COORD`.

.. c:macro:: CC_POS_STATIC_STEP

    Static position value, i.e. no-movement step; both coordinates are equal to :c:`0`.

.. c:macro:: CC_POS_ORIGIN_FIELD

    Origin field, i.e. coordinate system start; both coordinates are equal to :c:`0`.

.. c:struct:: CcPos

    Position :c:`struct`, either absolute or relative,
    i.e. either a location or a step.

    .. c:member:: int i

        File, horizontal coordinate.

    .. c:member:: int j

        Rank, vertical coordinate.

    :c:`CcPos` is tagged with the same :c:expr:`CcPos` name.

.. c:macro:: CC_POS_CAST_INVALID

    Casted invalid position value, i.e. :c:expr:`CC_POS_INVALID`.

.. c:macro:: CC_POS_CAST_STATIC_STEP

    Casted static position value, i.e. :c:expr:`CC_POS_STATIC_STEP`.

.. c:macro:: CC_POS_CAST_ORIGIN_FIELD

    Casted origin field, i.e. :c:expr:`CC_POS_ORIGIN_FIELD`.

.. c:macro:: CC_POS(int_i,int_j)

    Macro definition for instantiating position, i.e. :c:expr:`CcPos` :c:`struct`\ure.

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

    Coordinate is valid if it's not :c:expr:`CC_INVALID_COORD`.

    :param pos: A position, i.e. :c:expr:`CcPos` value.
    :returns: :c:`true` if valid position, :c:`false` otherwise.

.. c:macro:: CC_POS_IS_STATIC_STEP(pos)

    Macro to check if given position is static step.

    :param pos: A position, i.e. :c:expr:`CcPos` value.
    :returns: :c:`true` if static step, :c:`false` otherwise.

.. c:macro:: CC_POS_IS_DISAMBIGUATION(pos)

    Macro to check if given position is disambiguation.

    .. todo::

        move to design.rst

        Disambiguation is any position with at least one valid coordinate.

    :param pos: A position, i.e. :c:expr:`CcPos` value.
    :returns: :c:`true` if disambiguation, :c:`false` otherwise.

.. c:macro:: CC_POS_IS_PARTIAL(pos)

    Macro to check if given position is partial.

    .. todo::

        move to design.rst

        Partial position is any which have exactly one valid coordinate.

    :param pos: A position, i.e. :c:expr:`CcPos` value.
    :returns: :c:`true` if partial position, :c:`false` otherwise.

.. c:macro:: CC_POS_IS_EQUAL(pos_1,pos_2)

    Macro to check if given positions are equal.

    :param pos_1: A position, i.e. :c:expr:`CcPos` value.
    :param pos_2: Another position, i.e. :c:expr:`CcPos` value.
    :returns: :c:`true` if equal, :c:`false` otherwise.

.. _lbl-libcc-ccpos-functions:

Position functions
------------------

.. c:function:: CcPos cc_pos( int i, int j )

    Function returns a position.

    :param i: File, horizontal coordinate.
    :param j: Rank, vertical coordinate.
    :returns: Positions with a given coordinates.

.. c:function:: bool cc_pos_is_valid( CcPos pos )

    Function checks if position is valid.

    :param pos: A position.
    :returns: :c:`true` if position is valid, :c:`false` otherwise.

.. c:function:: bool cc_pos_is_static_step( CcPos pos )

    Function checks if position is static step.

    :param pos: A position.
    :returns: :c:`true` if position is static step, :c:`false` otherwise.

.. c:function:: bool cc_pos_is_disambiguation( CcPos pos )

    Function checks if position is disambiguation.

    .. todo::

        move to design.rst

        Disambiguation is any position with at least one valid coordinate.

    :param pos: A position.
    :returns: :c:`true` if position is disambiguation, :c:`false` otherwise.

.. c:function:: bool cc_pos_is_partial( CcPos pos )

    Function checks if position is partial.

    .. todo::

        move to design.rst

        Partial position is any which have exactly one valid coordinate.

    :param pos: A position.
    :returns: :c:`true` if position is partial, :c:`false` otherwise.

.. c:function:: bool cc_pos_is_equal( CcPos pos_1, CcPos pos_2 )

    Function checks if two positions are equal.

    :param pos_1: A position.
    :param pos_2: Another position.
    :returns: :c:`true` if positions are equal, :c:`false` otherwise.

.. c:function:: bool cc_pos_is_congruent( CcPos pos_1, CcPos pos_2 )

    Function checks if two positions are congruent.

    For positions to be congruent, at least one set of coordinates (files,
    or ranks) from both positions has to be valid, and the same.

    :param pos_1: A position.
    :param pos_2: Another position.
    :returns: :c:`true` if positions are congruent, :c:`false` otherwise.

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
              :c:expr:`CC_POS_INVALID` otherwise.

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
              :c:expr:`CC_POS_INVALID` otherwise.

.. c:function:: CcPos cc_pos_calc_step( CcPos start, CcPos destination )

    Function calculates step from :c:`start` to :c:`destination`.

    Returned step might not be legal step for any piece, it's just the
    shortest possible step between the two given positions.

    :param start: Starting position.
    :param destination: Destination field.
    :returns: A valid step if successful, :c:expr:`CC_POS_INVALID` otherwise.

.. c:function:: bool cc_pos_to_short_string( CcPos pos, cc_char_8 * pos_str__o )

    Function converts position into a user-readable :c:`<file char><rank number>` notation.

    Coordinates outside chessboard are converted into short integers, if possible.

    If outside of 2 decimal places, coordinate is represented as asterisk.

    :param pos: A position.
    :param pos_str__o: *Output*, pointer to short string array.
    :returns: :c:`true` if successful, :c:`false` otherwise.

.. _lbl-libcc-ccpos-steptypeenum:

Step type enum
--------------

.. c:enum:: CcStepTypeEnum

    Step types enumeration.

    .. c:enumerator:: CC_STE_None

        Undefined step type, equals to :c:`0`.

    .. c:enumerator:: CC_STE_Movement

        Just a step, movement. It can still cause side-effects other than capture.

    .. c:enumerator:: CC_STE_Capture

        Capturing step, i.e. movement + capture.

    .. c:enumerator:: CC_STE_Alternative

        Alternative step; one of color-change-, entrancement-, uplifting-, miracle-steps.

.. c:macro:: CC_TYPED_STEP_INVALID

    Invalid typed step value.

.. c:macro:: CC_TYPED_STEP_STATIC

    Static typed step value, i.e. no-movement, no-side-effects step.

.. _lbl-libcc-ccpos-typedstep:

Typed step
----------

.. c:struct:: CcTypedStep

    Structure holding a step and its type.

    Step is always relative to current position.

    .. c:member:: CcPos step

        Step, relative position.

    .. c:member:: CcStepTypeEnum type

        Type of a step.

    :c:`CcTypedStep` is tagged with the same :c:expr:`CcTypedStep` name.

.. c:macro:: CC_TYPED_STEP_CAST_INVALID

    Casted, invalid typed step value.

.. c:macro:: CC_TYPED_STEP_CAST_STATIC

    Casted, static typed step value, i.e. no-movement, no-side-effects step.

.. c:macro:: CC_TYPED_STEP_IS_VALID(ts)

    Macro to check if given typed step is valid.

    Typed step is valid if both coordinates of a :c:`step` member
    are valid, and :c:`type` is not :c:`CC_STE_None`.

    :param ts: A typed step, i.e. :c:expr:`CcTypedStep` value.
    :returns: :c:`true` if valid typed step, :c:`false` otherwise.

.. c:macro:: CC_TYPED_STEP_IS_EQUAL(ts_1,ts_2)

    Macro to check if given typed steps are equal.

    :param ts_1: A typed step, i.e. :c:expr:`CcTypedStep` value.
    :param ts_2: Another typed step, i.e. :c:expr:`CcTypedStep` value.
    :returns: :c:`true` if equal, :c:`false` otherwise.

.. c:macro:: CC_TYPED_STEP(int_i,int_j,enum_type)

    Macro definition for a typed step.

    :param int_i: File, horizontal coordinate; an integer value.
    :param int_j: Rank, vertical coordinate; an integer value.
    :param enum_type: Type of a step; :c:expr:`CcStepTypeEnum` value.
    :returns: Typed step with a given coordinates.

.. c:macro:: CC_TYPED_STEP_CAST(int_i,int_j,enum_type)

    Macro definition for a casted, typed step.

    :param int_i: File, horizontal coordinate; an integer value.
    :param int_j: Rank, vertical coordinate; an integer value.
    :param enum_type: Type of a step; :c:expr:`CcStepTypeEnum` value.
    :returns: Casted, typed step with a given coordinates.




.. _lbl-libcc-ccpos-sourcecodeheader:

Position source code header
---------------------------

Included source code file is ``cc_pos.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_pos.h
    :language: C
    :linenos:

.. _lbl-libcc-ccpos-sourcecodefile:

Position source code file
-------------------------

Included source code file is ``cc_pos.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_pos.c
    :language: C
    :linenos:
