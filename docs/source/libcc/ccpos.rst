.. Copyright (c) 2021, 2024, 2025 Mario Mlačak, mmlacak@gmail.com
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

.. c:macro:: CC_POS_IS_LEGAL(pos,board_size)

    Macro to check if given position is legal, i.e. if both coordinates are legal.

    Coordinate is legal if it's within board boundaries, i.e. between ``0`` and a
    given :c:var:`board_size`.

    :param pos: A position, i.e. :c:struct:`CcPos` value.
    :param board_size: Board size, integer.
    :returns: :c:data:`true` if legal position, :c:data:`false` otherwise.

.. c:macro:: CC_POS_IS_LEGAL_DISAMBIGUATION(pos,board_size)

    Macro to check if given position is legal disambiguation, i.e. if at least one
    coordinate is legal.

    :param pos: A position, i.e. :c:struct:`CcPos` value.
    :param board_size: Board size, integer.
    :returns: :c:data:`true` if legal disambiguation, :c:data:`false` otherwise.

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

.. c:function:: CcPos cc_pos_add( CcPos pos, int i, int j )

    Function adds relative coordinates to a given position.

    Function adds valid coordinates, if :c:`pos` is one of:

        * valid positions
        * file disambiguations
        * rank disambiguations

    and corresponding relative coordinate (:c:`i`, :c:`j`, or both) are valid.

    If arguments have no common valid coordinates, result is invalid position,
    e.g. if a rank disambiguation (:c:`pos`) is added to a file disambiguation
    (if :c:`i` is valid, but :c:`j` is not).

    :param pos: A position to add to.
    :param i: File, horizontal coordinate.
    :param j: Rank, vertical coordinate.
    :returns: A position with added relative coordinates if successful,
              :c:macro:`CC_POS_INVALID` otherwise.

.. c:function:: CcPos cc_pos_add_steps( CcPos pos, CcPos step, int count )

    Function adds step(s) to a given position.

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

.. c:function:: bool cc_pos_are_same_color( CcPos start, CcPos destination )

    Function checks if starting and destination fields are in the same color.

    :param start: Starting position.
    :param destination: Destination field.
    :returns: :c:data:`true` if in the same color, :c:data:`false` otherwise.

.. c:function:: bool cc_pos_piece_are_same_color( CcPos pos, CcPieceTagType piece )

    Function checks if piece and a field are in the same color.

    :param pos: A position.
    :param piece: A piece.
    :returns: :c:data:`true` if in the same color, :c:data:`false` otherwise.

.. c:function:: bool cc_pos_step_is_short_jump( CcPos step )

    Function checks if a given step is short jump, e.g. one that Knight might make.

    :param step: A step, relative position.
    :returns: :c:data:`true` if short jump, :c:data:`false` otherwise.

.. c:function:: bool cc_pos_step_is_long_jump( CcPos step )

    Function checks if a given step is long jump, e.g. one that Unicorn might make.

    :param step: A step, relative position.
    :returns: :c:data:`true` if long jump, :c:data:`false` otherwise.

.. c:function:: bool cc_pos_to_string( CcPos pos, cc_char_8 * pos_str__o )

    Function converts position into a user-readable :c:`<file char><rank number>` notation.

    Coordinates outside chessboard are converted into short integers, if possible.

    If outside of 2 decimal places, coordinate is represented as asterisk.

    :param pos: A position.
    :param pos_str__o: *Output*, pointer to short string array.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. _lbl-libcc-ccpos-linkedpositions:

Linked positions
----------------

.. c:struct:: CcPosLink

    Linked positions :c:`struct`\ure, linked list.

    .. c:member:: CcPos pos

        A position.

    .. c:member:: struct CcPosLink * next

        Next position in a linked list.

    :c:`struct` is tagged with the same :c:struct:`CcPosLink` name.

.. c:function:: CcPosLink * cc_pos_link__new( CcPos pos )

    Returns a newly allocated position link.

    :param pos: A position.
    :returns: A newly allocated position link if successful, :c:data:`NULL` otherwise.

.. c:function:: CcPosLink * cc_pos_link_append( CcPosLink ** pos_link__iod_a, CcPos pos )

    Appends a newly allocated position link to a given linked list.

    If linked list :c:`*pos_link__iod_a` is :c:data:`NULL`, it will be initialized
    with a newly allocated position link as its only element.

    :param pos_link__iod_a: **Ownership**, *optional* *input/output* parameter;
        linked list of positions to which a new position is appended, inner pointer
        can be :c:data:`NULL`.
    :param pos: A position.
    :returns: A weak pointer to newly allocated position link if successful,
        :c:data:`NULL` otherwise.

.. c:function:: CcPosLink * cc_pos_link_duplicate_all__new( CcPosLink * pos_link )

    Duplicates all given positions into a newly allocated linked list.

    :param pos_link: Linked list to duplicate.
    :returns: A newly allocated linked list if successful, :c:data:`NULL` otherwise.

.. c:function:: CcPosLink * cc_pos_link_extend( CcPosLink ** pos_link__iod_a, CcPosLink ** pos_link__n )

    Extends given linked list of positions with another.

    If linked list to extend (:c:`pos_link__iod_a`) hasn't been allocated yet,
    this will initialize it with content of an extending linked list, i.e.
    :c:`pos_link__n`.

    .. note::

        Extending linked list :c:`pos_link__n` has its ownership transferred to
        extended linked list :c:`pos_link__iod_a`; as a result, inner pointer
        :c:`*pos_link__n` is :c:data:`NULL`\ed.

    :param pos_link__iod_a: **Ownership**, *optional* *input/output*; linked list to extend.
    :param pos_link__n: **Ownership transfer**, *optional*; linked list to extend existing positions.
    :returns: Weak pointer to extended portion of a linked list if successful,
              :c:data:`NULL` otherwise.

.. c:function:: bool cc_pos_link_free_all( CcPosLink ** pos_link__f )

    Frees all positions in a linked list.

    :param pos_link__f: Linked list of positions.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. c:function:: size_t cc_pos_link_len( CcPosLink * pos_link )

    Function returning length of linked list of positions.

    :param pos_link: Linked list of positions.
    :returns: Length if successful, ``0`` otherwise.

.. c:function:: char * cc_pos_link_to_string__new( CcPosLink * pos_link )

    Function returns a newly allocated string, containing user-readable
    representation of positions in a given linked list, separated by ``'.'`` (dot);
    e.g. ``"b8.c10.d12.e14"``.

    :param pos_link: Linked list of positions.
    :returns: A newly allocated, null-terminated string if successful,
              :c:data:`NULL` otherwise

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

    .. c:member:: CcPieceTagType piece

        Piece found at position, and its tag.

    :c:`struct` is tagged with the same :c:struct:`CcPosDesc` name.

.. c:macro:: CC_POS_DESC_CAST_INVALID

    Casted invalid position descriptor value.

.. c:macro:: CC_POS_DESC_CAST_STATIC_STEP

    Casted static position descriptor value, i.e. no-movement step.

.. c:macro:: CC_POS_DESC_COORDS(int_i,int_j,piece_enum)

    Macro which constructs position descriptor struct.

    :param int_i: File, horizontal coordinate; integer.
    :param int_j: Rank, vertical coordinate; integer.
    :param piece_enum: A piece; :c:type:`CcPieceTagType` value.
    :returns: Position descriptor value.
    :seealso: :c:struct:`CcPosDesc`

.. c:macro:: CC_POS_DESC_COORDS_CAST(int_i,int_j,piece_enum)

    Macro which casts position descriptor macro.

    :param int_i: File, horizontal coordinate; integer.
    :param int_j: Rank, vertical coordinate; integer.
    :param piece_enum: A piece; :c:type:`CcPieceTagType` value.
    :returns: Casted position descriptor value.
    :seealso: :c:macro:`CC_POS_DESC_COORDS`

.. c:macro:: CC_POS_DESC(pos_v,piece_enum)

    Macro which constructs position descriptor struct.

    :param pos_v: A position; :c:type:`CcPos` value.
    :param piece_enum: A piece; :c:type:`CcPieceTagType` value.
    :returns: Position descriptor value.
    :seealso: :c:struct:`CcPosDesc`

.. c:macro:: CC_POS_DESC_CAST(pos_v,piece_enum)

    Macro which casts position descriptor macro.

    :param pos_v: A position; :c:type:`CcPos` value.
    :param piece_enum: A piece; :c:type:`CcPieceTagType` value.
    :returns: Casted position descriptor value.
    :seealso: :c:macro:`CC_POS_DESC`

.. c:macro:: CC_POS_DESC_IS_VALID(pd)

    Macro to check if given position descriptor is valid.

    :param pd: A position descriptor; :c:struct:`CcPosDesc` value.
    :returns: :c:data:`true` if valid position descriptor, :c:data:`false` otherwise.
    :seealso: :c:macro:`CC_POS_IS_VALID`

.. c:macro:: CC_POS_DESC_IS_LEGAL(pd,board_size)

    Macro to check if given position descriptor is legal.

    :param pd: A position descriptor; :c:struct:`CcPosDesc` value.
    :param board_size: Board size, an integer.
    :returns: :c:data:`true` if legal position descriptor, :c:data:`false` otherwise.
    :seealso: :c:macro:`CC_POS_IS_LEGAL`

.. c:macro:: CC_POS_DESC_IS_EQUAL(pd_1,pd_2)

    Macro to check if two given position descriptors are equal.

    :param pd_1: A position descriptor; :c:struct:`CcPosDesc` value.
    :param pd_2: Another position descriptor; :c:struct:`CcPosDesc` value.
    :returns: :c:data:`true` if equal, :c:data:`false` otherwise.

.. c:function:: bool cc_pos_desc_is_congruent( CcPosDesc pd_1, CcPosDesc pd_2, bool compare_only_piece_types )

    Function checks if two position descriptor values are congruent.

    For positions to be congruent, at least one set of coordinates
    (files, or ranks) from both positions has to be valid, and the
    same.

    For pieces to be congruent, they have to be valid, and the same
    type, e.g two Rooks, not necessarily in the same color.

    For pieces which can have tags (e.g. Pawns), flag :c:var:`compare_only_piece_types`
    controls whether pieces are stripped of their tags before comparison, or not.
    If tags are stripped then e.g. Pawn with delayed promotion tag is congruent
    to a Pawn which just rushed; otherwise, tags have to be the same.

    :param pd_1: A position descriptor.
    :param pd_2: Another position descriptor.
    :param compare_only_piece_types: Flag, whether pieces are stripped of their
        tags before comparison, or not.
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

.. _lbl-libcc-ccpos-linkedpositiondescriptors:

Linked position descriptors
---------------------------

.. c:struct:: CcPosDescLink

    Linked position descriptors :c:`struct`\ure, linked list.

    .. c:member:: CcPosDesc pd

        A position descriptor.

    .. c:member:: struct CcPosDescLink * next

        Next position descriptor in a linked list.

    :c:`struct` is tagged with the same :c:struct:`CcPosDescLink` name.

.. c:function:: CcPosDescLink * cc_pos_desc_link__new( CcPosDesc pd )

    Returns a newly allocated position descriptor link.

    :param pd: A position descriptor.
    :returns: A newly allocated position descriptor link if successful, :c:data:`NULL` otherwise.

.. c:function:: CcPosDescLink * cc_pos_desc_link_append( CcPosDescLink ** pd_link__iod_a, CcPosDesc pd )

    Appends a newly allocated position descriptor link to a given linked list.

    If linked list :c:`*pd_link__iod_a` is :c:data:`NULL`, it will be initialized
    with a newly allocated position descriptor link as its only element.

    :param pd_link__iod_a: **Ownership**, *optional* *input/output* parameter;
        linked list of position descriptors to which a new position descriptor
        is appended, inner pointer can be :c:data:`NULL`.
    :param pd: A position descriptor.
    :returns: A weak pointer to newly allocated position descriptor link if successful,
        :c:data:`NULL` otherwise.

.. c:function:: CcPosDescLink * cc_pos_desc_link_duplicate_all__new( CcPosDescLink * pd_link )

    Duplicates all given position descriptors into a newly allocated linked list.

    :param pd_link: Linked list to duplicate.
    :returns: A newly allocated linked list if successful, :c:data:`NULL` otherwise.

.. c:function:: CcPosDescLink * cc_pos_desc_link_extend( CcPosDescLink ** pd_link__iod_a, CcPosDescLink ** pd_link__n )

    Extends given linked list of position descriptors with another.

    If linked list to extend (:c:`pd_link__iod_a`) hasn't been allocated yet,
    this will initialize it with content of an extending linked list, i.e.
    :c:`pd_link__n`.

    .. note::

        Extending linked list :c:`pd_link__n` has its ownership transferred to
        extended linked list :c:`pd_link__iod_a`; as a result, inner pointer
        :c:`*pd_link__n` is :c:data:`NULL`\ed.

    :param pd_link__iod_a: **Ownership**, *optional* *input/output*; linked list to extend.
    :param pd_link__n: **Ownership transfer**, *optional*; linked list to extend existing position descriptors.
    :returns: Weak pointer to extended portion of a linked list if successful,
              :c:data:`NULL` otherwise.

.. c:function:: bool cc_pos_desc_link_free_all( CcPosDescLink ** pd_link__f )

    Frees all position descriptors in a linked list.

    :param pd_link__f: Linked list of position descriptors.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. c:function:: size_t cc_pos_desc_link_len( CcPosDescLink * pd_link )

    Function returning length of linked list of position descriptors.

    :param pd_link: Linked list of position descriptors.
    :returns: Length if successful, ``0`` otherwise.

.. c:function:: char * cc_pos_desc_link_to_string__new( CcPosDescLink * pd_link )

    Function returns a newly allocated string, containing user-readable
    representation of position descriptors in a given linked list, separated by
    ``','`` (comma); e.g. ``"Bb8,Rc10C,Pd12R,Se14"``.

    :param pd_link: Linked list of position descriptors.
    :returns: A newly allocated, null-terminated string if successful,
              :c:data:`NULL` otherwise

.. _lbl-libcc-ccpos-momentum:

Momentum
--------

.. c:enum:: CcMomentumUsageEnum

    Momentum usage enumeration.

    .. c:enumerator:: CC_MUE_NotUsing

        Momentum is not being used for movement; equals to ``0``.

    .. c:enumerator:: CC_MUE_Accumulating

        Momentum is being accumulated while moving.

    .. c:enumerator:: CC_MUE_Spending

        Momentum is being spent while moving.

    :c:`enum` is tagged with the same :c:enum:`CcMomentumUsageEnum` name.

.. c:macro:: CC_MOMENTUM_USAGE_IS_ENUMERATOR(mue)

    Macro to check if given step type is an enumerator, i.e. between
    :c:enumerator:`CC_MUE_NotUsing` and :c:enumerator:`CC_STE_Alternative` values.

    :param mue: Momentum usage, :c:type:`CcMomentumUsageEnum` value.
    :returns: :c:data:`true` if enumerator, :c:data:`false` otherwise.

.. c:macro:: CC_MOMENTUM_USAGE_IS_VALID(mue)

    Macro to check if given step type is a valid enumerator; it's the same as
    :c:macro:`CC_MOMENTUM_USAGE_IS_ENUMERATOR`, since there is no *None* value
    in the :c:type:`CcMomentumUsageEnum` type.

    :param mue: Momentum usage, :c:type:`CcMomentumUsageEnum` value.
    :returns: :c:data:`true` if valid enumerator, :c:data:`false` otherwise.

.. c:function:: CcMaybeBoolEnum cc_calc_momentum( CcMomentumUsageEnum usage, cc_uint_t count, cc_uint_t * momentum__io )

    Function calculates next momentum value by adding or subtracting :c:var:`count`,
    based on :c:var:`usage` argument; momentum is given, and result is returned via
    *input/output* :c:var:`momentum__io` parameter.

    Function checks if momentum calculation will over- or under-flow before
    actual calculation takes place.

    :param usage: Flag, whether momentum is accumulated, spent, or unchanged
        while piece is moving; :c:enum:`CcMomentumUsageEnum` value.
    :param count: Count of steps.
    :param momentum__io: *Input/output*; momentum.
    :returns: One of :c:enum:`CcMaybeBoolEnum` values:

        * :c:enumerator:`CC_MBE_True` if new momentum value has been successfully calculated,
        * :c:enumerator:`CC_MBE_False` if there is not enough momentum to subtract from (or, too much to add),
        * :c:enumerator:`CC_MBE_Void` in case of an error, insufficient data given.

.. c:function:: char cc_momentum_usage_as_char( CcMomentumUsageEnum usage )

    Function returns :c:`char` representing given usage:

        * :c:enumerator:`CC_MUE_NotUsing` returns :c:`'='`,
        * :c:enumerator:`CC_MUE_Accumulating` returns :c:`'+'`,
        * :c:enumerator:`CC_MUE_Spending` returns :c:`'-'`,

    :param usage: Flag, whether momentum is accumulated, spent, or unchanged
        while piece is moving; :c:enum:`CcMomentumUsageEnum` value.
    :returns: :c:`char` as described if given valid enumerator, :c:`'?'` otherwise.

.. _lbl-libcc-ccpos-activationdescriptor:

Activation descriptor
---------------------

.. c:struct:: CcActivationDesc

    Momentum :c:`struct` holding its value and usage.

    .. c:member:: CcPieceTagType activator

        An activator.

    .. c:member:: cc_uint_t momentum

        A momentum value.

    .. c:member:: CcMomentumUsageEnum usage

        Momentum usage.

    :c:`struct` is tagged with the same :c:struct:`CcActivationDesc` name.

.. c:macro:: CC_ACTIVATION_DESC_INITIAL

    Initial momentum, :c:struct:`CcActivationDesc` value; :c:member:`CcActivationDesc.activator`
    value is :c:enumerator:`CC_PTE_None`, :c:member:`CcActivationDesc.momentum` value is ``0``,
    and :c:member:`CcActivationDesc.usage` is :c:enumerator:`CC_MUE_Accumulating`.

.. c:macro:: CC_ACTIVATION_DESC_STATIC

    Static momentum, :c:struct:`CcActivationDesc` value; :c:member:`CcActivationDesc.activator`
    value is :c:enumerator:`CC_PTE_None`, :c:member:`CcActivationDesc.momentum` value is ``0``,
    and :c:member:`CcActivationDesc.usage` is :c:enumerator:`CC_MUE_NotUsing`.

.. c:macro:: CC_ACTIVATION_DESC_SPENT

    Spent momentum, :c:struct:`CcActivationDesc` value; :c:member:`CcActivationDesc.activator`
    value is :c:enumerator:`CC_PTE_None`, :c:member:`CcActivationDesc.momentum` value is ``0``,
    and :c:member:`CcActivationDesc.usage` is :c:enumerator:`CC_MUE_Spending`.

.. c:macro:: CC_ACTIVATION_DESC_CAST_INITIAL

    Casted initial momentum, i.e. :c:macro:`CC_ACTIVATION_DESC_INITIAL`.

.. c:macro:: CC_ACTIVATION_DESC_CAST_STATIC

    Casted static momentum, i.e. :c:macro:`CC_ACTIVATION_DESC_STATIC`.

.. c:macro:: CC_ACTIVATION_DESC_CAST_SPENT

    Casted spent momentum, i.e. :c:macro:`CC_ACTIVATION_DESC_SPENT`.

.. c:macro:: CC_ACTIVATION_DESC_IS_EQUAL(ad_1,ad_2)

    Macro to check if given activation descriptors are equal.

    :param ad_1: Activation descriptor, :c:type:`CcActivationDesc` value.
    :param ad_2: Activation descriptor, :c:type:`CcActivationDesc` value.
    :returns: :c:data:`true` if equal, :c:data:`false` otherwise.

.. c:function:: bool cc_activation_desc_is_valid( CcActivationDesc act_desc, CcPieceTagType moving, bool is_first_ply )

    Function checks if a given activation descriptor is valid,
    for a given piece and its ply within cascade.

    :param act_desc: An activation descriptor.
    :param moving: A moving piece, to which activation descriptor applies.
    :param is_first_ply: Flag, if current ply is first in a cascade.
    :returns: :c:data:`true` if valid activation descriptor, :c:data:`false` otherwise.
    :seealso: :c:macro:`CC_MOMENTUM_USAGE_IS_ENUMERATOR()`, :c:macro:`CC_PIECE_IS_ENUMERATOR()`

.. c:function:: CcMaybeBoolEnum cc_activation_desc_calc_momentum( CcActivationDesc * act_desc__io, cc_uint_t count )

    Convenience function to calculate next momentum value by adding or subtracting
    :c:var:`count`; momentum (including its usage) is given, and result is returned
    via *input/output* :c:var:`act_desc__io` parameter; :c:member:`CcActivationDesc.activator`
    and :c:member:`CcActivationDesc.usage` are not altered.

    Underlying function checks if momentum calculation will over- or under-flow
    before actual calculation takes place.

    :param act_desc__io: *Input/output*; an activation descriptor.
    :param count: Count of steps.
    :returns: One of :c:enum:`CcMaybeBoolEnum` values:

        * :c:enumerator:`CC_MBE_True` if new momentum value has been successfully calculated,
        * :c:enumerator:`CC_MBE_False` if there is not enough momentum to subtract from (or, too much to add),
        * :c:enumerator:`CC_MBE_Void` in case of an error, insufficient data given.

    :seealso: :c:func:`cc_calc_momentum()`

.. c:function:: bool cc_activation_desc_update_activator( CcActivationDesc * act_desc__io, CcPieceTagType moving, bool is_first_ply, CcPieceTagType new_activator )

    Function updates :c:member:`CcActivationDesc.activator` of an *input/output*
    :c:var:`act_desc__io` parameter with a given :c:var:`new_activator`.

    :param act_desc__io: *Input/output*; an activation descriptor.
    :param moving: A moving piece, to which activation descriptor applies.
    :param is_first_ply: Flag, if current ply is first in a cascade.
    :param new_activator: New activator, a piece.
    :returns: :c:data:`true` if activator has been successfully updated, :c:data:`false` otherwise.

.. c:function:: bool cc_activation_desc_is_usable( CcActivationDesc act_desc, CcPieceTagType moving, bool is_first_ply )

    Function checks if a given activation descriptor is valid and usable for at
    least one step.

    :param act_desc: An activation descriptor.
    :param moving: A moving piece, to which activation descriptor applies.
    :param is_first_ply: Flag, if current ply is first in a cascade.
    :returns: :c:data:`true` if activation descriptor is valid and usable, :c:data:`false` otherwise.
    :seealso: :c:func:`cc_activation_desc_is_valid()`

.. c:function:: bool cc_activation_desc_as_string( CcActivationDesc act_desc, cc_char_32 * act_dest_str__o )

    Function returns string representation of a given activation descriptor,
    via given *output* :c:`char` array.

    :param act_desc: An activation descriptor.
    :param act_dest_str__o: *Output*, pointer to :c:`char` array, size ``32``.
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
