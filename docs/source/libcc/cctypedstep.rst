.. Copyright (c) 2021, 2024, 2025 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.
   Included source code files are licensed under GNU GPL v3+ license. See LICENSING, COPYING files in root folder of the project for details.

.. include:: ../defines.rst

.. _lbl-libcc-cctypedstep:

Typed step
==========

Documents ``cc_typed_step.h`` and ``cc_typed_step.c`` files, which contain various
position definitions, linked lists and functions.

.. _lbl-libcc-cctypedstep-serpentdiagonalsenum:

Serpent diagonals enum
----------------------

.. c:enum:: CcSerpentDiagonalEnum

    Serpent diagonals enumeration.

    .. c:enumerator:: CC_SDE_BothDiagonals

        Both diagonals, i.e. left and right, equals to ``0``.

    .. c:enumerator:: CC_SDE_LeftDiagonal

        Left diagonal, consisting of upper-left and lower-right steps.

    .. c:enumerator:: CC_SDE_RightDiagonal

        Right diagonal, consisting of upper-right and lower-left steps.

    :c:`enum` is tagged with the same :c:enum:`CcSerpentDiagonalEnum` name.

.. c:macro:: CC_SERPENT_DIAGONAL_IS_ENUMERATOR(ste)

    Macro to check if given serpent diagonal is an enumerator, i.e. between
    :c:enumerator:`CC_SDE_BothDiagonals` and :c:enumerator:`CC_SDE_RightDiagonal` values.

    :param ste: Serpent diagonal, :c:type:`CcSerpentDiagonalEnum` value.
    :returns: :c:data:`true` if enumerator, :c:data:`false` otherwise.

.. c:macro:: CC_SERPENT_DIAGONAL_IS_VALID(ste)

    Macro to check if given serpent diagonal is a valid enumerator.

    Currently, there is no difference between this and :c:macro:`CC_SERPENT_DIAGONAL_IS_ENUMERATOR()`
    macro, since :c:enum:`CcSerpentDiagonalEnum` does not define *None* value.

    :param ste: Serpent diagonal, :c:type:`CcSerpentDiagonalEnum` value.
    :returns: :c:data:`true` if valid enumerator, :c:data:`false` otherwise.

.. _lbl-libcc-cctypedstep-journeytypesenum:

Journey types enum
------------------

.. c:enum:: CcJourneyTypeEnum

    Journey type enumeration, both for trance- and sense-journey.

    .. c:enumerator:: CC_JTE_None

        No journey.

    .. c:enumerator:: CC_JTE_Displacement

        Displacement trance-journey.

    .. c:enumerator:: CC_JTE_Capture

        Capturing trance-journey.

    .. c:enumerator:: CC_JTE_DoubleCapture

        Double capturing trance-journey.

    .. c:enumerator:: CC_JTE_Viewing

        Sense-journey.

    :c:`enum` is tagged with the same :c:enum:`CcJourneyTypeEnum` name.

.. c:macro:: CC_JOURNEY_TYPE_IS_ENUMERATOR(jte)

    Macro to check if given journey type is enumeration in :c:enum:`CcJourneyTypeEnum`,
    i.e. between :c:enumerator:`CC_JTE_None` and :c:enumerator:`CC_JTE_Viewing` values.

    :param jte: A journey type enumeration, integer value.
    :returns: :c:data:`true` if :c:type:`CcJourneyTypeEnum` enumerator,
              :c:data:`false` otherwise.

.. c:macro:: CC_JOURNEY_TYPE_IS_VALID(jte)

    Macro to check if given trance-journey type is :c:enum:`CcJourneyTypeEnum` enumerator,
    but not :c:enumerator:`CC_JTE_None`.

    :param jte: A journey type enumeration, integer value.
    :returns: :c:data:`true` if valid :c:type:`CcJourneyTypeEnum` enumerator,
              :c:data:`false` otherwise.

.. c:macro:: CC_JOURNEY_TYPE_IS_ANY_CAPTURE(jte)

    Macro to check if given trance-journey is a capturing one, or a double trance-journey.

    :param jte: A journey type enumeration, integer value.
    :returns: :c:data:`true` if trance-journey is capturing,
              :c:data:`false` otherwise.

.. _lbl-libcc-cctypedstep-steptypeenum:

Step type enum
--------------

.. c:enum:: CcStepTypeEnum

    Step types enumeration.

    .. c:enumerator:: CC_STE_None

        Undefined step type, equals to ``0``.

    .. c:enumerator:: CC_STE_MovementOnly

        Just a step, movement. It can still cause side-effects other than capture.

    .. c:enumerator:: CC_STE_CaptureOrMovement

        Capturing step, i.e. movement + capture.

    .. c:enumerator:: CC_STE_CaptureOnly

        Capturing step, i.e. valid movement only if piece can capture.

    .. c:enumerator:: CC_STE_Displacement

        Displacement step; it can be either step made by displacing or displaced
        piece, depending on context.

    .. c:enumerator:: CC_STE_ColorChange

        Serpent's color-changing step.

    .. c:enumerator:: CC_STE_Entrancement

        Shaman's entrancement step.

    .. c:enumerator:: CC_STE_Uplifting

        Starchild's upliftng step.

    .. c:enumerator:: CC_STE_Miracle

        Starchild's miracle step.

    :c:`enum` is tagged with the same :c:enum:`CcStepTypeEnum` name.

.. c:macro:: CC_STEP_TYPE_IS_ENUMERATOR(ste)

    Macro to check if given step type is an enumerator, i.e. between
    :c:enumerator:`CC_STE_None` and :c:enumerator:`CC_STE_Alternative` values.

    :param ste: Step type, :c:type:`CcStepTypeEnum` value.
    :returns: :c:data:`true` if enumerator, :c:data:`false` otherwise.

.. c:macro:: CC_STEP_TYPE_IS_VALID(ste)

    Macro to check if given step type is an enumerator, but not
    :c:enumerator:`CC_STE_None`.

    :param ste: Step type, :c:type:`CcStepTypeEnum` value.
    :returns: :c:data:`true` if valid enumerator, :c:data:`false` otherwise.

.. c:macro:: CC_STEP_TYPE_IS_MOVEMENT(ste)

    Macro to check if given step type is a movement, i.e. either
    :c:enumerator:`CC_STE_MovementOnly` or :c:enumerator:`CC_STE_CaptureOrMovement`.

    :param ste: Step type, :c:type:`CcStepTypeEnum` value.
    :returns: :c:data:`true` if movement, :c:data:`false` otherwise.

.. c:macro:: CC_STEP_TYPE_IS_CAPTURE(ste)

    Macro to check if given step type is a movement, i.e. either
    :c:enumerator:`CC_STE_CaptureOrMovement` or :c:enumerator:`CC_STE_CaptureOnly`.

    :param ste: Step type, :c:type:`CcStepTypeEnum` value.
    :returns: :c:data:`true` if capture, :c:data:`false` otherwise.

.. _lbl-libcc-cctypedstep-typedstepmacros:

Typed step, macros
------------------

.. c:macro:: CC_TYPED_STEP_INVALID

    Invalid typed step value.

.. c:macro:: CC_TYPED_STEP_STATIC

    Static typed step value, i.e. no-movement, no-side-effects step.

.. c:struct:: CcTypedStep

    Structure holding a step and its type.

    Step is always relative to current position.

    .. c:member:: CcPos step

        Step, relative position.

    .. c:member:: CcStepTypeEnum type

        Type of a step.

    :c:`struct` is tagged with the same :c:struct:`CcTypedStep` name.

.. c:macro:: CC_TYPED_STEP_CAST_INVALID

    Casted, invalid typed step value.

.. c:macro:: CC_TYPED_STEP_CAST_STATIC

    Casted, static typed step value, i.e. no-movement, no-side-effects step.

.. c:macro:: CC_TYPED_STEP_IS_VALID(ts)

    Macro to check if given typed step is valid.

    Typed step is valid if both coordinates of a :c:`step` member
    are valid, and :c:`type` is not :c:enumerator:`CC_STE_None`.

    :param ts: A typed step, i.e. :c:struct:`CcTypedStep` value.
    :returns: :c:data:`true` if valid typed step, :c:data:`false` otherwise.

.. c:macro:: CC_TYPED_STEP_IS_EQUAL(ts_1,ts_2)

    Macro to check if given typed steps are equal.

    :param ts_1: A typed step, i.e. :c:struct:`CcTypedStep` value.
    :param ts_2: Another typed step, i.e. :c:struct:`CcTypedStep` value.
    :returns: :c:data:`true` if equal, :c:data:`false` otherwise.

.. c:macro:: CC_TYPED_STEP(int_i,int_j,enum_type)

    Macro definition for a typed step.

    :param int_i: File, horizontal coordinate; an integer value.
    :param int_j: Rank, vertical coordinate; an integer value.
    :param enum_type: Type of a step; :c:enum:`CcStepTypeEnum` value.
    :returns: Typed step with a given coordinates.

.. c:macro:: CC_TYPED_STEP_CAST(int_i,int_j,enum_type)

    Macro definition for a casted, typed step.

    :param int_i: File, horizontal coordinate; an integer value.
    :param int_j: Rank, vertical coordinate; an integer value.
    :param enum_type: Type of a step; :c:enum:`CcStepTypeEnum` value.
    :returns: Casted, typed step with a given coordinates.

.. c:function:: CcTypedStep cc_typed_step( CcPos step, CcStepTypeEnum type )

    Function returns typed step.

    :param step: Step, relative position.
    :param type: Type of a step.
    :returns: A typed step value.

.. c:function:: bool cc_typed_step_is_equal( CcTypedStep ts_1, CcTypedStep ts_2 )

    Function checks if two typed steps are equal.

    :param ts_1: A typed step.
    :param ts_2: Another typed step.
    :returns: :c:data:`true` if equal, :c:data:`false` otherwise.

.. _lbl-libcc-cctypedstep-linkedtypedsteps:

Linked typed steps
------------------

.. c:struct:: CcTypedStepLink

    A linked list of typed steps.

    .. c:member:: CcTypedStep step

        Typed step value.

    .. c:member:: CcTypedStepLink * next

        Link to next typed step.

    :c:`struct` is tagged with the same :c:struct:`CcTypedStepLink` name.

.. c:function:: CcTypedStepLink * cc_typed_step_link__new( CcTypedStep step )

    Function allocates a new linked typed step.

    :param step: A typed step.
    :returns: Pointer to a newly allocated linked typed step if successful,
              :c:data:`NULL` otherwise.

.. c:function:: CcTypedStepLink * cc_typed_step_link_append( CcTypedStepLink ** ts_link__iod_a, CcTypedStep step )

    Function appends a newly allocated linked position to a given linked list.

    If linked list :c:`*ts_link__iod_a` is :c:data:`NULL`, it will be initialized
    with a newly allocated typed step link as its only element.

    :param ts_link__iod_a: **Ownership**, *optional* *input/output* parameter, linked list.
    :param step: A typed step.
    :returns: Weak pointer to a newly allocated linked typed step if successful,
              :c:data:`NULL` otherwise.

.. c:function:: CcTypedStepLink * cc_typed_step_link_extend( CcTypedStepLink ** ts_link__iod_a, CcTypedStepLink ** ts_link__n )

    Extends existing linked list with a another linked list.

    If linked list to extend (:c:`ts_link__iod_a`) hasn't been allocated yet,
    this will initialize it with content of an extending linked list, i.e.
    :c:`ts_link__n`.

    .. note::

        Extending linked list :c:`ts_link__n` has its ownership transferred to
        extended linked list :c:`ts_link__iod_a`; as a result, inner pointer
        :c:`*ts_link__n` is :c:data:`NULL`\ed.

    :param ts_link__iod_a: **Ownership**, *optional* *input/output* parameter, linked list.
    :param ts_link__n: **Ownership transfer**; linked list with which to extend existing steps.
    :returns: Weak pointer to extended portion of a linked list if successful,
              :c:data:`NULL` otherwise.

.. c:function:: bool cc_typed_step_link_free_all( CcTypedStepLink ** ts_link__f )

    Frees all typed steps in a linked list.

    :param ts_link__f: Linked list of typed steps.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. c:function:: size_t cc_typed_step_link_len( CcTypedStepLink * ts_link )

    Function returns length of a linked list.

    :param ts_link: A linked list of typed steps.
    :returns: Count of typed steps in a linked list if successful,
              ``0`` otherwise.

.. c:function:: char * cc_typed_step_link_to_string__new( CcTypedStepLink * ts_link )

    Function returns string containing user-readable representation
    of a linked list of typed steps.

    :param ts_link: A linked list of typed steps.
    :returns: A newly allocated, null-terminated string if successful,
              :c:data:`NULL` otherwise.

.. _lbl-libcc-cctypedstep-sourcecodeheader:

Header file
-----------

Included source header file is ``cc_typed_step.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_typed_step.h
    :language: C
    :linenos:

.. _lbl-libcc-cctypedstep-sourcecodefile:

Source code file
----------------

Included source code file is ``cc_typed_step.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_typed_step.c
    :language: C
    :linenos:
