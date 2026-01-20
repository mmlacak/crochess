.. Copyright (c) 2021, 2022, 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.
   Included source code files are licensed under GNU GPL v3+ license. See LICENSING, COPYING files in root folder of the project for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccstep:

Step
====

Documents ``cc_step.h`` and ``cc_step.c`` files, which contain various
parsed step definitions and functions.

.. _lbl-libcc-ccstep-data:

Data
----

.. c:macro:: CC_STEP_LINK_TYPE_IS_ENUMERATOR(sle)

    Macro to check if a given step link is enumerator.

    :param sle: Step link enumeration, :c:enum:`CcStepLinkTypeEnum` value.
    :returns: :c:data:`true` if enumerator, :c:data:`false` otherwise.

.. c:macro:: CC_STEP_LINK_TYPE_IS_VALID(sle)

    Macro to check if a given step link is valid enumerator.

    :param sle: Step link enumeration, :c:enum:`CcStepLinkTypeEnum` value.
    :returns: :c:data:`true` if valid enumerator, :c:data:`false` otherwise.

.. c:macro:: CC_STEP_LINK_TYPE_IS_DESTINATION(sle)

    Macro to check if a given step link is destination.

    :param sle: Step link enumeration, :c:enum:`CcStepLinkTypeEnum` value.
    :returns: :c:data:`true` if destination, :c:data:`false` otherwise.

.. c:enum:: CcStepLinkTypeEnum

    Step link enumeration.

    .. c:enumerator:: CC_SLTE_None

        Step link not found, uninitialized, not parsed yet, or error happened.

    .. c:enumerator:: CC_SLTE_InitialPosition

        Position at which a piece was located before it started moving.

    .. c:enumerator:: CC_SLTE_Reposition

        In trance-journey, dark Shaman's distant starting field; separated by ``,`` (comma).

    .. c:enumerator:: CC_SLTE_Next

        Step immediately following previous, separated by ``.`` (dot).

    .. c:enumerator:: CC_SLTE_Distant

        Step not immediately following previous, separated by ``..`` (double-dot).

    .. c:enumerator:: CC_SLTE_Destination

        Step to destination field, separated by ``-`` (hyphen).

    .. c:enumerator:: CC_SLTE_JustDestination

        Just destination field, no separators, no other steps.

    :c:`enum` is tagged with the same :c:enum:`CcStepLinkTypeEnum` name.

.. c:macro:: CC_MAX_LEN_STEP_LINK_TYPE_SYMBOL

    Macro constant for maximum length of a step link symbol; equals to ``2`` :c:`char`\s.

.. c:struct:: CcStep

    Step :c:`struct`\ure, linked list.

    .. c:member:: CcStepLinkTypeEnum link

        Type of a link to previous step.

    .. c:member:: CcPos field

        Field of a step.

    .. c:member:: CcSideEffect side_effect

        Side-effect structure.

    .. c:member:: CcSideEffectLink * tentative__d

        Possible side-effects, a linked list; it's used for displacements
        when building a path tree. Usually, it's not used, so it's denoted
        as *optional*.

        .. note::

            Even though this linked list is *optional*, pointer does have
            ownership when linked list is present.

    .. c:member:: struct CcStep * next

        Next step in a linked list.

    :c:`struct` is tagged with the same :c:struct:`CcStep` name.

.. _lbl-libcc-ccstep-functions:

Functions
---------

.. c:function:: char const * cc_step_link_type_symbol( CcStepLinkTypeEnum sle )

    Function returns string symbol, as used in algebraic notation,
    for a given step link.

    Returned string is not allocated, so do not :c:func:`free()` it.

    :param sle: A step linkage.
    :returns: String symbol if link is valid, :c:data:`NULL` otherwise.

.. c:function:: CcStep * cc_step__new( CcStepLinkTypeEnum link, CcPos field, CcSideEffect side_effect )

    Returns a newly allocated step.

    :param link: Type of a link to a previous step.
    :param field: A field.
    :param side_effect: Side-effect :c:`struct`\ure.
    :returns: A newly allocated step if successful, :c:data:`NULL` otherwise.

.. c:function:: CcStep * cc_step_initial_no_side_effect__new( CcPos field )

    Convenience function, returns a newly allocated step, with
    :c:enumerator:`CC_SLTE_InitialPosition` linkage, and no side-effect.

    :param field: A field.
    :returns: A newly allocated step if successful, :c:data:`NULL` otherwise.
    :seealso: :c:func:`cc_step__new()`

.. c:function:: CcStep * cc_step_initial__new( CcPos field, CcSideEffect side_effect )

    Convenience function, returns a newly allocated step, with
    :c:enumerator:`CC_SLTE_InitialPosition` linkage, and a given side-effect.

    :param field: A field.
    :param side_effect: Side-effect :c:`struct`\ure.
    :returns: A newly allocated step if successful, :c:data:`NULL` otherwise.
    :seealso: :c:func:`cc_step__new()`

.. c:function:: CcStep * cc_step_next_no_side_effect__new( CcPos field )

    Convenience function, returns a newly allocated step, with :c:enumerator:`CC_SLTE_Next`
    linkage, and no side-effect.

    :param field: A field.
    :returns: A newly allocated step if successful, :c:data:`NULL` otherwise.
    :seealso: :c:func:`cc_step__new()`

.. c:function:: CcStep * cc_step_next__new( CcPos field, CcSideEffect side_effect )

    Convenience function, returns a newly allocated step, with :c:enumerator:`CC_SLTE_Next`
    linkage.

    :param field: A field.
    :param side_effect: Side-effect :c:`struct`\ure.
    :returns: A newly allocated step if successful, :c:data:`NULL` otherwise.

.. c:function:: CcStep * cc_step_append( CcStep ** steps__iod_a, CcStepLinkTypeEnum link, CcPos field, CcSideEffect side_effect )

    Appends a newly allocated step to a given linked list.

    If linked list :c:`*steps__iod_a` is :c:data:`NULL`, it will be initialized
    with a newly allocated step link as its only element.

    :param steps__iod_a: **Ownership**, *optional* *input/output* parameter; linked list of steps
                         to which a new step is appended, inner pointer can be :c:data:`NULL`.
    :param link: Type of a link to a previous step.
    :param field: A field.
    :param side_effect: Side-effect :c:`struct`\ure.
    :returns: A weak pointer to newly allocated step if successful, :c:data:`NULL` otherwise.

.. c:function:: CcStep * cc_step_append_next_no_side_effect( CcStep ** steps__iod_a, CcPos field )

    Convenience function, appends a newly allocated step to a given linked list,
    with :c:enumerator:`CC_SLTE_Next` linkage, and no side-effect.

    :param field: A field.
    :returns: A weak pointer to newly allocated step if successful, :c:data:`NULL` otherwise.
    :seealso: :c:func:`cc_step_append()`

.. c:function:: CcStep * cc_step_duplicate_all__new( CcStep * steps, bool include_tentative )

    Duplicates all given steps into a newly allocated linked list.

    :param sle: Linked list to duplicate.
    :param include_tentative: Flag, whether :c:member:`CcStep.tentative__d` should also be duplicated, or not.
    :returns: A newly allocated steps if successful, :c:data:`NULL` otherwise.

.. c:function:: CcStep * cc_step_extend( CcStep ** steps__iod_a, CcStep ** steps__d_n )

    Extends given linked list of steps with another.

    If linked list to extend (:c:`steps__iod_a`) hasn't been allocated yet,
    this will initialize it with content of an extending linked list, i.e.
    :c:`steps__d_n`.

    .. note::

        Extending linked list :c:`steps__d_n` has its ownership transferred to
        extended linked list :c:`steps__iod_a`; as a result, inner pointer
        :c:`*steps__d_n` is :c:data:`NULL`\ed.

    :param steps__iod_a: **Ownership**, *optional* *input/output*; linked list to extend.
    :param steps__d_n: **Ownership transfer**, *optional*; linked list to extend existing steps.
    :returns: Weak pointer to extended portion of a linked list if successful,
              :c:data:`NULL` otherwise.

.. c:function:: size_t cc_step_count( CcStep * steps, bool do_momentum )

    Function returning count of steps, or momentum, depending on a given flag.

    For momentum, given :c:var:`steps` has to be linked as next, i.e. with
    :c:enumerator:`CC_SLTE_Next` link type; and only those steps are counted
    towards momentum. First step can be initial one (i.e. has
    :c:enumerator:`CC_SLTE_InitialPosition` link type), and second can be
    repositioning step (i.e. :c:enumerator:`CC_SLTE_Reposition`).

    :param steps: Linked list of steps.
    :param do_momentum: Flag, whether to count momentum, or any steps.
    :returns: Count of steps if successful, ``0`` otherwise.

.. c:function:: CcStep * cc_step_fetch_initial( CcStep * steps )

    Function returns step containing initial position.

    Function also checks if initial position is only present in the first step link.

    :param steps: Linked list of steps.
    :returns: Starting step if successful, :c:data:`NULL` otherwise.

.. c:function:: CcStep * cc_step_fetch_destination( CcStep * steps )

    Function returns destination step.

    Destination step returned by this function is the step preceded by
    destination separator, i.e. `-` (hyphen), or is the last
    (non-starting) step in a given list.

    :param steps: Linked list of steps.
    :returns: Destination step if successful, :c:data:`NULL` otherwise.

.. c:function:: CcSideEffect * cc_step_fetch_last_side_effect( CcStep * steps )

    Function returns side-effect of a destination step.

    :param steps: Linked list of steps.
    :returns: Side-effect if successful, :c:data:`NULL` otherwise.
    :seealso: :c:func:`cc_step_fetch_destination()`

.. c:function:: bool cc_step_free_all( CcStep ** steps__f )

    Frees all steps in a linked list.

    :param steps__f: Linked list of steps.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. c:function:: char * cc_step_all_to_string__new( CcStep * steps )

    Function returns a newly allocated string, containing user-readable
    representation of steps in a given linked list.

    Each step is preceded by the same separator as used in AN, e.g. ``..``
    (double dot) is used for a distant step.

    Starting step is preceded by ````` (back-tick).
    Steps with unknown linkage are preceded by ``?`` (question-mark).

    :param steps: Linked list of steps.
    :returns: A newly allocated, null-terminated string if successful,
              :c:data:`NULL` otherwise

.. _lbl-libcc-ccstep-newfunctions:

New step functions
^^^^^^^^^^^^^^^^^^

    The new step convenience functions are meant to be used instead of
    :c:func:`cc_step__new()`.

    They have minimal set of parameters required by the type of a step
    (its linkage), otherwise they behave exactly as their generic
    progenitor.

.. c:function:: CcStep * cc_step_none__new( CcStepLinkTypeEnum link, CcPos field )

.. c:function:: CcStep * cc_step_capture__new( CcStepLinkTypeEnum link, CcPos field, CcPieceTagType piece )

.. c:function:: CcStep * cc_step_displacement__new( CcStepLinkTypeEnum link, CcPos field, CcPieceTagType piece, CcPos destination )

.. c:function:: CcStep * cc_step_en_passant__new( CcStepLinkTypeEnum link, CcPos field, CcPieceTagType private, CcPos distant )

.. c:function:: CcStep * cc_step_castle__new( CcStepLinkTypeEnum link, CcPos field, CcPieceTagType rook, CcPos start, CcPos destination )

.. c:function:: CcStep * cc_step_promote__new( CcStepLinkTypeEnum link, CcPos field, CcPieceTagType captured, CcPieceTagType promoted_to )

.. c:function:: CcStep * cc_step_tag_for_promotion__new( CcStepLinkTypeEnum link, CcPos field, CcPieceTagType captured )

.. c:function:: CcStep * cc_step_convert__new( CcStepLinkTypeEnum link, CcPos field, CcPieceTagType piece )

.. c:function:: CcStep * cc_step_failed_conversion__new( CcStepLinkTypeEnum link, CcPos field )

.. c:function:: CcStep * cc_step_demote__new( CcStepLinkTypeEnum link, CcPos field, CcPieceTagType piece, CcPos distant )

.. c:function:: CcStep * cc_step_resurrect__new( CcStepLinkTypeEnum link, CcPos field, CcPieceTagType piece, CcPos destination )

.. c:function:: CcStep * cc_step_failed_resurrection__new( CcStepLinkTypeEnum link, CcPos field )

.. _lbl-libcc-ccstep-appendfunctions:

Append step functions
^^^^^^^^^^^^^^^^^^^^^

    The append new step convenience functions are meant to be used instead of
    :c:func:`cc_step_append()`.

    They have minimal set of parameters required by the type of a step
    (its linkage), otherwise they behave exactly as their generic
    progenitor.

.. c:function:: CcStep * cc_step_none_append( CcStep ** steps__iod_a, CcStepLinkTypeEnum link, CcPos field )

.. c:function:: CcStep * cc_step_capture_append( CcStep ** steps__iod_a, CcStepLinkTypeEnum link, CcPos field, CcPieceTagType piece )

.. c:function:: CcStep * cc_step_displacement_append( CcStep ** steps__iod_a, CcStepLinkTypeEnum link, CcPos field, CcPieceTagType piece, CcPos destination )

.. c:function:: CcStep * cc_step_en_passant_append( CcStep ** steps__iod_a, CcStepLinkTypeEnum link, CcPos field, CcPieceTagType private, CcPos distant )

.. c:function:: CcStep * cc_step_castle_append( CcStep ** steps__iod_a, CcStepLinkTypeEnum link, CcPos field, CcPieceTagType rook, CcPos start, CcPos destination )

.. c:function:: CcStep * cc_step_promote_append( CcStep ** steps__iod_a, CcStepLinkTypeEnum link, CcPos field, CcPieceTagType captured, CcPieceTagType promoted_to )

.. c:function:: CcStep * cc_step_tag_for_promotion_append( CcStep ** steps__iod_a, CcStepLinkTypeEnum link, CcPos field, CcPieceTagType captured )

.. c:function:: CcStep * cc_step_convert_append( CcStep ** steps__iod_a, CcStepLinkTypeEnum link, CcPos field, CcPieceTagType piece )

.. c:function:: CcStep * cc_step_failed_conversion_append( CcStep ** steps__iod_a, CcStepLinkTypeEnum link, CcPos field )

.. c:function:: CcStep * cc_step_demote_append( CcStep ** steps__iod_a, CcStepLinkTypeEnum link, CcPos field, CcPieceTagType piece, CcPos distant )

.. c:function:: CcStep * cc_step_resurrect_append( CcStep ** steps__iod_a, CcStepLinkTypeEnum link, CcPos field, CcPieceTagType piece, CcPos destination )

.. c:function:: CcStep * cc_step_failed_resurrection_append( CcStep ** steps__iod_a, CcStepLinkTypeEnum link, CcPos field )

.. _lbl-libcc-ccstep-sourcecodeheader:

Header file
-----------

Included source header file is ``cc_step.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_step.h
    :language: C
    :linenos:

.. _lbl-libcc-ccstep-sourcecodefile:

Source code file
----------------

Included source code file is ``cc_step.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_step.c
    :language: C
    :linenos:
