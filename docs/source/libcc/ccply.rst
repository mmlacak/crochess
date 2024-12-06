.. Copyright (c) 2021, 2022, 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.
   Included source code files are licensed under GNU GPL v3+ license. See LICENSING, COPYING files in root folder of the project for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccply:

Ply
===

Documents ``cc_ply.h`` and ``cc_ply.c`` files, which contain various
parsed ply definitions and functions.

.. _lbl-libcc-ccply-data:

Data
----

.. c:enum:: CcPlyLinkTypeEnum

    Ply link enumeration.

    This enumerates different ways plies can cascade,
    and directly corresponds to cascading plies separators and terminators.

    .. c:enumerator:: CC_PLTE_None

        Ply link not found, uninitialized, or error happened.

    .. c:enumerator:: CC_PLTE_StartingPly

        Just first ply, standalone or starting a cascade.

    .. c:enumerator:: CC_PLTE_CascadingPly

        Just one ply, continuing cascade. Corresponds to ``~``.

    .. c:enumerator:: CC_PLTE_Teleportation

        Teleportation of piece. Corresponds to ``|``.

    .. c:enumerator:: CC_PLTE_TeleportationReemergence

        Failed teleportation, corresponds to ``||``.

    .. c:enumerator:: CC_PLTE_TeleportationOblation

        Failed teleportation, corresponds to ``|||``.

    .. c:enumerator:: CC_PLTE_TranceJourney

        Trance-journey, corresponds to ``@``.

    .. c:enumerator:: CC_PLTE_DualTranceJourney

        Double trance-journey, corresponds to ``@@``.

    .. c:enumerator:: CC_PLTE_FailedTranceJourney

        Failed trance-journey, corresponds to ``@@@``.

    .. c:enumerator:: CC_PLTE_PawnSacrifice

        Pawn sacrifice, corresponds to ``;;``.

    .. c:enumerator:: CC_PLTE_SenseJourney

        Sense-journey, corresponds to ``"``.

    .. c:enumerator:: CC_PLTE_FailedSenseJourney

        Failed sense-journey, corresponds to ``'``.

    :c:`enum` is tagged with the same :c:enum:`CcPlyLinkTypeEnum` name.

.. c:struct:: CcPly

    Ply structure, linked list.

    :c:`steps` change meaning, depending on ply :c:`link`.

    :c:`steps` can have only one item in a linked list, if a single destination field is needed.

    :c:`steps` can be empty (:c:data:`NULL`) for certain ply links.

    .. list-table:: Steps depending on their links table
        :header-rows: 1
        :align: left
        :widths: 35 95

        * - link
          - steps
        * - :c:enumerator:`CC_PLTE_StartingPly`
          - steps taken by a piece
        * - :c:enumerator:`CC_PLTE_CascadingPly`
          - steps taken by a piece
        * - :c:enumerator:`CC_PLTE_Teleportation`
          - steps taken if Wave, otherwise destination field
        * - :c:enumerator:`CC_PLTE_TeleportationReemergence`
          - destination field
        * - :c:enumerator:`CC_PLTE_TeleportationOblation`
          - steps are empty (:c:data:`NULL`)
        * - :c:enumerator:`CC_PLTE_TranceJourney`
          - steps taken by entranced Shaman
        * - :c:enumerator:`CC_PLTE_DualTranceJourney`
          - fields at which pieces are captured, :c:`side_effect` contains captured, or displaced piece, and lost tag
        * - :c:enumerator:`CC_PLTE_FailedTranceJourney`
          - steps are empty (:c:data:`NULL`)
        * - :c:enumerator:`CC_PLTE_PawnSacrifice`
          - steps taken by a Serpent
        * - :c:enumerator:`CC_PLTE_SenseJourney`
          - steps taken by uplifted piece
        * - :c:enumerator:`CC_PLTE_FailedSenseJourney`
          - steps are empty (:c:data:`NULL`)

    .. c:member:: CcPlyLinkTypeEnum link

        Type of link, of this ply, related to previous ply in a cascade.

    .. c:member:: CcPieceType piece

        A piece being moved.

    .. c:member:: CcLosingTagType lost_tag

        Flag, whether moving piece has lost its tag.

    .. c:member:: CcStep * steps

        Steps taken by the piece.


    .. c:member:: struct CcPly * next

        Next ply in a cascade.

    :c:`struct` is tagged with the same :c:struct:`CcPly` name.

.. c:macro:: CC_MAX_LEN_PLY_LINK_TYPE_SYMBOL

    Maximum length of a ply link symbol in :term:`AN`; equal to ``3`` (:c:`char`\s).

.. _lbl-libcc-ccply-macros:

Macros
------

Macros below check if given value is ply link type enumerator, or valid ply link type.

.. c:macro:: CC_PLY_LINK_TYPE_IS_ENUMERATOR(plte)

    Macro to check if given value is ply link type enumerator.

    :param plte: A given integer value.
    :returns: :c:data:`true` if given integer is ply link type, i.e.
        :c:enum:`CcPlyLinkTypeEnum` value, :c:data:`false` otherwise.

.. c:macro:: CC_PLY_LINK_TYPE_IS_VALID(plte)

    Macro to check if given value is valid ply link type.

    :param plte: A given integer value.
    :returns: :c:data:`true` if given integer is valid ply link type, i.e.
        :c:enum:`CcPlyLinkTypeEnum` value, :c:data:`false` otherwise.

Macros in the table below check ply link type for specific :c:enum:`CcPlyLinkTypeEnum`
values, otherwise they are identical to macros in the table above.

.. list-table:: Macros to check ply link type for specific values
   :header-rows: 1
   :align: left
   :widths: 5 10

   * - Macro
     - Values
   * - CC_PLY_LINK_TYPE_IS_ANY_TELEPORTATION
     - .. line-block::
        :c:enumerator:`CC_PLTE_Teleportation`
        :c:enumerator:`CC_PLTE_TeleportationReemergence`
        :c:enumerator:`CC_PLTE_TeleportationOblation`
   * - CC_PLY_LINK_TYPE_IS_ANY_TRANCE_JOURNEY
     - .. line-block::
        :c:enumerator:`CC_PLTE_TranceJourney`
        :c:enumerator:`CC_PLTE_DualTranceJourney`
        :c:enumerator:`CC_PLTE_FailedTranceJourney`
   * - CC_PLY_LINK_TYPE_IS_ANY_SENSE_JOURNEY
     - .. line-block::
        :c:enumerator:`CC_PLTE_SenseJourney`
        :c:enumerator:`CC_PLTE_FailedSenseJourney`
   * - CC_PLY_LINK_TYPE_IS_ACTIVATING_PIECE
     - .. line-block::
        :c:enumerator:`CC_PLTE_CascadingPly`
        :c:enumerator:`CC_PLTE_TranceJourney`
        :c:enumerator:`CC_PLTE_DualTranceJourney`
        :c:enumerator:`CC_PLTE_FailedTranceJourney`
        :c:enumerator:`CC_PLTE_SenseJourney`
        :c:enumerator:`CC_PLTE_FailedSenseJourney`

.. _lbl-libcc-ccply-functions:

Functions
---------

.. c:function:: char const * cc_ply_link_type_symbol( CcPlyLinkTypeEnum plte )

    Function returns string symbol, as used in algebraic notation,
    for a given ply link.

    Returned string is not allocated, so do not :c:func:`free()` it.

    :param plte: A ply linkage.
    :returns: String symbol if link is valid, :c:data:`NULL` otherwise.

.. c:function:: CcPly * cc_ply__new( CcPlyLinkTypeEnum link, CcPieceType piece, CcLosingTagType lost_tag, CcStep ** steps__n )

    Function returns newly allocated ply.

    Takes ownership of :c:`steps__n`, inner pointer will be set to :c:data:`NULL`,
    if valid ply is produced.

    :param link: Link to previous ply in a cascade.
    :param piece: A piece making a ply.
    :param lost_tag: Tag lost by a moving piece.
    :param steps__n: **Ownership transfer**; steps, linked list, inner pointer can be :c:data:`NULL`.
    :returns: A newly allocated ply if successful, :c:data:`NULL` otherwise.

.. c:function:: CcPly * cc_ply_append( CcPly ** plies__iod_a, CcPlyLinkTypeEnum link, CcPieceType piece, CcLosingTagType lost_tag, CcStep ** steps__n )

    Appends a newly allocated ply to a given linked list.

    Takes ownership of :c:`steps__n`, inner pointer will be set to :c:data:`NULL`,
    if valid ply is produced.

    If linked list :c:`*plies__iod_a` is :c:data:`NULL`, it will be initialized
    with a newly allocated ply as its only element.

    :param plies__iod_a: **Ownership**, *optional* *input/output* parameter;
        linked list of plies, to which a new ply is appended, inner pointer
        can be :c:data:`NULL`.
    :param link: Link to previous ply in a cascade.
    :param piece: A piece making a ply.
    :param lost_tag: Tag lost by a moving piece.
    :param steps__n: **Ownership transfer**; steps, linked list, inner pointer
        can be :c:data:`NULL`.
    :returns: Weak pointer to a newly allocated ply if successful, :c:data:`NULL`
        otherwise.
    :seealso: :c:func:`cc_ply__new()`

.. c:function:: CcPly * cc_ply_duplicate_all__new( CcPly * plies )

    Duplicates a given plies, and all accompanying resources,
    into a newly allocated linked list.

    :param plies: Linked list to duplicate.
    :returns: A newly allocated duplicate of :c:`plies` if successful,
              :c:data:`NULL` otherwise.

.. c:function:: CcPly * cc_ply_extend( CcPly ** plies__iod_a, CcPly ** plies__d_n )

    Extends existing linked list with a newly allocated plies.

    If linked list to extend (:c:`plies__iod_a`) hasn't been allocated yet,
    this will initialize it with content of an extending linked list, i.e.
    :c:`plies__d_n`.

    .. note::

        Extending linked list :c:`plies__d_n` has its ownership transferred to
        extended linked list :c:`plies__iod_a`; as a result, inner pointer
        :c:`*plies__d_n` is :c:data:`NULL`\ed.

    :param plies__iod_a: **Ownership**, *optional* *input/output* parameter; linked list of plies,
                         to which a new ply is appended, inner pointer can be :c:data:`NULL`.
    :param plies__d_n: **Ownership transfer**, *optional*; plies, linked list, inner pointer can be
                       :c:data:`NULL`.
    :returns: Weak pointer to extended portion of a linked list if successful,
              :c:data:`NULL` otherwise.
    :seealso: :c:func:`cc_ply_append()`

.. c:function:: bool cc_ply_free_all( CcPly ** plies__f )

    Frees all plies in a linked list, and all associated entities.

    :param plies__f: Linked list of plies to :c:func:`free()`.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. c:function:: bool cc_ply_contains_side_effects( CcPly * ply )

    Checks whether any step in a ply has side-effects.

    :param ply: A ply.
    :returns: :c:data:`true` if any step has side-effects, :c:data:`false` otherwise.

.. c:function:: CcPieceType cc_ply_find_activator( CcPly * plies, CcPly * ply__d )

    Function finds :term:`activator` in a given linked list of plies.

    If a ply within that linked list is also specified, last :term:`activator`
    preceding specified ply is returned.

    .. note::

        If a ply is specified, but does not belong to a given linked list of
        plies, :c:enumerator:`CC_PE_None` is returned instead, indicating failure.

    :param plies: A linked list of plies.
    :param ply__d: *Optional*; a ply within given linked list, can be :c:data:`NULL`.
    :returns: :term:`Activator` if successful, :c:enumerator:`CC_PE_None` otherwise.

.. c:function:: char * cc_ply_all_to_string__new( CcPly * plies )

    Function returns newly allocated string, containing user-readable
    representation of a plies.

    :param plies: Linked list of plies.
    :returns: A newly allocated, zero-terminated string if successful,
              :c:data:`NULL` otherwise.

.. _lbl-libcc-ccply-sourcecodeheader:

Header file
-----------

Included source header file is ``cc_ply.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_ply.h
    :language: C
    :linenos:

.. _lbl-libcc-ccply-sourcecodefile:

Source code file
----------------

Included source code file is ``cc_ply.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_ply.c
    :language: C
    :linenos:
