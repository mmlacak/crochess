.. Copyright (c) 2021, 2022, 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.
   Included source code files are licensed under GNU GPL v3+ license. See LICENSING, COPYING files in root folder of the project for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccparsedply:

Parsed ply
==========

Documents ``cc_parsed_ply.h`` and ``cc_parsed_ply.c`` files, which contain various
parsed ply definitions and functions.

.. _lbl-libcc-ccparsedply-data:

Data
----

.. c:enum:: CcParsedPlyLinkEnum

    Ply link enumeration.

    This enumerates different ways plies can cascade,
    and directly corresponds to cascading plies separators and terminators.

    .. c:enumerator:: CC_PPLE_None

        Ply link not found, uninitialized, or error happened.

    .. c:enumerator:: CC_PPLE_StartingPly

        Just first ply, standalone or starting a cascade.

    .. c:enumerator:: CC_PPLE_CascadingPly

        Just one ply, continuing cascade. Corresponds to ``~``.

    .. c:enumerator:: CC_PPLE_Teleportation

        Teleportation of piece. Corresponds to ``|``.

    .. c:enumerator:: CC_PPLE_TeleportationReemergence

        Failed teleportation, corresponds to ``||``.

    .. c:enumerator:: CC_PPLE_TeleportationOblation

        Failed teleportation, corresponds to ``|||``.

    .. c:enumerator:: CC_PPLE_TranceJourney

        Trance-journey, corresponds to ``@``.

    .. c:enumerator:: CC_PPLE_DualTranceJourney

        Double trance-journey, corresponds to ``@@``.

    .. c:enumerator:: CC_PPLE_FailedTranceJourney

        Failed trance-journey, corresponds to ``@@@``.

    .. c:enumerator:: CC_PPLE_PawnSacrifice

        Pawn sacrifice, corresponds to ``;;``.

    .. c:enumerator:: CC_PPLE_SenseJourney

        Sense-journey, corresponds to ``"``.

    .. c:enumerator:: CC_PPLE_FailedSenseJourney

        Failed sense-journey, corresponds to ``'``.

    :c:`enum` is tagged with the same :c:enum:`CcParsedPlyLinkEnum` name.

.. c:struct:: CcParsedPly

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
        * - :c:enumerator:`CC_PPLE_Ply`
          - steps taken by a piece
        * - :c:enumerator:`CC_PPLE_Teleportation`
          - steps taken if Wave, otherwise destination field
        * - :c:enumerator:`CC_PPLE_TeleportationReemergence`
          - destination field
        * - :c:enumerator:`CC_PPLE_TeleportationOblation`
          - steps are empty (:c:data:`NULL`)
        * - :c:enumerator:`CC_PPLE_TranceJourney`
          - steps taken by entranced Shaman
        * - :c:enumerator:`CC_PPLE_DualTranceJourney`
          - fields at which pieces are captured, :c:`side_effect` contains captured, or displaced piece, and lost tag
        * - :c:enumerator:`CC_PPLE_FailedTranceJourney`
          - steps are empty (:c:data:`NULL`)
        * - :c:enumerator:`CC_PPLE_PawnSacrifice`
          - steps taken by a Serpent
        * - :c:enumerator:`CC_PPLE_SenseJourney`
          - steps taken by uplifted piece
        * - :c:enumerator:`CC_PPLE_FailedSenseJourney`
          - steps are empty (:c:data:`NULL`)

    .. c:member:: CcParsedPlyLinkEnum link

        Type of link, of this ply, related to previous ply in a cascade.

    .. c:member:: CcPieceType piece

        A piece being moved.

    .. c:member:: CcLosingTagEnum lost_tag

        Flag, whether moving piece has lost its tag.

    .. c:member:: CcParsedStep * steps

        Steps taken by the piece.


    .. c:member:: struct CcParsedPly * next

        Next ply in a cascade.

    :c:`struct` is tagged with the same :c:struct:`CcParsedPly` name.

.. c:macro:: CC_MAX_LEN_PARSED_PLY_LINK_SYMBOL

    Maximum length of a ply link symbol in :term:`AN`; equal to ``3`` (:c:`char`\s).

.. _lbl-libcc-ccparsedply-macros:

Macros
------

Table below contains macros to check if given ply link is specific
:c:enum:`CcParsedPlyLinkEnum` value.

All ply link value macros have a single parameter :c:data:`ple`, which is a ply
link enum, i.e. :c:enum:`CcParsedPlyLinkEnum` value; for instance, macro to check
if ply link is none is defined as :c:expr:`CC_PARSED_PLY_LINK_IS_NONE(ple)`.

All ply link value macros return :c:data:`true` if ply link is a specific value
as defined in a table below, or :c:data:`false` otherwise.

.. list-table:: Macros to check ply link for specific value
   :header-rows: 1
   :align: left
   :widths: 15 10

   * - Macro
     - Value
   * - CC_PARSED_PLY_LINK_IS_NONE
     - :c:enumerator:`CC_PPLE_None`
   * - CC_PARSED_PLY_LINK_IS_STARTING
     - :c:enumerator:`CC_PPLE_StartingPly`
   * - CC_PARSED_PLY_LINK_IS_CASCADING
     - :c:enumerator:`CC_PPLE_CascadingPly`

   * - CC_PARSED_PLY_LINK_IS_TELEPORTATION
     - :c:enumerator:`CC_PPLE_Teleportation`
   * - CC_PARSED_PLY_LINK_IS_TELEPORTATION_REEMERGENCE
     - :c:enumerator:`CC_PPLE_TeleportationReemergence`
   * - CC_PARSED_PLY_LINK_IS_TELEPORTATION_OBLATION
     - :c:enumerator:`CC_PPLE_TeleportationOblation`

   * - CC_PARSED_PLY_LINK_IS_TRANCE_JOURNEY
     - :c:enumerator:`CC_PPLE_TranceJourney`
   * - CC_PARSED_PLY_LINK_IS_DUAL_TRANCE_JOURNEY
     - :c:enumerator:`CC_PPLE_DualTranceJourney`
   * - CC_PARSED_PLY_LINK_IS_FAILED_TRANCE_JOURNEY
     - :c:enumerator:`CC_PPLE_FailedTranceJourney`

   * - CC_PARSED_PLY_LINK_IS_PAWN_SACRIFICE
     - :c:enumerator:`CC_PPLE_PawnSacrifice`

   * - CC_PARSED_PLY_LINK_IS_SENSE_JOURNEY
     - :c:enumerator:`CC_PPLE_SenseJourney`
   * - CC_PARSED_PLY_LINK_IS_FAILED_SENSE_JOURNEY
     - :c:enumerator:`CC_PPLE_FailedSenseJourney`

Macros below check if given value is ply link enumerator, or valid ply link.

.. c:macro:: CC_PARSED_PLY_LINK_IS_ENUMERATOR(ple)

    Macro to check if given value is ply link enumerator.

    :param ple: A given integer value.
    :returns: :c:data:`true` if given integer is ply link, i.e.
        :c:enum:`CcParsedPlyLinkEnum` value, :c:data:`false` otherwise.

.. c:macro:: CC_PARSED_PLY_LINK_IS_VALID(ple)

    Macro to check if given value is valid ply link.

    :param ple: A given integer value.
    :returns: :c:data:`true` if given integer is valid ply link, i.e.
        :c:enum:`CcParsedPlyLinkEnum` value, :c:data:`false` otherwise.

Macros in the table below check ply link for specific :c:enum:`CcParsedPlyLinkEnum`
values, otherwise they are identical to macros in the table above.

.. list-table:: Macros to check ply link for specific values
   :header-rows: 1
   :align: left
   :widths: 5 10

   * - Macro
     - Values
   * - CC_PARSED_PLY_LINK_IS_ANY_TELEPORTATION
     - :c:enumerator:`CC_PPLE_Teleportation`,
       :c:enumerator:`CC_PPLE_TeleportationReemergence`,
       :c:enumerator:`CC_PPLE_TeleportationOblation`
   * - CC_PARSED_PLY_LINK_IS_ANY_TRANCE_JOURNEY
     - :c:enumerator:`CC_PPLE_TranceJourney`,
       :c:enumerator:`CC_PPLE_DualTranceJourney`,
       :c:enumerator:`CC_PPLE_FailedTranceJourney`
   * - CC_PARSED_PLY_LINK_IS_ANY_SENSE_JOURNEY
     - :c:enumerator:`CC_PPLE_SenseJourney`,
       :c:enumerator:`CC_PPLE_FailedSenseJourney`

.. _lbl-libcc-ccparsedply-functions:

Functions
---------

.. c:function:: char const * cc_parsed_ply_link_symbol( CcParsedPlyLinkEnum ple )

    Function returns string symbol, as used in algebraic notation,
    for a given ply link.

    Returned string is not allocated, so do not :c:func:`free()` it.

    :param ple: A ply linkage.
    :returns: String symbol if link is valid, :c:data:`NULL` otherwise.

.. c:function:: CcParsedPly * cc_parsed_ply__new( CcParsedPlyLinkEnum link, CcPieceType piece, CcLosingTagEnum lost_tag, CcParsedStep ** steps__n )

    Function returns newly allocated ply.

    Takes ownership of :c:`steps__n`, inner pointer will be set to :c:data:`NULL`,
    if valid ply is produced.

    :param link: Link to previous ply in a cascade.
    :param piece: A piece making a ply.
    :param lost_tag: Tag lost by a moving piece.
    :param steps__n: **Ownership transfer**; steps, linked list, inner pointer can be :c:data:`NULL`.
    :returns: A newly allocated ply if successful, :c:data:`NULL` otherwise.

.. c:function:: CcParsedPly * cc_parsed_ply_append( CcParsedPly ** plies__iod_a, CcParsedPlyLinkEnum link, CcPieceType piece, CcLosingTagEnum lost_tag, CcParsedStep ** steps__n )

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
    :seealso: :c:func:`cc_parsed_ply__new()`

.. c:function:: CcParsedPly * cc_parsed_ply_duplicate_all__new( CcParsedPly * plies )

    Duplicates a given plies, and all accompanying resources,
    into a newly allocated linked list.

    :param plies: Linked list to duplicate.
    :returns: A newly allocated duplicate of :c:`plies` if successful,
              :c:data:`NULL` otherwise.

.. c:function:: CcParsedPly * cc_parsed_ply_extend( CcParsedPly ** plies__iod_a, CcParsedPly ** plies__d_n )

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
    :seealso: :c:func:`cc_parsed_ply_append()`

.. c:function:: bool cc_parsed_ply_free_all( CcParsedPly ** plies__f )

    Frees all plies in a linked list, and all associated entities.

    :param plies__f: Linked list of plies to :c:func:`free()`.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. c:function:: bool cc_parsed_ply_contains_side_effects( CcParsedPly * ply )

    Checks whether any step in a ply has side-effects.

    :param ply: A ply.
    :returns: :c:data:`true` if any step has side-effects, :c:data:`false` otherwise.

.. c:function:: CcPieceType cc_parsed_ply_find_activator( CcParsedPly * plies, CcParsedPly * ply__d )

    Function finds :term:`activator` in a given linked list of plies.

    If a ply within that linked list is also specified, last :term:`activator`
    preceding specified ply is returned.

    .. note::

        If a ply is specified, but does not belong to a given linked list of
        plies, :c:enumerator:`CC_PE_None` is returned instead, indicating failure.

    :param plies: A linked list of plies.
    :param ply__d: *Optional*; a ply within given linked list, can be :c:data:`NULL`.
    :returns: :term:`Activator` if successful, :c:enumerator:`CC_PE_None` otherwise.

.. c:function:: char * cc_parsed_ply_all_to_short_string__new( CcParsedPly * plies )

    Function returns newly allocated string, containing user-readable
    representation of a plies.

    :param plies: Linked list of plies.
    :returns: A newly allocated, zero-terminated string if successful,
              :c:data:`NULL` otherwise.

.. _lbl-libcc-ccparsedply-sourcecodeheader:

Header file
-----------

Included source header file is ``cc_parsed_ply.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_parsed_ply.h
    :language: C
    :linenos:

.. _lbl-libcc-ccparsedply-sourcecodefile:

Source code file
----------------

Included source code file is ``cc_parsed_ply.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_parsed_ply.c
    :language: C
    :linenos:
