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

    .. c:member:: char * notation

        Copy of move notation, originating this ply.


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

.. _lbl-libcc-ccparsedply-functions:

Functions
---------

.. c:function:: char const * cc_parsed_ply_link_symbol( CcParsedPlyLinkEnum ple )

    Function returns string symbol, as used in algebraic notation,
    for a given ply link.

    Returned string is not allocated, so do not :c:func:`free()` it.

    :param ple: A ply linkage.
    :returns: String symbol if link is valid, :c:data:`NULL` otherwise.

.. c:function:: CcParsedPly * cc_parsed_ply__new( char const * start_an__d, char const * end_an__d, size_t max_len__d, CcParsedPlyLinkEnum link, CcPieceType piece, CcLosingTagEnum lost_tag, CcParsedStep ** steps__n )

    Function returns newly allocated ply.

    .. warning::

        If no *optional* end arguments (:c:`end_an__d`, :c:`max_len__d`) are given,
        annotation string (:c:`start_an__d`) has to be zero-terminated, or :c:data:`NULL`.

    Takes ownership of :c:`steps__n`, inner pointer will be set to :c:data:`NULL`,
    if valid ply is produced.

    :param start_an__d: *Optional*; start of a ply notation substring. Can be :c:data:`NULL`, if so :c:`notation` member is initialized to :c:data:`NULL`.
    :param end_an__d: *Optional*; end of a ply notation substring. Can be :c:data:`NULL`, if so whole zero-terminated :c:`start_an__d` string is copied.
    :param max_len__d: *Optional*, maximum length of :c:`notation` to copy. Can be :c:macro:`CC_MAX_LEN_ZERO_TERMINATED`, if so whole zero-terminated :c:`start_an__d` string is copied.
    :param link: Link to previous ply in a cascade.
    :param piece: A piece making a ply.
    :param lost_tag: Tag lost by a moving piece.
    :param steps__n: **Ownership transfer**; steps, linked list, inner pointer can be :c:data:`NULL`.
    :returns: A newly allocated ply if successful, :c:data:`NULL` otherwise.

.. c:function:: CcParsedPly * cc_parsed_ply_append( CcParsedPly ** plies__iod_a, char const * start_an__d, char const * end_an__d, size_t max_len__d, CcParsedPlyLinkEnum link, CcPieceType piece, CcLosingTagEnum lost_tag, CcParsedStep ** steps__n )

    Appends a newly allocated ply to a given linked list.

    .. warning::

        If no *optional* end arguments (:c:`end_an__d`, :c:`max_len__d`) are given,
        annotation string (:c:`start_an__d`) has to be zero-terminated, or :c:data:`NULL`.

    Takes ownership of :c:`steps__n`, inner pointer will be set to :c:data:`NULL`,
    if valid ply is produced.

    If linked list :c:`*plies__iod_a` is :c:data:`NULL`, it will be initialized
    with a newly allocated ply as its only element.

    :param plies__iod_a: **Ownership**, *optional* *input/output* parameter; linked list of plies,
                         to which a new ply is appended, inner pointer can be :c:data:`NULL`.
    :param start_an__d: *Optional*; start of a ply notation substring. Can be :c:data:`NULL`, if so :c:`notation` member is initialized to :c:data:`NULL`.
    :param end_an__d: *Optional*; end of a ply notation substring. Can be :c:data:`NULL`, if so whole zero-terminated :c:`start_an__d` string is copied.
    :param max_len__d: *Optional*, maximum length of :c:`notation` to copy. Can be :c:macro:`CC_MAX_LEN_ZERO_TERMINATED`, if so whole zero-terminated :c:`start_an__d` string is copied.
    :param link: Link to previous ply in a cascade.
    :param piece: A piece making a ply.
    :param lost_tag: Tag lost by a moving piece.
    :param steps__n: **Ownership transfer**; steps, linked list, inner pointer can be :c:data:`NULL`.
    :returns: Weak pointer to a newly allocated ply if successful, :c:data:`NULL` otherwise.
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
