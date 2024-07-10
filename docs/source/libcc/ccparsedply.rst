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

Parsed ply data
---------------

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

    :c:`enum` is tagged with the same :c:expr:`CcParsedPlyLinkEnum` name.

.. _lbl-libcc-ccparsedply-functions:

.. c:struct:: CcParsedPly

    Ply structure, linked list.

    :c:`steps` change meaning, depending on ply :c:`link`.

    :c:`steps` can have only one item in a linked list, if a single destination field is needed.

    :c:`steps` can be empty (:c:`NULL`) for certain ply links.

    .. list-table:: Steps depending on their links table
        :header-rows: 1
        :align: left
        :widths: 35 95

        * - link
          - steps
        * - :c:`CC_PPLE_Ply`
          - steps taken by a piece
        * - :c:`CC_PPLE_Teleportation`
          - steps taken if Wave, otherwise destination field
        * - :c:`CC_PPLE_TeleportationReemergence`
          - destination field
        * - :c:`CC_PPLE_TeleportationOblation`
          - steps are empty (:c:`NULL`)
        * - :c:`CC_PPLE_TranceJourney`
          - steps taken by entranced Shaman
        * - :c:`CC_PPLE_DualTranceJourney`
          - fields at which pieces are captured, :c:`side_effect` contains captured, or displaced piece, and lost tag
        * - :c:`CC_PPLE_FailedTranceJourney`
          - steps are empty (:c:`NULL`)
        * - :c:`CC_PPLE_PawnSacrifice`
          - steps taken by a Serpent
        * - :c:`CC_PPLE_SenseJourney`
          - steps taken by uplifted piece
        * - :c:`CC_PPLE_FailedSenseJourney`
          - steps are empty (:c:`NULL`)

    .. c:member:: char * notation

        Copy of move notation, originating this ply.


    .. c:member:: CcParsedPlyLinkEnum link

        Type of link, of this ply, related to previous ply in a cascade.

    .. c:member:: CcPieceEnum piece

        A piece being moved.

    .. c:member:: CcLosingTagEnum lost_tag

        Flag, whether moving piece has lost its tag.

    .. c:member:: CcParsedStep * steps

        Steps taken by the piece.


    .. c:member:: struct CcParsedPly * next

        Next ply in a cascade.

    :c:`struct` is tagged with the same :c:expr:`CcParsedPly` name.

Parsed ply functions
--------------------

.. c:function:: char const * cc_parsed_ply_link_symbol( CcParsedPlyLinkEnum ple )

    Function returns string symbol, as used in algebraic notation,
    for a given ply link.

    Returned string is not allocated, so do not :c:`free()` it.

    :param ple: A ply linkage.
    :returns: String symbol if link is valid, :c:`NULL` otherwise.

.. c:function:: CcParsedPly * cc_parsed_ply__new( char const * start_an__d, char const * end_an__d, size_t max_len__d, CcParsedPlyLinkEnum link, CcPieceEnum piece, CcLosingTagEnum lost_tag, CcParsedStep ** steps__n )

    Function returns newly allocated ply.

    .. warning::

        If no *optional* end arguments (:c:`end_an__d`, :c:`max_len__d`) are given,
        annotation string (:c:`start_an__d`) has to be zero-terminated, or :c:`NULL`.

    Takes ownership of :c:`steps__n`, inner pointer will be set to :c:`NULL`,
    if valid ply is produced.

    :param start_an__d: *Optional*; start of a ply notation substring. Can be :c:`NULL`, if so :c:`notation` member is initialized to :c:`NULL`.
    :param end_an__d: *Optional*; end of a ply notation substring. Can be :c:`NULL`, if so whole zero-terminated :c:`start_an__d` string is copied.
    :param max_len__d: *Optional*, maximum length of :c:`notation` to copy. Can be :c:expr:`CC_MAX_LEN_ZERO_TERMINATED`, if so whole zero-terminated :c:`start_an__d` string is copied.
    :param link: Link to previous ply in a cascade.
    :param piece: A piece making a ply.
    :param lost_tag: Tag lost by a moving piece.
    :param steps__n: **Ownership transfer**; steps, linked list, can be :c:`NULL`.
    :returns: A newly allocated ply if successful, :c:`NULL` otherwise.













.. _lbl-libcc-ccparsedply-sourcecodeheader:

Parsed ply source code header
-----------------------------

Included source code file is ``cc_parsed_ply.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_parsed_ply.h
    :language: C
    :linenos:

.. _lbl-libcc-ccparsedply-sourcecodefile:

Parsed ply source code file
---------------------------

Included source code file is ``cc_parsed_ply.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_parsed_ply.c
    :language: C
    :linenos:
