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
