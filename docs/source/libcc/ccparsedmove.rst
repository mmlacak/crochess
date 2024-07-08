.. Copyright (c) 2021, 2022, 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.
   Included source code files are licensed under GNU GPL v3+ license. See LICENSING, COPYING files in root folder of the project for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccparsedmove:

Parsed move
===========

Documents ``cc_parsed_move.h`` and ``cc_parsed_move.c`` files, which contain various
parsed move definitions and functions.

.. _lbl-libcc-ccparsedmove-data:

Parsed move data
----------------

.. c:enum:: CcParsedMoveStatusEnum

    Move status enumeration, after a valid movement.

    .. c:enumerator:: CC_PMSE_None

        No status.

    .. c:enumerator:: CC_PMSE_DrawOffer

        Player offered a draw.

    .. c:enumerator:: CC_PMSE_DrawOffer_Revoked

        Player took back draw offer.


    .. c:enumerator:: CC_PMSE_Check

        Checking opponent.

    .. c:enumerator:: CC_PMSE_Check_DrawOffer

        Checking opponent, player offered a draw.

    .. c:enumerator:: CC_PMSE_Check_DrawOffer_Revoked

        Checking opponent, player took back draw offer.


    .. c:enumerator:: CC_PMSE_Checkmate

        Opponent checkmated.

    .. c:enumerator:: CC_PMSE_SelfCheckmate

        Opponent checkmated self, game ended.


    .. c:enumerator:: CC_PMSE_Resign

        Player resigned, game ended.

    .. c:enumerator:: CC_PMSE_DrawAccepted

        Player accepted draw offer, game ended.

    .. c:enumerator:: CC_PMSE_DrawByRules

        Game was drawn by rules, game ended.

    :c:`Enum` is tagged with the same :c:expr:`CcParsedMoveStatusEnum` name.






.. _lbl-libcc-ccparsedmove-sourcecodeheader:

Parsed move source code header
------------------------------

Included source code file is ``cc_parsed_move.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_parsed_move.h
    :language: C
    :linenos:

.. _lbl-libcc-ccparsedmove-sourcecodefile:

Parsed move source code file
----------------------------

Included source code file is ``cc_parsed_move.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_parsed_move.c
    :language: C
    :linenos:
