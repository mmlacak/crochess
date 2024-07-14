.. Copyright (c) 2021, 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.
   Included source code files are licensed under GNU GPL v3+ license. See LICENSING, COPYING files in root folder of the project for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccgame:

Game
====

Documents ``cc_game.h`` and ``cc_game.c`` files, which contain various
game definitions, linked lists and functions.

.. _lbl-libcc-ccgame-data:

Game data
---------

.. c:type:: char const CC_GAME_SEPARATORS_SETUP_FROM_STRING[]

    Separators constant, used to tokenize string setup.

.. _lbl-libcc-ccgame-status:

Game status
-----------

.. c:macro:: CC_GAME_STATUS_IS_LIGHT_TURN(gse)

    Macro to check if it's light player's turn.

    :param gse: Game status; :c:enum:`CcGameStatusEnum` value.
    :returns: :c:data:`true` if it's light player's turn, :c:data:`false` otherwise.

.. c:enum:: CcGameStatusEnum

    Game status enumeration.

    .. c:enumerator:: CC_GSE_None

        Uninitialized game.

    .. c:enumerator:: CC_GSE_Turn_Light

        Light player is on turn.

    .. c:enumerator:: CC_GSE_Turn_Dark

        Dark player is on turn.

    .. c:enumerator:: CC_GSE_Win_Light

        Light player has won.

    .. c:enumerator:: CC_GSE_Win_Dark

        Dark player has won.

    .. c:enumerator:: CC_GSE_Draw

        Game was drawn.

    :c:`enum` is tagged with the same :c:enum:`CcGameStatusEnum` name.





.. _lbl-libcc-ccgame-sourcecodeheader:

Game source code header
-----------------------

Included source code file is ``cc_game.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_game.h
    :language: C
    :linenos:

.. _lbl-libcc-ccgame-sourcecodefile:

Game source code file
---------------------

Included source code file is ``cc_game.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_game.c
    :language: C
    :linenos:
