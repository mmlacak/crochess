.. Copyright (c) 2021, 2022, 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.
   Included source code files are licensed under GNU GPL v3+ license. See LICENSING, COPYING files in root folder of the project for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccgame:

Game
====

Documents ``cc_game.h`` and ``cc_game.c`` files, which contain various
game definitions, linked lists and functions.

.. _lbl-libcc-ccgame-data:

Data
----

.. c:type:: char const CC_GAME_SEPARATORS_SETUP_FROM_STRING[]

    Separators constant, used to tokenize string setup.

.. _lbl-libcc-ccgame-status:

Status
------

.. c:macro:: CC_GAME_STATUS_IS_ENUMERATION(gse)

    Macro to check if given :term:`tag` is enumeration in :c:enum:`CcGameStatusEnum`,
    i.e. between :c:enumerator:`CC_GSE_None` and :c:enumerator:`CC_GSE_Draw` values.

    :param gse: A game status, integer value.
    :returns: :c:data:`true` if :c:enum:`CcGameStatusEnum` enumeration,
              :c:data:`false` otherwise.

.. c:macro:: CC_GAME_STATUS_IS_VALID(gse)

    Macro to check if given :term:`tag` is :c:enum:`CcGameStatusEnum` enumerator,
    and not :c:enumerator:`CC_GSE_None`.

    :param gse: A game status, integer value.
    :returns: :c:data:`true` if valid :c:enum:`CcGameStatusEnum` enumeration,
              :c:data:`false` otherwise.

.. c:macro:: CC_GAME_STATUS_IS_TURN(gse)

    Macro to check if it's either light or dark player's turn.

    :param gse: Game status; :c:enum:`CcGameStatusEnum` value.
    :returns: :c:data:`true` if it's either light or dark player's turn,
              :c:data:`false` otherwise.

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

.. c:function:: CcGameStatusEnum cc_game_status_next( CcGameStatusEnum gse, bool is_end, bool is_won )

    Function returns next game status, based on current one,
    and additional flags from user or position on chessboard.

    :param gse: Current game status.
    :param is_end: Flag, if game has ended due to rules.
    :param is_won: Flag, if current player has won the game.
    :returns: Next game status.

.. c:function:: CcGameStatusEnum cc_game_resign( CcGameStatusEnum gse )

    Function returns next game status, based on current player
    resignation.

    :param gse: Current game status.
    :returns: Next game status.

.. _lbl-libcc-ccgame-types:

Types
-----

.. c:struct:: CcGame

    Game :c:`struct`\ure.

    .. c:member:: CcGameStatusEnum status

        Current game status.

    .. c:member:: CcChessboard * chessboard

        Current position on a chessboard.

    .. c:member:: CcMove * moves

        Queue of moves played so far.

    :c:`struct` is tagged with the same :c:struct:`CcGame` name.

.. _lbl-libcc-ccgame-functions:

Functions
---------

.. c:function:: CcGame * cc_game__new( CcGameStatusEnum status, CcVariantType ve, bool do_setup )

    Function returns next game status, based on current player
    resignation.

    Linked list of performed moves will be empty. If necessary,
    it can be populated once newly allocated game is returned.

    :param status: Initial game status.
    :param ve: Variant to play.
    :param do_setup: Flag, if start from initial setup (:c:data:`true`),
                     or from manually set-up position (:c:data:`false`).
    :returns: A newly allocated game if successful, :c:data:`NULL` otherwise.

.. .. TODO :: FIX :: CcPosDescLink * pdl --> CcChessboard * cb
.. .. c:function:: bool cc_game_update_chessboard( CcGame * game__io, CcPosDescLink * pdl )
..
..     Function updates chessboard of a given game.
..
..     :param game__io: Game to update chessboard.
..     :param pdl: Linked list of positions to update with corresponding piece and tag.
..     :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. c:function:: bool cc_game_free_all( CcGame ** game__f )

    :c:func:`free()`\s game, and all owned resources (chessboard, moves).

    :param game__f: A game to free.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. c:function:: CcGame * cc_game_duplicate_all__new( CcGame * game, bool copy_history )

    Duplicates a given game into a newly allocated one.

    :param game: Game to duplicate.
    :param copy_history: Flag, whether to copy list of moves played so far.
    :returns: A newly allocated game if successful, :c:data:`NULL` otherwise.

.. c:function:: CcGame * cc_game_setup_from_string__new( char const * setup, CcGame * before_setup__d )

    Functions returns a newly allocated :c:struct:`CcGame`, from a given
    string, and optionally initial :c:struct:`CcGame`, containing starting
    positions, game status, and moves.

    Setup string contains list of ``<piece><file><rank>[<tag>]`` items, separated
    by ``,`` (comma). ``<tag>`` is optional.

    ``<piece>`` is usual piece symbol, as used in AN; dark/dim pieces are represented
    by lower-case letter, e.g. dark Bishop would be ``b``, instead of usual ``B``.

    If a particular field has to be cleared, ``' '`` (space) is used for ``<piece>``.

    ``<file>`` is any letter from ``a`` to ``z``, inclusive.

    ``<rank>`` is any number from ``1`` to ``26``, inclusive.

    ``<tag>`` is optional, if given it can be one of ``^``, ``&``, ``=``, ``:``, ``;``;
    representing rushing, castling, delayed promotion, rushed in previous turn, and
    rushed in current turn tags, see :ref:`lbl-libcc-cctags-characters` for details.

    If optional, initial game setup is not given, setup string has to be preceded
    by variant abbreviation, i.e. use one of:

        * cc  --> Classical Chess
        * ct  --> Croatian Ties
        * ma  --> Mayan Ascendancy
        * aoa --> Age Of Aquarius
        * mv  --> Miranda's Veil
        * n   --> Nineteen
        * hd  --> Hemera's Dawn
        * tr  --> Tamoanchan Revisited
        * cot --> Conquest Of Tlalocan
        * d   --> Discovery
        * o   --> One

    Use lower-cased variant abbreviation to set dark player on the move; otherwise, it's light player's move.

    Variant abbreviation has to be followed by ``' '`` (space).

    Some examples: ``"O Ra1&,Pa2^,Pb23=,bc24,Pc7,pd8,Pf11"``, ``"o Bh5,Bd9,Wk2,Ro2"``. ``"bd1, a11,Bl1,bd9"``;
    the last one can only be used along with an existing game setup.

    :param setup: A setup string.
    :param before_setup__d: An *optional*, initial game. If given, it is
        copied before any modification specified by a given string is applied.
    :returns: A newly allocated game if successful, :c:data:`NULL` otherwise.

.. _lbl-libcc-ccgame-sourcecodeheader:

Header file
-----------

Included source header file is ``cc_game.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_game.h
    :language: C
    :linenos:

.. _lbl-libcc-ccgame-sourcecodefile:

Source code file
----------------

Included source code file is ``cc_game.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_game.c
    :language: C
    :linenos:
