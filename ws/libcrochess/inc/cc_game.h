// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_GAME_H__
#define __CC_GAME_H__

#include <stdbool.h>

#include "cc_move.h"
#include "cc_chessboard.h"

/**
    @file cc_game.h
    @brief Storage for game status, current chessboard and all performed moves so far.
*/


/**
    Separators constant, used to toknize string setup.
*/
extern char const CC_GAME_SEPARATORS_SETUP_FROM_STRING[];

/**
    Macro to inline check if it's light player's turn.

    @param gse A game status enumeration.

    @return `1` if it's light player's turn, `0` otherwise.
*/
#define CC_GAME_STATUS_IS_LIGHT_TURN(gse) ( (gse) == CC_GSE_Turn_Light )

/**
    Macro to inline check if it's dark player's turn.

    @param gse A game status enumeration.

    @return `1` if it's dark player's turn, `0` otherwise.
*/
#define CC_GAME_STATUS_IS_DARK_TURN(gse) ( (gse) == CC_GSE_Turn_Dark )

/**
    Macro to inline check if game is on-going, i.e. it's either light or dark players turn.

    @param gse A game status enumeration.

    @return `1` if it's either light or dark player's turn, `0` otherwise.
*/
#define CC_GAME_STATUS_IS_TURN(gse) ( ( (gse) == CC_GSE_Turn_Light ) || ( (gse) == CC_GSE_Turn_Dark ) )


/**
    Game status enumeration.
*/
typedef enum CcGameStatusEnum {
    CC_GSE_None, /**< Uninitialized game. */
    CC_GSE_Turn_Light, /**< Light player is on turn. */
    CC_GSE_Turn_Dark, /**< Dark player is on turn. */
    CC_GSE_Win_Light, /**< Light player has won. */
    CC_GSE_Win_Dark, /**< Dark player has won. */
    CC_GSE_Draw, /**< Game was drawn. */
} CcGameStatusEnum;

/**
    Function returns next game status, based on current one, and additional flags
    from user or position on chessboard.

    @param gse Current game status.
    @param is_end Flag, if game has ended due to rules.
    @param is_won Flag, if current player has won the game.

    @return Next game status.
*/
CcGameStatusEnum cc_game_status_next( CcGameStatusEnum gse,
                                      bool is_end,
                                      bool is_won );

/**
    Function returns next game status, based on current player resignation.

    @param gse Current game status.

    @return Next game status.
*/
CcGameStatusEnum cc_game_resign( CcGameStatusEnum gse );


/**
    Game structure.
*/
typedef struct CcGame {
    CcGameStatusEnum status; /**< Current game status. */
    CcChessboard * chessboard; /**< Current position on a chessboard. */
    CcMove * moves; /**< Queue of moves played so far. */
} CcGame;

/**
    Returns a newly allocated game.

    @param status Initial game status.
    @param ve Variant to play.
    @param do_setup Flag, if start from initial setup (`true`), or from manually set-up position (`false`).

    @note
    Linked list of performed moves will be empty. If neccessary, it can be populated once newly allocated game is returned.

    @return
    A newly allocated game if successful, `NULL` otherwise.
*/
CcGame * cc_game__new( CcGameStatusEnum status,
                       CcVariantEnum ve,
                       bool do_setup );

/**
    Duplicates a given game into a newly allocated one.

    @param game Game to duplicate.

    @return
    A newly allocated game if successful, `NULL` otherwise.
*/
CcGame * cc_game_duplicate_all__new( CcGame * restrict game );

/**
    Frees game, and all owned resources (chessboard, moves).

    @param game__f A game to free.

    @return `true` if successful, `false` otherwise.
*/
bool cc_game_free_all( CcGame ** restrict game__f );

/**
    Functions returns a newly allocated `CcGame`, from a given string,
    and optionally initial `CcGame`, containing starting positions,
    game status, and moves.

    @param setup A setup string.
    @param before_setup__d An _optional_, initial game. If give, it is copied before any modification specified by a given string is applied.

    @note
    Setup string contains list of `<piece><file><rank>[<tag>]` items, `<tag>` is optional.

    @note
    `<piece>` is usual piece symbol, as used in AN; dark/dim pieces are represented by lower-case letter, e.g. dark Bishop would be `b`, instead of usual `B`.

    @note
    If a particular field has to be cleared, ``' '`` (space) is used for `<piece>`.

    @note
    `<file>` is any letter from `a` to `z`, inclusive.

    @note
    `<rank>` is any number from `1` to `26`, inclusive.

    @note
    `<tag>` is optional, if given it can be one of `P`, `R`, `E`, `C`; representing delayed promotion, rushing, en passant and castling tags.

    @note
    If optional, initial game setup is not given, setup string has to be preceeded by variant abbreviation, i.e. use one of:
    - cc  --> Classical Chess
    - ct  --> Croatian Ties
    - ma  --> Mayan Ascendancy
    - aoa --> Age Of Aquarius
    - mv  --> Miranda's Veil
    - n   --> Nineteen
    - hd  --> Hemera's Dawn
    - tr  --> Tamoanchan Revisited
    - cot --> Conquest Of Tlalocan
    - d   --> Discovery
    - o   --> One

    @note
    Use lower-cased variant abbreviation to set dark player on the move; otherwise, it's light player's move.

    @note
    Variant abbreviation has to be followed by ``' '`` (space).

    @note
    Some examples: `"O Ra1C,Pa2R,Pb23P,bc24,Pc7,pd8,Pf11E"`, `"o Bh5,Bd9,Wk2,Ro2"`. `"bd1, a11,Bl1,bd9"`; the last one can only be used along with an existing game setup.

    @return A newly allocated game if successful, `NULL` otherwise.
*/
CcGame * cc_game_setup_from_string__new( char const * restrict setup,
                                         CcGame * restrict before_setup__d );


#endif /* __CC_GAME_H__ */
