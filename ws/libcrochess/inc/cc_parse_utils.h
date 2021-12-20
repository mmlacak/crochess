// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PARSE_UTILS_H__
#define __CC_PARSE_UTILS_H__

#include <stdbool.h>
#include <stdlib.h>

#include "cc_chessboard.h"
#include "cc_ply.h"

/**
    @file cc_parse_utils.h
    @brief Helper functions to parse algebraic notation.
*/


#define CC_PARSE_STR_LOST_TAG_LENGTH (2)

/**
    Function checks if a given character is a ply gathering.

    Ply gathering characters are `[` and `]`.

    @param c A character.

    @return `true` if ply gatherer, `false` otherwise.
*/
bool cc_parse_utils_char_is_ply_gather( char c );

/**
    Function returns length of a ply link in an algebraic notation, in characters.

    @param ply_str A ply, algebraic notation string.

    @return Length of a ply link if successful, `0` otherwise.
*/
size_t cc_parse_utils_ply_link_len( char const * restrict ply_str );

/**
    Function returns string pointer, starting from a given location, and moving toward end of string.

    @param move_str A move, algebraic notation string.
    @param skip_or_stop_at A flag, whether to skip ply link characters, or stop at first of them.

    String pointer skips over almost all characters, and stops at ply link ones,
    if given `skip_or_stop_at` flag was `true`.

    If flag is `false`, string pointer will skip over ply link characters,
    and stop at any other.

    @see CcPlyLinkEnum
    @see cc_parse_utils_ply_link_len

    @return String pointer if successful, `NULL` otherwise.
*/
char const * cc_parse_utils_go_ply_link( char const * restrict move_str,
                                         bool skip_or_stop_at );

/**
    Iterator traversing over move algebraic notation (AN) string,
    returning next ply AN as newly allocated string.

    @param move_str Move AN string to traverse.
    @param ply_an__o An _output_ parameter, ply AN.
    @param initialize_iter Flag, whether to initialize iterator.

    @note
    Parameter `move_str` has to be valid pointer even if iterator is not being
    intialized, to ensure string (buffer) is still allocated and readable.

    @note
    _Output_ parameter `ply_an__o` has to have inner pointer initialized to `NULL`,
    i.e. `*ply_an__o == NULL` has to hold true.

    @note
    If used within a loop, newly allocated string has to be `free()`-ed, or
    ownership transferred to some other variable, before each new loop.
    ~~~{.c}
    char * ply_an__o = NULL;
    bool init = true;

    while ( !cc_parse_utils_ply_str_iter_new( ..., &ply_an__o, init ) )
    {
        init = false;

        ... // some stuff

        free( ply_an__o ); // Or transfer ownership.
        ply_an__o = NULL; // Must be set to NULL.
    }
    ~~~

    @note
    Iterator will continue to return next ply AN on each subsequent call,
    until it reaches end of an original string (`move_str`), or is initialized
    again with different string.

    @return `true` if next ply AN was found, `NULL` otherwise.
    If `true` was returned, _output_ argument `ply_an__o` contains a newly
    allocated copy of a ply AN found.
*/
// TODO :: CONVERT :: new iterator template
bool cc_parse_utils_ply_str_iter_new( char const * restrict move_str,
                                      char ** restrict ply_an__o,
                                      bool initialize_iter );
// TODO :: CONVERT :: new iterator template

/**
    Function getting ply link, for a given ply AN string.

    @param ply_str A ply, algebraic notation string.
    @param link__o An _output_ parameter, returned ply link.

    @note
    Ply link is returned via _output_ parameter, not via return value.

    @return `true` if successful, `false` otherwise.
*/
bool cc_parse_utils_get_ply_link( char const * restrict ply_str,
                                  CcPlyLinkEnum * restrict link__o );

/**
    Function getting piece, for a given ply AN string.

    @param ply_str A ply, algebraic notation string.
    @param is_light A flag, whether piece is light or dark (bright or dim, in case of Stars).
    @param piece__o An _output_ parameter, returned piece.

    @note
    Piece is returned via _output_ parameter, not via return value.

    @return `true` if successful, `false` otherwise.
*/
bool cc_parse_utils_get_ply_piece( char const * restrict ply_str,
                                   bool is_light,
                                   CcPieceEnum * restrict piece__o );

/**
    Function getting piece symbol, for a given ply AN string.

    @param ply_str A ply, algebraic notation string.
    @param piece_symbol__o An _output_ parameter, returned piece symbol.

    @note
    Piece symbol is returned via _output_ parameter, not via return value.

    @note
    Piece symbol is a single character, as written in AN string, or `P` for a Pawn.

    @return `true` if successful, `false` otherwise.
*/
bool cc_parse_utils_get_ply_piece_symbol( char const * restrict ply_str,
                                          char * restrict piece_symbol__o );


/**
    Function returns pointer to first step, within a given ply AN string.

    @param ply_str A ply, algebraic notation string.

    @return Valid pointer to first step if successful, `NULL` otherwise.
*/
char const * cc_parse_utils_get_steps_str( char const * restrict ply_str );

/**
    Function returns length of a step link in an algebraic notation, in characters.

    @param step_str A step, algebraic notation string.

    @return Length of a step link if successful, `0` otherwise.
*/
size_t cc_parse_utils_step_link_len( char const * restrict step_str );

/**
    Function returns string pointer, starting from a given location, and moving toward end of string.

    @param ply_str A ply, algebraic notation string.
    @param skip_or_stop_at A flag, whether to skip step link characters, or stop at first of them.

    String pointer skips over almost all characters, and stops at step link ones,
    if given `skip_or_stop_at` flag was `true`.

    If flag is `false`, string pointer will skip over step link characters,
    and stop at any other.

    @see CcStepLinkEnum
    @see cc_parse_utils_step_link_len

    @return String pointer if successful, `NULL` otherwise.
*/
char const * cc_parse_utils_go_step_link( char const * restrict ply_str,
                                          bool skip_or_stop_at );

/**
    Iterator returns newly allocated string, containing next step algebraic notation.

    @param ply_str__s A ply, _static_ parameter, AN string used to initialize iterator, can be `NULL`.

    @note
    Iterator is initialized with valid `ply_str__s` once, after which subsequent calls
    has to pass `NULL` as argument.

    @note
    Once AN string is traversed completely, `NULL` is returned.

    @return String pointer to newly allocated AN string if successful, `NULL` otherwise.
*/
char * cc_parse_utils_next_step_str_new( char const * restrict ply_str__s );

/**
    Function returns whether given ply has multiple steps.

    @param ply_str A ply, algebraic notation string.

    @return `true` if ply has multiple steps, `false` otherwise.
*/
bool cc_parse_utils_ply_has_multiple_steps( char const * restrict ply_str );

/**
    Function getting step link, for a given step AN string.

    @param ply_str A ply, algebraic notation string.
    @param step_str A step, algebraic notation string.
    @param link__o An _output_ parameter, returned step link.

    @note
    Step link is returned via _output_ parameter, not via return value.

    @return `true` if successful, `false` otherwise.
*/
bool cc_parse_utils_get_step_link( char const * restrict ply_str,
                                   char const * restrict step_str,
                                   CcStepLinkEnum * restrict link__o );

/**
    Function returns pointer to first character of a side effect, in a given step.

    @param step_str A step, algebraic notation string.

    @note
    Returned pointer points to first character of a side effect, if it exists.

    @note
    If it doesn't, returned pointer points to end of string, i.e. ``'\0'`` character.

    @return String pointer if successful, `NULL` otherwise.
*/
char const * cc_parse_utils_stop_at_side_effects( char const * restrict step_str );

/**
    Function returns newly allocated string containing fields string notation.

    @param step_str A step, algebraic notation string.

    @return Newly allocated string pointer if successful, `NULL` otherwise.
*/
char * cc_parse_utils_step_fields_str_new( char const * restrict step_str );

/**
    Function returns string pointer to side-effect, within given step AN string.

    @param step_str A step, algebraic notation string.

    @see cc_parse_utils_stop_at_side_effects()

    @return String pointer if successful, `NULL` otherwise.
*/
char const * cc_parse_utils_side_effect_str( char const * restrict step_str );

/**
    Function checks if fields string is valid.

    Valid fields string consists of optional disambiguation + field.
    Disambiguation consists of one of:
    - file
    - rank
    - file + rank.

    Field consists of file + rank.
    File is always lowercase letter, while rank can be 1 or 2 digit characters.

    @param fields_str A fields string.

    @return `true` if fields string is valid, `false` otherwise.
*/
bool cc_parse_utils_is_fields_str_valid( char const * restrict fields_str );

/**
    Function separates disambiguation and field data, from a given fields string.

    @param fields_str A fields string.
    @param cb A chessboard, used to check if parsed coordinates are on-board.
    @param disambiguation_file__o An _output_ parameter, disambiguation file.
    @param disambiguation_rank__o An _output_ parameter, disambiguation rank.
    @param file__o An _output_ parameter, file.
    @param rank__o  An _output_ parameter, rank.

    @note
    _Output_ arguments without corresponding data in fields string are initialized to `CC_INVALID_OFF_BOARD_COORD_MIN`.

    @see CC_INVALID_OFF_BOARD_COORD_MIN

    @return `true` if successful, `false` otherwise.
*/
bool cc_parse_utils_get_fields( char const * restrict fields_str,
                                CcChessboard * restrict cb,
                                int * restrict disambiguation_file__o,
                                int * restrict disambiguation_rank__o,
                                int * restrict file__o,
                                int * restrict rank__o );

// TODO :: DOCS
bool cc_parse_utils_get_lost_tag( char const * restrict lost_tag_str,
                                  CcChessboard * restrict cb,
                                  int step_i,
                                  int step_j,
                                  CcTagEnum * restrict lost_tag__o );

// TODO :: DOCS
bool cc_parse_utils_get_side_effect( char const * restrict step_str,
                                     CcChessboard * restrict cb,
                                     CcPieceEnum ply_piece,
                                     int step_i,
                                     int step_j,
                                     CcSideEffect * restrict side_effect__o );


#endif /* __CC_PARSE_UTILS_H__ */
