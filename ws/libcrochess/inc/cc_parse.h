// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PARSE_H__
#define __CC_PARSE_H__

#include <stddef.h>

#include "cc_piece.h"
#include "cc_pos.h"
#include "cc_parse_defs.h"

/**
    @file cc_parse.h
    @brief Functions separating a move (algebraic notation string) into list of enums, sub-strings.
*/


// /**
//     Ply algebraic notation structure.
// */
// typedef struct CcPlyANs
// {
//     char * link_an_str; /**< A link, algebraic notation for cascading. Can be `NULL`, in which case `CC_PLE_Ply` is assumed. */
//     CcPlyLinkEnum link; /**< Type of a link between this ply and previous (if in a cascade).  */

//     char * ply_an_str; /**< A ply, algebraic notation for a complete movement of a piece. */
//     CcPieceEnum piece; /**< A piece being moved in this ply. */

//     CcPos start; /**< Starting field; parsed, calculated, or copied from destination field of a previous ply. */

//     // TODO :: not needed ?
//     //
//     // struct CcPlyANs * next; /**< Next ply in a cascade. */
// } CcPlyANs;


CcPlyLinkEnum cc_starting_ply_link( char const * restrict an_str );

size_t cc_ply_link_len( CcPlyLinkEnum ple );

char const * cc_next_ply_link( char const * restrict an_str );

bool cc_ply_iter( char const * restrict an_str,
                  char const ** restrict start__io,
                  char const ** restrict end__io );


bool cc_ply_piece_symbol( char const * restrict an_str,
                          char * restrict piece_symbol__o );

CcLosingTagEnum cc_starting_losing_tag( char const * restrict an_str );

size_t cc_losing_tag_len( CcLosingTagEnum lte );

char const * cc_starting_pos( char const * restrict an_str,
                              char const * restrict ply_end,
                              bool is_disambiguation,
                              cc_char_8 * restrict pos__o );

bool cc_convert_starting_pos( char const * restrict pos,
                              int * restrict file__o,
                              int * restrict rank__o );


CcStepLinkEnum cc_starting_step_link( char const * restrict an_str );

size_t cc_step_link_len( CcStepLinkEnum sle );

char const * cc_next_step_link( char const * restrict an_str,
                                char const * restrict ply_end );

bool cc_step_iter( char const * restrict an_str,
                   char const * restrict ply_end,
                   char const ** restrict start__io,
                   char const ** restrict end__io );

bool cc_ply_has_steps( char const * restrict an_str,
                       char const * restrict ply_end );


CcSideEffectEnum cc_starting_side_effect( char const * restrict an_str );

char const * cc_find_side_effect( char const * restrict an_str,
                                  char const * restrict step_end,
                                  CcSideEffectEnum * restrict see__o );


#endif /* __CC_PARSE_H__ */
