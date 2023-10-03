// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PARSE_UTILS_H__
#define __CC_PARSE_UTILS_H__

#include <stddef.h>

#include "cc_piece.h"
#include "cc_tag.h"
#include "cc_pos.h"

#include "cc_parse_defs.h"
#include "cc_side_effect.h"
#include "cc_step.h"
#include "cc_ply.h"

/**
    @file cc_parse_utils.h
    @brief Functions separating a move (algebraic notation string) into list of enums, sub-strings.
*/


CcPlyLinkEnum cc_parse_ply_link( char const * restrict an_str );

size_t cc_ply_link_len( CcPlyLinkEnum ple );

char const * cc_next_ply_link( char const * restrict an_str );

bool cc_iter_ply( char const * restrict an_str,
                  char const ** restrict start__io,
                  char const ** restrict end__io );


bool cc_fetch_piece_symbol( char const * restrict an_str,
                            char * restrict piece_symbol__o,
                            bool default_to_pawn,
                            bool return_validity );

CcLosingTagEnum cc_parse_losing_tag( char const * restrict an_str );

size_t cc_losing_tag_len( CcLosingTagEnum lte );

bool cc_convert_coords( char const * restrict pos,
                        int * restrict file__o,
                        int * restrict rank__o );

bool cc_convert_pos( char const * restrict pos,
                     CcPos * restrict pos__o );

bool cc_parse_pos( char const * restrict an_str,
                   CcPos * restrict pos__o,
                   char const ** restrict pos_end__o );


CcStepLinkEnum cc_parse_step_link( char const * restrict an_str );

size_t cc_step_link_len( CcStepLinkEnum sle );

char const * cc_next_step_link( char const * restrict an_str,
                                char const * restrict ply_end );

bool cc_iter_step( char const * restrict an_str,
                   char const * restrict ply_end,
                   char const ** restrict start__io,
                   char const ** restrict end__io );

bool cc_ply_an_contains_steps( char const * restrict an_str,
                               char const * restrict ply_end );


CcSideEffectEnum cc_parse_side_effect_type( char const * restrict an_str,
                                            bool * restrict has_promotion_sign__o );

size_t cc_side_effect_type_len( CcSideEffectEnum see,
                                bool has_promotion_sign );


#endif /* __CC_PARSE_UTILS_H__ */
