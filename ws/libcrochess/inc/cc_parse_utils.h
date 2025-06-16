// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PARSE_UTILS_H__
#define __CC_PARSE_UTILS_H__

#include <stddef.h>

#include "cc_defines.h"

#include "cc_piece.h"
#include "cc_tag.h"
#include "cc_pos.h"

#include "cc_side_effect.h"
#include "cc_step.h"
#include "cc_ply.h"


#define CC_MAX_LEN_STEP_POS_AN (3)

#define CC_MAX_LEN_DISAMBIGUATION (3)

#define CC_MAX_LEN_DISAMBIGUATION_STEP (CC_MAX_LEN_STEP_POS_AN + CC_MAX_LEN_DISAMBIGUATION)

#define CC_CHAR_IS_PLY_GATHER(char_c) ( ( (char_c) == '[' ) || ( (char_c) == ']' ) )

#define CC_CHAR_IS_STEP_SEPARATOR(char_c) ( ( (char_c) == '.' ) || ( (char_c) == '-' ) )


CcPlyLinkTypeEnum cc_parse_ply_link( char const * ply_an_str );

size_t cc_ply_link_len( CcPlyLinkTypeEnum plte );

bool cc_is_ply_link_char( char const c );

char const * cc_next_ply_link( char const * pl_an_str );

bool cc_iter_ply( char const * move_an_str,
                  char const ** start__io,
                  char const ** end__io );


CcMaybeBoolEnum cc_fetch_piece_symbol( char const * piece_an,
                                       CcMaybeBoolEnum optional_to_pawn,
                                       char * piece_symbol__o );

CcLosingTagType cc_parse_losing_tag( char const * lt_an_str );

size_t cc_losing_tag_len( CcLosingTagType ltt );

bool cc_convert_coords( char const * pos_an_str, int * file__o, int * rank__o );

bool cc_convert_pos( char const * pos, CcPos * pos__o );

bool cc_parse_pos( char const * pos_an_str,
                   CcPos * pos__o,
                   char const ** pos_end__o );

char const * cc_skip_disambiguation( char const * pos_an_str );

char const * cc_rewind_ply_end_to_steps( char const * ply_end_an_str );

CcMaybeBoolEnum cc_an_has_separated_steps( char const * start_an,
                                           char const * end_an,
                                           bool check_intermediate_steps,
                                           bool check_destination_step );

CcMaybeBoolEnum cc_parse_step_link( char const * step_an_str,
                                    char const * ply_end,
                                    CcStepLinkTypeEnum * sle__o );

size_t cc_step_link_len( CcStepLinkTypeEnum sle );

char const * cc_next_step_link( char const * step_an_str,
                                char const * ply_end );

bool cc_iter_step( char const * ply_an_str,
                   char const * ply_end,
                   char const ** start__io,
                   char const ** end__io );


CcSideEffectTypeEnum cc_parse_side_effect_type( char const * step_an_str,
                                                bool * has_promotion_sign__o );

size_t cc_side_effect_type_len( CcSideEffectTypeEnum see,
                                bool has_promotion_sign );


#endif /* __CC_PARSE_UTILS_H__ */
