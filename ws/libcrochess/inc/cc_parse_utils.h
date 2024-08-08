// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PARSE_UTILS_H__
#define __CC_PARSE_UTILS_H__

#include <stddef.h>

#include "cc_piece.h"
#include "cc_tag.h"
#include "cc_pos.h"

#include "cc_parsed_side_effect.h"
#include "cc_parsed_step.h"
#include "cc_parsed_ply.h"


// TODO :: DOCS
#define CC_MAX_LEN_STEP_POS_AN (3)

// TODO :: DOCS
#define CC_MAX_LEN_DISAMBIGUATION (3)

// TODO :: DOCS
#define CC_MAX_LEN_DISAMBIGUATION_STEP (CC_MAX_LEN_STEP_POS_AN + CC_MAX_LEN_DISAMBIGUATION)

// TODO :: DOCS
#define CC_CHAR_IS_PLY_GATHER_START(char_c) ( (char_c) == '[' )

// TODO :: DOCS
#define CC_CHAR_IS_PLY_GATHER_END(char_c) ( (char_c) == ']' )

// TODO :: DOCS
#define CC_CHAR_IS_PLY_GATHER(char_c) ( ( (char_c) == '[' ) || ( (char_c) == ']' ) )

// TODO :: DOCS
#define CC_CHAR_IS_STEP_SEPARATOR(char_c) ( ( (char_c) == '.' ) || ( (char_c) == '-' ) )

// TODO :: DOCS
#define CC_CHAR_IS_PIECE_SYMBOL(char_c) ( isupper( (char_c) ) )


// TODO :: DOCS
bool cc_parse_ply_link( char const * an_str,
                        CcParsedPlyLinkEnum * ple__o );

// TODO :: DOCS
size_t cc_ply_link_len( CcParsedPlyLinkEnum ple );

// TODO :: DOCS
char const * cc_next_ply_link( char const * an_str );

// TODO :: DOCS
bool cc_iter_ply( char const * an_str,
                  char const ** start__io,
                  char const ** end__io );


// TODO :: DOCS
bool cc_fetch_piece_symbol( char const * an_str,
                            char * piece_symbol__o,
                            bool default_to_pawn,
                            bool return_validity );

// TODO :: DOCS
CcLosingTagEnum cc_parse_losing_tag( char const * an_str );

// TODO :: DOCS
size_t cc_losing_tag_len( CcLosingTagEnum lte );

// TODO :: DOCS
bool cc_convert_coords( char const * pos,
                        int * file__o,
                        int * rank__o );

// TODO :: DOCS
bool cc_convert_pos( char const * pos, CcPos * pos__o );

// TODO :: DOCS
bool cc_parse_pos( char const * an_str,
                   CcPos * pos__o,
                   char const ** pos_end__o );

// TODO :: DOCS
char const * cc_skip_disambiguation( char const * an_str );

// TODO :: DOCS
bool cc_has_separated_steps( char const * an_str,
                             char const * ply_end,
                             bool check_intermediate_steps,
                             bool check_destination_step );

// TODO :: DOCS
bool cc_parse_step_link( char const * an_str,
                         char const * ply_end,
                         CcParsedStepLinkEnum * sle__o );

// TODO :: DOCS
size_t cc_parsed_step_link_len( CcParsedStepLinkEnum sle );

// TODO :: DOCS
char const * cc_next_step_link( char const * an_str,
                                char const * ply_end );

// TODO :: DOCS
bool cc_iter_step( char const * an_str,
                   char const * ply_end,
                   char const ** start__io,
                   char const ** end__io );

// TODO :: DOCS
bool cc_ply_an_contains_steps( char const * an_str,
                               char const * ply_end );


// TODO :: DOCS
CcParsedSideEffectEnum cc_parse_side_effect_type( char const * an_str,
                                            bool * has_promotion_sign__o );

// TODO :: DOCS
size_t cc_parsed_side_effect_type_len( CcParsedSideEffectEnum see,
                                bool has_promotion_sign );


#endif /* __CC_PARSE_UTILS_H__ */
