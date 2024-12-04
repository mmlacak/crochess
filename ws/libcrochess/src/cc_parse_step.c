// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdio.h> // printf :: TODO :: DEBUG :: DELETE

#include "cc_setup_misc.h"
#include "cc_parsed_side_effect.h"

#include "cc_parse_utils.h"
#include "cc_parse_side_effect.h"
#include "cc_parse_step.h"


static void _cc_add_msg_invalid_step_link( char const * step_start_an,
                                           char const * step_end_an,
                                           CcParseMsg ** parse_msgs__iod ) {
    char * step_str__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
    cc_parse_msg_append_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "Invalid step separator in step '%s'.\n", step_str__a );
    CC_FREE( step_str__a );
}

static bool _cc_check_parsed_pos( char const * step_start_an,
                                  char const * step_end_an,
                                  CcStepLinkTypeEnum sle,
                                  CcPos * pos__o,
                                  char const ** pos_end_an__o,
                                  CcParseMsg ** parse_msgs__iod ) {
    char const * step_after_link_an = step_start_an + cc_step_link_len( sle );

    if ( !cc_parse_pos( step_after_link_an, pos__o, pos_end_an__o ) ) {
        char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
        cc_parse_msg_append_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "Error parsing step '%s'.\n", step_an__a );
        CC_FREE( step_an__a );
        return false;
    }

    return true;
}

// Returns true if successful, or nothing to do (not castling, not King, ...); otherwise false.
static bool _cc_fill_in_castling_partial_destination( CcVariantEnum ve,
                                                      CcPosDesc before_ply_start,
                                                      char const * pos_end_an,
                                                      CcPos * destination__io /* ,
                                                      CcParseMsg ** parse_msgs__iod */ ) {
    if ( !destination__io ) return false;
    if ( !CC_IS_COORD_VALID( destination__io->i ) ) return false;
    if ( CC_IS_COORD_VALID( destination__io->j ) ) return true; // Nothing to be done here.

    if ( destination__io->i == before_ply_start.pos.i ) return false; // Static step not welcome.

    if ( !CC_PIECE_IS_KING( before_ply_start.piece ) ) return true; // Nothing to be done here.
    if ( before_ply_start.tag != CC_TE_CanCastle ) return false;

    bool is_light = cc_piece_is_light( before_ply_start.piece );
    int init_i = cc_find_initial_figure_file( ve, before_ply_start.piece, false );
    int init_j = cc_variant_initial_figure_rank( ve, is_light );

    if ( ( before_ply_start.pos.i != init_i ) || ( before_ply_start.pos.j != init_j ) )
        return false;

    if ( !pos_end_an ) return false;
    char const * c = pos_end_an;
    bool do_fill_in = false;

    if ( *c++ == '&' ) { // Maybe castling ( "&" == castling, "&&" == losing castling tag )?
        if ( *c != '&' ) { // Definitely castling.
            do_fill_in = true;
        }
    } else {
        int max_dist = cc_get_kings_max_castling_distance( ve );
        if ( !CC_IS_COORD_VALID( max_dist ) ) return false;

        bool is_queen_side = ( destination__io->i < init_i );

        int min_i = is_queen_side ? init_i - max_dist
                                  : init_i + CC_KING_MIN_CASTLING_DISTANCE;

        int max_i = is_queen_side ? init_i - CC_KING_MIN_CASTLING_DISTANCE
                                  : init_i + max_dist;

        if ( min_i <= destination__io->i && destination__io->i <= max_i ) {
            do_fill_in = true;
        }
    }

    // if ( !parse_msgs__iod ) return CC_MBE_Void;

    if ( do_fill_in ) {
        destination__io->j = init_j;
        return true;
    } else
        return false;
}

static bool _cc_parse_step( char const * step_start_an,
                            char const * step_end_an,
                            char const * steps_end_an,
                            CcGame * game,
                            CcPosDesc before_ply_start,
                            bool is_first_step,
                            bool * had_disambiguation__io,
                            CcStep ** step__o,
                            CcChessboard ** cb__io,
                            CcParseMsg ** parse_msgs__iod ) {
    if ( !step_start_an ) return false;
    if ( !step_end_an ) return false;
    if ( !steps_end_an ) return false;
    if ( !game ) return false;
    if ( !had_disambiguation__io ) return false;
    if ( !step__o || *step__o ) return false;
    if ( !cb__io || !*cb__io ) return false;
    if ( !parse_msgs__iod ) return false;

    if ( is_first_step ) *had_disambiguation__io = false;

    CcStepLinkTypeEnum sle = CC_SLTE_None;
    if ( !cc_parse_step_link( step_start_an, steps_end_an, &sle ) ) {
        _cc_add_msg_invalid_step_link( step_start_an, step_end_an, parse_msgs__iod );
        return false;
    }

    if ( *had_disambiguation__io ) {
        if ( sle == CC_SLTE_Start ) sle = CC_SLTE_JustDestination;
        *had_disambiguation__io = false;
    }

    CcPos pos = CC_POS_CAST_INVALID;
    char const * pos_end_an = NULL;

    if ( !_cc_check_parsed_pos( step_start_an, step_end_an, sle, &pos, &pos_end_an, parse_msgs__iod ) )
        return false;

    if ( is_first_step ) {
        if ( cc_skip_disambiguation( step_start_an ) ) {
            *had_disambiguation__io = true;
        }
    }

    if ( !_cc_fill_in_castling_partial_destination( game->chessboard->type,
                                                    before_ply_start,
                                                    pos_end_an,
                                                    &pos /* ,
                                                    parse_msgs__iod */ ) )
        return false;

    CcParsedSideEffect se = cc_parsed_side_effect_none();

    if ( !*had_disambiguation__io ) // Disambiguation == starting position --> no side-effect.
        if ( !cc_parse_side_effect( pos_end_an, step_start_an, step_end_an, game, before_ply_start,
                                    *cb__io,
                                    sle,
                                    &pos,
                                    &se,
                                    parse_msgs__iod ) ) return false;

    CcStep * step__t = cc_step__new( sle, pos, se );
    if ( !step__t ) return false;

    *step__o = step__t;
    // step__t = NULL; // Not needed, local var.

    return (bool)( *step__o );
}


bool cc_parse_steps( char const * steps_start_an,
                     char const * steps_end_an,
                     CcGame * game,
                     CcPosDesc before_ply_start,
                     CcStep ** steps__o,
                     CcChessboard ** cb__io,
                     CcParseMsg ** parse_msgs__iod ) {
    if ( !steps_start_an ) return false;
    if ( !steps_end_an ) return false;
    if ( !game ) return false;
    if ( !steps__o || *steps__o ) return false;
    if ( !cb__io || !*cb__io ) return false;
    if ( !parse_msgs__iod ) return false;

    char const * step_start_an = NULL;
    char const * step_end_an = NULL;
    bool is_first_step = true;
    bool had_disambiguation = false;

    while ( cc_iter_step( steps_start_an, steps_end_an, &step_start_an, &step_end_an ) ) {
        CcStep * step__t = NULL;

        cc_str_print( step_start_an, step_end_an, 0, "Step: '%s'.\n", 0, NULL ); // TODO :: DEBUG :: DELETE

        if ( !_cc_parse_step( step_start_an, step_end_an, steps_end_an, game, before_ply_start,
                             is_first_step,
                             &had_disambiguation,
                             &step__t,
                             cb__io,
                             parse_msgs__iod ) ) {
            printf( "!_cc_parse_step\n" );  // TODO :: DEBUG :: DELETE

            cc_step_free_all( &step__t );
            return false;
        }

        if ( !cc_step_extend( steps__o, &step__t ) ) {
            cc_step_free_all( &step__t );
            return false;
        }

        is_first_step = false;
    }

    // TODO :: DEBUG :: DELETE
    //
    {
        char * step_str__a = cc_step_all_to_string__new( *steps__o );

        cc_str_print( step_str__a, NULL, 0, "Steps: '%s'.\n", 0, NULL );

        CC_FREE( step_str__a );
    }
    //
    // TODO :: DEBUG :: DELETE

    return true;
}
