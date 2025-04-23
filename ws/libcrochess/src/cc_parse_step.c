// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_setup_misc.h"
#include "cc_side_effect.h"

#include "cc_parse_utils.h"
#include "cc_parse_side_effect.h"
#include "cc_parse_step.h"


static bool _cc_fail_with_msg_in_step( char const * msg_fmt,
                                       char const * step_start_an,
                                       char const * step_an__d,
                                       char const * step_end_an,
                                       CcParseMsg ** parse_msgs__iod ) {
    char const * s_an = step_start_an;

    if ( step_an__d && ( step_an__d != step_end_an ) )
        s_an = step_an__d;

    char * step_str__a = cc_str_copy__new( s_an, step_end_an, CC_MAX_LEN_BUFFER );
    cc_parse_msg_append_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_BUFFER, msg_fmt, step_str__a );
    CC_FREE( step_str__a );
    return false;
}

static bool _cc_parse_step( char const * step_start_an,
                            char const * step_end_an,
                            char const * steps_end_an,
                            bool is_turn_light,
                            cc_uint_t board_size,
                            bool is_first_step,
                            CcStep ** step__o,
                            CcParseMsg ** parse_msgs__iod ) {
    if ( !step_start_an ) return false;
    if ( !step_end_an ) return false;
    if ( !steps_end_an ) return false;
    if ( !step__o || *step__o ) return false;
    if ( !parse_msgs__iod ) return false;

    char const * step_an = step_start_an;
    char const * se_end_an = NULL;
    CcSideEffect se = cc_side_effect_none();

    if ( is_first_step ) {
        if ( cc_skip_disambiguation( step_an ) ) {
            CcPos da = CC_POS_CAST_INVALID;
            char const * da_end_an = NULL;

            if ( !cc_parse_pos( step_an, &da, &da_end_an ) )
                return _cc_fail_with_msg_in_step( "Error parsing disambiguation in step '%s'.\n", step_start_an, step_an, step_end_an, parse_msgs__iod );

            CcStep * da__t = cc_step__new( CC_SLTE_InitialPosition, da, se );
            if ( !da__t ) return false;

            if ( !cc_step_extend( step__o, &da__t ) ) {
                cc_step_free_all( &da__t );
                return false;
            }
            // da__t = NULL; // Not needed, ownership transferred.

            step_an = da_end_an;
            return (bool)( *step__o );
        }
    }

    CcStepLinkTypeEnum sle = CC_SLTE_None;
    CcMaybeBoolEnum result = cc_parse_step_link( step_an, steps_end_an, &sle );

    if ( result == CC_MBE_True )
        step_an += cc_step_link_len( sle );
    else if ( result == CC_MBE_False )
        return _cc_fail_with_msg_in_step( "Invalid step separator in step '%s'.\n", step_start_an, step_an, step_end_an, parse_msgs__iod );

    CcPos pos = CC_POS_CAST_INVALID;
    char const * pos_end_an = NULL;

    if ( !cc_parse_pos( step_an, &pos, &pos_end_an ) )
        return _cc_fail_with_msg_in_step( "Error parsing step '%s'.\n", step_start_an, step_an, step_end_an, parse_msgs__iod );

    if ( !cc_parse_side_effect( pos_end_an, step_an, step_end_an,
                                is_turn_light,
                                board_size,
                                &se,
                                &se_end_an,
                                parse_msgs__iod ) )
        return false;

    if ( se_end_an != step_end_an )
        return _cc_fail_with_msg_in_step( "Unexpected notation at the end encountered, in step '%s'.\n", step_start_an, step_an, step_end_an, parse_msgs__iod );

    CcStep * step__t = cc_step__new( sle, pos, se );
    if ( !step__t ) return false;

    if ( !cc_step_extend( step__o, &step__t ) ) {
        cc_step_free_all( &step__t );
        return false;
    }
    // step__t = NULL; // Not needed, ownership transferred.

    return (bool)( *step__o );
}


bool cc_parse_steps( char const * steps_start_an,
                     char const * steps_end_an,
                     bool is_turn_light,
                     cc_uint_t board_size,
                     CcStep ** steps__o,
                     CcParseMsg ** parse_msgs__iod ) {
    if ( !steps_start_an ) return false;
    if ( !steps_end_an ) return false;
    if ( !steps__o || *steps__o ) return false;
    if ( !parse_msgs__iod ) return false;

    if ( !CC_IS_BOARD_SIZE_VALID( board_size ) ) return false;

    char const * step_start_an = NULL;
    char const * step_end_an = NULL;
    CcStep * steps__t = NULL;
    // bool is_first_step = true;

    size_t index = 0;
    bool had_just_destination = false;
    bool had_initial_pos = false;
    bool had_reposition = false;
    // bool had_starting_pos = false; // Contained in reposition step.
    bool had_destination = false;
    bool requires_another_step = false;

    while ( cc_iter_step( steps_start_an, steps_end_an, &step_start_an, &step_end_an ) ) {
        CcStep * step__t = NULL; // <!> Could contain more than one step!

        CC_STR_PRINT( step_start_an, step_end_an, 0, "Step: '%s'.\n", 0, NULL );

        if ( !_cc_parse_step( step_start_an, step_end_an, steps_end_an, is_turn_light, board_size,
                              ( index == 0 ), // is_first_step,
                              &step__t,
                              parse_msgs__iod ) ) {
            CC_PRINTF( "!_cc_parse_step\n" );

            cc_step_free_all( &step__t );
            cc_step_free_all( &steps__t );
            return false;
        }

        if ( step__t->link == CC_SLTE_JustDestination ) {
            bool just_dest_ok = ( ( index == 0 ) || ( index == 1 && had_initial_pos ) ); // Here, initial position == disambiguation.

            if ( had_just_destination || !just_dest_ok ) { // == true --> bug
                cc_step_free_all( &step__t );
                cc_step_free_all( &steps__t );
                return false;
            }

            had_just_destination = true;
        } else if ( step__t->link == CC_SLTE_InitialPosition ) {
            if ( had_just_destination || had_initial_pos || index != 0 ) { // == true --> bug
                if ( had_initial_pos && cc_an_has_separated_steps( steps_start_an, steps_end_an, true, true ) )
                    _cc_fail_with_msg_in_step( "Disambiguation may not be followed by initial position, in steps '%s'.\n", steps_start_an, NULL, steps_end_an, parse_msgs__iod );

                cc_step_free_all( &step__t );
                cc_step_free_all( &steps__t );
                return false;
            }

            had_initial_pos = true;
        } else if ( step__t->link == CC_SLTE_Reposition ) {
            bool reposition_ok = ( !had_initial_pos && ( index == 0 ) ) || ( had_initial_pos && ( index == 1 ) );

            if ( had_just_destination || had_reposition || !reposition_ok ) { // had_just_destination ? --> bug
                if ( !reposition_ok )
                    _cc_fail_with_msg_in_step( "Reposition can only be used for the first step, optionally preceded by initial position, in steps '%s'.\n", steps_start_an, NULL, steps_end_an, parse_msgs__iod );

                cc_step_free_all( &step__t );
                cc_step_free_all( &steps__t );
                return false;
            }

            had_reposition = true;
        } else if ( step__t->link == CC_SLTE_Destination ) {
            had_destination = true;
        } else if ( ( step__t->link == CC_SLTE_Next ) || ( step__t->link == CC_SLTE_Distant ) ) {
            if ( had_just_destination || had_destination ) { // had_just_destination ? --> bug
                if ( had_destination )
                    _cc_fail_with_msg_in_step( "There can be no more steps after destination, in steps '%s'.\n", steps_start_an, NULL, steps_end_an, parse_msgs__iod );

                cc_step_free_all( &step__t );
                cc_step_free_all( &steps__t );
                return false;
            }
        } else { // == CC_SLTE_None ? garbage ? --> bug
            cc_step_free_all( &step__t );
            cc_step_free_all( &steps__t );
            return false;
        }

        if ( step__t->link == CC_SLTE_InitialPosition ) {
            if ( !CC_POS_IS_DISAMBIGUATION( step__t->field ) ) {
                _cc_fail_with_msg_in_step( "Initial position has to be a valid disambiguation, in steps '%s'.\n", steps_start_an, NULL, steps_end_an, parse_msgs__iod );

                cc_step_free_all( &step__t );
                cc_step_free_all( &steps__t );
                return false;
            }
        } else {
            if ( !CC_POS_IS_VALID( step__t->field ) && ( step__t->side_effect.type != CC_SETE_Castle ) ) { // For castling it's enough to have just files.
                _cc_fail_with_msg_in_step( "All steps has to specify complete position, in steps '%s'.\n", steps_start_an, NULL, steps_end_an, parse_msgs__iod );

                cc_step_free_all( &step__t );
                cc_step_free_all( &steps__t );
                return false;
            }
        }

        requires_another_step = false;

        if ( CC_SIDE_EFFECT_TYPE_DOES_NOT_TERMINATE_PLY( step__t->side_effect.type ) )
            requires_another_step = true;

        if ( !cc_step_extend( &steps__t, &step__t ) ) { // <!> step__t could contain more than one step --> use cc_step_extend(), instead of cc_step_append().
            cc_step_free_all( &step__t );
            cc_step_free_all( &steps__t );
            return false;
        }

        // is_first_step = false;
        ++index;
    }

    if ( requires_another_step ) {
        _cc_fail_with_msg_in_step( "Transparency and divergence must be followed by destination step, in steps '%s'.\n", steps_start_an, NULL, steps_end_an, parse_msgs__iod );

        cc_step_free_all( &steps__t );
        return false;
    }

    #ifdef __CC_DEBUG__
    {
        char * step_str__a = cc_step_all_to_string__new( steps__t );

        CC_STR_PRINT( step_str__a, NULL, 0, "Steps: '%s'.\n", 0, NULL );

        CC_FREE( step_str__a );
    }
    #endif // __CC_DEBUG__

    *steps__o = steps__t; // Ownership transfer.
    // steps__t = NULL; // Not needed.

    return true;
}
