// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>

#include "cc_defines.h"
#include "cc_step.h"

/**
    @file cc_step.c
    @brief Step related functions.
*/


char const * cc_step_link_symbol( CcStepLinkEnum sle )
{
    switch ( sle )
    {
        case CC_SLE_None : return NULL;
        case CC_SLE_Start : return "";
        case CC_SLE_Reposition : return ",";
        case CC_SLE_Next : return ".";
        case CC_SLE_Distant : return "..";
        case CC_SLE_Destination : return "-";

        default : return NULL;
    }
}

CcStep * cc_step__new( CcStepLinkEnum link,
                       CcPos field, CcSideEffect side_effect )
{
    CcStep * step__a = malloc( sizeof( CcStep ) );
    if ( !step__a ) return NULL;

    step__a->link = link;
    step__a->field = field;
    step__a->side_effect = side_effect;

    step__a->next = NULL;

    return step__a;
}

CcStep * cc_step_append( CcStep * restrict steps__io,
                         CcStepLinkEnum link, CcPos field, CcSideEffect side_effect )
{
    if ( !steps__io ) return NULL;

    CcStep * step__t = cc_step__new( link, field, side_effect );
    if ( !step__t ) return NULL;

    CcStep * s = steps__io;
    while ( s->next ) s = s->next; // rewind
    s->next = step__t; // append // Ownership transfer --> step__t is now weak pointer.

    return step__t;
}

CcStep * cc_step_append_if( CcStep ** restrict steps__io,
                            CcStepLinkEnum link, CcPos field, CcSideEffect side_effect )
{
    if ( !steps__io ) return NULL;

    CcStep * step__w = NULL;

    if ( !*steps__io )
        *steps__io = step__w = cc_step__new( link, field, side_effect );
    else
        step__w = cc_step_append( *steps__io, link, field, side_effect );

    return step__w;
}

// TODO :: REWRITE :: using cc_step_append_if
CcStep * cc_step_duplicate_all__new( CcStep * restrict steps__io )
{
    if ( !steps__io ) return NULL;

    CcStep * steps__a = cc_step__new( steps__io->link,
                                      steps__io->field,
                                      steps__io->side_effect );
    if ( !steps__a ) return NULL;

    CcStep * from = steps__io->next;

    while ( from )
    {
        CcStep * step__w = cc_step_append( steps__a,
                                           from->link,
                                           from->field,
                                           from->side_effect );
        if ( !step__w )
        {
            cc_step_free_all( &steps__a );
            return NULL;
        }

        from = from->next;
    }

    return steps__a;
}
// TODO :: REWRITE :: using cc_step_append_if

CcStep * cc_step_extend( CcStep ** restrict steps__io,
                         CcStep ** restrict steps__n )
{
    if ( !steps__io ) return NULL;
    if ( !*steps__io ) return NULL;

    if ( !steps__n ) return NULL;
    if ( !*steps__n ) return NULL;

    CcStep * last = *steps__io;
    while ( last->next ) last = last->next;

    // Ownership transfer.
    last->next = *steps__n;
    *steps__n = NULL;

    return last->next;
}

CcStep * cc_step_extend_if( CcStep ** restrict steps__iod,
                            CcStep ** restrict steps__n )
{
    if ( !steps__iod ) return NULL;
    if ( !steps__n ) return NULL;

    if ( !*steps__n ) return *steps__iod;

    if ( !*steps__iod )
    {
        // Ownership transfer.
        *steps__iod = *steps__n;
        *steps__n = NULL;

        return *steps__iod;
    }

    return cc_step_extend( steps__iod, steps__n );
}

size_t cc_step_count( CcStep * restrict steps )
{
    if ( !steps ) return 0;

    size_t count = 0;
    CcStep * s = steps;

    while ( s )
    {
        ++count;
        s = s->next;
    }

    return count;
}

bool cc_step_is_valid( CcStep * restrict step, unsigned int board_size )
{
    if ( !step ) return false;

    if ( !CC_IS_COORD_2_ON_BOARD( board_size, step->field.i, step->field.j ) ) return false;

    if ( !cc_side_effect_is_valid( step->side_effect, board_size ) ) return false;

    return true;
}

bool cc_step_are_all_valid( CcStep * restrict steps, unsigned int board_size )
{
    if ( !steps ) return false;

    if ( !cc_step_is_valid( steps, board_size ) ) return false;
    if ( !steps->next ) return ( steps->link != CC_SLE_Destination ); // The only step must be destination.

    bool is_starting = ( steps->link == CC_SLE_Start );
    bool is_repositioning = ( steps->link == CC_SLE_Reposition );

    CcStep * s = steps->next;
    while ( s )
    {
        if ( s->link == CC_SLE_Start ) return false; // Only first step can be starting.
        if ( ( s->link == CC_SLE_Destination ) && ( s->next ) ) return false; // Destination step must not be in the middle.
        if ( ( !s->next ) && ( s->link != CC_SLE_Destination ) ) return false; // The last step must be destination.

        if ( s->link == CC_SLE_Reposition ) // Repositioning can be only on first step or second step, if following starting step.
        {
            if ( is_repositioning ) return false; // Already repositioning, but it can be only one.

            if ( s != steps->next ) return false; // If not on second step, repositioning is misplaced.
            if ( !is_starting ) return false; // Repositioning is on second step, but not following starting step.
        }

        if ( !cc_step_is_valid( s, board_size ) ) return false;

        s = s->next;
    }

    return true;
}

// bool cc_steps_are_congruent( CcSteps * restrict steps,
//                              CcPosLink * restrict positions )
// {
//     if ( !steps ) return false;
//     if ( !positions ) return false;

//     CcPosLink * p = positions;
//     CcSteps * prev_s = NULL;
//     CcSteps * s = steps;

//     while ( s->prev ) s = s->prev; // rewind

//     while ( s )
//     {
//         switch ( s->step_link )
//         {
//             case CC_SLE_Start :
//             {
//                 if ( !s->prev && ( p == positions ) ) // First step, and position?
//                 {
//                     if ( !cc_pos_is_congruent( s->pos, p->pos ) )
//                         return false;
//                 }
//                 else
//                     return false;

//                 break;
//             }

//             case CC_SLE_Reposition :
//             case CC_SLE_Next :
//             {
//                 if ( p && p->next )
//                 {
//                     // If reposition, position before must be the first one.
//                     if ( ( s->step_link == CC_SLE_Reposition ) &&
//                          ( p != positions ) )
//                             return false;

//                     p = p->next;

//                     if ( !cc_pos_is_equal( s->pos, p->pos ) )
//                         return false;
//                 }
//                 else
//                     return false;

//                 break;
//             }

//             case CC_SLE_Distant :
//             {
//                 if ( p )
//                     p = p->next;
//                 else
//                     return false;

//                 bool found = false;

//                 while ( p )
//                 {
//                     if ( cc_pos_is_equal( s->pos, p->pos ) )
//                     {
//                         found = true;
//                         break;
//                     }

//                     p = p->next;
//                 }

//                 if ( !found )
//                     return false;

//                 break;
//             }

//             case CC_SLE_Destination :
//             {
//                 if ( s->next ) return false; // Not the last one?

//                 while ( p && p->next ) p = p->next; // rewind

//                 if ( !cc_pos_is_equal( s->pos, p->pos ) )
//                     return false;

//                 break;
//             }

//             case CC_SLE_None :
//             default :
//                 return false;
//         }

//         prev_s = s;
//         s = s->next;
//     }

//     return ( prev_s && !prev_s->next && p && !p->next );
// }

bool cc_step_free_all( CcStep ** restrict steps__f )
{
    if ( !steps__f ) return false;
    if ( !*steps__f ) return true;

    CcStep * s = *steps__f;

    while ( s )
    {
        CcStep * tmp = s->next;
        CC_FREE( s );
        s = tmp;
    }

    *steps__f = NULL;
    return true;
}

// char * cc_steps_to_short_string__new( CcSteps * restrict steps )
// {
//     if ( !steps ) return NULL;

//     // unused len is certainly > 0, because steps != NULL
//     signed int unused = cc_steps_len( steps ) *
//                         ( CC_MAX_LEN_CHAR_8 + CC_MAX_LEN_CHAR_16 + 2 );
//                         // CC_MAX_LEN_CHAR_8, for position
//                         // + CC_MAX_LEN_CHAR_16, for side-effect
//                         // + 2, for step links, e.g. ".." before step

//     char * steps_str__a = malloc( unused + 1 ); // +1, for '\0'
//     if ( !steps_str__a ) return NULL;

//     // *steps_str__a = '\0'; // Not needed, done after a switch below.

//     char * steps_str = steps_str__a;
//     char * steps_end = steps_str;
//     cc_char_8 pos_c8 = CC_CHAR_8_EMPTY;
//     cc_char_16 se_c16 = CC_CHAR_16_EMPTY;
//     CcSteps * s = steps;

//     while ( s && ( unused > 0 ) )
//     {
//         switch ( s->step_link )
//         {
//             case CC_SLE_None :
//             {
//                 *steps_str++ = '?';
//                 break;
//             }

//             case CC_SLE_Start :
//             {
//                 *steps_str++ = '`';
//                 break;
//             }

//             case CC_SLE_Reposition :
//             {
//                 *steps_str++ = ',';
//                 break;
//             }

//             case CC_SLE_Next :
//             {
//                 *steps_str++ = '.';
//                 break;
//             }

//             case CC_SLE_Distant :
//             {
//                 *steps_str++ = '.';
//                 *steps_str++ = '.';
//                 break;
//             }

//             case CC_SLE_Destination :
//             {
//                 *steps_str++ = '-';
//                 break;
//             }

//             default :
//             {
//                 *steps_str++ = '!';
//                 break;
//             }
//         }

//         *steps_str = '\0';

//         if ( !cc_pos_to_short_string( s->pos, &pos_c8 ) )
//         {
//             CC_FREE( steps_str__a );
//             return NULL;
//         }

//         steps_end = cc_str_append_into( steps_str, unused, pos_c8, CC_MAX_LEN_CHAR_8 );
//         if ( !steps_end )
//         {
//             CC_FREE( steps_str__a );
//             return NULL;
//         }

//         unused -= ( steps_end - steps_str );
//         steps_str = steps_end;

//         if ( !cc_side_effect_to_short_str( s->side_effect, &se_c16 ) )
//         {
//             CC_FREE( steps_str__a );
//             return NULL;
//         }

//         steps_end = cc_str_append_into( steps_str, unused, se_c16, CC_MAX_LEN_CHAR_16 );
//         if ( !steps_end )
//         {
//             CC_FREE( steps_str__a );
//             return NULL;
//         }

//         unused -= ( steps_end - steps_str );
//         steps_str = steps_end;

//         s = s->next;
//     }

//     return steps_str__a;
// }


//
// new conveniences

CcStep * cc_step_none__new( CcStepLinkEnum link, CcPos field )
{
    CcSideEffect se = cc_side_effect_none();
    return cc_step__new( link, field, se );
}

CcStep * cc_step_capture__new( CcStepLinkEnum link, CcPos field,
                               CcPieceEnum piece, CcLosingTagEnum lost_tag, CcPieceEnum promoted_to )
{
    CcSideEffect se = cc_side_effect_capture( piece, lost_tag, promoted_to );
    return cc_step__new( link, field, se );
}

CcStep * cc_step_displacement__new( CcStepLinkEnum link, CcPos field,
                                    CcPieceEnum piece, CcLosingTagEnum lost_tag, CcPos destination )
{
    CcSideEffect se = cc_side_effect_displacement( piece, lost_tag, destination );
    return cc_step__new( link, field, se );
}

CcStep * cc_step_en_passant__new( CcStepLinkEnum link, CcPos field,
                                  CcPieceEnum pawn, CcPos distant )
{
    CcSideEffect se = cc_side_effect_en_passant( pawn, distant );
    return cc_step__new( link, field, se );
}

CcStep * cc_step_castle__new( CcStepLinkEnum link, CcPos field,
                              CcPieceEnum rook, CcPos start, CcPos destination )
{
    CcSideEffect se = cc_side_effect_castle( rook, start, destination );
    return cc_step__new( link, field, se );
}

CcStep * cc_step_promote__new( CcStepLinkEnum link, CcPos field,
                               CcPieceEnum piece )
{
    CcSideEffect se = cc_side_effect_promote( piece );
    return cc_step__new( link, field, se );
}

CcStep * cc_step_tag_for_promotion__new( CcStepLinkEnum link, CcPos field )
{
    CcSideEffect se = cc_side_effect_tag_for_promotion();
    return cc_step__new( link, field, se );
}

CcStep * cc_step_convert__new( CcStepLinkEnum link, CcPos field,
                               CcPieceEnum piece, CcLosingTagEnum lost_tag )
{
    CcSideEffect se = cc_side_effect_convert( piece, lost_tag );
    return cc_step__new( link, field, se );
}

CcStep * cc_step_failed_conversion__new( CcStepLinkEnum link, CcPos field )
{
    CcSideEffect se = cc_side_effect_failed_conversion();
    return cc_step__new( link, field, se );
}

CcStep * cc_step_demote__new( CcStepLinkEnum link, CcPos field,
                              CcPieceEnum piece, CcLosingTagEnum lost_tag, CcPos distant )
{
    CcSideEffect se = cc_side_effect_demote( piece, lost_tag, distant );
    return cc_step__new( link, field, se );
}

CcStep * cc_step_resurrect__new( CcStepLinkEnum link, CcPos field,
                                 CcPieceEnum piece, CcPos destination )
{
    CcSideEffect se = cc_side_effect_resurrect( piece, destination );
    return cc_step__new( link, field, se );
}

CcStep * cc_step_failed_resurrection__new( CcStepLinkEnum link, CcPos field )
{
    CcSideEffect se = cc_side_effect_failed_resurrection();
    return cc_step__new( link, field, se );
}

//
// append conveniences

CcStep * cc_step_none_append( CcStep * restrict steps__io,
                              CcStepLinkEnum link, CcPos field )
{
    CcSideEffect se = cc_side_effect_none();
    return cc_step_append( steps__io, link, field, se );
}

CcStep * cc_step_capture_append( CcStep * restrict steps__io,
                                 CcStepLinkEnum link, CcPos field,
                                 CcPieceEnum piece, CcLosingTagEnum lost_tag, CcPieceEnum promoted_to )
{
    CcSideEffect se = cc_side_effect_capture( piece, lost_tag, promoted_to );
    return cc_step_append( steps__io, link, field, se );
}

CcStep * cc_step_displacement_append( CcStep * restrict steps__io,
                                      CcStepLinkEnum link, CcPos field,
                                      CcPieceEnum piece, CcLosingTagEnum lost_tag, CcPos destination )
{
    CcSideEffect se = cc_side_effect_displacement( piece, lost_tag, destination );
    return cc_step_append( steps__io, link, field, se );
}

CcStep * cc_step_en_passant_append( CcStep * restrict steps__io,
                                    CcStepLinkEnum link, CcPos field,
                                    CcPieceEnum pawn, CcPos distant )
{
    CcSideEffect se = cc_side_effect_en_passant( pawn, distant );
    return cc_step_append( steps__io, link, field, se );
}

CcStep * cc_step_castle_append( CcStep * restrict steps__io,
                                CcStepLinkEnum link, CcPos field,
                                CcPieceEnum rook, CcPos start, CcPos destination )
{
    CcSideEffect se = cc_side_effect_castle( rook, start, destination );
    return cc_step_append( steps__io, link, field, se );
}

CcStep * cc_step_promote_append( CcStep * restrict steps__io,
                                 CcStepLinkEnum link, CcPos field,
                                 CcPieceEnum piece )
{
    CcSideEffect se = cc_side_effect_promote( piece );
    return cc_step_append( steps__io, link, field, se );
}

CcStep * cc_step_tag_for_promotion_append( CcStep * restrict steps__io,
                                           CcStepLinkEnum link, CcPos field )
{
    CcSideEffect se = cc_side_effect_tag_for_promotion();
    return cc_step_append( steps__io, link, field, se );
}

CcStep * cc_step_convert_append( CcStep * restrict steps__io,
                                 CcStepLinkEnum link, CcPos field,
                                 CcPieceEnum piece, CcLosingTagEnum lost_tag )
{
    CcSideEffect se = cc_side_effect_convert( piece, lost_tag );
    return cc_step_append( steps__io, link, field, se );
}

CcStep * cc_step_failed_conversion_append( CcStep * restrict steps__io,
                                           CcStepLinkEnum link, CcPos field )
{
    CcSideEffect se = cc_side_effect_failed_conversion();
    return cc_step_append( steps__io, link, field, se );
}

CcStep * cc_step_demote_append( CcStep * restrict steps__io,
                                CcStepLinkEnum link, CcPos field,
                                CcPieceEnum piece, CcLosingTagEnum lost_tag, CcPos distant )
{
    CcSideEffect se = cc_side_effect_demote( piece, lost_tag, distant );
    return cc_step_append( steps__io, link, field, se );
}

CcStep * cc_step_resurrect_append( CcStep * restrict steps__io,
                                   CcStepLinkEnum link, CcPos field,
                                   CcPieceEnum piece, CcPos destination )
{
    CcSideEffect se = cc_side_effect_resurrect( piece, destination );
    return cc_step_append( steps__io, link, field, se );
}

CcStep * cc_step_failed_resurrection_append( CcStep * restrict steps__io,
                                             CcStepLinkEnum link, CcPos field )
{
    CcSideEffect se = cc_side_effect_failed_resurrection();
    return cc_step_append( steps__io, link, field, se );
}

//
// append or init conveniences

CcStep * cc_step_none_append_if( CcStep ** restrict steps__io,
                                 CcStepLinkEnum link, CcPos field )
{
    CcSideEffect se = cc_side_effect_none();
    return cc_step_append_if( steps__io, link, field, se );
}

CcStep * cc_step_capture_append_if( CcStep ** restrict steps__io,
                                    CcStepLinkEnum link, CcPos field,
                                    CcPieceEnum piece, CcLosingTagEnum lost_tag, CcPieceEnum promoted_to )
{
    CcSideEffect se = cc_side_effect_capture( piece, lost_tag, promoted_to );
    return cc_step_append_if( steps__io, link, field, se );
}

CcStep * cc_step_displacement_append_if( CcStep ** restrict steps__io,
                                         CcStepLinkEnum link, CcPos field,
                                         CcPieceEnum piece, CcLosingTagEnum lost_tag, CcPos destination )
{
    CcSideEffect se = cc_side_effect_displacement( piece, lost_tag, destination );
    return cc_step_append_if( steps__io, link, field, se );
}

CcStep * cc_step_en_passant_append_if( CcStep ** restrict steps__io,
                                       CcStepLinkEnum link, CcPos field,
                                       CcPieceEnum pawn, CcPos distant )
{
    CcSideEffect se = cc_side_effect_en_passant( pawn, distant );
    return cc_step_append_if( steps__io, link, field, se );
}

CcStep * cc_step_castle_append_if( CcStep ** restrict steps__io,
                                   CcStepLinkEnum link, CcPos field,
                                   CcPieceEnum rook, CcPos start, CcPos destination )
{
    CcSideEffect se = cc_side_effect_castle( rook, start, destination );
    return cc_step_append_if( steps__io, link, field, se );
}

CcStep * cc_step_promote_append_if( CcStep ** restrict steps__io,
                                    CcStepLinkEnum link, CcPos field,
                                    CcPieceEnum piece )
{
    CcSideEffect se = cc_side_effect_promote( piece );
    return cc_step_append_if( steps__io, link, field, se );
}

CcStep * cc_step_tag_for_promotion_append_if( CcStep ** restrict steps__io,
                                              CcStepLinkEnum link, CcPos field )
{
    CcSideEffect se = cc_side_effect_tag_for_promotion();
    return cc_step_append_if( steps__io, link, field, se );
}

CcStep * cc_step_convert_append_if( CcStep ** restrict steps__io,
                                    CcStepLinkEnum link, CcPos field,
                                    CcPieceEnum piece, CcLosingTagEnum lost_tag )
{
    CcSideEffect se = cc_side_effect_convert( piece, lost_tag );
    return cc_step_append_if( steps__io, link, field, se );
}

CcStep * cc_step_failed_conversion_append_if( CcStep ** restrict steps__io,
                                              CcStepLinkEnum link, CcPos field )
{
    CcSideEffect se = cc_side_effect_failed_conversion();
    return cc_step_append_if( steps__io, link, field, se );
}

CcStep * cc_step_demote_append_if( CcStep ** restrict steps__io,
                                   CcStepLinkEnum link, CcPos field,
                                   CcPieceEnum piece, CcLosingTagEnum lost_tag, CcPos distant )
{
    CcSideEffect se = cc_side_effect_demote( piece, lost_tag, distant );
    return cc_step_append_if( steps__io, link, field, se );
}

CcStep * cc_step_resurrect_append_if( CcStep ** restrict steps__io,
                                      CcStepLinkEnum link, CcPos field,
                                      CcPieceEnum piece, CcPos destination )
{
    CcSideEffect se = cc_side_effect_resurrect( piece, destination );
    return cc_step_append_if( steps__io, link, field, se );
}

CcStep * cc_step_failed_resurrection_append_if( CcStep ** restrict steps__io,
                                                CcStepLinkEnum link, CcPos field )
{
    CcSideEffect se = cc_side_effect_failed_resurrection();
    return cc_step_append_if( steps__io, link, field, se );
}
