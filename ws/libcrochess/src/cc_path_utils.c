// Copyright (c) 2021, 2022, 2023 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_pos_defs.h"
// #include "cc_path_defs.h"
#include "cc_path_utils.h"

/**
    @file cc_path_utils.c
    @brief Path utils.
*/


#include "cc_path_defs.h"

// TODO ::
//
// bool cc_steps_are_congruent( CcSteps * restrict steps,
//                              CcPptLink * restrict positions )
// {
//     if ( !steps ) return false;
//     if ( !positions ) return false;

//     CcPptLink * p = positions;
//     CcSteps * prev_s = NULL;
//     CcSteps * s = steps;

//     CC_REWIND( s );

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

//                 CC_FASTFORWARD( p );

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
//
// TODO ::
