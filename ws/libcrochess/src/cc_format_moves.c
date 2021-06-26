// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "cc_format_moves.h"
#include "cc_str_utils.h"


char * cc_format_step_new( CcChessboard * const restrict cb,
                           CcMove const * const restrict move,
                           CcPly const * const restrict ply,
                           CcStep const * const restrict step )
{
    return NULL;
}

char * cc_format_ply_new( CcChessboard * const restrict cb,
                          CcMove const * const restrict move,
                          CcPly const * const restrict ply )
{
    char * xxx = (char *)malloc( 5 );
    sprintf( xxx, "~Xxx" );
    return xxx;
}

char * cc_format_move_new( CcChessboard * const restrict cb,
                           CcMove const * const restrict move )
{
    if ( !cb ) return NULL;
    if ( !move ) return NULL;
    if ( !move->plies ) return NULL;

    char * result = NULL;
    CcPly * ply = move->plies;

    while ( ply )
    {
        char * new = cc_format_ply_new( cb, move, ply );
        char * appended = cc_str_concatenate_len_new( result, new, BUFSIZ );

        free( result );
        free( new );
        result = appended;

        ply = ply->next;
    }

    char status = '\0';
    if ( move->status  == CC_MSE_Check ) status = '+';
    else if ( move->status  == CC_MSE_Checkmate ) status = '#';

    if ( status != '\0' )
    {
        size_t len = cc_str_len( result ) + 1;
        result = realloc( result, len + 1 );

        if ( result )
        {
            char * r = result;
            while ( *r ) ++r;

            *r = status;
            *(++r) = '\0';
        }
    }

    return result;
}
