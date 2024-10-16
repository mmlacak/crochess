// Copyright (c) 2021, 2022, 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>
#include <stdio.h>

#include "cc_defines.h"
#include "cc_math.h"
#include "cc_str_utils.h"

#include "cc_parsed_move.h"


CcParsedMove * cc_parsed_move__new( char const * notation,
                                    size_t max_len__d,
                                    CcParsedPly ** plies__d_n,
                                    CcParsedMoveStatusEnum status ) {
    CcParsedMove * mv__a = malloc( sizeof( CcParsedMove ) );
    if ( !mv__a ) return NULL;

    mv__a->notation = cc_str_duplicate__new( notation, false, max_len__d );
    if ( notation && ( !mv__a->notation ) ) {
        CC_FREE( mv__a );
        return NULL;
    }

    if ( plies__d_n ) {
        mv__a->plies = *plies__d_n;
        *plies__d_n = NULL; // Taking ownership.
    } else
        mv__a->plies = NULL;

    mv__a->status = status;

    mv__a->prev__w = NULL;
    mv__a->next = NULL;

    return mv__a;
}

CcParsedMove * cc_parsed_move_append( CcParsedMove ** moves__iod_a,
                                      char const * notation,
                                      size_t max_len__d,
                                      CcParsedPly ** plies__d_n,
                                      CcParsedMoveStatusEnum status ) {
    if ( !moves__iod_a ) return NULL;

    CcParsedMove * mv__t = cc_parsed_move__new( notation, max_len__d, plies__d_n, status );
    if ( !mv__t ) return NULL;

    if ( !*moves__iod_a ) {
        *moves__iod_a = mv__t; // Ownership transfer.
    } else {
        CcParsedMove * m = *moves__iod_a;
        CC_FASTFORWARD( m );

        m->next = mv__t; // Append + ownership transfer.
        mv__t->prev__w = m;
    }

    return mv__t; // Weak pointer.
}

CcParsedMove * cc_parsed_move_duplicate_all__new( CcParsedMove * moves ) {
    if ( !moves ) return NULL;

    CcParsedMove * mv__a = NULL;
    CcParsedMove * from = moves;

    CC_REWIND( from );

    do {
        CcParsedPly * plies__t = cc_parsed_ply_duplicate_all__new( from->plies );
        if ( !plies__t ) {
            cc_parsed_move_free_all( &mv__a );
            return NULL;
        }

        CcParsedMove * mv__w = cc_parsed_move_append( &mv__a,
                                         from->notation,
                                         CC_MAX_LEN_ZERO_TERMINATED,
                                         &plies__t,
                                         from->status );
        if ( !mv__w ) {
            cc_parsed_ply_free_all( &plies__t ); // Failed append --> no ownership transfer ...
            cc_parsed_move_free_all( &mv__a );
            return NULL;
        }

        from = from->next;
    }
    while ( from );

    return mv__a;
}

bool cc_parsed_move_free_all( CcParsedMove ** moves__f ) {
    if ( !moves__f ) return false;
    if ( !*moves__f ) return true;

    bool result = true;
    CcParsedMove * mv = *moves__f;

    CC_REWIND( mv );

    while ( mv ) {
        CC_FREE( mv->notation );

        CcParsedPly ** plies = &( mv->plies );
        result = cc_parsed_ply_free_all( plies ) && result;

        CcParsedMove * tmp = mv->next;
        CC_FREE( mv );
        mv = tmp;
    }

    *moves__f = NULL;
    return result;
}

size_t cc_parsed_move_plies_count( CcParsedMove * move ) {
    if ( !move ) return 0;
    if ( !move->plies ) return 0;

    size_t count = 1;
    CcParsedPly * p = move->plies;

    while ( p->next ) {
        ++count;
        p = p->next;
    }

    return count;
}

size_t cc_parsed_move_all_notations_size( CcParsedMove * move, bool is_score ) {
    if ( !move ) return 0;

    size_t size = 0;
    size_t count = 0;
    CcParsedMove * m = move;

    CC_REWIND( m );

    while ( m ) {
        size += cc_str_len( m->notation, NULL, CC_MAX_LEN_ZERO_TERMINATED ); // TODO :: replace CC_MAX_LEN_ZERO_TERMINATED with SIZBUF (?)
        count += 1;

        m = m->next;
    }

    if ( is_score ) {
        // Game score migth look like this:
        //      ___previous moves___
        // 123. <move_light> <move_dark>
        // 124. <move_light> ...

        // cycle == light move + dark move
        // +1 == last (incomplete) cycle, because /2 == floor( integer )
        size_t cycles = count / 2 + 1;

        // 1st 3 == ". " after cycle index + " " between moves in a cycle
        // +digits == count of digits in cycle index, e.g. 3 in "123"
        // +1 == '\n'
        // *cycles == count of cycles
        // last +3 == "..."
        // last +1 == '\0'
        size += ( 3 + cc_count_of_digits( cycles ) + 1 ) * cycles + 3 + 1;
    } else {
        // List of moves migth look like this:
        //      ___previous moves___
        // <move_light>
        // <move_dark>
        // <move_light>

        // count == '\n'
        // +1 == '\0'
        size += count + 1;
    }

    return size;
}

char * cc_parsed_move_as_string__new( CcParsedMove * move, bool is_score ) {
    if ( !move ) return NULL;

    size_t size = cc_parsed_move_all_notations_size( move, is_score );
    if ( size == 0 ) return NULL;

    char * move_str__a = malloc( size );
    if ( !move_str__a ) return NULL;

    size_t i = 0;
    size_t index = 0;
    int result = 0;

    char * str = move_str__a;
    CcParsedMove * m = move;
    CcParsedMove * l = NULL; // light move
    CcParsedMove * d = NULL; // dark move

    CC_REWIND( m );

    if ( is_score ) {
        while ( m ) {
            if ( i++ % 2 == 0 ) {
                l = m;

                if ( !m->next ) {
                    // printf( "%lu. %s ...\n", index+1, l->notation );
                    result = snprintf( str, size, "%zu. %s ...\n", ++index, l->notation );
                    if ( result >= 0 ) str += result;
                    break;
                }
            } else {
                d = m;
                // printf( "%lu. %s %s\n", ++index, l->notation, d->notation );
                result = snprintf( str, size, "%zu. %s %s\n", ++index, l->notation, d->notation );
                str += result;
                if ( result < 0 ) break;
            }

            m = m->next;
        }
    } else {
        while ( m ) {
            result = snprintf( str, size, "%s\n", m->notation );
            str += result;
            if ( result < 0 ) break;

            m = m->next;
        }
    }

    if ( result < 0 ) {
        // From: https://en.cppreference.com/w/c/io/fprintf
        // ... a negative value if an encoding error (for string and character conversion specifiers) occurred.
        CC_FREE( move_str__a );
        return NULL;
    }

    return move_str__a;
}
