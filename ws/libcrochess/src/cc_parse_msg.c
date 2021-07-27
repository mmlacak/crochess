// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

#include "cc_str_utils.h"
#include "cc_tokenizer.h"
#include "cc_parse_msg.h"

/**
    @file cc_parse_msg.c
    @brief Parse message related functions.
*/



CcParseMsg * cc_parse_msg_new( CcParseMsgEnum type,
                               size_t pos,
                               char const * const restrict msg )
{
    CcParseMsg * new = malloc( sizeof( CcParseMsg ) );
    if ( !new ) return NULL;

    new->type = type;
    new->pos = pos;
    new->msg = cc_str_duplicate_len_new( msg, BUFSIZ );
    new->next = NULL;

    return new;
}

CcParseMsg * cc_parse_msg_append_new( CcParseMsg * const restrict parse_msgs,
                                      CcParseMsgEnum type,
                                      size_t pos,
                                      char const * const restrict msg )
{
    CcParseMsg * new = cc_parse_msg_new( type, pos, msg );
    if ( !new ) return NULL;
    if ( !parse_msgs ) return new;

    CcParseMsg * pm = parse_msgs;
    while ( pm->next ) pm = pm->next; // rewind
    pm->next = new; // append

    return new;
}

CcParseMsg * cc_parse_msg_init_or_append_new( CcParseMsg ** const restrict parse_msgs_io,
                                              CcParseMsgEnum type,
                                              size_t pos,
                                              char const * const restrict msg )
{
    if ( !parse_msgs_io ) return NULL;

    CcParseMsg * new = cc_parse_msg_append_new( *parse_msgs_io, type, pos, msg );

    if ( !*parse_msgs_io ) *parse_msgs_io = new;

    return new;
}

bool cc_parse_msg_free_all( CcParseMsg ** const restrict parse_msgs_f )
{
    if ( !parse_msgs_f ) return false;
    if ( !*parse_msgs_f ) return true;

    CcParseMsg * pm = *parse_msgs_f;

    while ( pm )
    {
        // free() doesn't do pointers to const.
        free( (char *)pm->msg );

        CcParseMsg * tmp = pm->next;
        free( pm );
        pm = tmp;
    }

    *parse_msgs_f = NULL;
    return true;
}
