// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSE, COPYING files for details.

#include <stdio.h>

#include "cc_str_utils.h"
#include "cc_tokenizer.h"
#include "cc_parse_msg.h"

/**
    @file cc_parse_msg.c
    @brief Parse message related functions.
*/


CcParseMsg * cc_parse_msg_new( CcParseMsgEnum type,
                               size_t const pos,
                               char const * const restrict msg )
{
    CcParseMsg * new = malloc( sizeof( CcParseMsg ) );
    if ( !new ) return NULL;

    new->type = type;
    new->pos = pos;
    new->msg = cc_str_duplicate_len_new( msg, false, BUFSIZ );
    new->next = NULL;

    return new;
}

CcParseMsg * cc_parse_msg_append( CcParseMsg * const restrict parse_msgs,
                                  CcParseMsgEnum const type,
                                  size_t const pos,
                                  char const * const restrict msg )
{
    if ( !parse_msgs ) return NULL;

    CcParseMsg * new = cc_parse_msg_new( type, pos, msg );
    if ( !new ) return NULL;

    CcParseMsg * pm = parse_msgs;
    while ( pm->next ) pm = pm->next; // rewind
    pm->next = new; // append

    return new;
}

CcParseMsg * cc_parse_msg_init_or_append( CcParseMsg ** const restrict parse_msgs_io,
                                          CcParseMsgEnum const type,
                                          size_t const pos,
                                          char const * const restrict msg )
{
    if ( !parse_msgs_io ) return NULL;

    CcParseMsg * new = cc_parse_msg_append( *parse_msgs_io, type, pos, msg );

    if ( !*parse_msgs_io ) *parse_msgs_io = new;

    return new;
}

bool cc_parse_msg_free_all( CcParseMsg ** const restrict parse_msgs__f )
{
    if ( !parse_msgs__f ) return false;
    if ( !*parse_msgs__f ) return true;

    CcParseMsg * pm = *parse_msgs__f;

    while ( pm )
    {
        // free() doesn't do pointers to const.
        free( (char *)pm->msg );

        CcParseMsg * tmp = pm->next;
        free( pm );
        pm = tmp;
    }

    *parse_msgs__f = NULL;
    return true;
}


CcParseMsg * cc_parse_msg_get_last( CcParseMsg const * const restrict parse_msgs )
{
    if ( !parse_msgs ) return NULL;

    CcParseMsg * pm = (CcParseMsg *)parse_msgs;

    while ( pm->next )
        pm = pm->next;

    return pm;
}
