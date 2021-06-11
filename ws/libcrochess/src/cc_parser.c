// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdbool.h>
#include <stdlib.h>

#include "cc_tokenizer.h"
#include "cc_parser.h"


CcParseMsg * cc_parse_msg_empty_new()
{
    return cc_parse_msg_new( CC_PE_Empty, NULL, NULL );
}

CcParseMsg * cc_parse_msg_new( CcParseMsgEnum type,
                               char const * const restrict sub,
                               char const * const restrict msg )
{
    CcParseMsg * new = malloc( sizeof( CcParseMsg ) );
    if ( !new ) return NULL;

    new->type = type;
    new->sub = sub;
    new->msg = msg;
    new->next = NULL;

    return new;
}

CcParseMsg * cc_parse_msg_append_new( CcParseMsg * const restrict parse_msgs,
                                      CcParseMsgEnum type,
                                      char const * const restrict sub,
                                      char const * const restrict msg )
{
    CcParseMsg * new = cc_parse_msg_new( type, sub, msg );
    if ( !new ) return NULL;
    if ( !parse_msgs ) return new;

    CcParseMsg * pm = parse_msgs;
    while ( pm->next ) pm = pm->next; // rewind
    pm->next = new; // append

    return new;
}

bool cc_parse_msg_free_all( CcParseMsg ** const restrict parse_msgs )
{
    if ( !parse_msgs ) return true;
    if ( !*parse_msgs ) return false;

    CcParseMsg * pm = *parse_msgs;

    while ( pm )
    {
        // free() doesn't do pointers to const.
        free( (char *)pm->sub );
        free( (char *)pm->msg );

        CcParseMsg * tmp = pm->next;
        free( pm );
        pm = tmp;
    }

    *parse_msgs = NULL;
    return true;
}
