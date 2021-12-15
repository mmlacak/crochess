// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdio.h>
#include <stdarg.h>

#include "cc_str_utils.h"
#include "cc_tokenizer.h"
#include "cc_parse_msg.h"

/**
    @file cc_parse_msg.c
    @brief Parse message related functions.
*/


CcParseMsg * cc_parse_msg_new( CcParseMsgEnum type,
                               char const * const restrict msg )
{
    CcParseMsg * new = malloc( sizeof( CcParseMsg ) );
    if ( !new ) return NULL;

    new->type = type;
    new->msg = cc_str_duplicate_new( msg, false, BUFSIZ );
    new->next = NULL;

    return new;
}

CcParseMsg * cc_parse_msg_append( CcParseMsg * const restrict parse_msgs,
                                  CcParseMsgEnum const type,
                                  char const * const restrict msg )
{
    if ( !parse_msgs ) return NULL;

    CcParseMsg * new = cc_parse_msg_new( type, msg );
    if ( !new ) return NULL;

    CcParseMsg * pm = parse_msgs;
    while ( pm->next ) pm = pm->next; // rewind
    pm->next = new; // append

    return new;
}

CcParseMsg * cc_parse_msg_append_or_init( CcParseMsg ** const restrict parse_msgs_io,
                                          CcParseMsgEnum const type,
                                          char const * const restrict msg )
{
    if ( !parse_msgs_io ) return NULL;

    CcParseMsg * new = cc_parse_msg_append( *parse_msgs_io, type, msg );

    if ( !*parse_msgs_io ) *parse_msgs_io = new;

    return new;
}

CcParseMsg * cc_parse_msg_append_or_init_format( CcParseMsg ** const restrict parse_msgs_io,
                                                 CcParseMsgEnum const type,
                                                 char const * const restrict fmt, ... )
{

    va_list args;
    va_start( args, fmt );

    char * msg__t = cc_str_format_new( BUFSIZ, fmt, args );

    va_end( args );

    if ( !msg__t ) return NULL;

    return cc_parse_msg_append_or_init( parse_msgs_io, type, msg__t );
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
