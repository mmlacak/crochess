// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdio.h>
#include <stdarg.h>

#include "cc_defines.h"
#include "cc_str_utils.h"
#include "cc_tokenizer.h"
#include "cc_parse_msg.h"

/**
    @file cc_parse_msg.c
    @brief Parse messages related functions.
*/


CcParseMsg * cc_parse_msg_new( CcParseMsgEnum type,
                               char const * restrict msg )
{
    CcParseMsg * pm__a = malloc( sizeof( CcParseMsg ) );
    if ( !pm__a ) return NULL;

    pm__a->type = type;
    pm__a->msg = cc_str_duplicate_new( msg, false, BUFSIZ );
    pm__a->next = NULL;

    return pm__a;
}

CcParseMsg * cc_parse_msg_append( CcParseMsg * restrict parse_msgs__io,
                                  CcParseMsgEnum type,
                                  char const * restrict msg )
{
    if ( !parse_msgs__io ) return NULL;

    CcParseMsg * pm__t = cc_parse_msg_new( type, msg );
    if ( !pm__t ) return NULL;

    CcParseMsg * pm = parse_msgs__io;
    while ( pm->next ) pm = pm->next; // rewind
    pm->next = pm__t; // append // Ownersip transfer --> pm__t is now weak pointer.

    return pm__t;
}

CcParseMsg * cc_parse_msg_append_or_init( CcParseMsg ** restrict parse_msgs__io,
                                          CcParseMsgEnum type,
                                          char const * restrict msg )
{
    if ( !parse_msgs__io ) return NULL;

    CcParseMsg * pm__t = cc_parse_msg_append( *parse_msgs__io, type, msg );

    if ( !*parse_msgs__io ) *parse_msgs__io = pm__t; // Ownersip transfer --> pm__t is now weak pointer.

    return pm__t;
}

CcParseMsg * cc_parse_msg_append_or_init_format( CcParseMsg ** restrict parse_msgs__io,
                                                 CcParseMsgEnum type,
                                                 char const * restrict fmt, ... )
{

    va_list args;
    va_start( args, fmt );

    char * msg__a = cc_str_format_new( BUFSIZ, fmt, args );

    va_end( args );

    if ( !msg__a ) return NULL;

    CcParseMsg * pm__w = cc_parse_msg_append_or_init( parse_msgs__io, type, msg__a );

    CC_FREE( msg__a );

    return pm__w;
}

bool cc_parse_msg_free_all( CcParseMsg ** restrict parse_msgs__f )
{
    if ( !parse_msgs__f ) return false;
    if ( !*parse_msgs__f ) return true;

    CcParseMsg * pm = *parse_msgs__f;

    while ( pm )
    {
        CC_FREE( pm->msg );

        CcParseMsg * tmp = pm->next;
        CC_FREE( pm );
        pm = tmp;
    }

    *parse_msgs__f = NULL;
    return true;
}


CcParseMsg * cc_parse_msg_get_last( CcParseMsg * restrict parse_msgs )
{
    if ( !parse_msgs ) return NULL;

    CcParseMsg * pm__w = (CcParseMsg *)parse_msgs;

    while ( pm__w->next )
        pm__w = pm__w->next;

    return pm__w;
}
