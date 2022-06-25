// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdio.h>
#include <stdarg.h>

#include "cc_defines.h"
#include "cc_str_utils.h"
#include "cc_token.h"
#include "cc_parse_msg.h"

/**
    @file cc_parse_msg.c
    @brief Parse messages related functions.
*/


CcParseMsg * cc_parse_msg__new( CcParseMsgTypeEnum type,
                                char const * restrict msg,
                                size_t max_len__d )
{
    CcParseMsg * pm__a = malloc( sizeof( CcParseMsg ) );
    if ( !pm__a ) return NULL;

    pm__a->type = type;
    pm__a->msg = cc_str_duplicate__new( msg, false, max_len__d );
    pm__a->next = NULL;

    return pm__a;
}

CcParseMsg * cc_parse_msg_append( CcParseMsg * restrict parse_msgs__io,
                                  CcParseMsgTypeEnum type,
                                  char const * restrict msg,
                                  size_t max_len__d )
{
    if ( !parse_msgs__io ) return NULL;

    CcParseMsg * pm__t = cc_parse_msg__new( type, msg, max_len__d );
    if ( !pm__t ) return NULL;

    CcParseMsg * pm = parse_msgs__io;
    while ( pm->next ) pm = pm->next; // rewind
    pm->next = pm__t; // append // Ownership transfer --> pm__t is now weak pointer.

    return pm__t;
}

CcParseMsg * cc_parse_msg_append_or_init( CcParseMsg ** restrict parse_msgs__io,
                                          CcParseMsgTypeEnum type,
                                          char const * restrict msg,
                                          size_t max_len__d )
{
    if ( !parse_msgs__io ) return NULL;

    CcParseMsg * pm__w = NULL;

    if ( !*parse_msgs__io )
        *parse_msgs__io = pm__w = cc_parse_msg__new( type, msg, max_len__d );
    else
        pm__w = cc_parse_msg_append( *parse_msgs__io, type, msg, max_len__d );

    return pm__w;
}

CcParseMsg * cc_parse_msg_append_or_init_format( CcParseMsg ** restrict parse_msgs__io,
                                                 CcParseMsgTypeEnum type,
                                                 size_t max_len__d,
                                                 char const * restrict fmt, ... )
{
    if ( !parse_msgs__io ) return NULL; // To avoid alloc() + free() of msg__a;
                                        // even though this is never referenced.

    va_list args;
    va_start( args, fmt );

    char * msg__a = cc_str_format__new( max_len__d, fmt, args );

    va_end( args );

    if ( !msg__a ) return NULL;

    CcParseMsg * pm__w = cc_parse_msg_append_or_init( parse_msgs__io, type, msg__a, max_len__d );

    CC_FREE( msg__a );

    return pm__w;
}

bool cc_parse_msg_free_all( CcParseMsg ** restrict parse_msgs__f )
{
    if ( !parse_msgs__f ) return false;
    if ( !*parse_msgs__f ) return true;

    CcParseMsg * pm = *parse_msgs__f;
    CcParseMsg * tmp = NULL;

    while ( pm )
    {
        CC_FREE( pm->msg );

        tmp = pm->next;
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
