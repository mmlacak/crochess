// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdio.h>
#include <stdarg.h>

#include "cc_defines.h"
#include "cc_str_utils.h"
#include "cc_token.h"
#include "cc_parse_msgs.h"

/**
    @file cc_parse_msgs.c
    @brief Parse messages related functions.
*/


CcParseMsgs * cc_parse_msgs__new( CcParseMsgTypeEnum type,
                                  char const * restrict msg,
                                  size_t max_len__d )
{
    CcParseMsgs * pm__a = malloc( sizeof( CcParseMsgs ) );
    if ( !pm__a ) return NULL;

    pm__a->type = type;
    pm__a->msg = cc_str_duplicate__new( msg, false, max_len__d );
    pm__a->next = NULL;

    return pm__a;
}

CcParseMsgs * cc_parse_msgs_append( CcParseMsgs * restrict parse_msgs__io,
                                    CcParseMsgTypeEnum type,
                                    char const * restrict msg,
                                    size_t max_len__d )
{
    if ( !parse_msgs__io ) return NULL;

    CcParseMsgs * pm__t = cc_parse_msgs__new( type, msg, max_len__d );
    if ( !pm__t ) return NULL;

    CcParseMsgs * pm = parse_msgs__io;
    while ( pm->next ) pm = pm->next; // rewind
    pm->next = pm__t; // append // Ownership transfer --> pm__t is now weak pointer.

    return pm__t;
}

CcParseMsgs * cc_parse_msgs_append_if( CcParseMsgs ** restrict parse_msgs__io,
                                       CcParseMsgTypeEnum type,
                                       char const * restrict msg,
                                       size_t max_len__d )
{
    if ( !parse_msgs__io ) return NULL;

    CcParseMsgs * pm__w = NULL;

    if ( !*parse_msgs__io )
        *parse_msgs__io = pm__w = cc_parse_msgs__new( type, msg, max_len__d );
    else
        pm__w = cc_parse_msgs_append( *parse_msgs__io, type, msg, max_len__d );

    return pm__w;
}

CcParseMsgs * cc_parse_msgs_append_format_if( CcParseMsgs ** restrict parse_msgs__io,
                                              CcParseMsgTypeEnum type,
                                              size_t max_len__d,
                                              char const * restrict fmt, ... )
{
    if ( !parse_msgs__io ) return NULL; // To avoid alloc() + free() of msg__a;
                                        // even though this is never referenced.

    va_list args;
    va_start( args, fmt );

    char * msg__a = cc_str_format_va__new( max_len__d, fmt, args );

    va_end( args );

    if ( !msg__a ) return NULL;

    CcParseMsgs * pm__w = cc_parse_msgs_append_if( parse_msgs__io, type, msg__a, max_len__d );

    CC_FREE( msg__a );

    return pm__w;
}

bool cc_parse_msgs_free_all( CcParseMsgs ** restrict parse_msgs__f )
{
    if ( !parse_msgs__f ) return false;
    if ( !*parse_msgs__f ) return true;

    CcParseMsgs * pm = *parse_msgs__f;
    CcParseMsgs * tmp = NULL;

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


CcParseMsgs * cc_parse_msgs_get_last( CcParseMsgs * restrict parse_msgs )
{
    if ( !parse_msgs ) return NULL;

    CcParseMsgs * pm__w = (CcParseMsgs *)parse_msgs;

    while ( pm__w->next )
        pm__w = pm__w->next;

    return pm__w;
}
