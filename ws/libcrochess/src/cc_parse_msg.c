// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
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
                                char const * msg,
                                size_t max_len__d ) {
    CcParseMsg * pm__a = malloc( sizeof( CcParseMsg ) );
    if ( !pm__a ) return NULL;

    pm__a->type = type;
    pm__a->msg = cc_str_duplicate__new( msg, false, max_len__d );
    pm__a->next = NULL;

    return pm__a;
}

CcParseMsg * cc_parse_msg_append( CcParseMsg ** parse_msgs__iod_a,
                                  CcParseMsgTypeEnum type,
                                  char const * msg,
                                  size_t max_len__d ) {
    if ( !parse_msgs__iod_a ) return NULL;

    CcParseMsg * pm__t = cc_parse_msg__new( type, msg, max_len__d );
    if ( !pm__t ) return NULL;

    if ( !*parse_msgs__iod_a ) {
        *parse_msgs__iod_a = pm__t; // Ownership transfer.
    } else {
        CcParseMsg * pm = *parse_msgs__iod_a;
        CC_FASTFORWARD( pm );
        pm->next = pm__t; // Append + ownership transfer.
    }

    return pm__t; // Weak pointer.
}

CcParseMsg * cc_parse_msg_append_fmt_va( CcParseMsg ** parse_msgs__iod_a,
                                         CcParseMsgTypeEnum type,
                                         size_t max_len__d,
                                         char const * fmt,
                                         va_list args ) {
    if ( !parse_msgs__iod_a ) return NULL; // To avoid alloc() + free() of msg__a needlessly.

    va_list tmp;
    va_copy( tmp, args );

    char * msg__a = cc_str_fmt_va__new( max_len__d, fmt, tmp );
    va_end( tmp );

    if ( !msg__a ) return NULL;

    CcParseMsg * pm__w = cc_parse_msg_append( parse_msgs__iod_a, type, msg__a, max_len__d );

    CC_FREE( msg__a );

    return pm__w;
}

CcParseMsg * cc_parse_msg_append_fmt( CcParseMsg ** parse_msgs__iod_a,
                                      CcParseMsgTypeEnum type,
                                      size_t max_len__d,
                                      char const * fmt, ... ) {
    if ( !parse_msgs__iod_a ) return NULL; // To avoid constructing va_list needlessly.

    va_list args;
    va_start( args, fmt );

    CcParseMsg * pm__w = cc_parse_msg_append_fmt_va( parse_msgs__iod_a, type, max_len__d, fmt, args );

    va_end( args );

    return pm__w;
}

bool cc_parse_msg_free_all( CcParseMsg ** parse_msgs__f ) {
    if ( !parse_msgs__f ) return false;
    if ( !*parse_msgs__f ) return true;

    CcParseMsg * pm = *parse_msgs__f;
    CcParseMsg * tmp = NULL;

    while ( pm ) {
        CC_FREE( pm->msg );

        tmp = pm->next;
        CC_FREE( pm );
        pm = tmp;
    }

    *parse_msgs__f = NULL;
    return true;
}


CcParseMsg * cc_parse_msg_get_last( CcParseMsg * parse_msgs ) {
    if ( !parse_msgs ) return NULL;

    CcParseMsg * pm__w = (CcParseMsg *)parse_msgs;

    while ( pm__w->next )
        pm__w = pm__w->next;

    return pm__w;
}
