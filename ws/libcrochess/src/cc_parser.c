// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdbool.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "cc_str_utils.h"
#include "cc_tokenizer.h"
#include "cc_parser.h"


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

CcParseMsg * cc_parse_msg_init_or_append_new( CcParseMsg ** const restrict parse_msgs,
                                              CcParseMsgEnum type,
                                              char const * const restrict sub,
                                              char const * const restrict msg )
{
    if ( !parse_msgs ) return NULL;

    CcParseMsg * new = cc_parse_msg_append_new( *parse_msgs, type, sub, msg );

    if ( !*parse_msgs ) *parse_msgs = new;

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


char * cc_parse_next_ply_str_new( char const * const restrict move_str /* = NULL */,
                                  CcParseMsg ** parse_msgs )
{
    if ( !parse_msgs ) return NULL;

    static char const * start = NULL;
    static char const * end = NULL;

    bool skipped_opening_bracket = false;
    bool skipped_tilde = false;

// TODO :: move status, i.e. #, +

    if ( move_str )
    {
        start = move_str;

        if ( *start == '[' )
        {
            skipped_opening_bracket = true;
            ++start;
        }
    }
    else
    {
        if ( !end ) return NULL;
        start = end + 1;

        if ( *start == ']' ) ++start;

        if ( *start == '~' )
        {
            skipped_tilde = true;
            ++start;
        }

        if ( *start == '[' )
        {
            skipped_opening_bracket = true;
            ++start;
        }
    }

    if ( *start == '\0' )
    {
        if ( skipped_tilde )
        {
            cc_parse_msg_init_or_append_new( parse_msgs,
                                            CC_PME_Error,
                                            cc_str_duplicate_len_new( start, BUFSIZ ),
                                            "premature end of ply" );
        }

        return NULL;
    }

    if ( ( *start == ']' ) || ( *start == '~' ) || ( *start == '[' ) )
    {
        cc_parse_msg_init_or_append_new( parse_msgs,
                                         CC_PME_Error,
                                         cc_str_duplicate_len_new( start, BUFSIZ ),
                                         "ply separators inside ply notation" );
        return NULL;
    }

    end = start + 1;
    while ( isalnum( *end ) ) ++end;

    if ( ( skipped_opening_bracket ) && ( *(end + 1) != ']' ) )
    {
        cc_parse_msg_init_or_append_new( parse_msgs,
                                         CC_PME_Error,
                                         cc_str_duplicate_len_new( end, BUFSIZ ),
                                         "no closing bracket" );
        return NULL;
    }

    if ( ( *(end + 1) == ']' ) && ( !skipped_opening_bracket ) )
    {
        cc_parse_msg_init_or_append_new( parse_msgs,
                                         CC_PME_Error,
                                         cc_str_duplicate_len_new( end, BUFSIZ ),
                                         "no opening bracket" );
        return NULL;
    }

    if ( end == start ) return NULL;

    size_t len = end - start;
    char * pos = malloc( len + 1 );
    if ( !pos ) return NULL;

    strncpy( pos, start, len );
    pos[ len ] = '\0';

    return pos;
}
