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

CcParseMsg * cc_parse_msg_init_or_append_new( CcParseMsg ** const restrict parse_msgs,
                                              CcParseMsgEnum type,
                                              size_t pos,
                                              char const * const restrict msg )
{
    if ( !parse_msgs ) return NULL;

    CcParseMsg * new = cc_parse_msg_append_new( *parse_msgs, type, pos, msg );

    if ( !*parse_msgs ) *parse_msgs = new;

    return new;
}

bool cc_parse_msg_free_all( CcParseMsg ** const restrict parse_msgs_f )
{
    if ( !parse_msgs_f ) return true;
    if ( !*parse_msgs_f ) return false;

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


char * cc_parse_next_ply_str_new( char const * const restrict move_str_s,
                                  CcParseMsg ** parse_msgs )
{
    if ( !parse_msgs ) return NULL;

    static char const * move_start = NULL;
    static char const * ply_start = NULL;
    static char const * ply_end = NULL;

    bool parse_1st = (bool)move_str_s;
    bool skipped_opening_bracket = false;
    bool skipped_separators = false;
    bool skipped_teminators = false;

    if ( move_str_s )
    {
        move_start = ply_start = move_str_s;

        if ( *ply_start == '[' )
        {
            skipped_opening_bracket = true;
            ++ply_start;
        }
    }
    else
    {
        if ( !ply_end ) return NULL;
        ply_start = ply_end + 1;

        if ( *ply_start == ']' ) ++ply_start;

        if ( *ply_start == '~' )
        {
            ++ply_start;
            skipped_separators = true;
        }
        else if ( *ply_start == '|' )
        {
            ++ply_start;

            if ( *ply_start == '|' )
            {
                ++ply_start;
                skipped_teminators = true;
            }
            else skipped_separators = true;
        }
        else if ( *ply_start == '@' )
        {
            ++ply_start;

            if ( *ply_start == '@' )
            {
                ++ply_start;
                skipped_teminators = true;

                if ( *ply_start == '@' ) ++ply_start;
            }
            else skipped_separators = true;
        }
        else if ( *ply_start == ':' )
        {
            ++ply_start;
            skipped_separators = true;

            if ( *ply_start == ':' ) ++ply_start;
            else
            {
                cc_parse_msg_init_or_append_new( parse_msgs, CC_PME_Error, ply_start - move_start, "malformed ply separator" );
                return NULL;
            }
        }

        if ( *ply_start == '[' )
        {
            skipped_opening_bracket = true;
            ++ply_start;
        }
    }

    if ( ( !parse_1st ) && ( !skipped_separators ) && skipped_opening_bracket )
    {
        cc_parse_msg_init_or_append_new( parse_msgs, CC_PME_Error, ply_start - move_start, "ply started without separator" );
        return NULL;
    }

    if ( *ply_start == '+' )
    {
        ply_end = NULL; // Move end, invalidate another call on the same str.
        return "+";
    }
    else if ( *ply_start == '#' )
    {
        ply_end = NULL; // Move end, invalidate another call on the same str.
        return "#";
    }
    else if ( *ply_start == '\0' )
    {
        ply_end = NULL; // String end, invalidate another call on the same str.

        if ( skipped_separators || skipped_teminators || skipped_opening_bracket )
        {
            cc_parse_msg_init_or_append_new( parse_msgs, CC_PME_Error, ply_start - move_start, "premature end of ply" );
        }

        return NULL;
    }

    ply_end = ply_start + 1;
    while ( isalnum( *ply_end ) ) ++ply_end;

    if ( ( skipped_opening_bracket ) && ( *(ply_end + 1) != ']' ) )
    {
        cc_parse_msg_init_or_append_new( parse_msgs, CC_PME_Error, ply_start - move_start, "no closing bracket" );
        return NULL;
    }

    if ( ( *(ply_end + 1) == ']' ) && ( !skipped_opening_bracket ) )
    {
        cc_parse_msg_init_or_append_new( parse_msgs, CC_PME_Error, ply_start - move_start, "no opening bracket" );
        return NULL;
    }

    if ( ply_end == ply_start ) return NULL;

    size_t len = ply_end - ply_start;
    char * pos = malloc( len + 1 );
    if ( !pos ) return NULL;

    strncpy( pos, ply_start, len );
    pos[ len ] = '\0';

    return pos;
}
