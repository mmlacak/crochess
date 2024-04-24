// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>
#include <stdio.h>

#include "cc_defines.h"
#include "cc_str_utils.h"

#include "test_msgs.h"


char * test_msgs_enum_label( TestMsgEnum tme ) {
    switch ( tme ) {
        case TME_Debug : return "Debug";
        case TME_Info : return "Info";
        case TME_Warning : return "Warning";
        case TME_Error : return "Error";
        case TME_Fatal : return "Fatal";

        default : return CC_DEFAULT_ENTITY_STRING;
    }
}

bool test_print_failure( bool expr,
                         TestMsgEnum type,
                         char const * msg,
                         char const * file,
                         size_t line,
                         char const * func ) {
    bool result = expr;

    if ( !result ) {
        printf( "%s: %s; in %s(), at %s[%lu].\n", test_msgs_enum_label( type ), msg, func, file, line );
        fflush( stdout );
    }

    return result;
}


TestMsgs * test_msgs__new( TestMsgEnum type,
                           char const * msg,
                           char const * file,
                           size_t line,
                           char const * func ) {
    TestMsgs * new = malloc( sizeof( TestMsgs ) );
    if ( !new ) return NULL;

    new->type = type;
    new->msg = cc_str_duplicate__new( msg, false, BUFSIZ ); // msg; // Don't borrow, unknown scope!

    new->file = cc_str_duplicate__new( file, false, BUFSIZ ); // file; // Don't borrow, unknown scope!
    new->line = line;
    new->func = cc_str_duplicate__new( func, false, BUFSIZ ); // func; // Don't borrow, unknown scope!

    new->next = NULL;

    return new;
}

TestMsgs * test_msgs_append( TestMsgs ** test_msgs__iod,
                             TestMsgEnum type,
                             char const * msg,
                             char const * file,
                             size_t line,
                             char const * func ) {
    if ( !test_msgs__iod ) return NULL;

    TestMsgs * tm__t = test_msgs__new( type, msg, file, line, func );
    if ( !tm__t ) return NULL;

    if ( !*test_msgs__iod ) {
        *test_msgs__iod = tm__t; // Ownership transfer.
    } else {
        TestMsgs * tm = *test_msgs__iod;
        CC_FASTFORWARD( tm );
        tm->next = tm__t; // Append + ownership transfer.
    }

    return tm__t; // Weak pointer.
}

bool test_msgs_free_all( TestMsgs ** test_msgs__f ) {
    if ( !test_msgs__f ) return false;
    if ( !*test_msgs__f ) return true;

    TestMsgs * tm = *test_msgs__f;
    TestMsgs * tmp = NULL;

    while ( tm ) {
        CC_FREE( tm->msg );
        CC_FREE( tm->file );
        CC_FREE( tm->func );

        tmp = tm->next;
        CC_FREE( tm );
        tm = tmp;
    }

    *test_msgs__f = NULL;
    return true;
}

bool test_msgs_print_all( TestMsgs * test_msgs,
                          TestMsgEnum level ) {
    if ( !test_msgs ) return false;

    TestMsgs * tm = test_msgs;

    while ( tm ) {
        if ( tm->type >= level ) {
            printf( "%s: %s; in %s(), at %s[%lu].\n",
                    test_msgs_enum_label( tm->type ),
                    tm->msg,
                    tm->func,
                    tm->file,
                    tm->line );

            fflush( stdout );
        }

        tm = tm->next;
    }

    return true;
}
