// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdlib.h>
#include <stdio.h>

#include "cc_str_utils.h"

#include "test_msgs.h"


// https://stackoverflow.com/questions/15927583/how-to-suppress-warning-control-reaches-end-of-non-void-function
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wreturn-type"

char const * test_msg_enum_label( TestMsgEnum tme )
{
    switch ( tme )
    {
        // gcc doesn't check if all current options in enum are covered.

        case TME_Debug : return "Debug";
        case TME_Info : return "Info";
        case TME_Warning : return "Warning";
        case TME_Error : return "Error";
        case TME_Fatal : return "Fatal";
        // default : return ""; // Won't be suitable --> make compilers complain.
    }
}

#pragma GCC diagnostic pop

bool test_print_failure( bool expr,
                         TestMsgEnum type,
                         char const * const restrict msg,
                         char const * const restrict file,
                         size_t line,
                         char const * const restrict func )
{
    bool result = expr;

    if ( !result )
    {
        printf( "%s: %s; in %s(), at %s[%lu].\n", test_msg_enum_label( type ), msg, func, file, line );
        fflush( stdout );
    }

    return result;
}


TestMsg * test_msg_new( TestMsgEnum type,
                        char const * const restrict msg,
                        char const * const restrict file,
                        size_t line,
                        char const * const restrict func )
{
    TestMsg * new = malloc( sizeof( TestMsg ) );
    if ( !new ) return NULL;

    new->type = type;
    new->msg = cc_str_duplicate_len_new( msg, BUFSIZ ); // msg; // Don't borrow, unknown scope!

    new->file = cc_str_duplicate_len_new( file, BUFSIZ ); // file; // Don't borrow, unknown scope!
    new->line = line;
    new->func = cc_str_duplicate_len_new( func, BUFSIZ ); // func; // Don't borrow, unknown scope!

    new->next = NULL;

    return new;
}

TestMsg * test_msg_append_new( TestMsg * const restrict test_msgs,
                               TestMsgEnum type,
                               char const * const restrict msg,
                               char const * const restrict file,
                               size_t line,
                               char const * const restrict func )
{
    TestMsg * new = test_msg_new( type, msg, file, line, func );
    if ( !new ) return NULL;
    if ( !test_msgs ) return new;

    TestMsg * tm = test_msgs;
    while ( tm->next ) tm = tm->next; // rewind
    tm->next = new; // append

    return new;
}

TestMsg * test_msg_init_or_append_new( TestMsg ** const restrict test_msgs,
                                       TestMsgEnum type,
                                       char const * const restrict msg,
                                       char const * const restrict file,
                                       size_t line,
                                       char const * const restrict func )
{
    if ( !test_msgs ) return NULL;

    TestMsg * new = test_msg_append_new( *test_msgs, type, msg, file, line, func );

    if ( !*test_msgs ) *test_msgs = new;

    return new;
}

bool test_msg_free_all( TestMsg ** const restrict test_msgs_f )
{
    if ( !test_msgs_f ) return true;
    if ( !*test_msgs_f ) return false;

    TestMsg * tm = *test_msgs_f;

    while ( tm )
    {
        // free() doesn't do pointers to const.
        free( (char *)tm->msg );
        free( (char *)tm->file );
        free( (char *)tm->func );

        TestMsg * tmp = tm->next;
        free( tm );
        tm = tmp;
    }

    *test_msgs_f = NULL;
    return true;
}

bool test_msg_print_all( TestMsg const * const restrict test_msgs,
                         TestMsgEnum level )
{
    if ( !test_msgs ) return false;

    TestMsg const * tm = test_msgs;

    while ( tm )
    {
        if ( tm->type >= level )
        {
            printf( "%s: %s; in %s(), at %s[%lu].\n",
                    test_msg_enum_label( tm->type ),
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
