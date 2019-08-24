/* Copyright (C) 2011, 2019 Mario Mlačak, mmlacak@gmail.com */

#include <stdio.h>
#include <stdlib.h>

#include "test_map_node.h"


int INT_LEN = 11;


//lst_node * init_list_ints( int min, int max )
//{
    //lst_node * p_lst = NULL;

    //for ( int i = min; i <= max; ++i )
    //{
        //char * s = (char *)malloc( INT_LEN );
        //sprintf( s, "%d", i );

        //if ( !p_lst )
        //{
            //p_lst = lst_new_node( (void *)s );
        //}
        //else
        //{
            //lst_node * p_new = lst_new_node( (void *)s );
            //p_lst = lst_append_list( p_lst, p_new );
        //}
    //}

    //return p_lst;
//}


//void print_string_list( char * msg, lst_node * p_strings, bool append_newline )
//{
    //if ( msg ) printf( "%s: ", msg );
    //printf( "{<%p>", (void *)p_strings );

    //size_t len = lst_length( p_strings );
    //printf( " [%lu]", len );

    //if ( p_strings )
    //{
        //lst_node * p = p_strings;
        //char * fmt = " %s"; // (char *)( " %s\n" ? append_newline : " %s" );

        //while ( p )
        //{
            //printf( fmt, p->p_data );
            //p = p->p_next;
        //}
    //}

    //printf( "}\n" );
//}


//void test_list_free()
//{
    //lst_node * p_lst = init_list_ints( 3, 17 );
    //print_string_list( "init", p_lst, false );
    //printf( " --- --- --- --- --- --- --- --- --- --- ---\n" );

    //lst_node * p_node = lst_get_node( p_lst, 3 );
    //print_string_list( "node 3", p_node, false );

    //p_lst = lst_free_node( p_lst, 3, NULL );
    //print_string_list( "free 3", p_lst, false );

    //p_lst = lst_free_last_node( p_lst, NULL );
    //print_string_list( "free last", p_lst, false );

    //size_t len = lst_length( p_lst );
    //printf( "len: %lu\n", len );

    //p_lst = lst_free_node( p_lst, len-1, NULL );
    //print_string_list( "free len-1", p_lst, false );
    //printf( " --- --- --- --- --- --- --- --- --- --- ---\n" );

    //char * p_str = NULL;
    //p_lst = lst_delete_first_node( p_lst, (void **)&p_str );
    //printf( "data 1st: %s\n", p_str );
    //print_string_list( "del 1st", p_lst, false );
    //free( p_str );
    //p_str = NULL;

    //p_lst = lst_delete_last_node( p_lst, (void **)&p_str );
    //printf( "data last: %s\n", p_str );
    //print_string_list( "del last", p_lst, false );
    //free( p_str );
    //p_str = NULL;

    //p_lst = lst_delete_node( p_lst, 7, (void **)&p_str );
    //printf( "data 7: %s\n", p_str );
    //print_string_list( "del 7", p_lst, false );
    //free( p_str );
    //p_str = NULL;
    //printf( " --- --- --- --- --- --- --- --- --- --- ---\n" );

    //lst_node * p_prepend = init_list_ints( 23, 27 );
    //print_string_list( "to prepend", p_prepend, false );
    //p_lst = lst_prepend_list( p_lst, p_prepend );
    //print_string_list( "prepend", p_lst, false );

    //lst_node * p_append = init_list_ints( 37, 39 );
    //print_string_list( "to append", p_append, false );
    //p_lst = lst_append_list( p_lst, p_append );
    //print_string_list( "append", p_lst, false );

    //lst_node * p_insert = init_list_ints( 42, 45 );
    //print_string_list( "to insert", p_insert, false );
    //p_lst = lst_insert_list( p_lst, 9, p_insert );
    //print_string_list( "insert", p_lst, false );

    //printf( " --- --- --- --- --- --- --- --- --- --- ---\n" );

    //lst_node * p_new = lst_free_first_node( p_lst, NULL );
    //p_lst = NULL;
    //print_string_list( "free 1st", p_new, false );

    //while ( p_new )
    //{
        //p_new = lst_free_first_node( p_new, NULL );
        //print_string_list( NULL, p_new, false );
    //}

    //p_new = NULL;
//}


//int main( int argc, char *argv[] )
//{
    //test_list_free();

    //return 0;
//}

