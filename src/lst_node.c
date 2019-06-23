/*
 * Copyright (C) 2005, 2007, 2011, 2019 Mario Mlačak, mmlacak@gmail.com
 * Licensed under 3-clause (modified) BSD license. See LICENSE for details.
 *
 * Generic list implementation.
 * User is responsible for memory allocation, useage and freeing of data.
 * */

#include <stdlib.h>

#include "lst_node.h"


lst_node * lst_new_node( void * p_data )
{
    lst_node * p = (lst_node *)malloc( sizeof( lst_node ) );

    if ( !p ) return NULL;

    p->p_data = p_data;
    p->p_next = NULL;

    return p;
}

size_t lst_length( lst_node * p_list )
{
    if ( !p_list ) return 0;

    lst_node * p = p_list;
    size_t len = 1;

    while ( (p = p->p_next) ) len += 1;

    return len;
}

lst_node * lst_last_node( lst_node * p_list )
{
    if ( !p_list ) return NULL;

    lst_node * p = p_list;

    while ( p->p_next ) p = p->p_next;

    return p;
}

lst_node * lst_get_node( lst_node * p_list, size_t index )
{
    if ( !p_list ) return NULL;

    lst_node * p = p_list;

    for ( size_t i = 0; i < index; ++i )
    {
        if ( p->p_next )
            p = p->p_next;
        else
            return NULL;
    }

    return p;
}

lst_node * lst_prepend_list( lst_node * p_list, lst_node * p_other )
{
    if ( !p_list ) return p_other;
    if ( !p_other ) return p_list;

    lst_node * p = lst_last_node( p_other );

    p->p_next = p_list; // p != NULL <== p_other != NULL

    return p_other;
}

lst_node * lst_append_list( lst_node * p_list, lst_node * p_other )
{
    if ( !p_list ) return p_other;
    if ( !p_other ) return p_list;

    lst_node * p = lst_last_node( p_list );

    p->p_next = p_other; // p != NULL <== p_list != NULL

    return p_list;
}

lst_node * lst_insert_list( lst_node * p_list, size_t index, lst_node * p_other )
{
    if ( !p_list ) return p_other;
    if ( !p_other ) return p_list;

    if ( index == 0 ) return lst_prepend_list( p_list, p_other );

    lst_node * p = lst_get_node( p_list, index-1 );

    if ( !p ) return p_list; // <== index > length

    lst_node * p_last = lst_last_node( p_other );

    p_last->p_next = p->p_next; // p_last != NULL <== p_other != NULL

    p->p_next = p_other;

    return p_list;
}

lst_node * lst_delete_first_node( lst_node * p_list, void ** pp_data )
{
    if ( !p_list ) return NULL;

    // if ( pp_data ) *pp_data = NULL; // no *return* before setting it to a proper value

    lst_node * p = p_list;

    if ( pp_data )
        *pp_data = p->p_data;
    else
        if ( p->p_data ) return p_list;
    // p->p_data = NULL; // will be free'd next anyway

    lst_node * p_next = p->p_next;

    free( (void *)p );

    return p_next;
}

lst_node * lst_delete_last_node( lst_node * p_list, void ** pp_data )
{
    if ( !p_list ) return NULL;

    // if ( pp_data ) *pp_data = NULL; // no *return* before setting it to a proper value

    lst_node * p_prev = NULL;
    lst_node * p = p_list;

    while ( p->p_next )
    {
        p_prev = p;
        p = p->p_next;
    }

    if ( pp_data )
        *pp_data = p->p_data;
    else
        if ( p->p_data ) return p_list;
    // p->p_data = NULL; // will be free'd next anyway

    if ( p_prev ) p_prev->p_next = NULL;

    free( (void *)p );

    return p_list;
}

lst_node * lst_delete_node( lst_node * p_list, size_t index, void ** pp_data )
{
    if ( !p_list ) return NULL;

    if ( index == 0 ) return lst_delete_first_node( p_list, pp_data );

    if ( pp_data ) *pp_data = NULL;

    lst_node * p_prev = NULL;
    lst_node * p = p_list;

    for ( size_t i = 0; i < index; ++i )
    {
        if ( p->p_next )
        {
            p_prev = p;
            p = p->p_next;
        }
        else
            return p_list;
    }

    if ( pp_data )
        *pp_data = p->p_data;
    else
        if ( p->p_data ) return p_list;
    // p->p_data = NULL; // will be free'd next anyway

    p_prev->p_next = p->p_next; // p_prev != NULL <== index > 0 && length > index
    // p->p_next = NULL; // will be free'd next anyway
    free( (void *)p );

    return p_list;
}

bool lst_free_data( void ** pp_data, f_lst_free_data_t f_lst_free_data )
{
    if ( !pp_data ) return false;
    if ( !*pp_data ) return false;

    bool result = true;

    if ( f_lst_free_data )
        result = (*f_lst_free_data)( *pp_data );
    else
        free( *pp_data );

    *pp_data = NULL;

    return result;
}

lst_node * lst_free_first_node( lst_node * p_list, f_lst_free_data_t f_lst_free_data )
{
    if ( !p_list ) return NULL;

    void * p_data = NULL;

    lst_node * p = lst_delete_first_node( p_list, &p_data );

    lst_free_data( &p_data, f_lst_free_data );

    return p;
}

lst_node * lst_free_last_node( lst_node * p_list, f_lst_free_data_t f_lst_free_data )
{
    if ( !p_list ) return NULL;

    void * p_data = NULL;

    lst_node * p = lst_delete_last_node( p_list, &p_data );

    lst_free_data( &p_data, f_lst_free_data );

    return p;
}

lst_node * lst_free_node( lst_node * p_list, size_t index, f_lst_free_data_t f_lst_free_data )
{
    if ( !p_list ) return NULL;

    void * p_data = NULL;

    lst_node * p = lst_delete_node( p_list, index, &p_data );

    lst_free_data( &p_data, f_lst_free_data );

    return p;
}

void lst_free( lst_node * p_list, f_lst_free_data_t f_lst_free_data )
{
    lst_node * p = p_list;

    while ( p ) p = lst_free_first_node( p, f_lst_free_data );
}

