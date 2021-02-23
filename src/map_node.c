/*
 * Copyright (C) 2019 Mario Mlačak, mmlacak@gmail.com
 * Licensed under 3-clause (modified) BSD license. See LICENSE for details.
 *
 * Generic map implementation, as associative list.
 * User is responsible for memory allocation, useage and freeing of data.
 * */

#include <stdlib.h>

#include "map_node.h"


map_node * map_new_node( int key, void * p_value )
{
    map_node * p = (map_node *)malloc( sizeof( map_node ) );

    if ( !p ) return NULL;

    p->key = key;
    p->p_value = p_value;
    p->p_next = NULL;

    return p;
}

size_t map_length( map_node * p_dict )
{
    if ( !p_dict ) return 0;

    map_node * p = p_dict;
    size_t len = 1;

    while ( (p = p->p_next) ) len += 1;

    return len;
}

map_node * map_get_node( map_node * p_dict, int key )
{
    if ( !p_dict ) return NULL;

    map_node * p = p_dict;

    while ( p )
    {
        if ( p->key == key ) return p;
        p = p->p_next;
    }

    return NULL;
}

void ** map_get_value( map_node * p_dict, int key )
{
    if ( !p_dict ) return NULL;

    map_node * p = map_get_node( p_dict, key );

    if ( !p ) return NULL;

    return &p->p_value;
}

bool map_has_key( map_node * p_dict, int key )
{
    if ( !p_dict ) return NULL;

    map_node * p = map_get_node( p_dict, key );

    return (bool)( p );
}

map_node * map_prepend_node( map_node * p_dict, int key, void * p_value )
{
    if ( !p_dict ) return NULL;

    if ( map_has_key( p_dict, key ) ) return p_dict;

    map_node * p = map_new_node( key, p_value );

    if ( !p ) return p_dict;

    p->p_next = p_dict;

    return p;
}

map_node * map_update_value( map_node * p_node, void * p_value, void ** pp_old_value )
{
    if ( !p_node ) return NULL;

    if ( pp_old_value )
        *pp_old_value = p_node->p_value;
    else
        if ( p_node->p_value && p_node->p_value != p_value ) return p_node;

    p_node->p_value = p_value;

    return p_node;
}

map_node * map_update_value_free( map_node * p_node, void * p_value, f_map_free_value_t f_map_free_value )
{
    if ( !p_node ) return NULL;

    void * p_old_value = NULL;

    map_update_value( p_node, p_value, &p_old_value );

    map_free_value( &p_old_value, f_map_free_value );

    return p_node;
}

map_node * map_update_node( map_node * p_dict, int key, void * p_value, void ** pp_old_value )
{
    if ( !p_dict ) return NULL;

    map_node * p = map_get_node( p_dict, key );

    if ( p ) map_update_value( p, p_value, pp_old_value );

    return p_dict;
}

map_node * map_update_node_free( map_node * p_dict, int key, void * p_value, f_map_free_value_t f_map_free_value )
{
    if ( !p_dict ) return NULL;

    map_node * p = map_get_node( p_dict, key );

    if ( p ) map_update_value_free( p, p_value, f_map_free_value );

    return p_dict;
}

map_node * map_merge_node( map_node * p_dict, int key, void * p_value, void ** pp_old_value )
{
    if ( !p_dict ) return NULL;

    map_node * p_node = map_get_node( p_dict, key );

    if ( p_node )
        map_update_value( p_node, p_value, pp_old_value );
    else
        return map_prepend_node( p_dict, key, p_value );

    return p_dict;
}

map_node * map_merge_node_free( map_node * p_dict, int key, void * p_value, f_map_free_value_t f_map_free_value )
{
    if ( !p_dict ) return NULL;

    map_node * p_node = map_get_node( p_dict, key );

    if ( p_node )
        map_update_value_free( p_node, p_value, f_map_free_value );
    else
        return map_prepend_node( p_dict, key, p_value );

    return p_dict;
}

map_node * map_delete_node( map_node * p_dict, int key, void ** pp_old_value )
{
    if ( !p_dict ) return NULL;

    map_node * p_prev = NULL;
    map_node * p = p_dict;
    bool found = false;

    while ( p )
    {
        if ( p->key == key )
        {
            found = true;
            break;
        }

        p_prev = p;
        p = p->p_next;
    }

    if ( !found ) return p_dict;

    if ( pp_old_value )
        *pp_old_value = p->p_value; // transfer ownership
    else
        if ( p->p_value ) return p_dict;
    // p->p_value = NULL; // p will be free'd next anyway

    if ( p_prev ) p_prev->p_next = p->p_next;
    // p->p_next = NULL; // p will be free'd next anyway

    free( (void *)p );

    return p_dict;
}

bool map_free_value( void ** pp_value, f_map_free_value_t f_map_free_value )
{
    if ( !pp_value ) return false;
    if ( !*pp_value ) return false;

    bool result = true;

    if ( f_map_free_value )
        result = (*f_map_free_value)( *pp_value );
    else
        free( *pp_value );

    *pp_value = NULL;

    return result;
}

map_node * map_free_node( map_node * p_dict, int key, f_map_free_value_t f_map_free_value )
{
    if ( !p_dict ) return NULL;

    void * p_old_value = NULL;

    map_node * p = map_delete_node( p_dict, key, &p_old_value );

    map_free_value( &p_old_value, f_map_free_value );

    return p;
}

void map_free( map_node * p_dict, f_map_free_value_t f_map_free_value )
{
    if ( !p_dict ) return;

    map_node * p = p_dict;
    map_node * p_next = p->p_next;

    while ( p )
    {
        map_free_value( &p->p_value, f_map_free_value );

        free( p );

        p = p_next;
        p_next = p->p_next;
    }
}

