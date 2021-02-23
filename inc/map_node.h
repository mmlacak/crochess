/*
 * Copyright (C) 2019 Mario Mlačak, mmlacak@gmail.com
 * Licensed under 3-clause (modified) BSD license. See LICENSE for details.
 *
 * Generic map implementation, as associative list.
 * User is responsible for memory allocation, use and freeing of data.
 * */

#ifndef __MAP_NODE_H__
#define __MAP_NODE_H__

#include <stdbool.h>

typedef struct map_node
{
    int key;
    void * p_value;

    struct map_node * p_next;
} map_node;

typedef bool (*f_map_free_value_t)( void * p_value );

map_node * map_new_node( int key, void * p_value );
size_t map_length( map_node * p_dict );

map_node * map_get_node( map_node * p_dict, int key );
void ** map_get_value( map_node * p_dict, int key );
bool map_has_key( map_node * p_dict, int key );

map_node * map_prepend_node( map_node * p_dict, int key, void * p_value );

// TODO :: REMOVE !!!
//
map_node * map_update_value( map_node * p_node, void * p_value, void ** pp_old_value );
map_node * map_update_value_free( map_node * p_node, void * p_value, f_map_free_value_t f_map_free_value );
//
// TODO :: REMOVE !!!

map_node * map_update_node( map_node * p_dict, int key, void * p_value, void ** pp_old_value );
map_node * map_update_node_free( map_node * p_dict, int key, void * p_value, f_map_free_value_t f_map_free_value );

map_node * map_merge_node( map_node * p_dict, int key, void * p_value, void ** pp_old_value );
map_node * map_merge_node_free( map_node * p_dict, int key, void * p_value, f_map_free_value_t f_map_free_value );

map_node * map_delete_node( map_node * p_dict, int key, void ** pp_old_value );

bool map_free_value( void ** pp_value, f_map_free_value_t f_map_free_value );
map_node * map_free_node( map_node * p_dict, int key, f_map_free_value_t f_map_free_value );
void map_free( map_node * p_dict, f_map_free_value_t f_map_free_value );

#endif /* __MAP_NODE_H__ */
