/*
 * Copyright (C) 2005, 2007, 2011, 2019 Mario Mlačak, mmlacak@gmail.com
 * Licensed under 3-clause (modified) BSD license. See LICENSE for details.
 *
 * Generic list implementation.
 * User is responsible for memory allocation, use and freeing of data.
 * */

#ifndef __LST_NODE_H__
#define __LST_NODE_H__

#include <stdbool.h>

typedef struct lst_node
{
    void * p_data;

    struct lst_node * p_next;
} lst_node;

typedef bool (*f_lst_free_data_t)( void * p_data );

lst_node * lst_new_node( void * p_data );
size_t lst_length( lst_node * p_list );

lst_node * lst_last_node( lst_node * p_list );
lst_node * lst_get_node( lst_node * p_list, size_t index );

// There shouldn't be common sublist between the 2 lists --> great for making cyclic structures!
lst_node * lst_prepend_list( lst_node * p_list, lst_node * p_other );
lst_node * lst_append_list( lst_node * p_list, lst_node * p_other );
lst_node * lst_insert_list( lst_node * p_list, size_t index, lst_node * p_other );

lst_node * lst_delete_first_node( lst_node * p_list, void ** pp_old_data );
lst_node * lst_delete_last_node( lst_node * p_list, void ** pp_old_data );
lst_node * lst_delete_node( lst_node * p_list, size_t index, void ** pp_old_data );

bool lst_free_data( void ** pp_data, f_lst_free_data_t f_lst_free_data );
lst_node * lst_free_first_node( lst_node * p_list, f_lst_free_data_t f_lst_free_data );
lst_node * lst_free_last_node( lst_node * p_list, f_lst_free_data_t f_lst_free_data );
lst_node * lst_free_node( lst_node * p_list, size_t index, f_lst_free_data_t f_lst_free_data );
void lst_free( lst_node * p_list, f_lst_free_data_t f_lst_free_data );

#endif /* __LST_NODE_H__ */
