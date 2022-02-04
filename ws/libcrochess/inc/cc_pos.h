// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_POS_H__
#define __CC_POS_H__

#include <stdbool.h>

/**
    @file cc_pos.h
    @brief Position, linked list of positions.
*/


/**
    Macro to allocate new position link, with given coordinates.

    @param i File, horizontal coordinate.
    @param j Rank, vertical coordinate.

    @return Pointer to a newly allocated linked position if successful, `NULL` otherwise.

    @see cc_pos_link__new()
*/
#define CC_POS_LINK_NEW(i,j) cc_pos_link__new( cc_pos( (i), (j) ) )

/**
    Macro to append a newly allocated new position link, with given coordinates.

    @param pos_link__io a position linked list, to be appended.
    @param i File, horizontal coordinate.
    @param j Rank, vertical coordinate.

    @return A weak pointer to a newly allocated linked position if successful, `NULL` otherwise.

    @see cc_pos_link_append()
*/
#define CC_POS_LINK_APPEND(pos_link__io,i,j) cc_pos_link_append( (pos_link__io), cc_pos( (i), (j) ) )

/**
    Macro to initialize or append a position linked list, with given coordinates.

    @param pos_link__io a position linked list, to be appended.
    @param i File, horizontal coordinate.
    @param j Rank, vertical coordinate.

    @return A weak pointer to a newly allocated linked position if successful, `NULL` otherwise.

    @see cc_pos_link_append_or_init()
*/
#define CC_POS_LINK_APPEND_OR_INIT(pos_link__io,i,j) cc_pos_link_append_or_init( (pos_link__io), cc_pos( (i), (j) ) )


/**
    Structure holding a position, either absolute or relative,
    i.e. either a location or a step.
*/
typedef struct CcPos {
    int i; /**< File, horizontal coordinate. */
    int j; /**< Rank, vertical coordinate. */
} CcPos;

/**
    Function returns a position.

    @param i File, horizontal coordinate.
    @param j Rank, vertical coordinate.

    @return Positions with a given coordinates.
*/
CcPos cc_pos( int i, int j );

/**
    Function returns invalid position.

    @note
    Invalid position is one way past normal off-board calculation.

    @return An invalid position.
*/
CcPos cc_pos_invalid();

/**
    Function adds step to a position.

    @param pos A position to add to.
    @param step A step to be added.

    @return A position with added step.

*/
CcPos cc_pos_add( CcPos pos, CcPos step );

/**
    Function subtracts step from aposition.

    @param pos A position to subtract from.
    @param step A step to be subtracted.

    @return A position with subtracted step.

*/
CcPos cc_pos_subtract( CcPos pos, CcPos step );

/**
    Function checks if two positions are the same.

    @param pos_1 A position.
    @param pos_2 An other position.

    @return `true` if positions are the same, `false` otherwise.
*/
bool cc_pos_is_equal( CcPos pos_1, CcPos pos_2 );

/**
    Function checks if position is valid.

    @param pos A position.

    @see cc_pos_invalid()

    @return `true` if position is valid, `false` otherwise.
*/
bool cc_pos_is_valid( CcPos pos );


/**
    Structure forming a linked list of positions.
*/
typedef struct CcPosLink {
    CcPos pos; /**< A position. */
    struct CcPosLink * next; /**< Link to a next position. */
} CcPosLink;

/**
    Function allocates a new linked position.

    @param pos A position.

    @return Pointer to a newly allocated linked position if successful, `NULL` otherwise.
*/
CcPosLink * cc_pos_link__new( CcPos pos );

/**
    Function appends a newly allocated linked position to a given linked list.

    @param pos_link__io _Input/output_ parameter, linked list.
    @param pos A position.

    @return A weak pointer to a newly allocated linked position if successful, `NULL` otherwise.
*/
CcPosLink * cc_pos_link_append( CcPosLink * restrict pos_link__io,
                                CcPos pos );

/**
    Allocates a new linked position, appends it to a linked list.

    @param pos_link__io _Input/output_ parameter, linked list, can be `NULL`.
    @param pos A position.

    @note
    Linked list `*pos_link__io` can be `NULL`, a linked position will still be
    allocated, and returned.

    @note
    If linked list `*pos_link__io` is `NULL`, it will be initialized,
    with a newly allocated linked position as its first element.

    @return A weak pointer to a newly allocated linked position if successful, `NULL` otherwise.
*/
CcPosLink * cc_pos_link_append_or_init( CcPosLink ** restrict pos_link__io,
                                        CcPos pos );

/**
    Frees all positions in a linked list.

    @param pos_link__f Linked list of positions.

    @return `true` if successful, `false` otherwise.
*/
bool cc_pos_link_free_all( CcPosLink ** restrict pos_link__f );


#endif /* __CC_POS_H__ */
