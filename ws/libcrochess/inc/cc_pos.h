// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_POS_H__
#define __CC_POS_H__


typedef struct CcPos {
    int i;
    int j;
} CcPos;

CcPos cc_pos( int const i, int const j );
CcPos cc_pos_empty();


typedef struct CcPosLink {
    int i;
    int j;

    struct CcPosLink * next;
} CcPosLink;

CcPosLink * cc_pos_link_new( int const i, int const j );

CcPosLink * cc_pos_link_append( CcPosLink * const restrict pos_link,
                                int const i,
                                int const j );

CcPosLink * cc_pos_link_append_or_init( CcPosLink ** const restrict pos_link_io,
                                        int const i,
                                        int const j );

#endif /* __CC_POS_H__ */
