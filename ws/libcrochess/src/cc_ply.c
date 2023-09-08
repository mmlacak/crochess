// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>

#include "cc_defines.h"
#include "cc_ply.h"

/**
    @file cc_ply.c
    @brief Ply, plies linked list functions.
*/


char const * cc_ply_link_symbol( CcPlyLinkEnum ple ) {
    switch ( ple ) {
        case CC_PLE_None : return NULL;
        case CC_PLE_StartingPly : return "";
        case CC_PLE_CascadingPly : return "~";
        case CC_PLE_Teleportation : return "|";
        case CC_PLE_TeleportationReemergence : return "||";
        case CC_PLE_TeleportationOblation : return "|||";
        case CC_PLE_TranceJourney : return "@";
        case CC_PLE_DualTranceJourney : return "@@";
        case CC_PLE_FailedTranceJourney : return "@@@";
        case CC_PLE_PawnSacrifice : return ";;";
        case CC_PLE_SenseJourney : return "\"";
        case CC_PLE_FailedSenseJourney : return "'";

        default : return NULL; } }


CcPly * cc_ply__new( char const * restrict start_an__d,
                     char const * restrict end_an__d,
                     size_t max_len__d,
                     CcPlyLinkEnum link,
                     CcPieceEnum piece,
                     CcLosingTagEnum lost_tag,
                     CcStep ** restrict steps__n ) {
    CcPly * ply__a = malloc( sizeof( CcPly ) );
    if ( !ply__a ) return NULL;

    ply__a->notation = cc_str_copy__new( start_an__d, end_an__d, max_len__d );

    ply__a->link = link;
    ply__a->piece = piece;
    ply__a->lost_tag = lost_tag;

    if ( steps__n ) {
        ply__a->steps = *steps__n;
        *steps__n = NULL; }
    else
        ply__a->steps = NULL;

    ply__a->next = NULL;

    return ply__a; }

CcPly * cc_ply_append( CcPly * restrict plies__io,
                       char const * restrict start_an__d,
                       char const * restrict end_an__d,
                       size_t max_len__d,
                       CcPlyLinkEnum link,
                       CcPieceEnum piece,
                       CcLosingTagEnum lost_tag,
                       CcStep ** restrict steps__n ) {
    if ( !plies__io ) return NULL;

    CcPly * ply__t = cc_ply__new( start_an__d, end_an__d, max_len__d, link, piece, lost_tag, steps__n );
    if ( !ply__t ) return NULL;

    CcPly * p = plies__io;
    while ( p->next ) p = p->next; // rewind
    p->next = ply__t; // append // Ownership transfer --> ply__t is now weak pointer.

    return ply__t; }

CcPly * cc_ply_append_if( CcPly ** restrict plies__io,
                          char const * restrict start_an__d,
                          char const * restrict end_an__d,
                          size_t max_len__d,
                          CcPlyLinkEnum link,
                          CcPieceEnum piece,
                          CcLosingTagEnum lost_tag,
                          CcStep ** restrict steps__n ) {
    if ( !plies__io ) return NULL;

    CcPly * ply__w = NULL;

    if ( !*plies__io )
        *plies__io = ply__w = cc_ply__new( start_an__d,
                                           end_an__d,
                                           max_len__d,
                                           link,
                                           piece,
                                           lost_tag,
                                           steps__n );
    else
        ply__w = cc_ply_append( *plies__io,
                                start_an__d,
                                end_an__d,
                                max_len__d,
                                link,
                                piece,
                                lost_tag,
                                steps__n );

    return ply__w; }

CcPly * cc_ply_duplicate_all__new( CcPly * restrict plies ) {
    if ( !plies ) return NULL;

    CcPly * ply__a = NULL;
    CcPly * from = plies;

    do {
        CcStep * steps__t = cc_step_duplicate_all__new( from->steps );
        if ( !steps__t ) {
            cc_ply_free_all( &ply__a );
            return NULL; }

        CcPly * ply__w = cc_ply_append_if( &ply__a,
                                           from->notation,
                                           NULL,
                                           CC_MAX_LEN_ZERO_TERMINATED,
                                           from->link,
                                           from->piece,
                                           from->lost_tag,
                                           &steps__t );
        if ( !ply__w ) {
            cc_step_free_all( &steps__t ); // Failed append --> ownership not transferred ...
            cc_ply_free_all( &ply__a );
            return NULL; }

        from = from->next; }
    while ( from );

    return ply__a; }

CcPly * cc_ply_extend( CcPly ** restrict plies__io,
                       CcPly ** restrict plies__n ) {
    if ( !plies__io ) return NULL;
    if ( !*plies__io ) return NULL;

    if ( !plies__n ) return NULL;
    if ( !*plies__n ) return NULL;

    CcPly * last = *plies__io;
    while ( last->next ) last = last->next;

    // Ownership transfer.
    last->next = *plies__n;
    *plies__n = NULL;

    return last->next; }

CcPly * cc_ply_extend_if( CcPly ** restrict plies__iod,
                          CcPly ** restrict plies__n ) {
    if ( !plies__iod ) return NULL;
    if ( !plies__n ) return NULL;

    if ( !*plies__n ) return *plies__iod;

    if ( !*plies__iod ) {
        // Ownership transfer.
        *plies__iod = *plies__n;
        *plies__n = NULL;

        return *plies__iod; }

    return cc_ply_extend( plies__iod, plies__n ); }

bool cc_ply_free_all( CcPly ** restrict plies__f ) {
    if ( !plies__f ) return false;
    if ( !*plies__f ) return true;

    bool result = true;
    CcPly * ply = *plies__f;

    while ( ply ) {
        CC_FREE( ply->notation );

        CcStep ** steps = &( ply->steps );
        result = cc_step_free_all( steps ) && result;

        CcPly * tmp = ply->next;
        CC_FREE( ply );
        ply = tmp; }

    *plies__f = NULL;
    return result; }


bool cc_ply_contains_side_effects( CcPly * restrict ply ) {
    if ( !ply ) return false;
    if ( !ply->steps ) return false;

    CcStep * s = ply->steps;
    while ( s->next ) {
        if ( s->side_effect.type != CC_SEE_None ) return true;
        s = s->next; }

    return false; }

CcPieceEnum cc_ply_last_active_piece( CcPly * restrict plies,
                                      CcPly * restrict ply__d ) {
    if ( !plies ) return CC_PE_None;

    if ( plies == ply__d ) // First ply in a linked list.
        return CC_PIECE_IS_ACTIVE( plies->piece ) ? plies->piece
                                                  : CC_PE_None;

    // <!> Shadows issue if ply is not contained in plies.
    //
    // if ( ply__d && CC_PIECE_IS_ACTIVE( ply__d->piece ) )
    //     return ply__d->piece;

    CcPieceEnum last_active_piece = CC_PE_None;
    bool ply_encountered = ( !ply__d );
    CcPly * p = plies;

    while ( p ) {
        if ( CC_PIECE_IS_ACTIVE( p->piece ) )
            last_active_piece = p->piece;

        if ( p == ply__d ) {
            ply_encountered = true;
            break; }

        p = p->next; }

    return ply_encountered ? last_active_piece : CC_PE_None; }

char * cc_ply_all_to_short_string__new( CcPly * restrict plies ) {
    if ( !plies ) return NULL;

    //
    // Count plies, steps.

    CcPly * p_count = plies;
    size_t count_plies = 0;
    size_t count_steps = 0;

    while ( p_count ) {
        ++count_plies;
        count_steps += cc_step_count( p_count->steps );

        p_count = p_count->next; }

    //
    // Calc max string size, allocate.

    size_t step_size = CC_MAX_LEN_CHAR_8 + CC_MAX_LEN_CHAR_16 + 2;
                       // CC_MAX_LEN_CHAR_8, for position
                       // + CC_MAX_LEN_CHAR_16, for side-effect
                       // + 2, for step links, e.g. ".." before step

    size_t unused_size = ( count_plies * CC_MAX_LEN_PLY_LINK_SYMBOL )
                       + ( count_steps * step_size )
                       + 1; // +1, for '\0'

    char * plies_str__a = malloc( unused_size );
    if ( !plies_str__a ) return NULL;

    // **Must** be zero-terminated!
    if ( !cc_str_clear( plies_str__a, unused_size ) ) /* Using size (instead of length) here is ok! */ {
        CC_FREE( plies_str__a );
        return NULL; }

    //
    // Collect ply string, append to result.

    CcPly * p = plies;
    char * s = plies_str__a;

    while ( p ) {
        // Append ply link symbol.

        char const * pl = cc_ply_link_symbol( p->link );
        char * end_ple = cc_str_append_into( s, unused_size, pl, CC_MAX_LEN_PLY_LINK_SYMBOL );

        if ( !end_ple ) {
            CC_FREE( plies_str__a );
            return NULL; }

        unused_size -= ( end_ple - s );
        s = end_ple;

        // Append piece symbol, lost tag.

        char piece_symbol = cc_piece_symbol( p->piece );
        *s++ = piece_symbol;

        char const * lte_str = cc_losing_tag_as_string( p->lost_tag );
        char * end_lte = cc_str_append_into( s, unused_size, lte_str, CC_MAX_LEN_LOSING_TAG );

        if ( !lte_str ) {
            CC_FREE( plies_str__a );
            return NULL; }

        unused_size -= ( end_lte - s );
        s = end_lte;

        // Append steps.

        char * steps_str__a = cc_step_all_to_short_string__new( p->steps );

        if ( !steps_str__a ) {
            CC_FREE( plies_str__a );
            return NULL; }

        char * end_steps = cc_str_append_into( s, unused_size, steps_str__a, CC_MAX_LEN_ZERO_TERMINATED );

        if ( !end_steps ) {
            CC_FREE( steps_str__a );
            CC_FREE( plies_str__a );
            return NULL; }

        unused_size -= ( end_steps - s );
        s = end_steps;

        CC_FREE( steps_str__a );
        p = p->next; }

    return plies_str__a; }
