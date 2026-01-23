// Copyright (c) 2021, 2022, 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_SIDE_EFFECT_H__
#define __CC_SIDE_EFFECT_H__

#include <stddef.h>

#include "cc_defines.h"
#include "cc_str_utils.h"

#include "cc_piece.h"
#include "cc_tag.h"
#include "cc_pos.h"

//
// Side-effect.

#define CC_SIDE_EFFECT_TYPE_IS_ENUMERATOR(sete) ( ( CC_SETE_None <= (sete) ) && ( (sete) <= CC_SETE_FailedResurrection ) )

#define CC_SIDE_EFFECT_TYPE_IS_VALID(sete) ( ( CC_SETE_None < (sete) ) && ( (sete) <= CC_SETE_FailedResurrection ) )

// All side-effects which must be followed by another step.
#define CC_SIDE_EFFECT_TYPE_DOES_NOT_TERMINATE_PLY(sete) ( ( (sete) == CC_SETE_Transparency ) \
                                                        || ( (sete) == CC_SETE_Divergence ) )

// All side-effects which may end a ply.
#define CC_SIDE_EFFECT_TYPE_MAY_TERMINATE_PLY(sete) ( ( (sete) == CC_SETE_Capture ) \
                                                   || ( (sete) == CC_SETE_Displacement ) )

// All side-effects which always ends a ply.
#define CC_SIDE_EFFECT_TYPE_TERMINATES_PLY(sete) ( ( (sete) == CC_SETE_EnPassant ) \
                                                || ( (sete) == CC_SETE_Castle ) \
                                                || ( (sete) == CC_SETE_Promotion ) \
                                                || ( (sete) == CC_SETE_TagForPromotion ) \
                                                || ( (sete) == CC_SETE_Conversion ) \
                                                || ( (sete) == CC_SETE_FailedConversion ) \
                                                || ( (sete) == CC_SETE_DemoteToPawn ) \
                                                || ( (sete) == CC_SETE_Resurrection ) \
                                                || ( (sete) == CC_SETE_ResurrectingOpponent ) \
                                                || ( (sete) == CC_SETE_FailedResurrection ) )

// All side-effects with which a ply can end.
#define CC_SIDE_EFFECT_TYPE_CAN_TERMINATE_PLY(sete) ( CC_SIDE_EFFECT_TYPE_MAY_TERMINATE_PLY(sete) \
                                                   || CC_SIDE_EFFECT_TYPE_TERMINATES_PLY(sete) )

#define CC_MAX_LEN_SIDE_EFFECT_TYPE_SYMBOL (3)

typedef enum CcSideEffectTypeEnum {
    CC_SETE_None, /* No side effects. */
    CC_SETE_Capture, /* Corresponds to `*`. */
    CC_SETE_Displacement, /* Corresponds to `<`. */
    CC_SETE_EnPassant, /* Corresponds to `:`. */
    CC_SETE_Castle, /* Corresponds to `&`. */
    CC_SETE_Promotion, /* Corresponds to `=`, ``. */
    CC_SETE_TagForPromotion, /* Corresponds to `=`. */
    CC_SETE_Conversion, /* Corresponds to `%`. */
    CC_SETE_FailedConversion, /* Corresponds to `%%`. */
    CC_SETE_Transparency, /* Corresponds to `^`. */
    CC_SETE_Divergence, /* Corresponds to `/`. */
    CC_SETE_DemoteToPawn, /* Corresponds to `>`. */
    CC_SETE_Resurrection, /* Corresponds to `$`. */
    CC_SETE_ResurrectingOpponent, /* Corresponds to `$$`. */
    CC_SETE_FailedResurrection, /* Corresponds to `$$$`. */
} CcSideEffectTypeEnum;

char const * cc_side_effect_type_symbol( CcSideEffectTypeEnum sete );

CcMaybeBoolEnum cc_side_effect_type_is_terminating( CcPieceTagType piece,
                                                    CcSideEffectTypeEnum sete );


typedef struct CcSideEffect
{
    CcSideEffectTypeEnum type; /* Type of side-effect. */

    union {
        struct {
            CcPieceTagType piece; /* Piece which has been captured, and its (lost) tag. */
        } capture; /* Capture. */

        struct {
            CcPieceTagType piece; /* Piece which has been displaced, and its (lost) tag. */
            CcPos destination; /* Displacement destination. */
        } displacement; /* Displacement, used during light Shaman's trance-journey. */

        struct {
            CcPieceTagType private; /* Private which has been captured. */
            CcPos distant; /* Position at which Pawn has been captured. */
        } en_passant; /* En passant. */

        struct {
            CcPieceTagType rook; /* Rook which castled. */
            CcPos start; /* Starting position of a Rook. */
            CcPos destination; /* Castling Rook destination. */
        } castle; /* Castling. */

        struct {
            CcPieceTagType captured; /* Piece which has been captured, if any, and its (lost) tag. */
            CcPieceTagType promoted_to; /* Piece to which Pawn has been promoted. */
        } promote; /* Promotion. */

        struct {
            CcPieceTagType captured; /* Piece which has been captured, if any, and its (lost) tag. */
        } tag_for_promotion; /* Tag for promotion. */

        struct {
            CcPieceTagType piece; /* Piece which has been converted, and its (lost) tag. */
        } convert; /* Conversion. */

        struct {
            CcPieceTagType piece; /* Piece which has been "passed-over". */
        } transparency; /* Transparency. */

        struct {
            CcPieceTagType piece; /* Piece from which currently moving piece has been diverged. */
        } diversion; /* Divergence. */

        struct {
            CcPieceTagType piece; /* Piece which has been demoted to Pawn, and its (lost) tag. */
            CcPos distant; /* Position at which piece has been demoted. */
        } demote; /* Demoting. */

        struct {
            CcPieceTagType piece; /* Piece which has been resurrected. */
            CcPos destination; /* Position at which Wave, Starchild has been resurrected. */
        } resurrect; /* Resurrection. */
    }; /* Union of all substructures used by different step side-effects. */
} CcSideEffect;

CcSideEffect cc_side_effect( CcSideEffectTypeEnum type,
                             CcPieceTagType piece,
                             CcPos start,
                             CcPos destination,
                             CcPieceTagType promoted_to );

CcPieceTagType cc_side_effect_piece( CcSideEffect se );

CcPos cc_side_effect_destination( CcSideEffect se );

bool cc_side_effect_has_destination( CcSideEffect se );

bool cc_side_effect_is_valid( CcSideEffect se, bool include_none );

// TODO :: DOCS
CcMaybeBoolEnum cc_side_effect_is_congruent( CcSideEffect se_1, CcSideEffect se_2 );

bool cc_side_effect_to_str( CcSideEffect se,
                            cc_char_16 * se_str__o );

// Convenience functions.
CcSideEffect cc_side_effect_none( void );
CcSideEffect cc_side_effect_capture( CcPieceTagType piece );
CcSideEffect cc_side_effect_displacement( CcPieceTagType piece, CcPos destination );
CcSideEffect cc_side_effect_en_passant( CcPieceTagType private, CcPos distant );
CcSideEffect cc_side_effect_castle( CcPieceTagType rook, CcPos start, CcPos destination );
CcSideEffect cc_side_effect_promote( CcPieceTagType captured, CcPieceTagType promoted_to );
CcSideEffect cc_side_effect_tag_for_promotion( CcPieceTagType captured );
CcSideEffect cc_side_effect_convert( CcPieceTagType piece );
CcSideEffect cc_side_effect_failed_conversion( void );
CcSideEffect cc_side_effect_transparency( CcPieceTagType piece );
CcSideEffect cc_side_effect_diversion( CcPieceTagType piece );
CcSideEffect cc_side_effect_demote( CcPieceTagType piece, CcPos distant );
CcSideEffect cc_side_effect_resurrect( CcPieceTagType piece, CcPos destination );
CcSideEffect cc_side_effect_failed_resurrection( void );

//
// Side-effect linked list.

typedef struct CcSideEffectLink {
    CcSideEffect side_effect;
    struct CcSideEffectLink * next;
} CcSideEffectLink;

CcSideEffectLink * cc_side_effect_link__new( CcSideEffect side_effect );

CcSideEffectLink * cc_side_effect_link_append( CcSideEffectLink ** se_link__iod_a,
                                               CcSideEffect side_effect );

CcSideEffectLink * cc_side_effect_link_duplicate_all__new( CcSideEffectLink * se_link );

CcSideEffectLink * cc_side_effect_link_extend( CcSideEffectLink ** se_link__iod_a,
                                               CcSideEffectLink ** se_link__n );

bool cc_side_effect_link_free_all( CcSideEffectLink ** se_link__f );

size_t cc_side_effect_link_len( CcSideEffectLink * se_link );

char * cc_side_effect_link_to_string__new( CcSideEffectLink * se_link );


#endif /* __CC_SIDE_EFFECT_H__ */
