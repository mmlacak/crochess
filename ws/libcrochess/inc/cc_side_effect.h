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

CcMaybeBoolEnum cc_side_effect_type_is_terminating( CcPieceEnum piece,
                                                    CcSideEffectTypeEnum sete );


typedef struct CcSideEffect
{
    CcSideEffectTypeEnum type; /* Type of side-effect. */

    union {
        struct {
            CcPieceType piece; /* Piece which has been captured. */
            CcLosingTagType lost_tag; /* Flag, whether captured piece has lost its tag. */
        } capture; /* Capture. */

        struct {
            CcPieceType piece; /* Piece which has been displaced. */
            CcLosingTagType lost_tag; /* Flag, whether displaced piece has lost its tag. */
            CcPos destination; /* Displacement destination. */
        } displacement; /* Displacement, used during light Shaman's trance-journey. */

        struct {
            CcPieceType pawn; /* Pawn which has been captured. */
            CcPos distant; /* Position at which Pawn has been captured. */
        } en_passant; /* En passant. */

        struct {
            CcPieceType rook; /* Rook which castled. */
            CcPos start; /* Starting position of a Rook. */
            CcPos destination; /* Castling Rook destination. */
        } castle; /* Castling. */

        struct {
            CcPieceType captured; /* Piece which has been captured, if any. */
            CcLosingTagType lost_tag; /* Flag, whether captured piece has lost its tag. */
            CcPieceType promoted_to; /* Piece to which Pawn has been promoted. */
        } promote; /* Promotion. */

        struct {
            CcPieceType captured; /* Piece which has been captured, if any. */
            CcLosingTagType lost_tag; /* Flag, whether captured piece has lost its tag. */
        } tag_for_promotion; /* Tag for promotion. */

        struct {
            CcPieceType piece; /* Piece which has been converted. */
            CcLosingTagType lost_tag; /* Flag, if converted piece has lost its tag. */
        } convert; /* Conversion. */

        struct {
            CcPieceType piece; /* Piece which has been "passed-over". */
        } transparency; /* Transparency. */

        struct {
            CcPieceType piece; /* Piece from which currently moving piece has been diverged. */
        } diversion; /* Divergence. */

        struct {
            CcPieceType piece; /* Piece which has been demoted to Pawn. */
            CcLosingTagType lost_tag; /* Flag, whether demoted piece has lost its tag. */
            CcPos distant; /* Position at which piece has been demoted. */
        } demote; /* Demoting. */

        struct {
            CcPieceType piece; /* Piece which has been resurrected. */
            CcPos destination; /* Position at which Wave, Starchild has been resurrected. */
        } resurrect; /* Resurrection. */
    }; /* Union of all substructures used by different step side-effects. */
} CcSideEffect;

CcSideEffect cc_side_effect( CcSideEffectTypeEnum type,
                             CcPieceType piece,
                             CcLosingTagType lost_tag,
                             CcPos start,
                             CcPos destination,
                             CcPieceType promoted_to );

CcPieceType cc_side_effect_piece( CcSideEffect se );

CcPos cc_side_effect_destination( CcSideEffect se );

// TODO :: check side-effect validity --> _cc_path_link_steps_are_valid()

bool cc_side_effect_to_str( CcSideEffect se,
                            cc_char_16 * se_str__o );


CcSideEffect cc_side_effect_none( void );
CcSideEffect cc_side_effect_capture( CcPieceType piece, CcLosingTagType lost_tag );
CcSideEffect cc_side_effect_displacement( CcPieceType piece, CcLosingTagType lost_tag, CcPos destination );
CcSideEffect cc_side_effect_en_passant( CcPieceType pawn, CcPos distant );
CcSideEffect cc_side_effect_castle( CcPieceType rook, CcPos start, CcPos destination );
CcSideEffect cc_side_effect_promote( CcPieceType captured, CcLosingTagType lost_tag, CcPieceType promoted_to );
CcSideEffect cc_side_effect_tag_for_promotion( CcPieceType captured, CcLosingTagType lost_tag );
CcSideEffect cc_side_effect_convert( CcPieceType piece, CcLosingTagType lost_tag );
CcSideEffect cc_side_effect_failed_conversion( void );
CcSideEffect cc_side_effect_transparency( CcPieceType piece );
CcSideEffect cc_side_effect_diversion( CcPieceType piece );
CcSideEffect cc_side_effect_demote( CcPieceType piece, CcLosingTagType lost_tag, CcPos distant );
CcSideEffect cc_side_effect_resurrect( CcPieceType piece, CcPos destination );
CcSideEffect cc_side_effect_failed_resurrection( void );


#endif /* __CC_SIDE_EFFECT_H__ */
