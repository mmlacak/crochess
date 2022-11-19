// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_SIDE_EFFECT_H__
#define __CC_SIDE_EFFECT_H__

#include <stddef.h>

#include "cc_str_utils.h"

#include "cc_piece.h"
#include "cc_tag.h"
#include "cc_pos.h"

/**
    @file cc_side_effect.h
    @brief Side-effects enum, functions.
*/


/**
    Step side-effect enumeration.
*/
typedef enum CcSideEffectEnum
{
    CC_SEE_None, /**< No side effects. */
    CC_SEE_Capture, /**< Corresponds to `*`. */
    CC_SEE_Displacement, /**< Corresponds to `<`. */
    CC_SEE_EnPassant, /**< Corresponds to `:`. */
    CC_SEE_Castle, /**< Corresponds to `&`. */
    CC_SEE_Promotion, /**< Corresponds to `=`, ``. */
    CC_SEE_TagForPromotion, /**< Corresponds to `=`. */
    CC_SEE_Conversion, /**< Corresponds to `%`. */
    CC_SEE_FailedConversion, /**< Corresponds to `%%`. */
    CC_SEE_DemoteToPawn, /**< Corresponds to `>`. */
    CC_SEE_Resurrection, /**< Corresponds to `$`. */
    CC_SEE_FailedResurrection, /**< Corresponds to `$$`. */
} CcSideEffectEnum;

/**
    Macro to inline check if given side-effect enum is castling.

    @param see A side effect enumeration, i.e. one of `CcSideEffectEnum` values.

    @see CcSideEffectEnum

    @return `true` if castling, `false` otherwise.
*/
#define CC_SIDE_EFFECT_ENUM_IS_CASTLING(see) ( (see) == CC_SEE_Castle )

/** TODO :: DOCS . */
char const * cc_side_effect_symbol( CcSideEffectEnum see );


/**
    Step side-effect structure.
*/
typedef struct CcSideEffect
{
    CcSideEffectEnum type; /**< Type of side-effect. */

    union
    {
        struct  { CcPieceEnum piece; /**< Piece which has been captured. */
                  CcTagEnum lost_tag; /**< Flag, whether captured piece has lost its tag. */
                } capture; /**< Capture. */

        struct  { CcPieceEnum piece; /**< Piece which has been displaced. */
                  CcTagEnum lost_tag; /**< Flag, whether displaced piece has lost its tag. */
                  CcPos destination; /**< Displacement destination. */
                } displacement; /**< Displacement, used during light Shaman's trance-journey. */

        struct  { CcPieceEnum pawn; /**< Pawn which has been captured. */
                  CcPos distant; /**< Position at which Pawn has been captured. */
                } en_passant; /**< En passant. */

        struct  { CcPieceEnum rook; /**< Rook which castled. */
                  CcPos start; /**< Starting position of a Rook. */
                  CcPos destination; /**< Castling Rook destination. */
                } castle; /**< Castling. */

        struct  { CcPieceEnum piece; /**< Piece to which Pawn has been promoted. */
                } promote; /**< Promotion. */

        struct  { CcPieceEnum piece; /**< Piece which has been converted. */
                  CcTagEnum lost_tag; /**< Flag, if converted piece has lost its tag. */
                } convert; /**< Conversion. */

        struct  { CcPieceEnum piece; /**< Piece which has been demoted to Pawn. */
                  CcTagEnum lost_tag; /**< Flag, whether demoted piece has lost its tag. */
                  CcPos distant; /**< Position at which piece has been demoted. */
                } demote; /**< Demoting. */

        struct  { CcPieceEnum piece; /**< Piece which has been resurrected. */
                  CcPos destination; /**< Position at which Wave, Starchild has been resurrected. */
                } resurrect; /**< Resurrection. */
    }; /**< Union of all substructures used by different step side-effects. */
} CcSideEffect;

/**
    Function returning step side-effect structure.

    @param type Side-effect enum.
    @param piece A piece.
    @param lost_tag Flag, if Pawn has lost its delayed promotion tag.
    @param start Starting position.
    @param destination Destination position.

    @note
    Side-effect structure is returned as a value on the stack,
    not as a pointer to a newly allocated memory block on the heap.

    @return Step side-effect structure.
*/
CcSideEffect cc_side_effect( CcSideEffectEnum type,
                             CcPieceEnum piece,
                             CcTagEnum lost_tag,
                             CcPos start,
                             CcPos destination );

/**
    Function checks if a given side-effect is valid.

    @param see A side-effect.
    @param board_size A chessboard size.

    @return `true` if valid, `false` otherwise.
*/
bool cc_side_effect_is_valid( CcSideEffect see, unsigned int board_size );

/**
    Function returns piece affected by a side-effect.

    @param se Side-effect.

    @return A piece.
*/
CcPieceEnum cc_side_effect_piece( CcSideEffect se );

/**
    Function returns position affected by a side-effect.

    @param se Side-effect.

    @return A position.
*/
CcPos cc_side_effect_destination( CcSideEffect se );

/** @defgroup side_effect_convenience The side-effect conveniences
 *  The side-effect convenience functions are meant to be used instead of `cc_side_effect()`.

    They have minimal set of arguments required by the type of a side-effect,
    otherwise they behave exactly as their generic progenitor.

    @see cc_side_effect()
 *  @{
 */

CcSideEffect cc_side_effect_none();
CcSideEffect cc_side_effect_capture( CcPieceEnum piece, CcTagEnum lost_tag );
CcSideEffect cc_side_effect_displacement( CcPieceEnum piece, CcTagEnum lost_tag, CcPos destination );
CcSideEffect cc_side_effect_en_passant( CcPieceEnum pawn, CcPos distant );
CcSideEffect cc_side_effect_castle( CcPieceEnum rook, CcPos start, CcPos destination );
CcSideEffect cc_side_effect_promote( CcPieceEnum piece );
CcSideEffect cc_side_effect_tag_for_promotion();
CcSideEffect cc_side_effect_convert( CcPieceEnum piece, CcTagEnum lost_tag );
CcSideEffect cc_side_effect_failed_conversion();
CcSideEffect cc_side_effect_demote( CcPieceEnum piece, CcTagEnum lost_tag, CcPos distant );
CcSideEffect cc_side_effect_resurrect( CcPieceEnum piece, CcPos destination );
CcSideEffect cc_side_effect_failed_resurrection();

/** @} */ // end of ply_convenience_new


/** TODO :: DOCS . */
bool cc_side_effect_to_short_str( CcSideEffect se,
                                  cc_char_16 * restrict see_str__o );


#endif /* __CC_SIDE_EFFECT_H__ */
