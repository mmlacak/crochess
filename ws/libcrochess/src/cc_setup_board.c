// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>

#include "cc_defines.h"
#include "cc_piece.h"
#include "cc_variant.h"
#include "cc_setup_board.h"


// a b c D e F g h i J k L m n o p q r s t u V w X Y Z

static const CcPieceTagType t = CC_PTE_DimStar;

static const CcPieceTagType i = CC_PTE_DarkStarchild;
static const CcPieceTagType h = CC_PTE_DarkShaman;
static const CcPieceTagType s = CC_PTE_DarkSerpent;
static const CcPieceTagType g = CC_PTE_DarkGrenadier_CanRush; // CC_PTE_DarkGrenadier;
static const CcPieceTagType o = CC_PTE_DarkScout_CanRush; // CC_PTE_DarkScout;
static const CcPieceTagType c = CC_PTE_DarkCentaur;
static const CcPieceTagType w = CC_PTE_DarkWave;
static const CcPieceTagType u = CC_PTE_DarkUnicorn;
static const CcPieceTagType a = CC_PTE_DarkPyramid;
static const CcPieceTagType e = CC_PTE_DarkPegasus;
static const CcPieceTagType k = CC_PTE_DarkKing_CanCastle; // CC_PTE_DarkKing;
static const CcPieceTagType q = CC_PTE_DarkQueen;
static const CcPieceTagType r = CC_PTE_DarkRook_CanCastle; // CC_PTE_DarkRook;
static const CcPieceTagType b = CC_PTE_DarkBishop;
static const CcPieceTagType n = CC_PTE_DarkKnight;
static const CcPieceTagType p = CC_PTE_DarkPawn_CanRush; // CC_PTE_DarkPawn;

static const CcPieceTagType x = CC_PTE_None;

static const CcPieceTagType P = CC_PTE_LightPawn_CanRush; // CC_PTE_LightPawn;
static const CcPieceTagType N = CC_PTE_LightKnight;
static const CcPieceTagType B = CC_PTE_LightBishop;
static const CcPieceTagType R = CC_PTE_LightRook_CanCastle; // CC_PTE_LightRook;
static const CcPieceTagType Q = CC_PTE_LightQueen;
static const CcPieceTagType K = CC_PTE_LightKing_CanCastle; // CC_PTE_LightKing;
static const CcPieceTagType E = CC_PTE_LightPegasus;
static const CcPieceTagType A = CC_PTE_LightPyramid;
static const CcPieceTagType U = CC_PTE_LightUnicorn;
static const CcPieceTagType W = CC_PTE_LightWave;
static const CcPieceTagType C = CC_PTE_LightCentaur;
static const CcPieceTagType O = CC_PTE_LightScout_CanRush; // CC_PTE_LightScout;
static const CcPieceTagType G = CC_PTE_LightGrenadier_CanRush; // CC_PTE_LightGrenadier;
static const CcPieceTagType S = CC_PTE_LightSerpent;
static const CcPieceTagType H = CC_PTE_LightShaman;
static const CcPieceTagType I = CC_PTE_LightStarchild;

static const CcPieceTagType T = CC_PTE_BrightStar;

static const CcPieceTagType M = CC_PTE_Monolith;

// a b c D e F g h i J k L m n o p q r s t u V w X Y Z


CcPieceTagType const CC_SETUP_BOARD_CLASSICAL_CHESS[ CC_VARIANT_BOARD_SIZE_CLASSICAL_CHESS ][ CC_VARIANT_BOARD_SIZE_CLASSICAL_CHESS ] = {
    { r, n, b, q, k, b, n, r },
    { p, p, p, p, p, p, p, p },
    { x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x },
    { P, P, P, P, P, P, P, P },
    { R, N, B, Q, K, B, N, R },
};

CcPieceTagType const CC_SETUP_BOARD_CROATIAN_TIES[ CC_VARIANT_BOARD_SIZE_CROATIAN_TIES ][ CC_VARIANT_BOARD_SIZE_CROATIAN_TIES ] = {
    { r, e, n, b, q, k, b, n, e, r },
    { p, p, p, p, p, p, p, p, p, p },
    { x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x },
    { P, P, P, P, P, P, P, P, P, P },
    { R, E, N, B, Q, K, B, N, E, R },
};

CcPieceTagType const CC_SETUP_BOARD_MAYAN_ASCENDANCY[ CC_VARIANT_BOARD_SIZE_MAYAN_ASCENDANCY ][ CC_VARIANT_BOARD_SIZE_MAYAN_ASCENDANCY ] = {
    { r, e, a, n, b, q, k, b, n, a, e, r },
    { p, p, p, p, p, p, p, p, p, p, p, p },
    { x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x },
    { P, P, P, P, P, P, P, P, P, P, P, P },
    { R, E, A, N, B, Q, K, B, N, A, E, R },
};

CcPieceTagType const CC_SETUP_BOARD_AGE_OF_AQUARIUS[ CC_VARIANT_BOARD_SIZE_AGE_OF_AQUARIUS ][ CC_VARIANT_BOARD_SIZE_AGE_OF_AQUARIUS ] = {
    { r, e, a, u, n, b, q, k, b, n, u, a, e, r },
    { p, p, p, p, p, p, p, p, p, p, p, p, p, p },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { P, P, P, P, P, P, P, P, P, P, P, P, P, P },
    { R, E, A, U, N, B, Q, K, B, N, U, A, E, R },
};

CcPieceTagType const CC_SETUP_BOARD_MIRANDAS_VEIL[ CC_VARIANT_BOARD_SIZE_MIRANDAS_VEIL ][ CC_VARIANT_BOARD_SIZE_MIRANDAS_VEIL ] = {
    { r, e, a, u, w, n, b, q, k, b, n, w, u, a, e, r },
    { p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P },
    { R, E, A, U, W, N, B, Q, K, B, N, W, U, A, E, R },
};

CcPieceTagType const CC_SETUP_BOARD_NINETEEN[ CC_VARIANT_BOARD_SIZE_NINETEEN ][ CC_VARIANT_BOARD_SIZE_NINETEEN ] = {
    { t, r, n, b, w, e, u, a, q, k, a, u, e, w, b, n, r, T },
    { p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p },
    { p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P },
    { P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P },
    { T, R, N, B, W, E, U, A, Q, K, A, U, E, W, B, N, R, t },
};

CcPieceTagType const CC_SETUP_BOARD_HEMERAS_DAWN[ CC_VARIANT_BOARD_SIZE_HEMERAS_DAWN ][ CC_VARIANT_BOARD_SIZE_HEMERAS_DAWN ] = {
    { t, r, n, b, c, w, e, u, a, q, k, a, u, e, w, c, b, n, r, T },
    { p, p, p, g, p, g, p, p, p, p, p, p, p, p, g, p, g, p, p, p },
    { p, p, g, p, p, p, g, p, p, p, p, p, p, g, p, p, p, g, p, p },
    { x, x, o, x, x, x, o, x, x, x, x, x, x, o, x, x, x, o, x, x },
    { x, x, x, o, x, o, x, x, x, x, x, x, x, x, o, x, o, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, O, x, O, x, x, x, x, x, x, x, x, O, x, O, x, x, x },
    { x, x, O, x, x, x, O, x, x, x, x, x, x, O, x, x, x, O, x, x },
    { P, P, G, P, P, P, G, P, P, P, P, P, P, G, P, P, P, G, P, P },
    { P, P, P, G, P, G, P, P, P, P, P, P, P, P, G, P, G, P, P, P },
    { T, R, N, B, C, W, E, U, A, Q, K, A, U, E, W, C, B, N, R, t },
};

CcPieceTagType const CC_SETUP_BOARD_TAMOANCHAN_REVISITED[ CC_VARIANT_BOARD_SIZE_TAMOANCHAN_REVISITED ][ CC_VARIANT_BOARD_SIZE_TAMOANCHAN_REVISITED ] = {
    { t, r, n, b, s, w, u, e, c, a, q, k, a, c, e, u, w, s, b, n, r, T },
    { p, p, p, p, p, p, p, g, p, g, p, p, g, p, g, p, p, p, p, p, p, p },
    { p, p, p, p, p, p, g, p, p, p, g, g, p, p, p, g, p, p, p, p, p, p },
    { x, x, x, x, x, x, o, x, x, x, o, o, x, x, x, o, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, o, x, o, x, x, o, x, o, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, O, x, O, x, x, O, x, O, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, O, x, x, x, O, O, x, x, x, O, x, x, x, x, x, x },
    { P, P, P, P, P, P, G, P, P, P, G, G, P, P, P, G, P, P, P, P, P, P },
    { P, P, P, P, P, P, P, G, P, G, P, P, G, P, G, P, P, P, P, P, P, P },
    { T, R, N, B, S, W, U, E, C, A, Q, K, A, C, E, U, W, S, B, N, R, t },
};

CcPieceTagType const CC_SETUP_BOARD_CONQUEST_OF_TLALOCAN[ CC_VARIANT_BOARD_SIZE_CONQUEST_OF_TLALOCAN ][ CC_VARIANT_BOARD_SIZE_CONQUEST_OF_TLALOCAN ] = {
    { t, r, n, b, s, c, u, w, e, a, h, q, k, h, a, e, w, u, c, s, b, n, r, T },
    { p, p, p, p, g, p, g, p, p, g, p, g, g, p, g, p, p, g, p, g, p, p, p, p },
    { p, p, p, g, p, p, p, g, g, p, p, g, g, p, p, g, g, p, p, p, g, p, p, p },
    { x, x, x, o, x, x, x, o, o, x, x, o, o, x, x, o, o, x, x, x, o, x, x, x },
    { x, x, x, x, o, x, o, x, x, o, x, o, o, x, o, x, x, o, x, o, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, O, x, O, x, x, O, x, O, O, x, O, x, x, O, x, O, x, x, x, x },
    { x, x, x, O, x, x, x, O, O, x, x, O, O, x, x, O, O, x, x, x, O, x, x, x },
    { P, P, P, G, P, P, P, G, G, P, P, G, G, P, P, G, G, P, P, P, G, P, P, P },
    { P, P, P, P, G, P, G, P, P, G, P, G, G, P, G, P, P, G, P, G, P, P, P, P },
    { T, R, N, B, S, C, U, W, E, A, H, Q, K, H, A, E, W, U, C, S, B, N, R, t },
};

CcPieceTagType const CC_SETUP_BOARD_DISCOVERY[ CC_VARIANT_BOARD_SIZE_DISCOVERY ][ CC_VARIANT_BOARD_SIZE_DISCOVERY ] = {
    { t, r, n, b, s, c, u, w, e, a, h, q, k, h, a, e, w, u, c, s, b, n, r, T },
    { p, p, p, p, g, p, g, p, p, g, p, g, g, p, g, p, p, g, p, g, p, p, p, p },
    { p, p, p, g, p, p, p, g, g, p, p, g, g, p, p, g, g, p, p, p, g, p, p, p },
    { x, x, x, o, x, x, x, o, o, x, x, o, o, x, x, o, o, x, x, x, o, x, x, x },
    { x, x, x, x, o, x, o, x, x, o, x, o, o, x, o, x, x, o, x, o, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, M, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, M, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, O, x, O, x, x, O, x, O, O, x, O, x, x, O, x, O, x, x, x, x },
    { x, x, x, O, x, x, x, O, O, x, x, O, O, x, x, O, O, x, x, x, O, x, x, x },
    { P, P, P, G, P, P, P, G, G, P, P, G, G, P, P, G, G, P, P, P, G, P, P, P },
    { P, P, P, P, G, P, G, P, P, G, P, G, G, P, G, P, P, G, P, G, P, P, P, P },
    { T, R, N, B, S, C, U, W, E, A, H, Q, K, H, A, E, W, U, C, S, B, N, R, t },
};

CcPieceTagType const CC_SETUP_BOARD_ONE[ CC_VARIANT_BOARD_SIZE_ONE ][ CC_VARIANT_BOARD_SIZE_ONE ] = {
    { t, r, n, b, s, i, c, u, e, w, a, h, q, k, h, a, w, e, u, c, i, s, b, n, r, T },
    { p, p, p, p, p, g, p, g, p, p, g, p, g, g, p, g, p, p, g, p, g, p, p, p, p, p },
    { p, p, p, p, g, p, p, p, g, g, p, p, g, g, p, p, g, g, p, p, p, g, p, p, p, p },
    { x, x, x, x, o, x, x, x, o, o, x, x, o, o, x, x, o, o, x, x, x, o, x, x, x, x },
    { x, x, x, x, x, o, x, o, x, x, o, x, o, o, x, o, x, x, o, x, o, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, M, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, M, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, O, x, O, x, x, O, x, O, O, x, O, x, x, O, x, O, x, x, x, x, x },
    { x, x, x, x, O, x, x, x, O, O, x, x, O, O, x, x, O, O, x, x, x, O, x, x, x, x },
    { P, P, P, P, G, P, P, P, G, G, P, P, G, G, P, P, G, G, P, P, P, G, P, P, P, P },
    { P, P, P, P, P, G, P, G, P, P, G, P, G, G, P, G, P, P, G, P, G, P, P, P, P, P },
    { T, R, N, B, S, I, C, U, E, W, A, H, Q, K, H, A, W, E, U, C, I, S, B, N, R, t },
};

CcPieceTagType const CC_SETUP_BOARD_CLASSICAL_CHESS_14[ CC_VARIANT_BOARD_SIZE_CLASSICAL_CHESS_14 ][ CC_VARIANT_BOARD_SIZE_CLASSICAL_CHESS_14 ] = {
    { r, n, b, r, n, b, q, k, b, n, r, b, n, r },
    { p, p, p, p, p, p, p, p, p, p, p, p, p, p },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { P, P, P, P, P, P, P, P, P, P, P, P, P, P },
    { R, N, B, R, N, B, Q, K, B, N, R, B, N, R },
};

CcPieceTagType const CC_SETUP_BOARD_CLASSICAL_CHESS_20[ CC_VARIANT_BOARD_SIZE_CLASSICAL_CHESS_20 ][ CC_VARIANT_BOARD_SIZE_CLASSICAL_CHESS_20 ] = {
    { r, n, b, r, n, b, r, n, b, q, k, b, n, r, b, n, r, b, n, r },
    { p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P },
    { R, N, B, R, N, B, R, N, B, Q, K, B, N, R, B, N, R, B, N, R },
};

CcPieceTagType const CC_SETUP_BOARD_CLASSICAL_CHESS_26[ CC_VARIANT_BOARD_SIZE_CLASSICAL_CHESS_26 ][ CC_VARIANT_BOARD_SIZE_CLASSICAL_CHESS_26 ] = {
    { r, n, b, r, n, b, r, n, b, r, n, b, q, k, b, n, r, b, n, r, b, n, r, b, n, r },
    { p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P },
    { R, N, B, R, N, B, R, N, B, R, N, B, Q, K, B, N, R, B, N, R, B, N, R, B, N, R },
};

CcPieceTagType const CC_SETUP_BOARD_CROATIAN_TIES_14[ CC_VARIANT_BOARD_SIZE_CROATIAN_TIES_14 ][ CC_VARIANT_BOARD_SIZE_CROATIAN_TIES_14 ] = {
    { r, e, b, r, e, b, q, k, b, e, r, b, e, r },
    { p, p, p, p, p, p, p, p, p, p, p, p, p, p },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { P, P, P, P, P, P, P, P, P, P, P, P, P, P },
    { R, E, B, R, E, B, Q, K, B, E, R, B, E, R },
};

CcPieceTagType const CC_SETUP_BOARD_CROATIAN_TIES_20[ CC_VARIANT_BOARD_SIZE_CROATIAN_TIES_20 ][ CC_VARIANT_BOARD_SIZE_CROATIAN_TIES_20 ] = {
    { r, e, b, r, e, b, r, e, b, q, k, b, e, r, b, e, r, b, e, r },
    { p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P },
    { R, E, B, R, E, B, R, E, B, Q, K, B, E, R, B, E, R, B, E, R },
};

CcPieceTagType const CC_SETUP_BOARD_CROATIAN_TIES_26[ CC_VARIANT_BOARD_SIZE_CROATIAN_TIES_26 ][ CC_VARIANT_BOARD_SIZE_CROATIAN_TIES_26 ] = {
    { r, e, b, r, e, b, r, e, b, r, e, b, q, k, b, e, r, b, e, r, b, e, r, b, e, r },
    { p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x },
    { P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P },
    { R, E, B, R, E, B, R, E, B, R, E, B, Q, K, B, E, R, B, E, R, B, E, R, B, E, R },
};


CcPieceTagType const * cc_setup_board_get( CcVariantType ve ) {
    switch ( ve ) {
        case CC_VE_ClassicalChess : return (CcPieceTagType const *)CC_SETUP_BOARD_CLASSICAL_CHESS;
        case CC_VE_CroatianTies : return (CcPieceTagType const *)CC_SETUP_BOARD_CROATIAN_TIES;
        case CC_VE_MayanAscendancy : return (CcPieceTagType const *)CC_SETUP_BOARD_MAYAN_ASCENDANCY;
        case CC_VE_AgeOfAquarius : return (CcPieceTagType const *)CC_SETUP_BOARD_AGE_OF_AQUARIUS;
        case CC_VE_MirandasVeil : return (CcPieceTagType const *)CC_SETUP_BOARD_MIRANDAS_VEIL;
        case CC_VE_Nineteen : return (CcPieceTagType const *)CC_SETUP_BOARD_NINETEEN;
        case CC_VE_HemerasDawn : return (CcPieceTagType const *)CC_SETUP_BOARD_HEMERAS_DAWN;
        case CC_VE_TamoanchanRevisited : return (CcPieceTagType const *)CC_SETUP_BOARD_TAMOANCHAN_REVISITED;
        case CC_VE_ConquestOfTlalocan : return (CcPieceTagType const *)CC_SETUP_BOARD_CONQUEST_OF_TLALOCAN;
        case CC_VE_Discovery : return (CcPieceTagType const *)CC_SETUP_BOARD_DISCOVERY;
        case CC_VE_One : return (CcPieceTagType const *)CC_SETUP_BOARD_ONE;

        case CC_VE_ClassicalChess_14 : return (CcPieceTagType const *)CC_SETUP_BOARD_CLASSICAL_CHESS_14;
        case CC_VE_ClassicalChess_20 : return (CcPieceTagType const *)CC_SETUP_BOARD_CLASSICAL_CHESS_20;
        case CC_VE_ClassicalChess_26 : return (CcPieceTagType const *)CC_SETUP_BOARD_CLASSICAL_CHESS_26;

        case CC_VE_CroatianTies_14 : return (CcPieceTagType const *)CC_SETUP_BOARD_CROATIAN_TIES_14;
        case CC_VE_CroatianTies_20 : return (CcPieceTagType const *)CC_SETUP_BOARD_CROATIAN_TIES_20;
        case CC_VE_CroatianTies_26 : return (CcPieceTagType const *)CC_SETUP_BOARD_CROATIAN_TIES_26;

        default : return NULL;
    }
}


// TODO :: RETURN :: maybe bool
bool cc_setup_board_has_piece( CcVariantType ve, CcPieceTagType pe ) {
    CcPieceTagType const * su = cc_setup_board_get( ve );
    if ( !su ) return false;

    size_t size = cc_variant_board_size( ve );
    if ( !CC_IS_BOARD_SIZE_VALID( size ) ) return false;

    for ( int i = 0; i < (int)size; ++i ) {
        for ( int j = 0; j < (int)size; ++j ) {
            int z = size * i + j;

            if ( su[ z ] == pe )
                return true;
        }
    }

    return false;
}
