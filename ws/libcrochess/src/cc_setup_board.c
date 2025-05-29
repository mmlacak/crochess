// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>

#include "cc_defines.h"
#include "cc_piece.h"
#include "cc_variant.h"
#include "cc_setup_board.h"


static const CcPieceType t = CC_PE_DimStar;

static const CcPieceType i = CC_PE_DarkStarchild;
static const CcPieceType h = CC_PE_DarkShaman;
static const CcPieceType s = CC_PE_DarkSerpent;
static const CcPieceType g = CC_PE_DarkGrenadier;
static const CcPieceType o = CC_PE_DarkScout;
static const CcPieceType c = CC_PE_DarkCentaur;
static const CcPieceType w = CC_PE_DarkWave;
static const CcPieceType u = CC_PE_DarkUnicorn;
static const CcPieceType a = CC_PE_DarkPyramid;
static const CcPieceType e = CC_PE_DarkPegasus;
static const CcPieceType k = CC_PE_DarkKing;
static const CcPieceType q = CC_PE_DarkQueen;
static const CcPieceType r = CC_PE_DarkRook;
static const CcPieceType b = CC_PE_DarkBishop;
static const CcPieceType n = CC_PE_DarkKnight;
static const CcPieceType p = CC_PE_DarkPawn;

static const CcPieceType x = CC_PE_None;

static const CcPieceType P = CC_PE_LightPawn;
static const CcPieceType N = CC_PE_LightKnight;
static const CcPieceType B = CC_PE_LightBishop;
static const CcPieceType R = CC_PE_LightRook;
static const CcPieceType Q = CC_PE_LightQueen;
static const CcPieceType K = CC_PE_LightKing;
static const CcPieceType E = CC_PE_LightPegasus;
static const CcPieceType A = CC_PE_LightPyramid;
static const CcPieceType U = CC_PE_LightUnicorn;
static const CcPieceType W = CC_PE_LightWave;
static const CcPieceType C = CC_PE_LightCentaur;
static const CcPieceType O = CC_PE_LightScout;
static const CcPieceType G = CC_PE_LightGrenadier;
static const CcPieceType S = CC_PE_LightSerpent;
static const CcPieceType H = CC_PE_LightShaman;
static const CcPieceType I = CC_PE_LightStarchild;

static const CcPieceType T = CC_PE_BrightStar;

static const CcPieceType M = CC_PE_Monolith;


CcPieceType const CC_SETUP_BOARD_CLASSICAL_CHESS[ CC_VARIANT_BOARD_SIZE_CLASSICAL_CHESS ][ CC_VARIANT_BOARD_SIZE_CLASSICAL_CHESS ] = {
    { r, n, b, q, k, b, n, r },
    { p, p, p, p, p, p, p, p },
    { x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x },
    { P, P, P, P, P, P, P, P },
    { R, N, B, Q, K, B, N, R },
};

CcPieceType const CC_SETUP_BOARD_CROATIAN_TIES[ CC_VARIANT_BOARD_SIZE_CROATIAN_TIES ][ CC_VARIANT_BOARD_SIZE_CROATIAN_TIES ] = {
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

CcPieceType const CC_SETUP_BOARD_MAYAN_ASCENDANCY[ CC_VARIANT_BOARD_SIZE_MAYAN_ASCENDANCY ][ CC_VARIANT_BOARD_SIZE_MAYAN_ASCENDANCY ] = {
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

CcPieceType const CC_SETUP_BOARD_AGE_OF_AQUARIUS[ CC_VARIANT_BOARD_SIZE_AGE_OF_AQUARIUS ][ CC_VARIANT_BOARD_SIZE_AGE_OF_AQUARIUS ] = {
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

CcPieceType const CC_SETUP_BOARD_MIRANDAS_VEIL[ CC_VARIANT_BOARD_SIZE_MIRANDAS_VEIL ][ CC_VARIANT_BOARD_SIZE_MIRANDAS_VEIL ] = {
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

CcPieceType const CC_SETUP_BOARD_NINETEEN[ CC_VARIANT_BOARD_SIZE_NINETEEN ][ CC_VARIANT_BOARD_SIZE_NINETEEN ] = {
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

CcPieceType const CC_SETUP_BOARD_HEMERAS_DAWN[ CC_VARIANT_BOARD_SIZE_HEMERAS_DAWN ][ CC_VARIANT_BOARD_SIZE_HEMERAS_DAWN ] = {
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

CcPieceType const CC_SETUP_BOARD_TAMOANCHAN_REVISITED[ CC_VARIANT_BOARD_SIZE_TAMOANCHAN_REVISITED ][ CC_VARIANT_BOARD_SIZE_TAMOANCHAN_REVISITED ] = {
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

CcPieceType const CC_SETUP_BOARD_CONQUEST_OF_TLALOCAN[ CC_VARIANT_BOARD_SIZE_CONQUEST_OF_TLALOCAN ][ CC_VARIANT_BOARD_SIZE_CONQUEST_OF_TLALOCAN ] = {
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

CcPieceType const CC_SETUP_BOARD_DISCOVERY[ CC_VARIANT_BOARD_SIZE_DISCOVERY ][ CC_VARIANT_BOARD_SIZE_DISCOVERY ] = {
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

CcPieceType const CC_SETUP_BOARD_ONE[ CC_VARIANT_BOARD_SIZE_ONE ][ CC_VARIANT_BOARD_SIZE_ONE ] = {
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


CcPieceType const * cc_setup_board_get( CcVariantType ve ) {
    switch ( ve ) {
        case CC_VE_ClassicalChess : return (CcPieceType const *)CC_SETUP_BOARD_CLASSICAL_CHESS;
        case CC_VE_CroatianTies : return (CcPieceType const *)CC_SETUP_BOARD_CROATIAN_TIES;
        case CC_VE_MayanAscendancy : return (CcPieceType const *)CC_SETUP_BOARD_MAYAN_ASCENDANCY;
        case CC_VE_AgeOfAquarius : return (CcPieceType const *)CC_SETUP_BOARD_AGE_OF_AQUARIUS;
        case CC_VE_MirandasVeil : return (CcPieceType const *)CC_SETUP_BOARD_MIRANDAS_VEIL;
        case CC_VE_Nineteen : return (CcPieceType const *)CC_SETUP_BOARD_NINETEEN;
        case CC_VE_HemerasDawn : return (CcPieceType const *)CC_SETUP_BOARD_HEMERAS_DAWN;
        case CC_VE_TamoanchanRevisited : return (CcPieceType const *)CC_SETUP_BOARD_TAMOANCHAN_REVISITED;
        case CC_VE_ConquestOfTlalocan : return (CcPieceType const *)CC_SETUP_BOARD_CONQUEST_OF_TLALOCAN;
        case CC_VE_Discovery : return (CcPieceType const *)CC_SETUP_BOARD_DISCOVERY;
        case CC_VE_One : return (CcPieceType const *)CC_SETUP_BOARD_ONE;

        default : return NULL;
    }
}


bool cc_setup_board_has_piece( CcVariantType ve, CcPieceType pe ) {
    CcPieceType const * su = cc_setup_board_get( ve );
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
