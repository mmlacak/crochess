// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>

#include "cc_defines.h"
#include "cc_piece.h"
#include "cc_variant.h"
#include "cc_setup_board.h"

/**
    @file cc_setup_board.c
    @brief Board setups data arrays, and related functions.
*/


static const int t = CC_PE_DimStar;

static const int i = CC_PE_DarkStarchild;
static const int h = CC_PE_DarkShaman;
static const int s = CC_PE_DarkSerpent;
static const int g = CC_PE_DarkGrenadier;
static const int o = CC_PE_DarkScout;
static const int c = CC_PE_DarkCentaur;
static const int w = CC_PE_DarkWave;
static const int u = CC_PE_DarkUnicorn;
static const int a = CC_PE_DarkPyramid;
static const int e = CC_PE_DarkPegasus;
static const int k = CC_PE_DarkKing;
static const int q = CC_PE_DarkQueen;
static const int r = CC_PE_DarkRook;
static const int b = CC_PE_DarkBishop;
static const int n = CC_PE_DarkKnight;
static const int p = CC_PE_DarkPawn;

static const int x = CC_PE_None;

static const int P = CC_PE_LightPawn;
static const int N = CC_PE_LightKnight;
static const int B = CC_PE_LightBishop;
static const int R = CC_PE_LightRook;
static const int Q = CC_PE_LightQueen;
static const int K = CC_PE_LightKing;
static const int E = CC_PE_LightPegasus;
static const int A = CC_PE_LightPyramid;
static const int U = CC_PE_LightUnicorn;
static const int W = CC_PE_LightWave;
static const int C = CC_PE_LightCentaur;
static const int O = CC_PE_LightScout;
static const int G = CC_PE_LightGrenadier;
static const int S = CC_PE_LightSerpent;
static const int H = CC_PE_LightShaman;
static const int I = CC_PE_LightStarchild;

static const int T = CC_PE_BrightStar;

static const int M = CC_PE_Monolith;


CcPieceEnum const CC_SETUP_BOARD_CLASSICAL_CHESS[ CC_VARIANT_BOARD_SIZE_CLASSICAL_CHESS ][ CC_VARIANT_BOARD_SIZE_CLASSICAL_CHESS ] =
{
    { r, n, b, q, k, b, n, r },
    { p, p, p, p, p, p, p, p },
    { x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x },
    { P, P, P, P, P, P, P, P },
    { R, N, B, Q, K, B, N, R },
};

CcPieceEnum const CC_SETUP_BOARD_CROATIAN_TIES[ CC_VARIANT_BOARD_SIZE_CROATIAN_TIES ][ CC_VARIANT_BOARD_SIZE_CROATIAN_TIES ] =
{
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

CcPieceEnum const CC_SETUP_BOARD_MAYAN_ASCENDANCY[ CC_VARIANT_BOARD_SIZE_MAYAN_ASCENDANCY ][ CC_VARIANT_BOARD_SIZE_MAYAN_ASCENDANCY ] =
{
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

CcPieceEnum const CC_SETUP_BOARD_AGE_OF_AQUARIUS[ CC_VARIANT_BOARD_SIZE_AGE_OF_AQUARIUS ][ CC_VARIANT_BOARD_SIZE_AGE_OF_AQUARIUS ] =
{
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

CcPieceEnum const CC_SETUP_BOARD_MIRANDAS_VEIL[ CC_VARIANT_BOARD_SIZE_MIRANDAS_VEIL ][ CC_VARIANT_BOARD_SIZE_MIRANDAS_VEIL ] =
{
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

CcPieceEnum const CC_SETUP_BOARD_NINETEEN[ CC_VARIANT_BOARD_SIZE_NINETEEN ][ CC_VARIANT_BOARD_SIZE_NINETEEN ] =
{
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

CcPieceEnum const CC_SETUP_BOARD_HEMERAS_DAWN[ CC_VARIANT_BOARD_SIZE_HEMERAS_DAWN ][ CC_VARIANT_BOARD_SIZE_HEMERAS_DAWN ] =
{
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

CcPieceEnum const CC_SETUP_BOARD_TAMOANCHAN_REVISITED[ CC_VARIANT_BOARD_SIZE_TAMOANCHAN_REVISITED ][ CC_VARIANT_BOARD_SIZE_TAMOANCHAN_REVISITED ] =
{
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

CcPieceEnum const CC_SETUP_BOARD_CONQUEST_OF_TLALOCAN[ CC_VARIANT_BOARD_SIZE_CONQUEST_OF_TLALOCAN ][ CC_VARIANT_BOARD_SIZE_CONQUEST_OF_TLALOCAN ] =
{
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

CcPieceEnum const CC_SETUP_BOARD_DISCOVERY[ CC_VARIANT_BOARD_SIZE_DISCOVERY ][ CC_VARIANT_BOARD_SIZE_DISCOVERY ] =
{
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

CcPieceEnum const CC_SETUP_BOARD_ONE[ CC_VARIANT_BOARD_SIZE_ONE ][ CC_VARIANT_BOARD_SIZE_ONE ] =
{
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


CcPieceEnum const * cc_setup_board_get( CcVariantEnum ve )
{
    switch ( ve )
    {
        case CC_VE_ClassicalChess : return (CcPieceEnum const *)CC_SETUP_BOARD_CLASSICAL_CHESS;
        case CC_VE_CroatianTies : return (CcPieceEnum const *)CC_SETUP_BOARD_CROATIAN_TIES;
        case CC_VE_MayanAscendancy : return (CcPieceEnum const *)CC_SETUP_BOARD_MAYAN_ASCENDANCY;
        case CC_VE_AgeOfAquarius : return (CcPieceEnum const *)CC_SETUP_BOARD_AGE_OF_AQUARIUS;
        case CC_VE_MirandasVeil : return (CcPieceEnum const *)CC_SETUP_BOARD_MIRANDAS_VEIL;
        case CC_VE_Nineteen : return (CcPieceEnum const *)CC_SETUP_BOARD_NINETEEN;
        case CC_VE_HemerasDawn : return (CcPieceEnum const *)CC_SETUP_BOARD_HEMERAS_DAWN;
        case CC_VE_TamoanchanRevisited : return (CcPieceEnum const *)CC_SETUP_BOARD_TAMOANCHAN_REVISITED;
        case CC_VE_ConquestOfTlalocan : return (CcPieceEnum const *)CC_SETUP_BOARD_CONQUEST_OF_TLALOCAN;
        case CC_VE_Discovery : return (CcPieceEnum const *)CC_SETUP_BOARD_DISCOVERY;
        case CC_VE_One : return (CcPieceEnum const *)CC_SETUP_BOARD_ONE;

        default : return NULL;
    }
}


bool cc_setup_board_has_piece( CcVariantEnum ve, CcPieceEnum pe )
{
    CcPieceEnum const * su = cc_setup_board_get( ve );
    if ( !su ) return false;

    size_t size = cc_variant_board_size( ve );

    for ( int i = 0; i < (int)size; ++i )
    {
        for ( int j = 0; j < (int)size; ++j )
        {
            int z = size * i + j;

            if ( su[ z ] == pe )
                return true;
        }
    }

    return false;
}

int cc_setup_board_get_figure_row_initial_file( CcVariantEnum ve,
                                                CcPieceEnum pe,
                                                bool search_left_first )
{
    // Not figure row pieces.
    if ( ( CC_PIECE_IS_NONE( pe ) ) ||
         ( CC_PIECE_IS_PAWN( pe ) ) ||
         ( CC_PIECE_IS_SCOUT( pe ) ) ||
         ( CC_PIECE_IS_GRENADIER( pe ) ) ||
         ( CC_PIECE_IS_MONOLITH( pe ) ) )
        return CC_INVALID_COORD;

    CcPieceEnum const * su = cc_setup_board_get( ve );
    if ( !su ) return CC_INVALID_COORD;

    size_t size = cc_variant_board_size( ve );
    int start = search_left_first ? 0 : (int)(size - 1);
    int step = search_left_first ? 1 : -1;

    int rank =
        cc_piece_is_light( pe ) ||
        ( pe == CC_PE_BrightStar ) ? (int)(size - 1)
                                   : 0;

    for ( int j = start; (0 <= j) && (j < (int)size); j += step )
    {
        int z = size * rank + j;

        if ( su[ z ] == pe )
            return j;
    }

    return CC_INVALID_COORD;
}
