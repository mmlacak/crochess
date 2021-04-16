// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include "piece_type.h"
#include "board_type.h"
#include "setup_board.h"


static const int t = PT_DimStar;

static const int i = PT_DarkStarchild;
static const int h = PT_DarkShaman;
static const int s = PT_DarkSerpent;
static const int c = PT_DarkCentaur;
static const int w = PT_DarkWave;
static const int u = PT_DarkUnicorn;
static const int a = PT_DarkPyramid;
static const int g = PT_DarkPegasus;
static const int k = PT_DarkKing;
static const int q = PT_DarkQueen;
static const int r = PT_DarkRook;
static const int b = PT_DarkBishop;
static const int n = PT_DarkKnight;
static const int p = PT_DarkPawn;

static const int x = PT_None;

static const int P = PT_LightPawn;
static const int N = PT_LightKnight;
static const int B = PT_LightBishop;
static const int R = PT_LightRook;
static const int Q = PT_LightQueen;
static const int K = PT_LightKing;
static const int G = PT_LightPegasus;
static const int A = PT_LightPyramid;
static const int U = PT_LightUnicorn;
static const int W = PT_LightWave;
static const int C = PT_LightCentaur;
static const int S = PT_LightSerpent;
static const int H = PT_LightShaman;
static const int I = PT_LightStarchild;

static const int T = PT_BrightStar;

static const int M = PT_Monolith;


PieceType const SETUP_BOARD_CLASSICAL_CHESS[ BOARD_SIZE_CLASSICAL_CHESS ][ BOARD_SIZE_CLASSICAL_CHESS ] =
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

PieceType const SETUP_BOARD_CROATIAN_TIES[ BOARD_SIZE_CROATIAN_TIES ][ BOARD_SIZE_CROATIAN_TIES ] =
{
    { r, g, n, b, q, k, b, n, g, r },
    { p, p, p, p, p, p, p, p, p, p },
    { x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, x, x, x },
    { P, P, P, P, P, P, P, P, P, P },
    { R, G, N, B, Q, K, B, N, G, R },
};

PieceType const SETUP_BOARD_MAYAN_ASCENDANCY[ BOARD_SIZE_MAYAN_ASCENDANCY ][ BOARD_SIZE_MAYAN_ASCENDANCY ] =
{
    { r, g, a, n, b, q, k, b, n, a, g, r },
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
    { R, G, A, N, B, Q, K, B, N, A, G, R },
};

PieceType const SETUP_BOARD_AGE_OF_AQUARIUS[ BOARD_SIZE_AGE_OF_AQUARIUS ][ BOARD_SIZE_AGE_OF_AQUARIUS ] =
{
    { r, g, a, u, n, b, q, k, b, n, u, a, g, r },
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
    { R, G, A, U, N, B, Q, K, B, N, U, A, G, R },
};

PieceType const SETUP_BOARD_MIRANDAS_VEIL[ BOARD_SIZE_MIRANDAS_VEIL ][ BOARD_SIZE_MIRANDAS_VEIL ] =
{
    { r, g, a, u, w, n, b, q, k, b, n, w, u, a, g, r },
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
    { R, G, A, U, W, N, B, Q, K, B, N, W, U, A, G, R },
};

PieceType const SETUP_BOARD_NINETEEN[ BOARD_SIZE_NINETEEN ][ BOARD_SIZE_NINETEEN ] =
{
    { t, r, n, b, w, g, u, a, q, k, a, u, g, w, b, n, r, T },
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
    { T, R, N, B, W, G, U, A, Q, K, A, U, G, W, B, N, R, t },
};

PieceType const SETUP_BOARD_HEMERAS_DAWN[ BOARD_SIZE_HEMERAS_DAWN ][ BOARD_SIZE_HEMERAS_DAWN ] =
{
    { t, r, n, b, c, w, g, u, a, q, k, a, u, g, w, c, b, n, r, T },
    { p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p },
    { p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p },
    { x, x, p, x, x, x, p, x, x, x, x, x, x, p, x, x, x, p, x, x },
    { x, x, x, p, x, p, x, x, x, x, x, x, x, x, p, x, p, x, x, x },
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
    { x, x, x, P, x, P, x, x, x, x, x, x, x, x, P, x, P, x, x, x },
    { x, x, P, x, x, x, P, x, x, x, x, x, x, P, x, x, x, P, x, x },
    { P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P },
    { P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P },
    { T, R, N, B, C, W, G, U, A, Q, K, A, U, G, W, C, B, N, R, t },
};

PieceType const SETUP_BOARD_TAMOANCHAN_REVISITED[ BOARD_SIZE_TAMOANCHAN_REVISITED ][ BOARD_SIZE_TAMOANCHAN_REVISITED ] =
{
    { t, r, n, b, s, w, u, g, c, a, q, k, a, c, g, u, w, s, b, n, r, T },
    { p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p },
    { p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p },
    { x, x, x, x, x, x, p, x, x, x, p, p, x, x, x, p, x, x, x, x, x, x },
    { x, x, x, x, x, x, x, p, x, p, x, x, p, x, p, x, x, x, x, x, x, x },
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
    { x, x, x, x, x, x, x, P, x, P, x, x, P, x, P, x, x, x, x, x, x, x },
    { x, x, x, x, x, x, P, x, x, x, P, P, x, x, x, P, x, x, x, x, x, x },
    { P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P },
    { P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P },
    { T, R, N, B, S, W, U, G, C, A, Q, K, A, C, G, U, W, S, B, N, R, t },
};

PieceType const SETUP_BOARD_CONQUEST_OF_TLALOCAN[ BOARD_SIZE_CONQUEST_OF_TLALOCAN ][ BOARD_SIZE_CONQUEST_OF_TLALOCAN ] =
{
    { t, r, n, b, s, c, u, w, g, a, h, q, k, h, a, g, w, u, c, s, b, n, r, T },
    { p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p },
    { p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p },
    { x, x, x, p, x, x, x, p, p, x, x, p, p, x, x, p, p, x, x, x, p, x, x, x },
    { x, x, x, x, p, x, p, x, x, p, x, p, p, x, p, x, x, p, x, p, x, x, x, x },
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
    { x, x, x, x, P, x, P, x, x, P, x, P, P, x, P, x, x, P, x, P, x, x, x, x },
    { x, x, x, P, x, x, x, P, P, x, x, P, P, x, x, P, P, x, x, x, P, x, x, x },
    { P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P },
    { P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P },
    { T, R, N, B, S, C, U, W, G, A, H, Q, K, H, A, G, W, U, C, S, B, N, R, t },
};

PieceType const SETUP_BOARD_DISCOVERY[ BOARD_SIZE_DISCOVERY ][ BOARD_SIZE_DISCOVERY ] =
{
    { t, r, n, b, s, c, u, w, g, a, h, q, k, h, a, g, w, u, c, s, b, n, r, T },
    { p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p },
    { p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p },
    { x, x, x, p, x, x, x, p, p, x, x, p, p, x, x, p, p, x, x, x, p, x, x, x },
    { x, x, x, x, p, x, p, x, x, p, x, p, p, x, p, x, x, p, x, p, x, x, x, x },
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
    { x, x, x, x, P, x, P, x, x, P, x, P, P, x, P, x, x, P, x, P, x, x, x, x },
    { x, x, x, P, x, x, x, P, P, x, x, P, P, x, x, P, P, x, x, x, P, x, x, x },
    { P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P },
    { P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P },
    { T, R, N, B, S, C, U, W, G, A, H, Q, K, H, A, G, W, U, C, S, B, N, R, t },
};

PieceType const SETUP_BOARD_ONE[ BOARD_SIZE_ONE ][ BOARD_SIZE_ONE ] =
{
    { t, r, n, b, s, i, c, u, g, w, a, h, q, k, h, a, w, g, u, c, i, s, b, n, r, T },
    { p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p },
    { p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p },
    { x, x, x, x, p, x, x, x, p, p, x, x, p, p, x, x, p, p, x, x, x, p, x, x, x, x },
    { x, x, x, x, x, p, x, p, x, x, p, x, p, p, x, p, x, x, p, x, p, x, x, x, x, x },
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
    { x, x, x, x, x, P, x, P, x, x, P, x, P, P, x, P, x, x, P, x, P, x, x, x, x, x },
    { x, x, x, x, P, x, x, x, P, P, x, x, P, P, x, x, P, P, x, x, x, P, x, x, x, x },
    { P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P },
    { P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P },
    { T, R, N, B, S, I, C, U, G, W, A, H, Q, K, H, A, W, G, U, C, I, S, B, N, R, t },
};
