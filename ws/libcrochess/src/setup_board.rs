// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

// use crate::piece_type as pt;
use crate::piece_type::PieceType as PT;

use crate::board_type as bt;
use crate::board_type::BoardType as BT;

// use crate::board as b;
// use crate::board::Chessboard as CB;


pub fn new_setup(board_type: bt::BoardType) -> &'static [ &'static [ PT ] ] {

    use crate::piece_type::PieceType::DimStar as t;

    use crate::piece_type::PieceType::DarkStarchild as i;
    use crate::piece_type::PieceType::DarkShaman as h;
    use crate::piece_type::PieceType::DarkSerpent as s;
    use crate::piece_type::PieceType::DarkCentaur as c;
    use crate::piece_type::PieceType::DarkWave as w;
    use crate::piece_type::PieceType::DarkUnicorn as u;
    use crate::piece_type::PieceType::DarkPyramid as a;
    use crate::piece_type::PieceType::DarkPegasus as g;
    use crate::piece_type::PieceType::DarkKing as k;
    use crate::piece_type::PieceType::DarkQueen as q;
    use crate::piece_type::PieceType::DarkRook as r;
    use crate::piece_type::PieceType::DarkBishop as b;
    use crate::piece_type::PieceType::DarkKnight as n;
    use crate::piece_type::PieceType::DarkPawn as p;

    use crate::piece_type::PieceType::None as x;

    use crate::piece_type::PieceType::LightPawn as P;
    use crate::piece_type::PieceType::LightKnight as N;
    use crate::piece_type::PieceType::LightBishop as B;
    use crate::piece_type::PieceType::LightRook as R;
    use crate::piece_type::PieceType::LightQueen as Q;
    use crate::piece_type::PieceType::LightKing as K;
    use crate::piece_type::PieceType::LightPegasus as G;
    use crate::piece_type::PieceType::LightPyramid as A;
    use crate::piece_type::PieceType::LightUnicorn as U;
    use crate::piece_type::PieceType::LightWave as W;
    use crate::piece_type::PieceType::LightCentaur as C;
    use crate::piece_type::PieceType::LightSerpent as S;
    use crate::piece_type::PieceType::LightShaman as H;
    use crate::piece_type::PieceType::LightStarchild as I;

    use crate::piece_type::PieceType::BrightStar as T;

    use crate::piece_type::PieceType::Monolith as M;

    fn new_cc_setup() -> &'static [ &'static [ PT ] ] {
        return &[
            &[ r, n, b, q, k, b, n, r ],
            &[ p, p, p, p, p, p, p, p ],
            &[ x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x ],
            &[ P, P, P, P, P, P, P, P ],
            &[ R, N, B, Q, K, B, N, R ],
        ];
    }

    fn new_ct_setup() -> &'static [ &'static [ PT ] ] {
        return &[
            &[ r, g, n, b, q, k, b, n, g, r ],
            &[ p, p, p, p, p, p, p, p, p, p ],
            &[ x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x ],
            &[ P, P, P, P, P, P, P, P, P, P ],
            &[ R, G, N, B, Q, K, B, N, G, R ],
        ];
    }

    fn new_ma_setup() -> &'static [ &'static [ PT ] ] {
        return &[
            &[ r, g, a, n, b, q, k, b, n, a, g, r ],
            &[ p, p, p, p, p, p, p, p, p, p, p, p ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ P, P, P, P, P, P, P, P, P, P, P, P ],
            &[ R, G, A, N, B, Q, K, B, N, A, G, R ],
        ];
    }

    fn new_aoa_setup() -> &'static [ &'static [ PT ] ] {
        return &[
            &[ r, g, a, u, n, b, q, k, b, n, u, a, g, r ],
            &[ p, p, p, p, p, p, p, p, p, p, p, p, p, p ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ P, P, P, P, P, P, P, P, P, P, P, P, P, P ],
            &[ R, G, A, U, N, B, Q, K, B, N, U, A, G, R ],
        ];
    }

    fn new_mv_setup() -> &'static [ &'static [ PT ] ] {
        return &[
            &[ r, g, a, u, w, n, b, q, k, b, n, w, u, a, g, r ],
            &[ p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P ],
            &[ R, G, A, U, W, N, B, Q, K, B, N, W, U, A, G, R ],
        ];
    }

    fn new_n_setup() -> &'static [ &'static [ PT ] ] {
        return &[
            &[ t, r, n, b, w, g, u, a, q, k, a, u, g, w, b, n, r, T ],
            &[ p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p ],
            &[ p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P ],
            &[ P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P ],
            &[ T, R, N, B, W, G, U, A, Q, K, A, U, G, W, B, N, R, t ],
        ];
    }

    fn new_hd_setup() -> &'static [ &'static [ PT ] ] {
        return &[
            &[ t, r, n, b, c, w, g, u, a, q, k, a, u, g, w, c, b, n, r, T ],
            &[ p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p ],
            &[ p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p ],
            &[ x, x, p, x, x, x, p, x, x, x, x, x, x, p, x, x, x, p, x, x ],
            &[ x, x, x, p, x, p, x, x, x, x, x, x, x, x, p, x, p, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, P, x, P, x, x, x, x, x, x, x, x, P, x, P, x, x, x ],
            &[ x, x, P, x, x, x, P, x, x, x, x, x, x, P, x, x, x, P, x, x ],
            &[ P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P ],
            &[ P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P ],
            &[ T, R, N, B, C, W, G, U, A, Q, K, A, U, G, W, C, B, N, R, t ],
        ];
    }

    fn new_tr_setup() -> &'static [ &'static [ PT ] ] {
        return &[
            &[ t, r, n, b, s, w, u, g, c, a, q, k, a, c, g, u, w, s, b, n, r, T ],
            &[ p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p ],
            &[ p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p ],
            &[ x, x, x, x, x, x, p, x, x, x, p, p, x, x, x, p, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, p, x, p, x, x, p, x, p, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, P, x, P, x, x, P, x, P, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, P, x, x, x, P, P, x, x, x, P, x, x, x, x, x, x ],
            &[ P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P ],
            &[ P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P ],
            &[ T, R, N, B, S, W, U, G, C, A, Q, K, A, C, G, U, W, S, B, N, R, t ],
        ];
    }

    fn new_cot_setup() -> &'static [ &'static [ PT ] ] {
        return &[
            &[ t, r, n, b, s, c, u, w, g, a, h, q, k, h, a, g, w, u, c, s, b, n, r, T ],
            &[ p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p ],
            &[ p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p ],
            &[ x, x, x, p, x, x, x, p, p, x, x, p, p, x, x, p, p, x, x, x, p, x, x, x ],
            &[ x, x, x, x, p, x, p, x, x, p, x, p, p, x, p, x, x, p, x, p, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, P, x, P, x, x, P, x, P, P, x, P, x, x, P, x, P, x, x, x, x ],
            &[ x, x, x, P, x, x, x, P, P, x, x, P, P, x, x, P, P, x, x, x, P, x, x, x ],
            &[ P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P ],
            &[ P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P ],
            &[ T, R, N, B, S, C, U, W, G, A, H, Q, K, H, A, G, W, U, C, S, B, N, R, t ],
        ];
    }

    fn new_d_setup() -> &'static [ &'static [ PT ] ] {
        return &[
            &[ t, r, n, b, s, c, u, w, g, a, h, q, k, h, a, g, w, u, c, s, b, n, r, T ],
            &[ p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p ],
            &[ p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p ],
            &[ x, x, x, p, x, x, x, p, p, x, x, p, p, x, x, p, p, x, x, x, p, x, x, x ],
            &[ x, x, x, x, p, x, p, x, x, p, x, p, p, x, p, x, x, p, x, p, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, M, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, M, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, P, x, P, x, x, P, x, P, P, x, P, x, x, P, x, P, x, x, x, x ],
            &[ x, x, x, P, x, x, x, P, P, x, x, P, P, x, x, P, P, x, x, x, P, x, x, x ],
            &[ P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P ],
            &[ P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P ],
            &[ T, R, N, B, S, C, U, W, G, A, H, Q, K, H, A, G, W, U, C, S, B, N, R, t ],
        ];
    }

    fn new_o_setup() -> &'static [ &'static [ PT ] ] {
        return &[
            &[ t, r, n, b, s, i, c, u, g, w, a, h, q, k, h, a, w, g, u, c, i, s, b, n, r, T ],
            &[ p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p ],
            &[ p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p ],
            &[ x, x, x, x, p, x, x, x, p, p, x, x, p, p, x, x, p, p, x, x, x, p, x, x, x, x ],
            &[ x, x, x, x, x, p, x, p, x, x, p, x, p, p, x, p, x, x, p, x, p, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, M, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, M, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
            &[ x, x, x, x, x, P, x, P, x, x, P, x, P, P, x, P, x, x, P, x, P, x, x, x, x, x ],
            &[ x, x, x, x, P, x, x, x, P, P, x, x, P, P, x, x, P, P, x, x, x, P, x, x, x, x ],
            &[ P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P ],
            &[ P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P ],
            &[ T, R, N, B, S, I, C, U, G, W, A, H, Q, K, H, A, W, G, U, C, I, S, B, N, R, t ],
        ];
    }

    return match board_type {
        BT::ClassicalChess => new_cc_setup(),
        BT::CroatianTies => new_ct_setup(),
        BT::MayanAscendancy => new_ma_setup(),
        BT::AgeOfAquarius => new_aoa_setup(),
        BT::MirandasVeil => new_mv_setup(),
        BT::Nineteen => new_n_setup(),
        BT::HemerasDawn => new_hd_setup(),
        BT::TamoanchanRevisited => new_tr_setup(),
        BT::ConquestOfTlalocan => new_cot_setup(),
        BT::Discovery => new_d_setup(),
        BT::One => new_o_setup(),
    };
}
