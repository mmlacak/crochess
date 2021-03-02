// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

use std::fmt;

// use crate::piece_type as pt;
use crate::piece_type::PieceType as PT;

use crate::board_type as bt;
use crate::board_type::BoardType as BT;

use crate::board as b;
use crate::board::Board;

use crate::piece_flags as pf;
use crate::piece_flags::PieceFlag as PF;

use crate::rules_flags as rf;
// use crate::rules_flags::Flags;

#[derive(Debug, Clone)]
pub struct Rules {
    board: b::Board,
    flags: rf::Flags,
}

impl Rules {

    pub fn new(board_type: bt::BoardType, do_initial_setup: bool) -> Rules {
        let mut b = Board::new(board_type);

        if do_initial_setup {
            let cb = new_setup(board_type);
            b.set_chessboard(cb);
        }

        let fs = rf::new_flags(board_type);

        let mut rules = Rules { board: b,
                                flags: fs };

        if do_initial_setup {
            let f = new_setup_rules(board_type);
            rules.set_flags(f);
        }

        return rules;
    }

    pub fn board(&self) -> &b::Board {
        return &self.board;
    }

    pub fn flags(&self) -> &rf::Flags {
        return &self.flags;
    }

    pub fn flag_at(&self, i: i32, j: i32) -> PF {
        if self.board.is_on_chessboard(i, j) {
            return self.flags.0[i as usize][j as usize];
        }
        else {
            return PF::None;
        }
    }

    pub fn set_flag_at(&mut self, i: i32, j: i32, flag: PF) -> bool {
        if self.board.is_on_chessboard(i, j) {
            self.flags.0[i as usize][j as usize] = flag;
            return true;
        }
        else {
            return false;
        }
    }

    pub fn set_flags(&mut self, flags: &[ &[ PF ] ]) -> bool {
        let size = self.board.variant().size();
        if size != flags.len() { return false; }

        for i in 0 .. size {
            if size != flags[ i ].len() {
                return false;
            }
        }

        let mut result = true;
        for i in 0 .. size {
            for j in 0 .. size {
                // beware: lazy logical and
                result = self.set_flag_at(i as i32, j  as i32, flags[ size - j - 1 ][ i ]) && result;
            }
        }

        return result;
    }

}


fn new_setup(board_type: bt::BoardType) -> &'static [ &'static [ PT ] ] {

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


impl fmt::Display for Rules {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let size = self.board().variant().size();
        let len = 2 * size + 1;
        let divider = "-".to_string().repeat(len);
        // let cb = format!("{}", self.board().chessboard());
        let fs = format!("{}", self.flags());
        let mut files = "".to_string();

        // Ranges don't include upper bound; '{'  is char positioned after 'z' in ASCII table.
        for (i, c) in ('a' .. '{').enumerate() {
            if i >= size { break; }
            files += format!(" {}", c).as_str();
        }

        write!(f, "    {}\n", files) ?;
        write!(f, "    {}\n", divider) ?;
        // for (i, line_i) in cb.lines().enumerate() {
        for (i, line_i) in fs.lines().enumerate() {
            let row = size - i;
            write!(f, "{:2} |", row) ?;
            write!(f, "{}", line_i) ?;
            write!(f, " | {:2}\n", row) ?;
        }
        write!(f, "    {}\n", divider) ?;
        write!(f, "    {}\n", files) ?;

        return Ok(());
    }
}



fn new_setup_rules(board_type: bt::BoardType) -> &'static [ &'static [ PF ] ] {

    use crate::piece_flags::PieceFlag::None as n;
    use crate::piece_flags::PieceFlag::CanRush as R;
    use crate::piece_flags::PieceFlag::CanCastle as C;
    use crate::piece_flags::PieceFlag::TagForPromotion as P;

    fn new_cc_setup() -> &'static [ &'static [ PF ] ] {
        return &[
            &[ C, n, n, n, C, n, n, C ],
            &[ R, R, R, R, R, R, R, R ],
            &[ n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n ],
            &[ R, R, R, R, R, R, R, R ],
            &[ C, n, n, n, C, n, n, C ],
        ];
    }

    fn new_ct_setup() -> &'static [ &'static [ PF ] ] {
        return &[
            &[ C, n, n, n, n, C, n, n, n, C ],
            &[ R, R, R, R, R, R, R, R, R, R ],
            &[ n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n ],
            &[ R, R, R, R, R, R, R, R, R, R ],
            &[ C, n, n, n, n, C, n, n, n, C ],
        ];
    }

    fn new_ma_setup() -> &'static [ &'static [ PF ] ] {
        return &[
            &[ C, n, n, n, n, n, C, n, n, n, n, C ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ C, n, n, n, n, n, C, n, n, n, n, C ],
        ];
    }

    fn new_aoa_setup() -> &'static [ &'static [ PF ] ] {
        return &[
            &[ C, n, n, n, n, n, n, C, n, n, n, n, n, C ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ C, n, n, n, n, n, n, C, n, n, n, n, n, C ],
        ];
    }

    fn new_mv_setup() -> &'static [ &'static [ PF ] ] {
        return &[
            &[ C, n, n, n, n, n, n, n, C, n, n, n, n, n, n, C ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ C, n, n, n, n, n, n, n, C, n, n, n, n, n, n, C ],
        ];
    }

    fn new_n_setup() -> &'static [ &'static [ PF ] ] {
        return &[
            &[ n, C, n, n, n, n, n, n, n, C, n, n, n, n, n, n, C, n ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ n, C, n, n, n, n, n, n, n, C, n, n, n, n, n, n, C, n ],
        ];
    }

    fn new_hd_setup() -> &'static [ &'static [ PF ] ] {
        return &[
            &[ n, C, n, n, n, n, n, n, n, n, C, n, n, n, n, n, n, n, C, n ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ n, n, R, n, n, n, R, n, n, n, n, n, n, R, n, n, n, R, n, n ],
            &[ n, n, n, R, n, R, n, n, n, n, n, n, n, n, R, n, R, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, P, n, P, n, n, n, n, n, n, n, n, P, n, P, n, n, n ],
            &[ n, n, P, n, n, n, P, n, n, n, n, n, n, P, n, n, n, P, n, n ],
            &[ P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P ],
            &[ P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P ],
            &[ n, C, n, n, n, n, n, n, n, n, C, n, n, n, n, n, n, n, C, n ],
        ];
    }

    fn new_tr_setup() -> &'static [ &'static [ PF ] ] {
        return &[
            &[ n, C, n, n, n, n, n, n, n, n, n, C, n, n, n, n, n, n, n, n, C, n ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ n, n, n, n, n, n, R, n, n, n, R, R, n, n, n, R, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, R, n, R, n, n, R, n, R, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, R, n, R, n, n, R, n, R, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, R, n, n, n, R, R, n, n, n, R, n, n, n, n, n, n ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ n, C, n, n, n, n, n, n, n, n, n, C, n, n, n, n, n, n, n, n, C, n ],
        ];
    }

    fn new_cot_setup() -> &'static [ &'static [ PF ] ] {
        return &[
            &[ n, C, n, n, n, n, n, n, n, n, n, n, C, n, n, n, n, n, n, n, n, n, C, n ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ n, n, n, R, n, n, n, R, R, n, n, R, R, n, n, R, R, n, n, n, R, n, n, n ],
            &[ n, n, n, n, R, n, R, n, n, R, n, R, R, n, R, n, n, R, n, R, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, R, n, R, n, n, R, n, R, R, n, R, n, n, R, n, R, n, n, n, n ],
            &[ n, n, n, R, n, n, n, R, R, n, n, R, R, n, n, R, R, n, n, n, R, n, n, n ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ n, C, n, n, n, n, n, n, n, n, n, n, C, n, n, n, n, n, n, n, n, n, C, n ],
        ];
    }

    fn new_d_setup() -> &'static [ &'static [ PF ] ] {
        return &[
            &[ n, C, n, n, n, n, n, n, n, n, n, n, C, n, n, n, n, n, n, n, n, n, C, n ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ n, n, n, R, n, n, n, R, R, n, n, R, R, n, n, R, R, n, n, n, R, n, n, n ],
            &[ n, n, n, n, R, n, R, n, n, R, n, R, R, n, R, n, n, R, n, R, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, R, n, R, n, n, R, n, R, R, n, R, n, n, R, n, R, n, n, n, n ],
            &[ n, n, n, R, n, n, n, R, R, n, n, R, R, n, n, R, R, n, n, n, R, n, n, n ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ n, C, n, n, n, n, n, n, n, n, n, n, C, n, n, n, n, n, n, n, n, n, C, n ],
        ];
    }

    fn new_o_setup() -> &'static [ &'static [ PF ] ] {
        return &[
            &[ n, C, n, n, n, n, n, n, n, n, n, n, n, C, n, n, n, n, n, n, n, n, n, n, C, n ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ n, n, n, n, R, n, n, n, R, R, n, n, R, R, n, n, R, R, n, n, n, R, n, n, n, n ],
            &[ n, n, n, n, n, R, n, R, n, n, R, n, R, R, n, R, n, n, R, n, R, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, R, n, R, n, n, R, n, R, R, n, R, n, n, R, n, R, n, n, n, n, n ],
            &[ n, n, n, n, R, n, n, n, R, R, n, n, R, R, n, n, R, R, n, n, n, R, n, n, n, n ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ n, C, n, n, n, n, n, n, n, n, n, n, n, C, n, n, n, n, n, n, n, n, n, n, C, n ],
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
