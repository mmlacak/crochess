// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.


// use crate::piece_type as pt;
use crate::piece_type::PieceType as PT;

use crate::board_type as bt;
use crate::board_type::BoardType as BT;

use crate::board as b;

use crate::setup_board as sb;

// use crate::piece_flag as pf;
use crate::piece_flag::PieceFlag as PF;

use crate::flags as f;
// use crate::flags::Flags;

use crate::setup_flags as sf;


#[derive(Debug, Clone)]
pub struct Rules {
    variant: BT,
    board: b::Board,
    flags: f::Flags,
}


impl Rules {

    pub fn new(board_type: bt::BoardType, do_initial_setup: bool) -> Rules {
        let cb = b::new_chessboard(board_type);
        let fs = f::new_flags(board_type);

        let mut rules = Rules { variant: board_type,
                                board: cb,
                                flags: fs };

        if do_initial_setup {
            let cb = sb::new_setup(board_type);
            rules.set_board(cb);

            let f = sf::new_setup_rules(board_type);
            rules.set_flags(f);
        }

        return rules;
    }

    pub fn variant(&self) -> BT {
        return self.variant;
    }

    pub fn board(&self) -> &b::Board {
        return &self.board;
    }

    pub fn flags(&self) -> &f::Flags {
        return &self.flags;
    }

    pub fn is_on_board(&self, i: i32, j: i32) -> bool {
        let size: i32 = self.variant.size() as i32;
        return (0 <= i) && (i <= size) &&
               (0 <= j) && (j <= size);
    }

    pub fn piece_at(&self, i: i32, j: i32) -> PT {
        if self.is_on_board(i, j) {
            return self.board.0[i as usize][j as usize];
        }
        else {
            return PT::None;
        }
    }

    pub fn set_piece_at(&mut self, i: i32, j: i32, pt: PT) -> bool {
        if self.is_on_board(i, j) {
            self.board.0[i as usize][j as usize] = pt;
            return true;
        }
        else {
            return false;
        }
    }

    pub fn set_board(&mut self, pieces: &[ &[ PT ] ]) -> bool {
        let size = self.variant.size();
        if size != pieces.len() { return false; }

        for i in 0 .. size {
            if size != pieces[ i ].len() {
                return false;
            }
        }

        let mut result = true;
        for i in 0 .. size {
            for j in 0 .. size {
                // Beware: lazy logical "true and ?" --> trying to set all pieces, even if previous failed.
                result = self.set_piece_at(i as i32, j  as i32, pieces[ size - j - 1 ][ i ]) && result;
            }
        }

        return result;
    }

    pub fn flag_at(&self, i: i32, j: i32) -> PF {
        if self.is_on_board(i, j) {
            return self.flags.0[i as usize][j as usize];
        }
        else {
            return PF::None;
        }
    }

    pub fn set_flag_at(&mut self, i: i32, j: i32, flag: PF) -> bool {
        if self.is_on_board(i, j) {
            self.flags.0[i as usize][j as usize] = flag;
            return true;
        }
        else {
            return false;
        }
    }

    pub fn set_flags(&mut self, flags: &[ &[ PF ] ]) -> bool {
        let size = self.variant.size();
        if size != flags.len() { return false; }

        for i in 0 .. size {
            if size != flags[ i ].len() {
                return false;
            }
        }

        let mut result = true;
        for i in 0 .. size {
            for j in 0 .. size {
                // Beware: lazy logical "true and ?" --> trying to set all flags, even if previous failed.
                result = self.set_flag_at(i as i32, j  as i32, flags[ size - j - 1 ][ i ]) && result;
            }
        }

        return result;
    }

}
