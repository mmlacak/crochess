// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

use std::fmt;

// use crate::piece_type as pt;
use crate::piece_type::PieceType as PT;

use crate::board_type as bt;
use crate::board_type::BoardType as BT;

use crate::board as b;
// use crate::board::Board;

use crate::setup_board as sb;

use crate::piece_flag as pf;
use crate::piece_flag::PieceFlag as PF;

use crate::rules_flags as rf;
// use crate::rules_flags::Flags;

use crate::setup_flags as sf;


#[derive(Debug, Clone)]
pub struct Rules {
    // board: b::Board,
    variant: BT,
    chessboard: b::Chessboard,
    flags: rf::Flags,
}


impl Rules {

    pub fn new(board_type: bt::BoardType, do_initial_setup: bool) -> Rules {
        let cb = b::new_chessboard(board_type);
        let fs = rf::new_flags(board_type);

        let mut rules = Rules { variant: board_type,
                                chessboard: cb,
                                flags: fs };

        if do_initial_setup {
            let cb = sb::new_setup(board_type);
            rules.chessboard.set_board(cb);

            let f = sf::new_setup_rules(board_type);
            rules.set_flags(f);
        }

        return rules;
    }

    pub fn variant(&self) -> BT {
        return self.variant;
    }

    pub fn chessboard(&self) -> &b::Chessboard {
        return &self.chessboard;
    }

    // pub fn board(&self) -> &b::Board {
    //     return &self.board;
    // }

    pub fn flags(&self) -> &rf::Flags {
        return &self.flags;
    }

    pub fn flag_at(&self, i: i32, j: i32) -> PF {
        if self.chessboard.is_on_board(i, j) {
            return self.flags.0[i as usize][j as usize];
        }
        else {
            return PF::None;
        }
    }

    pub fn set_flag_at(&mut self, i: i32, j: i32, flag: PF) -> bool {
        if self.chessboard.is_on_board(i, j) {
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
                // beware: lazy logical and
                result = self.set_flag_at(i as i32, j  as i32, flags[ size - j - 1 ][ i ]) && result;
            }
        }

        return result;
    }

}


impl fmt::Display for Rules {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let size = self.variant().size();
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
