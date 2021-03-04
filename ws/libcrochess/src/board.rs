// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

use std::fmt;

// use crate::piece_type as pt;
use crate::piece_type::PieceType as PT;

use crate::board_type as bt;
use crate::board_type::BoardType as BT;


pub fn is_field_light(i: i32, j: i32) -> bool {
    return (i + j) % 2 == 0;
}


#[derive(Debug, Clone)]
pub struct Board(pub Box<[ Box<[ PT ]> ]>);


impl fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let size = self.0.len();
        let len = 2 * size + 1;
        let divider = "-".to_string().repeat(len);
        let mut files = "".to_string();

        // Ranges don't include upper bound; '{'  is char positioned immediately after 'z' in ASCII table.
        for (i, c) in ('a' .. '{').enumerate() {
            if i >= size { break; }
            files += format!(" {}", c).as_str();
        }

        write!(f, "    {}\n", files) ?;
        write!(f, "    {}\n", divider) ?;

        for i in 0 .. size {
            let row = size - i;
            write!(f, "{:2} |", row) ?;

            for j in 0 .. size {
                let p = self.0[ j ][ size - i - 1 ];
                if p != PT::None {
                    write!(f, " {}", p) ?;
                }
                else {
                    if is_field_light(i as i32, j as i32) {
                        write!(f, " .") ?;
                    }
                    else {
                        write!(f, " ,") ?;
                    }
                }
            }

            write!(f, " | {:2}\n", row) ?;
        }

        write!(f, "    {}\n", divider) ?;
        write!(f, "    {}", files) ?;

        return Ok(());
    }
}


pub fn new_chessboard(board_type: bt::BoardType) -> Board {

    use crate::piece_type::PieceType::None as n;

    fn new_cc_chessboard() -> Board {
        return Board( Box::new( [
            Box::new( [ n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n ] ),
        ] ) );
    }

    fn new_ct_chessboard() -> Board {
        return Board( Box::new( [
            Box::new( [ n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n ] ),
        ] ) );
    }

    fn new_ma_chessboard() -> Board {
        return Board( Box::new( [
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n ] ),
        ] ) );
    }

    fn new_aoa_chessboard() -> Board {
        return Board( Box::new( [
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
        ] ) );
    }

    fn new_mv_chessboard() -> Board {
        return Board( Box::new( [
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
        ] ) );
    }

    fn new_n_chessboard() -> Board {
        return Board( Box::new( [
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
        ] ) );
    }

    fn new_hd_chessboard() -> Board {
        return Board( Box::new( [
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
        ] ) );
    }

    fn new_tr_chessboard() -> Board {
        return Board( Box::new( [
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
        ] ) );
    }

    fn new_cot_chessboard() -> Board {
        return Board( Box::new( [
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
        ] ) );
    }

    fn new_d_chessboard() -> Board {
        return Board( Box::new( [
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
        ] ) );
    }

    fn new_o_chessboard() -> Board {
        return Board( Box::new( [
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
            Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
        ] ) );
    }

    return match board_type {
        BT::ClassicalChess => new_cc_chessboard(),
        BT::CroatianTies => new_ct_chessboard(),
        BT::MayanAscendancy => new_ma_chessboard(),
        BT::AgeOfAquarius => new_aoa_chessboard(),
        BT::MirandasVeil => new_mv_chessboard(),
        BT::Nineteen => new_n_chessboard(),
        BT::HemerasDawn => new_hd_chessboard(),
        BT::TamoanchanRevisited => new_tr_chessboard(),
        BT::ConquestOfTlalocan => new_cot_chessboard(),
        BT::Discovery => new_d_chessboard(),
        BT::One => new_o_chessboard(),
    };
}
