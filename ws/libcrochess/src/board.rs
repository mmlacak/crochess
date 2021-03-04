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
pub struct Chessboard(pub Box<[ Box<[ PT ]> ]>);

impl Chessboard {

    pub fn new(board_type: bt::BoardType) -> Chessboard {
        let cb = new_chessboard(board_type);
        return cb;
    }

    pub fn size(&self) -> usize {
        return (*self.0).len();
    }

    pub fn is_on_board(&self, i: i32, j: i32) -> bool {
        let size: i32 = self.size() as i32;
        return (0 <= i) && (i <= size) &&
               (0 <= j) && (j <= size);
    }

    pub fn piece_at(&self, i: i32, j: i32) -> PT {
        if self.is_on_board(i, j) {
            return self.0[i as usize][j as usize];
        }
        else {
            return PT::None;
        }
    }

    pub fn set_piece_at(&mut self, i: i32, j: i32, pt: PT) -> bool {
        if self.is_on_board(i, j) {
            self.0[i as usize][j as usize] = pt;
            return true;
        }
        else {
            return false;
        }
    }

    pub fn set_board(&mut self, pieces: &[ &[ PT ] ]) -> bool {
        let size = self.size();
        if size != pieces.len() { return false; }

        for i in 0 .. size {
            if size != pieces[ i ].len() {
                return false;
            }
        }

        let mut result = true;
        for i in 0 .. size {
            for j in 0 .. size {
                // Beware: lazy logical "true and ?"" --> trying to set all pieces, even if previous failed.
                result = self.set_piece_at(i as i32, j  as i32, pieces[ size - j - 1 ][ i ]) && result;
            }
        }

        return result;
    }
}


impl fmt::Display for Chessboard {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let size = self.0.len();

        for i in 0 .. size {
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
            write!(f, "\n") ?;
        }

        return Ok(());
    }
}


// #[derive(Debug, Clone)]
// pub struct Board {
//     variant: BT,
//     chessboard: Chessboard,
// }

// impl Board {

//     pub fn new(board_type: bt::BoardType) -> Board {
//         let cb = new_chessboard(board_type);
//         let b = Board { variant: board_type,
//                         chessboard: cb };
//         return b;
//     }

//     pub fn variant(&self) -> BT {
//         return self.variant;
//     }

//     pub fn chessboard(&self) -> &Chessboard {
//         return &self.chessboard;
//     }

//     pub fn is_on_chessboard(&self, i: i32, j: i32) -> bool {
//         // return (0 <= i) && (i <= (*self.chessboard.0).len() as i32) &&
//         //        (0 <= j) && (j <= (*(*self.chessboard.0)[0]).len() as i32);
//         let size: i32 = self.variant.size() as i32;
//         return (0 <= i) && (i <= size) &&
//                (0 <= j) && (j <= size);
//     }

//     pub fn piece_at(&self, i: i32, j: i32) -> PT {
//         if self.is_on_chessboard(i, j) {
//             return self.chessboard.0[i as usize][j as usize];
//         }
//         else {
//             return PT::None;
//         }
//     }

//     pub fn set_piece_at(&mut self, i: i32, j: i32, pt: PT) -> bool {
//         if self.is_on_chessboard(i, j) {
//             self.chessboard.0[i as usize][j as usize] = pt;
//             return true;
//         }
//         else {
//             return false;
//         }
//     }

//     pub fn set_chessboard(&mut self, pieces: &[ &[ PT ] ]) -> bool {
//         let size = self.variant.size();
//         if size != pieces.len() { return false; }

//         for i in 0 .. size {
//             if size != pieces[ i ].len() {
//                 return false;
//             }
//         }

//         let mut result = true;
//         for i in 0 .. size {
//             for j in 0 .. size {
//                 // beware: lazy logical and
//                 result = self.set_piece_at(i as i32, j  as i32, pieces[ size - j - 1 ][ i ]) && result;
//             }
//         }

//         return result;
//     }

// }


// impl fmt::Display for Board {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         let size = self.variant.size();
//         let len = 2 * size + 1;
//         let divider = "-".to_string().repeat(len);
//         let cb = format!("{}", self.chessboard);
//         let mut files = "".to_string();

//         // Ranges don't include upper bound; '{'  is char positioned after 'z' in ASCII table.
//         for (i, c) in ('a' .. '{').enumerate() {
//             if i >= size { break; }
//             files += format!(" {}", c).as_str();
//         }

//         write!(f, "    {}\n", files) ?;
//         write!(f, "    {}\n", divider) ?;
//         for (i, line_i) in cb.lines().enumerate() {
//             let row = size - i;
//             write!(f, "{:2} |", row) ?;
//             write!(f, "{}", line_i) ?;
//             write!(f, " | {:2}\n", row) ?;
//         }
//         write!(f, "    {}\n", divider) ?;
//         write!(f, "    {}", files) ?;

//         return Ok(());
//     }
// }


pub fn new_chessboard(board_type: bt::BoardType) -> Chessboard {

    use crate::piece_type::PieceType::None as n;

    fn new_cc_chessboard() -> Chessboard {
        return Chessboard( Box::new( [
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

    fn new_ct_chessboard() -> Chessboard {
        return Chessboard( Box::new( [
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

    fn new_ma_chessboard() -> Chessboard {
        return Chessboard( Box::new( [
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

    fn new_aoa_chessboard() -> Chessboard {
        return Chessboard( Box::new( [
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

    fn new_mv_chessboard() -> Chessboard {
        return Chessboard( Box::new( [
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

    fn new_n_chessboard() -> Chessboard {
        return Chessboard( Box::new( [
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

    fn new_hd_chessboard() -> Chessboard {
        return Chessboard( Box::new( [
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

    fn new_tr_chessboard() -> Chessboard {
        return Chessboard( Box::new( [
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

    fn new_cot_chessboard() -> Chessboard {
        return Chessboard( Box::new( [
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

    fn new_d_chessboard() -> Chessboard {
        return Chessboard( Box::new( [
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

    fn new_o_chessboard() -> Chessboard {
        return Chessboard( Box::new( [
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
