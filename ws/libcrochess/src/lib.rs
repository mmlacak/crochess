// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

pub mod piece_type;
pub mod board_type;
pub mod board;
pub mod setup_board;
pub mod piece_flag;
pub mod setup_flags;
pub mod rules_flags;
pub mod rules;

// fn dbg(x: &impl std::fmt::Debug) {
//     println!("{:?}", &x);
// }

// fn dbgv(x: &impl std::fmt::Debug) {
//     println!("{:#?}", x);
// }

pub fn dbg<T: std::fmt::Debug>(x: &T) {
    println!("{:?}", &x);
}

pub fn dbgv<T: std::fmt::Debug>(x: &T) {
    println!("{:#?}", x);
}

pub fn dbg_<T: std::fmt::Debug>(x: &T) {
    print!("{:?}", &x);
}



#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
