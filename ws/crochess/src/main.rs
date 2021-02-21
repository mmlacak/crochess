
use libcrochess;
use libcrochess::piece_type::piece_type;
use libcrochess::board_type::board_type;

fn main() {
    println!("Hello, world!");

    // let pt = libcrochess::piece_type::piece_type::PieceType::LightStarchild; // piece_type::PieceType::Starchild;
    let pt = piece_type::PieceType::LightStarchild; // piece_type::PieceType::Starchild;
    // println!("{:?}", pt);
    libcrochess::dbg( &pt );
    libcrochess::dbg( &piece_type::label(pt) );
    libcrochess::dbg( &piece_type::symbol(pt) );

    let o = piece_type::opposite(pt);
    libcrochess::dbg( &o );
    libcrochess::dbg( &piece_type::label(o) );
    libcrochess::dbg( &piece_type::symbol(o) );

    let bt = board_type::BoardType::AgeOfAquarius;
    libcrochess::dbg( &bt );
    libcrochess::dbg( &board_type::label(bt) );
}
