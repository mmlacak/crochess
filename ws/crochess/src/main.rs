// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

use libcrochess as libcc;
// use libcrochess::piece_type as pt;
use libcrochess::piece_type::PieceType as PT;
// use libcrochess::board_type as bt;
use libcrochess::board_type::BoardType as BT;
use libcrochess::board as b;

fn main() {
    // println!("Hello, world!");

    // #[allow(non_snake_case)]
    // let lT = PT::LightStar; // PT::from_symbol('T', true);
    // libcc::dbg( &lT );
    // libcc::dbg( &lT.label() );
    // libcc::dbg( &lT.symbol() );

    // #[allow(non_snake_case)]
    // let dT = lT.opposite();
    // libcc::dbg( &dT );
    // libcc::dbg( &dT.label() );
    // libcc::dbg( &dT.symbol() );

    // let aoa = BT::AgeOfAquarius;
    // libcc::dbg( &aoa );
    // libcc::dbg( &aoa.label() );

    // let mut b2 = b::Board { variant: BT::Discovery,
    //                         chessboard: Box::new([ Box::new([ PT::LightPawn, PT::LightKing, PT::None ]),
    //                                                Box::new([ PT::None, PT::DarkKnight, PT::None ]),
    //                                                Box::new([ PT::DarkKing, PT::None, PT::DarkBishop ]) ]) };
    let mut b2 = b::Board::new(BT::ClassicalChess);
    libcc::dbg( &b2.variant().label() );
    // libcc::dbg( &b2.variant().size() );
    // println!( "{}", b2 );
    // println!( "{}", b2.chessboard() );
    // libcc::dbgv( &b2 );
    println!( "{}", b2 );

    // b2.chessboard[1][1] = PT::DarkMonolith;
    // libcc::dbgv( &b2.is_on_chessboard(1, 1) );
    // libcc::dbgv( &b2.is_on_chessboard(11, 11) );
    // libcc::dbgv( &b2.piece_at(1, 1) );
    b2.set_piece_at(7, 2, PT::DarkMonolith);
    // libcc::dbgv( &b2.piece_at(1, 1) );
    // libcc::dbgv( &b2 );
    println!( "{}", b2 );

    let mut bb = b::Board::new(BT::One);
    libcc::dbg( &bb.variant().label() );
    // libcc::dbg( &bb.variant().size() );
    // println!( "{}", bb );
    // println!( "{}", bb.chessboard() );
    // libcc::dbgv( &bb );
    println!( "{}", bb );

    // bb.chessboard[3][5] = PT::DarkMonolith;
    // libcc::dbgv( &b2.is_on_chessboard(3, 5) );
    // libcc::dbgv( &b2.is_on_chessboard(-3, -5) );
    // libcc::dbgv( &bb.piece_at(3, 5) );
    bb.set_piece_at(3, 5, PT::DarkMonolith);
    // libcc::dbgv( &bb.piece_at(3, 5) );
    // libcc::dbgv( &bb );
    println!( "{}", bb );
}
