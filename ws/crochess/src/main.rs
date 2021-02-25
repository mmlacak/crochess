
use libcrochess as libcc;
use libcrochess::piece_type as pt;
use libcrochess::piece_type::PieceType as PT;
use libcrochess::board_type as bt;
use libcrochess::board_type::BoardType as BT;
use libcrochess::board as b;

fn main() {
    println!("Hello, world!");

    let lT = PT::LightStarchild; // PT::from_symbol('T', true);
    libcc::dbg( &lT );
    libcc::dbg( &lT.label() );
    libcc::dbg( &lT.symbol() );

    let dT = lT.opposite();
    libcc::dbg( &dT );
    libcc::dbg( &dT.label() );
    libcc::dbg( &dT.symbol() );

    let aoa = BT::AgeOfAquarius;
    libcc::dbg( &aoa );
    libcc::dbg( &aoa.label() );

    let mut b = b::Board { variant: BT::Discovery,
                           chessboard: Box::new([ Box::new([ PT::LightPawn, PT::LightKing, PT::None ]),
                                                  Box::new([ PT::None, PT::DarkKnight, PT::None ]),
                                                  Box::new([ PT::DarkKing, PT::None, PT::DarkBishop ]) ]) };
    // let mut cs = [ & mut [ PT::LightPawn, PT::LightKing, PT::None ],
    //                & mut [ PT::None, PT::DarkKnight, PT::None ],
    //                & mut [ PT::DarkKing, PT::None, PT::DarkBishop ] ];
    // let b = b::Board { variant: BT::Discovery,
    //                    chessboard: &[cs] };
    libcc::dbg( &b );
    libcc::dbg( &b.variant.size() );
    libcc::dbgv( &b );
    libcc::dbg( &b.variant.label() );

    b.chessboard[1][1] = PT::DarkMonolith;
    libcc::dbg( &b );
    libcc::dbgv( &b );


    let mut bb = b::Board::new(BT::CroatianTies);
    libcc::dbg( &bb );
    libcc::dbg( &bb.variant.size() );
    libcc::dbgv( &bb );
    libcc::dbg( &bb.variant.label() );

    bb.chessboard[3][5] = PT::DarkMonolith;
    libcc::dbg( &bb );
    libcc::dbgv( &bb );
}
