// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

use std::io;
use std::io::Write;

use libcrochess as libcc;
// use libcrochess::piece_type as pt;
use libcrochess::piece_type::PieceType as PT;
// use libcrochess::board_type as bt;
use libcrochess::board_type::BoardType as BT;
use libcrochess::board as b;

fn main() {
    // println!("Hello, world!");

    // #[allow(non_snake_case)]
    // let lT = PT::BrightStar; // PT::from_symbol('T', true);
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

    // b2.chessboard[1][1] = PT::Monolith;
    // libcc::dbgv( &b2.is_on_chessboard(1, 1) );
    // libcc::dbgv( &b2.is_on_chessboard(11, 11) );
    // libcc::dbgv( &b2.piece_at(1, 1) );
    b2.set_piece_at(7, 2, PT::Monolith);
    b2.set_piece_at(1, 3, PT::DarkKing);
    b2.set_piece_at(5, 2, PT::LightQueen);
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

    // bb.chessboard[3][5] = PT::Monolith;
    // libcc::dbgv( &b2.is_on_chessboard(3, 5) );
    // libcc::dbgv( &b2.is_on_chessboard(-3, -5) );
    // libcc::dbgv( &bb.piece_at(3, 5) );
    bb.set_piece_at(3, 5, PT::Monolith);
    bb.set_piece_at(7, 4, PT::DimStar);
    bb.set_piece_at(11, 11, PT::BrightStar);
    // libcc::dbgv( &bb.piece_at(3, 5) );
    // libcc::dbgv( &bb );
    println!( "{}", bb );


    let mut board = b::Board::new(BT::One);

    loop {
        let mut input = String::new();

        print!( "> " );
        io::stdout().flush();
        io::stdin().read_line(&mut input);

        let args: Vec<&str> = input.trim().split_whitespace().collect();
        if args.len() > 0 {
            let cmd = args[ 0 ];

            match cmd {
                "q" | "quit" => { break; }
                "d" | "display" => { println!( "{}", board ); }
                "n" | "new" => {
                    if args.len() > 1 {
                        let code = args[ 1 ];
                        let b_t = BT::from_str( code );
                        match b_t {
                            Some(bt) => board = b::Board::new( bt ),
                            None => { println!( "Unrecognized code: {}

Use following code for new variant game:
cc  -> Classical
ct  -> Croatian Ties
ma  -> Mayan Ascendancy
aoa -> Age Of Aquarius
mv  -> Miranda's Veil
n   -> Nineteen
hd  -> Hemera's Dawn
tr  -> Tamoanchan Revisited
cot -> Conquest Of Tlalocan
d   -> Discovery
o   -> One
", code ); }
                        };
                    }
                    else {
                        board = b::Board::new( board.variant() );
                    }

                    println!( "{}", board );
                }
                "h" | "help" | "?" => {
                    println!( "Croatian chess - console application
Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com.
Licensed under 3-clause (modified) BSD license. Use a(bout) command for details.

Based on book 'Croatian chess and other variants', by Mario Mlačak.

Commands:
h, help       - prints this screen
a, about      - prints about info
v, version    - prints version(s) info
q, quit       - quits program
d, display    - display current positions
* i, info     - display list of all moves played, time
* t, time     - (re)sets time counter(s)
n, new        - starts new game, keeps variant
                to change variant use code from table below, e.g. n ct
* p, players  - sets up players
                takes two parameters, both are one of 'bot', 'human'
* m, move     - moves piece(s)
                takes notation as argument, e.g. m Nc3
* s, save     - saves current game into PGN file
                takes <path> as argument, e.g. s my_new_game.pgn
* l, load     - loads game/positions from PGN file
                takes <path> as argument, e.g. l my_new_game.pgn

Commands marked with * are not currently implemented.

Supported variants (use code as argument to 'new' command):
cc  -> Classical
ct  -> Croatian Ties
ma  -> Mayan Ascendancy
aoa -> Age Of Aquarius
mv  -> Miranda's Veil
n   -> Nineteen
hd  -> Hemera's Dawn
tr  -> Tamoanchan Revisited
cot -> Conquest Of Tlalocan
d   -> Discovery
o   -> One" );
                }
                "a" | "about" => {
                    println!( "Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived
   from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
\"AS IS\" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE." );
                }
                "v" | "version" => {
                    println!( "Croatian chess - console application
Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com.
Licensed under 3-clause (modified) BSD license. Use a(bout) command for details.

2021-2-27: ver. 0.1.0.945
Initial public hosting, more for backup than for public useage." );
                }
                _ => { println!("Unrecognized: {}", input.trim()); }
            }
        }

        // println!("You typed: {}", input.trim());

        // for s in args.iter() {
        //     println!("You typed: {}", s);
        // }
    }
}
