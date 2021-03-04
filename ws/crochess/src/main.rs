// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

use std::io;
use std::io::Write;

// use libcrochess as libcc;
// use libcrochess::piece_type as pt;
use libcrochess::piece_type::PieceType as PT;
// use libcrochess::board_type as bt;
use libcrochess::board_type::BoardType as BT;
use libcrochess::board as b;
use libcrochess::piece_flags as pf;
use libcrochess::rules as r;


fn main() {
    // let mut board = b::Board::new( BT::One );
    let mut rules = r::Rules::new( BT::One, true );

    println!( "
Croatian chess - console application
Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com.

Use `h(elp)` for command list, `h(elp) cmd` for detailed info.
    " );

    loop {
        let mut input = String::new();

        print!( "> " );
        io::stdout().flush().unwrap();
        io::stdin().read_line(&mut input).unwrap();

        let args: Vec<&str> = input.trim().split_whitespace().collect();
        if args.len() > 0 {
            let cmd = args[ 0 ];

            match cmd {
                "q" | "quit" => { break; }
                "d" | "display" => { println!( "{}", rules.board() ); }
                "t" | "tags" => { println!( "{}", rules.flags() ); }
                "n" | "new" => {
                    if args.len() > 1 {
                        let code = args[ 1 ];
                        let b_t = BT::from_str( code );
                        match b_t {
                            Some(bt) => {
                                // board = b::Board::new( bt );
                                rules = r::Rules::new( bt, true );
                            }
                            None => { println!( "
Unrecognized code: {}

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
                        // board = b::Board::new( board.variant() );
                        rules = r::Rules::new( rules.board().variant(), true );
                    }

                    // println!( "{}", board );
                    println!( "{}", rules.board() );
                }
                "h" | "help" | "?" => {
                    println!( "
Croatian chess - console application
Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com.
Licensed under 3-clause (modified) BSD license. Use `a(bout)` command for details.

Based on book 'Croatian chess and other variants', by Mario Mlačak.

Commands:
h, help       - prints this screen, `h(elp) cmd` for command details
a, about      - prints about, license info
v, version    - prints version(s) info
q, quit       - quits program
d, display    - displays current position
* t, tags     - displays current tags
* i, info     - displays list of all moves played, time
* t, time     - (re)sets time counter(s)
n, new        - starts new game, keeps variant
                to change variant use code from table below, e.g. `n ct`
* p, players  - sets up players
                takes two parameters, both are one of `bot`, `human`
* m, move     - moves piece(s)
                takes notation as argument, e.g. `m Nc3`
* s, save     - saves current game into PGN file
                takes <path> as argument, e.g. `s my_new_game.pgn`
* l, load     - loads game/positions from PGN file
                takes <path> as argument, e.g. `l my_new_game.pgn`

Commands marked with * are not currently implemented.

Supported variants (use code as argument to `n(ew)` command):
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
                    " );
                }
                "a" | "about" => {
                    println!( "
Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
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
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
                    " );
                }
                "v" | "version" => {
                    println!( "
Croatian chess - console application
Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com.
Licensed under 3-clause (modified) BSD license. Use `a(bout)` command for details.

2021-2-27: ver. 0.1.0.945
Initial public hosting, more for backup than for public useage.
                    " );
                }

                "x" => {

                    use PT::DimStar as t;

                    use PT::DarkStarchild as i;
                    use PT::DarkShaman as h;
                    use PT::DarkSerpent as s;
                    use PT::DarkCentaur as c;
                    use PT::DarkWave as w;
                    use PT::DarkUnicorn as u;
                    use PT::DarkPyramid as a;
                    use PT::DarkPegasus as g;
                    use PT::DarkKing as k;
                    use PT::DarkQueen as q;
                    use PT::DarkRook as r;
                    use PT::DarkBishop as b;
                    use PT::DarkKnight as n;
                    use PT::DarkPawn as p;

                    use PT::None as x;

                    use PT::LightPawn as P;
                    use PT::LightKnight as N;
                    use PT::LightBishop as B;
                    use PT::LightRook as R;
                    use PT::LightQueen as Q;
                    use PT::LightKing as K;
                    use PT::LightPegasus as G;
                    use PT::LightPyramid as A;
                    use PT::LightUnicorn as U;
                    use PT::LightWave as W;
                    use PT::LightCentaur as C;
                    use PT::LightSerpent as S;
                    use PT::LightShaman as H;
                    use PT::LightStarchild as I;

                    use PT::BrightStar as T;

                    use PT::Monolith as M;

                    let cb: &[ &[ PT ] ] = &[
                        &[ r, n, b, q, k, b, n, r ],
                        &[ p, p, p, p, p, p, p, p ],
                        &[ x, x, x, x, x, x, x, x ],
                        &[ x, x, x, x, x, x, x, x ],
                        &[ x, x, x, x, x, x, x, x ],
                        &[ x, x, x, x, x, x, x, x ],
                        &[ P, P, P, P, P, P, P, P ],
                        &[ R, N, B, Q, K, B, N, R ],
                    ];

                    // println!( "Setup: {}.", board.set_chessboard(cb) );
                    // println!( "{}", board );
                    // println!( "Setup: {}.", rules.board().set_chessboard(cb) );
                    println!( "{}", rules.board() );
                }

                "y" => {
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

    // // println!("Hello, world!");

    // // #[allow(non_snake_case)]
    // // let lT = PT::BrightStar; // PT::from_symbol('T', true);
    // // libcc::dbg( &lT );
    // // libcc::dbg( &lT.label() );
    // // libcc::dbg( &lT.symbol() );

    // // #[allow(non_snake_case)]
    // // let dT = lT.opposite();
    // // libcc::dbg( &dT );
    // // libcc::dbg( &dT.label() );
    // // libcc::dbg( &dT.symbol() );

    // // let aoa = BT::AgeOfAquarius;
    // // libcc::dbg( &aoa );
    // // libcc::dbg( &aoa.label() );

    // // let mut b2 = b::Board { variant: BT::Discovery,
    // //                         chessboard: Box::new([ Box::new([ PT::LightPawn, PT::LightKing, PT::None ]),
    // //                                                Box::new([ PT::None, PT::DarkKnight, PT::None ]),
    // //                                                Box::new([ PT::DarkKing, PT::None, PT::DarkBishop ]) ]) };
    // let mut b2 = b::Board::new(BT::ClassicalChess);
    // libcc::dbg( &b2.variant().label() );
    // // libcc::dbg( &b2.variant().size() );
    // // println!( "{}", b2 );
    // // println!( "{}", b2.chessboard() );
    // // libcc::dbgv( &b2 );
    // println!( "{}", b2 );

    // // b2.chessboard[1][1] = PT::Monolith;
    // // libcc::dbgv( &b2.is_on_chessboard(1, 1) );
    // // libcc::dbgv( &b2.is_on_chessboard(11, 11) );
    // // libcc::dbgv( &b2.piece_at(1, 1) );
    // b2.set_piece_at(7, 2, PT::Monolith);
    // b2.set_piece_at(1, 3, PT::DarkKing);
    // b2.set_piece_at(5, 2, PT::LightQueen);
    // // libcc::dbgv( &b2.piece_at(1, 1) );
    // // libcc::dbgv( &b2 );
    // println!( "{}", b2 );

    // let mut bb = b::Board::new(BT::One);
    // libcc::dbg( &bb.variant().label() );
    // // libcc::dbg( &bb.variant().size() );
    // // println!( "{}", bb );
    // // println!( "{}", bb.chessboard() );
    // // libcc::dbgv( &bb );
    // println!( "{}", bb );

    // // bb.chessboard[3][5] = PT::Monolith;
    // // libcc::dbgv( &b2.is_on_chessboard(3, 5) );
    // // libcc::dbgv( &b2.is_on_chessboard(-3, -5) );
    // // libcc::dbgv( &bb.piece_at(3, 5) );
    // bb.set_piece_at(3, 5, PT::Monolith);
    // bb.set_piece_at(7, 4, PT::DimStar);
    // bb.set_piece_at(11, 11, PT::BrightStar);
    // // libcc::dbgv( &bb.piece_at(3, 5) );
    // // libcc::dbgv( &bb );
    // println!( "{}", bb );
