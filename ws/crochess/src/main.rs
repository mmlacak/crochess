// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#![cfg_attr(debug_assertions, allow(dead_code, unused_imports))]


use std::io;
use std::io::Write;

use libcrochess as libcc;

// use libcrochess::piece_type as pt;
use libcrochess::piece_type::PieceType as PT;

// use libcrochess::board_type as bt;
use libcrochess::board_type::BoardType as BT;

// use libcrochess::board as b;
// use libcrochess::piece_flag as pf;
use libcrochess::rules as r;


pub mod hlp_msgs;

use hlp_msgs as hm;


pub const VERSION: &str = "0.1.6+20210314231327"; /* source-new-app-version-major-minor-patch+build-place-marker */


fn main() {
    let mut rules = r::Rules::new( BT::One, true );

    hm::print_intro();

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
                "d" | "display" => { println!( "\n{}\n", rules.board() ); }
                "t" | "tags" => { println!( "\n{}\n", rules.flags() ); }
                "a" | "about" => { hm::print_about(); }
                "v" | "version" => { hm::print_versions(VERSION, libcc::VERSION); }
                "n" | "new" => {
                    let mut do_display = true;

                    if args.len() > 1 {
                        let code = args[ 1 ];
                        let b_t = BT::from_str( code );
                        match b_t {
                            Some(bt) => { rules = r::Rules::new( bt, true ); }
                            None => {
                                do_display = false;
                                hm::print_new_code_invalid( code );
                            }
                        };
                    }
                    else { rules = r::Rules::new( rules.variant(), true ); }

                    if do_display { println!( "\n{}\n", rules.board() ); }
                }
                "m" | "move" => { println!( "\n{}\n", args[ 1 ] ); }
                "h" | "help" | "?" => {
                    if args.len() > 1 {
                        let cmd = args[ 1 ];
                        match cmd {
                            "q" | "quit" => { hm::print_help_quit(); }
                            "d" | "display" => { hm::print_help_display(); }
                            "t" | "tags" => { hm::print_help_tags(); }
                            "a" | "about" => { hm::print_help_about(); }
                            "v" | "version" => { hm::print_help_version(); }
                            "n" | "new" => { hm::print_help_new(); }
                            _ => { println!("Unrecognized: {}", cmd.trim()); }
                        }
                    }
                    else { hm::print_help(); }
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

                    // let cb: &[ &[ PT ] ] = &[
                    //     &[ r, n, b, q, k, b, n, r ],
                    //     &[ p, p, p, p, p, p, p, p ],
                    //     &[ x, x, x, x, x, x, x, x ],
                    //     &[ x, x, x, x, x, x, x, x ],
                    //     &[ x, x, x, x, x, x, x, x ],
                    //     &[ x, x, x, x, x, x, x, x ],
                    //     &[ P, P, P, P, P, P, P, P ],
                    //     &[ R, N, B, Q, K, B, N, R ],
                    // ];
                    let cb: &[ &[ PT ] ] = &[
                        &[ r, n, b, q, k, b, x, r ],
                        &[ p, p, p, p, p, p, p, p ],
                        &[ x, x, x, x, x, n, x, x ],
                        &[ x, x, x, P, x, x, x, x ],
                        &[ x, x, x, P, x, x, x, x ],
                        &[ x, x, x, x, P, x, x, x ],
                        &[ P, P, P, x, x, P, P, P ],
                        &[ R, N, B, Q, K, B, N, R ],
                    ];

                    println!( "Setup: {}.", rules.set_board(cb) );
                    println!( "\n{}\n", rules.board() );
                }

                "y" => {
                }

                _ => { println!("Unrecognized: {}", input.trim()); }
            }
        }
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
    // //                         board: Box::new([ Box::new([ PT::LightPawn, PT::LightKing, PT::None ]),
    // //                                                Box::new([ PT::None, PT::DarkKnight, PT::None ]),
    // //                                                Box::new([ PT::DarkKing, PT::None, PT::DarkBishop ]) ]) };
    // let mut b2 = b::Board::new(BT::ClassicalChess);
    // libcc::dbg( &b2.variant().label() );
    // // libcc::dbg( &b2.variant().size() );
    // // println!( "{}", b2 );
    // // println!( "{}", b2.board() );
    // // libcc::dbgv( &b2 );
    // println!( "{}", b2 );

    // // b2.board[1][1] = PT::Monolith;
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
    // // println!( "{}", bb.board() );
    // // libcc::dbgv( &bb );
    // println!( "{}", bb );

    // // bb.board[3][5] = PT::Monolith;
    // // libcc::dbgv( &b2.is_on_chessboard(3, 5) );
    // // libcc::dbgv( &b2.is_on_chessboard(-3, -5) );
    // // libcc::dbgv( &bb.piece_at(3, 5) );
    // bb.set_piece_at(3, 5, PT::Monolith);
    // bb.set_piece_at(7, 4, PT::DimStar);
    // bb.set_piece_at(11, 11, PT::BrightStar);
    // // libcc::dbgv( &bb.piece_at(3, 5) );
    // // libcc::dbgv( &bb );
    // println!( "{}", bb );
