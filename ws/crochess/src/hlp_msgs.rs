// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

// use std::io;
// use std::io::Write;

// use libcrochess as libcc;


pub fn print_app_header() {
    print!( "
Croatian chess - console application
Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com." );
}

pub fn print_licensed() {
    print_app_header();
    print!( "
Licensed under 3-clause (modified) BSD license. Use `a(bout)` command for details." );
}

pub fn print_intro() {
    print_licensed();
    print!( "

Based on book 'Croatian chess and other variants', by Mario Mlačak.

Use `h(elp)` for command list, `h(elp) cmd` for detailed info.

" );
}

pub fn print_versions( app_ver: &str, lib_ver: &str ) {
    print!( "
Application: {}
Library: {}

", app_ver, lib_ver );
}

pub fn print_help() {
    print!( "
Commands:
h, help       - prints this screen, `h(elp) cmd` for command details
a, about      - prints about, license info
v, version    - prints version(s) info
q, quit       - quits program
d, display    - displays current position
t, tags       - displays current tags
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

Commands marked with * are currently not implemented.

" );
}

pub fn print_help_quit() {
    print!( "
Quits application.

" );
}

pub fn print_help_display() {
    print!( "
Displays current position, light player is positioned at bottom,
dark player is positioned at top of the chessboard.

Light pieces are printed as upper-case, dark pieces are printed
as lower-case symbols.

" );
}

pub fn print_help_tags() {
    print!( "
Displays current tags.
Tags refer to pieces located at the same position as tag.

R -> Pawn can rush
C -> Rooks, Kings can castle
P -> Pawn tagged for promotion

" );
}

pub fn print_help_about() {
    print!( "
Displays info, license about application.

" );
}

pub fn print_help_version() {
    print!( "
Displays versions of application, library; currently they are the same.
Version has <major>.<minor>.<patch>[-<prerelease>]{{+<build>}} numbers,
<prerelease> is used as needed, <build> is used regularly.

<build> is squished UTC date-time, with year taking 4 digits, and all the
others 2: <year><month><day><hour><minute><second>, and is comparable to
version found in the book.

" );
}

pub fn print_help_new_code() {
    print!( "cc  -> Classical
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

pub fn print_new_code_invalid(code: &str) {
    print!( "
Unrecognized code: {}

Use following code for new variant game:
", code );
    print_help_new_code();
}

pub fn print_help_new() {
    print!( "
Starts new game, in the same variant as the last one.
To change variant, use code below as argument, e.g. `n(ew) ct`:

" );
    print_help_new_code();
}

pub fn print_about() {
    print_app_header();
    print!( "
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

" );}
