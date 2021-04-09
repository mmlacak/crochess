// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdio.h>

#include "hlp_msgs.h"

void print_license_intro()
{
    printf( "Croatian chess - console application\n"
            "Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com.\n" );
    // fflush( stdout );
}

void print_app_intro()
{
    print_license_intro();

    printf( "Licensed under 3-clause (modified) BSD license. Use `a(bout)` for details.\n"
            "\n"
            "Based on book \"Croatian chess and other variants\", by Mario Mlačak.\n"
            "\n"
            "Use `h(elp)` for command list, `h(elp) cmd` for detailed info.\n"
            "\n" );

    // fflush( stdout );
}

void print_version_info(char const * restrict lib_ver, char const * restrict app_ver)
{
    printf( "Library: %s\n", lib_ver );
    printf( "Application: %s\n", app_ver );
    // fflush( stdout );
}

void print_licence_text()
{
    printf( "All rights reserved.\n"
            "\n"
            "Redistribution and use in source and binary forms, with or without\n"
            "modification, are permitted provided that the following conditions\n"
            "are met:\n"
            "\n"
            "1. Redistributions of source code must retain the above copyright\n"
            "notice, this list of conditions and the following disclaimer.\n"
            "\n"
            "2. Redistributions in binary form must reproduce the above copyright\n"
            "notice, this list of conditions and the following disclaimer in the\n"
            "documentation and/or other materials provided with the distribution.\n"
            "\n"
            "3. Neither the name of the copyright holder nor the names of its\n"
            "contributors may be used to endorse or promote products derived\n"
            "from this software without specific prior written permission.\n"
            "\n"
            "THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS\n"
            "\"AS IS\" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT\n"
            "LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR\n"
            "A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT\n"
            "HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,\n"
            "SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT\n"
            "LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,\n"
            "DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY\n"
            "THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT\n"
            "(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE\n"
            "OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.\n" );
    // fflush( stdout );
}

void print_about_info()
{
    print_license_intro();
    print_licence_text();
}

void print_help()
{
    printf( "Commands:\n"
            "h, help       - prints this screen, `h(elp) cmd` for command details\n"
            "a, about      - prints about, license info\n"
            "v, version    - prints version(s) info\n"
            "q, quit       - quits program\n"
            "d, display    - displays current position\n"
            "t, tags       - displays current tags\n"
            "* i, info     - displays list of all moves played, time\n"
            "* t, time     - (re)sets time counter(s)\n"
            "n, new        - starts new game, keeps variant\n"
            "                to change variant use code from table below, e.g. `n ct`\n"
            "* p, players  - sets up players\n"
            "                takes two parameters, both are one of `bot`, `human`\n"
            "* m, move     - moves piece(s)\n"
            "                takes notation as argument, e.g. `m Nc3`\n"
            "* s, save     - saves current game into PGN file\n"
            "                takes <path> as argument, e.g. `s my_new_game.pgn`\n"
            "* l, load     - loads game/positions from PGN file\n"
            "                takes <path> as argument, e.g. `l my_new_game.pgn`\n"
            "\n"
            "Commands marked with * are currently not implemented.\n" );
}



// pub fn print_help_quit() {
//     print!( "
// Quits application.

// " );
// }

// pub fn print_help_display() {
//     print!( "
// Displays current position, light player is positioned at bottom,
// dark player is positioned at top of the chessboard.

// Light pieces are printed as upper-case, dark pieces are printed
// as lower-case symbols.

// " );
// }

// pub fn print_help_tags() {
//     print!( "
// Displays current tags.
// Tags refer to pieces located at the same position as tag.

// R -> Pawn can rush
// C -> Rooks, Kings can castle
// P -> Pawn tagged for promotion

// " );
// }

// pub fn print_help_about() {
//     print!( "
// Displays info, license about application.

// " );
// }

// pub fn print_help_version() {
//     print!( "
// Displays versions of application, library; currently they are the same.
// Version has <major>.<minor>.<patch>[-<prerelease>]{{+<build>}} numbers,
// <prerelease> is used as needed, <build> is used regularly.

// <build> is squished UTC date-time, with year taking 4 digits, and all the
// others 2: <year><month><day><hour><minute><second>, and is comparable to
// version found in the book.

// " );
// }

// pub fn print_help_new_code() {
//     print!( "cc  -> Classical
// ct  -> Croatian Ties
// ma  -> Mayan Ascendancy
// aoa -> Age Of Aquarius
// mv  -> Miranda's Veil
// n   -> Nineteen
// hd  -> Hemera's Dawn
// tr  -> Tamoanchan Revisited
// cot -> Conquest Of Tlalocan
// d   -> Discovery
// o   -> One

// " );
// }

// pub fn print_new_code_invalid(code: &str) {
//     print!( "
// Unrecognized code: {}

// Use following code for new variant game:
// ", code );
//     print_help_new_code();
// }

// pub fn print_help_new() {
//     print!( "
// Starts new game, in the same variant as the last one.
// To change variant, use code below as argument, e.g. `n(ew) ct`:

// " );
//     print_help_new_code();
// }
