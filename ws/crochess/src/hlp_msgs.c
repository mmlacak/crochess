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

void print_version_info(char const * restrict lib_ver, char const * restrict app_ver)
{
    printf( "Library: %s\n", lib_ver );
    printf( "Application: %s\n", app_ver );
    // fflush( stdout );
}

void print_app_intro(char const * restrict lib_ver, char const * restrict app_ver)
{
    print_license_intro();

    printf( "Licensed under 3-clause (modified) BSD license. Use `a(bout)` for details.\n"
            "\n"
            "Based on book \"Croatian chess and other variants\", by Mario Mlačak.\n"
            "\n"
            "Versions:\n" );

    print_version_info(lib_ver, app_ver);

    printf( "\nUse `h(elp)` for command list, `h(elp) cmd` for detailed info.\n"
            "\n" );

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

void print_help_quit()
{
    printf( "Quits application, any unsaved progress is lost.\n" );
}

void print_help_display()
{
    printf( "Displays current position, light player is positioned at the bottom,\n"
            "dark player is positioned at the top of the chessboard.\n"
            "\n"
            "Light pieces are printed as upper-case, dark pieces are printed\n"
            "as lower-case symbols.\n" );
}

void print_help_tags()
{
    printf( "Displays current tags.\n"
            "Tags refer to pieces located at the same position as tag.\n"
            "\n"
            "R -> Pawn can rush\n"
            "C -> Rooks, Kings can castle\n"
            "P -> Pawn tagged for promotion\n" );
}

void print_help_about()
{
    printf( "Displays copyright, license info.\n" );
}

void print_help_version()
{
    printf( "Displays versions of application, library; currently they are the same.\n"
            "Version has <major>.<minor>[.<feature>[.<commit>]] numbers, optionally with\n"
            "[-<prerelease>][+<meta>][~<breakage>] info; <meta> is used regularly.\n"
            "\n"
            "<meta> is compressed UTC <date>.<time> format, comparable to version found\n"
            "in the book. For details, see Natural Versioning 1.0, at:\n"
            "https://croatian-chess.blogspot.com/p/natver.html\n" );
}

void print_help_new_code()
{
    printf( "cc  -> Classical\n"
            "ct  -> Croatian Ties\n"
            "ma  -> Mayan Ascendancy\n"
            "aoa -> Age Of Aquarius\n"
            "mv  -> Miranda's Veil\n"
            "n   -> Nineteen\n"
            "hd  -> Hemera's Dawn\n"
            "tr  -> Tamoanchan Revisited\n"
            "cot -> Conquest Of Tlalocan\n"
            "d   -> Discovery\n"
            "o   -> One-n" );
}

void print_help_new()
{
    printf( "Starts new game, in the same variant as the last one.\n"
            "To change variant, use code below as argument, e.g. `n(ew) ct`:\n\n" );

    print_help_new_code();
}

void print_new_code_invalid( char const * restrict str )
{
    if ( str ) printf( "Unrecognized code: '%s'.\n", str );

    printf( "\nUse following code for new variant game:\n" );
    print_help_new_code();
}


// pub fn print_new_code_invalid(code: &str) {
//     print!( "
// Unrecognized code: {}

// Use following code for new variant game:
// ", code );
//     print_help_new_code();
// }
