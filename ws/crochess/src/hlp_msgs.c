// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdio.h>

#include "cc_str_utils.h"

#include "hlp_msgs.h"

void print_license_intro( void ) {
    printf( "Croatian chess - chess variants console application\n"
            "Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com.\n"
            "Repository: https://github.com/mmlacak/crochess.\n" );
    // fflush( stdout );
}

void print_version_info( char const * restrict lib_ver, char const * restrict app_ver ) {
    printf( "Library: %s\n", lib_ver );
    printf( "Application: %s\n", app_ver );
    // fflush( stdout );
}

void print_app_intro( char const * restrict lib_ver, char const * restrict app_ver ) {
    print_license_intro();

    printf( "Licensed under GNU GPL v3+ license. Use `about` for details.\n"
            "\n"
            "Based on book \"Croatian chess and other variants\", by Mario Mlačak.\n"
            "\n"
            "Versions:\n" );

    print_version_info(lib_ver, app_ver);

    printf( "\nUse `help` for command list, `help <cmd>` for detailed info.\n"
            "\n" );

    // fflush( stdout );
}

void print_licence_text( void ) {
    printf( "\n"
            "This program is free software: you can redistribute it and/or modify\n"
            "it under the terms of the GNU General Public License as published by\n"
            "the Free Software Foundation, either version 3 of the License, or\n"
            "(at your option) any later version.\n"
            "\n"
            "This program is distributed in the hope that it will be useful,\n"
            "but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
            "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n"
            "GNU General Public License for more details.\n"
            "\n"
            "You should have received a copy of the GNU General Public License\n"
            "along with this program.  If not, see <https://www.gnu.org/licenses/>.\n" );
    // fflush( stdout );
}

#ifdef __WITH_LINE_NOISE__
void print_linenoise_intro_text( void ) {
    printf( "\n"
            "-----------------------------------------------------------------\n"
            "\n"
            "Optional dependency is used: linenoise, obtained from:\n"
            "https://github.com/antirez/linenoise.\n"
            "\n"
            "Linenoise - A minimal, zero-config, BSD licensed, readline replacement used in\n"
            "Redis, MongoDB, Android and many other projects.\n" );
    // fflush( stdout );
}

void print_linenoise_licence_text( void ) {
    printf( "\n"
            "Copyright (c) 2010-2014, Salvatore Sanfilippo <antirez at gmail dot com>\n"
            "Copyright (c) 2010-2013, Pieter Noordhuis <pcnoordhuis at gmail dot com>\n"
            "\n"
            "All rights reserved.\n"
            "\n"
            "Redistribution and use in source and binary forms, with or without\n"
            "modification, are permitted provided that the following conditions are met:\n"
            "\n"
            "* Redistributions of source code must retain the above copyright notice,\n"
            "  this list of conditions and the following disclaimer.\n"
            "\n"
            "* Redistributions in binary form must reproduce the above copyright notice,\n"
            "  this list of conditions and the following disclaimer in the documentation\n"
            "  and/or other materials provided with the distribution.\n"
            "\n"
            "THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS \"AS IS\" AND\n"
            "ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED\n"
            "WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE\n"
            "DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR\n"
            "ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES\n"
            "(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;\n"
            "LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON\n"
            "ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT\n"
            "(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS\n"
            "SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.\n" );
    // fflush( stdout );
}
#endif // __WITH_LINE_NOISE__

void print_about_info( void ) {
    print_license_intro();
    print_licence_text();

#ifdef __WITH_LINE_NOISE__
    print_linenoise_intro_text();
    print_linenoise_licence_text();
#endif // __WITH_LINE_NOISE__
}

void print_help( void ) {
    printf( "Commands:\n"
            "h, help       - prints this screen, `help <cmd>` for command details\n"
            "                e.g. `help tags` prints help about `tags` command\n"
            "about         - prints about, license info\n"
            "version       - prints version(s) info\n"
            "quit          - quits program\n"
            "d, display    - displays current position\n"
            "t, tags       - displays current tags\n"
            "* i, info     - displays list of all moves played, time\n"
            "* t, time     - (re)sets time counter(s)\n"
            "new           - starts new game, keeps current variant\n"
            "                to change variant specify code, e.g. `new aoa`\n"
            "                to see all suported codes type `help new`\n"
            "* p, players  - sets up players\n"
            "                takes two parameters, both are one of `bot`, `human`\n"
            "* m, move     - moves piece(s)\n"
            "                takes notation as argument, e.g. `move Nc3`\n"
            "* s, save     - saves current game into PGN file\n"
            "                takes <path> as argument, e.g. `save my_new_game.pgn`\n"
            "* l, load     - loads game/positions from PGN file\n"
            "                takes <path> as argument, e.g. `load my_new_game.pgn`\n"
            "\n"
            "Commands marked with * are currently not implemented.\n" );
}

void print_help_quit( void ) {
    printf( "Quits application, any unsaved progress is lost.\n" );
}

void print_help_display( void ) {
    printf( "Displays current position, light player is positioned at the bottom,\n"
            "dark player is positioned at the top of the chessboard.\n"
            "\n"
            "Light pieces are printed as upper-case, dark pieces are printed\n"
            "as lower-case symbols.\n" );
}

void print_help_tags( void ) {
    printf( "Displays current tags.\n"
            "Tags refer to pieces located at the same position as tag.\n"
            "\n"
            "R -> Pawn can rush\n"
            "C -> Rooks, Kings can castle\n"
            "P -> Pawn tagged for promotion\n" );
}

void print_help_about( void ) {
    printf( "Displays copyright, license info.\n" );
}

void print_help_version( void ) {
    printf( "Displays versions of application, library; currently they are the same.\n"
            "Version has <major>.<minor>[.<feature>[.<commit>]] numbers, optionally with\n"
            "[-<prerelease>][+<meta>][~<breakage>] info; <meta> is used regularly.\n"
            "\n"
            "<meta> is compressed UTC <date>.<time> format, comparable to version found\n"
            "in the book. For details, see Natural Versioning 1.2, at:\n"
            "https://croatian-chess.blogspot.com/p/natver.html\n" );
}

void print_help_new_code( void ) {
    printf( "cc  -> Classical Chess\n"
            "ct  -> Croatian Ties\n"
            "ma  -> Mayan Ascendancy\n"
            "aoa -> Age Of Aquarius\n"
            "mv  -> Miranda's Veil\n"
            "n   -> Nineteen\n"
            "hd  -> Hemera's Dawn\n"
            "tr  -> Tamoanchan Revisited\n"
            "cot -> Conquest Of Tlalocan\n"
            "d   -> Discovery\n"
            "o   -> One\n" );
}

void print_help_new( void ) {
    printf( "Starts new game, in the same variant as the last one.\n"
            "To change variant, use code below as argument, e.g. `new ct`:\n\n" );

    print_help_new_code();
}

void print_new_code_invalid( char const * restrict str,
                             size_t max_len__d ) {
    if ( str ) {
        printf( "Unrecognized code: '" );

        if ( max_len__d == CC_MAX_LEN_ZERO_TERMINATED )
            printf( "%s", str );
        else {
            for ( size_t i = 0; i < max_len__d; ++i )
                printf( "%c", *str++ );
        }

        printf( "'.\n" );
    }

    printf( "\nUse following code for new variant game:\n" );
    print_help_new_code();
    printf( "\ne.g. use `new aoa` to play \"Age Of Aquarius\" variant.\n" );
}
