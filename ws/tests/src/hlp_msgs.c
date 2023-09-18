// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdio.h>

// #include "cc_defines.h"
#include "cc_str_utils.h"

#include "hlp_msgs.h"

void print_license_intro( void ) {
    printf( "Croatian chess - chess variants tests application\n"
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
            "Versions:\n" );

    print_version_info(lib_ver, app_ver);

    printf( "\n" );

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
