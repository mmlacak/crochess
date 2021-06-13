// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdio.h>

#include "hlp_msgs.h"

void print_license_intro()
{
    printf( "Croatian chess - tests application\n"
            "Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com.\n" );
    // fflush( stdout );
}

void print_version_info(char const * const restrict lib_ver, char const * const restrict app_ver)
{
    printf( "Library: %s\n", lib_ver );
    printf( "Application: %s\n", app_ver );
    // fflush( stdout );
}

void print_app_intro(char const * const restrict lib_ver, char const * const restrict app_ver)
{
    print_license_intro();

    printf( "Licensed under 3-clause (modified) BSD license. Use `about` for details.\n"
            "\n"
            "Versions:\n" );

    print_version_info(lib_ver, app_ver);

    printf( "\n" );

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
            "contributors may ve used to endorse or promote products derived\n"
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
