// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdio.h>

#include "hlp_msgs.h"

void print_license_intro()
{
    printf( "Croatian chess - chess variants tests application\n"
            "Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com.\n" );
    // fflush( stdout );
}

void print_version_info( char const * restrict lib_ver, char const * restrict app_ver )
{
    printf( "Library: %s\n", lib_ver );
    printf( "Application: %s\n", app_ver );
    // fflush( stdout );
}

void print_app_intro( char const * restrict lib_ver, char const * restrict app_ver )
{
    print_license_intro();

    printf( "Licensed under GNU GPL v3+ license. Use `about` for details.\n"
            "\n"
            "Versions:\n" );

    print_version_info(lib_ver, app_ver);

    printf( "\n" );

    // fflush( stdout );
}

void print_licence_text()
{
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

void print_about_info()
{
    print_license_intro();
    print_licence_text();
}
