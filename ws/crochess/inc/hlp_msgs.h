// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __HLP_MSGS_H__
#define __HLP_MSGS_H__

void print_license_intro();
void print_version_info(char const * const restrict lib_ver, char const * const restrict app_ver);
void print_app_intro(char const * const restrict lib_ver, char const * const restrict app_ver);
void print_licence_text();
void print_about_info();

void print_help();
void print_help_quit();
void print_help_display();
void print_help_tags();
void print_help_about();
void print_help_version();
void print_help_new_code();
void print_help_new();
void print_new_code_invalid( char const * const restrict str );

#endif /* __HLP_MSGS_H__ */
