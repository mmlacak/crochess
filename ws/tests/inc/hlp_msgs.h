// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __HLP_MSGS_H__
#define __HLP_MSGS_H__

void print_license_intro( void );
void print_version_info( char const * restrict lib_ver, char const * restrict app_ver );
void print_app_intro( char const * restrict lib_ver, char const * restrict app_ver );
void print_licence_text( void );

#ifdef __WITH_LINE_NOISE__
void print_linenoise_intro_text( void );
void print_linenoise_licence_text( void );
#endif // __WITH_LINE_NOISE__

void print_about_info( void );

void print_help( void );
void print_help_quit( void );
void print_help_display( void );
void print_help_tags( void );
void print_help_about( void );
void print_help_version( void );
void print_help_new_code( void );
void print_help_new( void );
void print_new_code_invalid( char const * restrict str,
                             size_t max_len__d );

void print_help_clear( void );
void print_help_update( void );
void print_help_setup( void );

#endif /* __HLP_MSGS_H__ */
