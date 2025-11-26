// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __TESTS_MISC_H__
#define __TESTS_MISC_H__


void test_gcd( int x, int y );
void test_pos_step( int i1, int j1, int i2, int j2 );

char * test_str_append_into( char const * buffer,
                             char * start_str__io,
                             char const * end_str__d,
                             size_t size_dest__d,
                             char const * start_sub_str,
                             char const * end_sub_str,
                             size_t max_len__d );


bool tests_gcds( void );
bool tests_pos_steps( void );
bool tests_str_append_into( void );
bool tests_str_len( void );
bool tests_iter_monolith_steps( void );
bool tests_iter_piece_steps( void );
bool tests_pos_desc_link( void );
bool tests_transparencies( void );

bool tests_activation( CcPieceTagType moving,
                       CcPieceTagType encounter );
bool tests_activations( CcPieceTagType moving,
                        CcPieceTagType encounter );

bool tests_misc( int test_number,
                 int moving,
                 int encounter );


#endif /* __TESTS_MISC_H__ */
