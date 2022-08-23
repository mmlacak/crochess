// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CROCHESS_H__
#define __CROCHESS_H__


extern char const CROCHESS_VERSION[];

bool test_move( char const * restrict an_str,
                CcGame * restrict game__io );


int main(void);


#endif /* __CROCHESS_H__ */
