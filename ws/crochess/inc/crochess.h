// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CROCHESS_H__
#define __CROCHESS_H__


extern char const CROCHESS_VERSION[];

#ifdef __WITH_LINE_NOISE__
extern char const CROCHESS_HISTORY_FILE_NAME[];
#define CROCHESS_HISTORY_LENGTH (1000)
#endif // __WITH_LINE_NOISE__


bool cc_move_print_all( CcMove * restrict moves );


int main(void);


#endif /* __CROCHESS_H__ */
