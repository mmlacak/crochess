// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.


export immutable APP_VERSION = "0.0.2.1+20210403.040604"; // source-new-app-version-major-minor-feature-commit+meta~breaks-place-marker


import io = std.stdio;

import lcc = libcrochess;


void main() {
    // io.writeln("Hello, world!");
    lcc.test_lib();

    io.writeln("");
    io.writefln("Library version: %s.", lcc.LIB_VERSION);
    io.writefln("Application version: %s.", APP_VERSION);
}
