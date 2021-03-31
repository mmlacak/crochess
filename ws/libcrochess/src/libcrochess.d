// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.


export immutable LIB_VERSION = "0.0.0.1-prebuild+20210330.105821"; // source-new-lib-version-major-minor-feature-commit+meta~breaks-place-marker


import io = std.stdio;

export void test_lib() {
    io.writeln("Hello, library world!");
}
