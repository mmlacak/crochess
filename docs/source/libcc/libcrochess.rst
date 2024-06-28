.. Copyright (c) 2021, 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.

.. include:: ../defines.rst

.. _lbl-libcc-libcrochess-library:

Croatian Chess library
======================

``libcrochess`` is the main library facilitating everything needed to play any
of Croatian Chess variants:

* chessboard, pieces, tags
* formatting output for chessboard position, and algebraic notation
* parsing user input, algebraic notation
* lexer for user input, algebraic notation [1]_
* enforcing rules, timings [1]_
* applying user input, algebraic notation [1]_
* list of moves, undo [1]_
* save and load game [1]_
* export into various files [1]_
* list, select, start bot(s) [1]_

``libcrochess`` does not implement bots on its own. Each bot will be implemented
as separate library using unified interface, to allow for different bots to play
against each other, or a human.

``libcrochess`` is aimed at developers whishing to support Croatian Chess variants.
One needs to do little more than to capture user inputs (e.g. in GUI application),
and show responses from the library. There is an accompanying console application,
``crochess``, which demonstrates just that.

``libcrochess`` is designed to be as portable as possible, so it uses only standard
C library. As such, there is no networking support. It is possible to redirect
``stdin``, ``stdout`` and ``stderr`` into and from crochess application so that
other application, or server can take over networking.

.. _lbl-libcc-libcrochess-library-conditionalcompilation:

Conditional compilation
-----------------------

.. c:macro:: __CC_STR_PRINT_INFO__

    .. todo::

        Move out of library / return newly allocated, formatted string.
        Remove library dependecy on ``<stdio.h>``.

    Controls presence of various debug :c:`printf()`\s and the like in
    the library.

    It's enough if that constant is defined in a command line while
    compiling library, like so:

    .. code-block:: sh
        :force:

        gcc ... -D__CC_STR_PRINT_INFO__ ...


.. _lbl-libcc-libcrochess-library-librarycontents:

Library contents
----------------

.. _lbl-libcc-libcrochess-library-toctree:

.. toctree::
    :maxdepth: 9
    :caption: Contents
    :name: croatian-chess-library-toctree

    terms
    memory
    design
    organization

    ccdefines
    ccmath
    cctags
    ccpiece

    ccstrutils
    ccstrings
    cctoken
    ccversion

.. _lbl-libcc-libcrochess-library-footnotes:

.. rubric:: Footnotes

.. [1] Currently not implemented.
