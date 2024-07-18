.. Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.

.. include:: ../defines.rst

.. _lbl-libcc-concepts:

Concepts
========

Explanation of some concepts, design choices in ``libcrochess``.

.. _lbl-libcc-concepts-validity:

Validity
--------

For :c:`int`\eger values, one is designated to be invalid, all the others are
then considered valid.

This is mostly relevant to coordiantes, various positions, steps, etc. where one
value (in this case, :c:macro:`CC_INVALID_COORD`) is invalid, all the other are
valid, even if a coordinate may be way off-board.

For :c:`enum`\s, first :c:`macro` checks if a given value is enumerated within
the :c:`enum`; this macro is named ``"is enumerator?"``, e.g.
:c:enumerator:`CC_PARSED_STEP_LINK_IS_ENUMERATOR`.

Second :c:`macro` also checks if given value is different from pre-designed
:c:`None` (or :c:`Void`, or :c:`Empty`) enumeration value; this macro is named
``"is valid?"``, e.g. :c:enumerator:`CC_PARSED_STEP_LINK_IS_VALID`.

.. _lbl-libcc-concepts-strings:

Strings
-------

All newly allocated strings are zero-terminated (i.e. they end in ``'\0'``).

Array strings are not necessary zero-terminated even though arrays are zeroed,
since it's possible for a string to be exactly the same size as a holding array.

Always do use array size, to limit processing to array proper.

For strings starting, ending, first and last characters, length, and size are
defined like so::

    start <-------- size ----------> end
      |                               |
      v                               v
    +---+---+---+... ...+---+---+---+---+
    | a | b | c |       | x | y | z | 0 |
    +---+---+---+... ...+---+---+---+---+
      ^                           ^
      |                           |
    first <------ length ------> last

Length of a string counts all of its content, i.e. it includes everything from
first to last :c:`char`, without zero-terminating :c:`char`.

Size of a string counts everything from starting to ending :c:`char`,
including zero-terminating :c:`char`.

..
    Positions
    ---------

    TODO :: about positions, steps

..
    Linked lists
    ------------

    TODO :: about linked lists


.. _lbl-libcc-concepts-designs:

Designs
-------

Describes design of parsing user :term:`AN`.

.. todo::

    This document is placeholder, to fulfill when implementation is done.

.. _lbl-libcc-concepts-designs-anchors:

Anchors
^^^^^^^

An anchor is a chessboard position, which then can serve as a starting
point for parsing :term:`AN`.

.. _lbl-libcc-concepts-designs-anchors-hard:

Hard anchors
""""""""""""

Hard anchor is a chessboard position with either initial setup, or after
any legal move.

.. _lbl-libcc-concepts-designs-anchors-soft:

Soft anchors
""""""""""""

Soft anchor is a chessboard position after a legal ply is applied to it,
while parsing a (cascading) move :term:`AN`.

.. _lbl-libcc-concepts-designs-endpoints:

End-points
^^^^^^^^^^

End-point refers to generated paths, path segments, and other data necessary
to parse next ply :term:`AN`.

.. _lbl-libcc-concepts-designs-endpoints-hard:

Hard end-points
"""""""""""""""

Hard end-points are all generated, complete paths a piece can take at the
beginning of a ply.

It is comprised of list of paths, each path is a list of steps.

It provides the same interface for all pieces, regardless if they choose
one, or two initial steps, or each step independently.

.. _lbl-libcc-concepts-designs-endpoints-soft:

Soft end-points
"""""""""""""""

Soft end-point is generated path segment, which can be common for multiple
paths.

For instance, path segment can be part of a complete path leading a piece
to divergence.
