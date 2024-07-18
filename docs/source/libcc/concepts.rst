.. Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.

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
the :c:`enum`; this macro is named ``"is enumerator?"``.

Second :c:`macro` also checks if given value is different from pre-designed
:c:`None` (or :c:`Void`, or :c:`Empty`) enumeration value; this macro is named
``"is valid?"``.

..
    Strings
    -------

    TODO :: about strings

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
