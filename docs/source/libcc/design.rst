.. Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.

.. _lbl-libcc-design-concepts:

Design concepts
===============

Describes main design concepts, mostly related to parsing user :term:`AN`.

.. todo::

    This document is placeholder, to fulfill when implementation is done.

.. _lbl-libcc-design-concepts-anchors:

Anchors
-------

An anchor is a chessboard position, which then can serve as a starting
point for parsing :term:`AN`.

.. _lbl-libcc-design-concepts-anchors-hard:

Hard anchors
^^^^^^^^^^^^

Hard anchor is a chessboard position with either initial setup, or after
any legal move.

.. _lbl-libcc-design-concepts-anchors-soft:

Soft anchors
^^^^^^^^^^^^

Soft anchor is a chessboard position after a legal ply is applied to it,
while parsing a (cascading) move :term:`AN`.

.. _lbl-libcc-design-concepts-endpoints:

End-points
----------

End-point refers to generated paths, path segments, and other data necessary
to parse next ply :term:`AN`.

.. _lbl-libcc-design-concepts-endpoints-hard:

Hard end-points
^^^^^^^^^^^^^^^

Hard end-points are all generated, complete paths a piece can take at the
beginning of a ply.

It is comprised of list of paths, each path is a list of steps.

It provides the same interface for all pieces, regardless if they choose
one, or two initial steps, or each step independently.

.. _lbl-libcc-design-concepts-endpoints-soft:

Soft end-points
^^^^^^^^^^^^^^^

Soft end-point is generated path segment, which can be common for multiple
paths.

For instance, path segment can be part of a complete path leading a piece
to divergence.

..
    Strings
    -------

    TODO :: about strings

..
    Positions
    ---------

    TODO :: about positions

..
    Linked lists
    ------------

    TODO :: about linked lists
