.. Copyright (c) 2025 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.

.. include:: ../defines.rst

.. _lbl-libcc-paths:

Paths
=====

Describes generating all legal plies a piece can take.

.. _lbl-libcc-paths-description:

Description
-----------

Position a piece have at the beginning of its ply is its initial position.

Position from which a piece starts its ply is a starting position.

Most of the time initial and starting positions are the same, but not always.
If positions are not the same, a piece is repositioned from initial onto starting
position, before commencing its ply.

Path is a list of steps a piece can take from its starting position to a destination.

Some pieces can have multiple, different paths connecting the same starting
position and destination; route is collection of all those paths.

In a single ply, from its starting position a piece can have multiple legal
destinations, so it can take multiple routes; each route can have multiple paths;
along each path a piece can interact with multiple other pieces.

.. _lbl-libcc-paths-path:

Path
----

Path segment is a list of steps from one position to another, last step can also
have an interaction.

Complete such a path tree is represented by :c:type:`CcPathLink`, its
:c:member:`CcPathLink.steps` contain a path segment, and :c:member:`CcPathLink.momentum` at the end
of its path segment.

Path::

    +---+   next    +---+   alt           diverge    +---+
    | A |  ------>  | B |  ------>  ...  --------->  | Z |
    +---+           +---+                            +---+

      |               |                                |
      | steps         | steps                          | steps
      |               |                                |
      V               V                                V

    +----+          +----+                           +----+
    | a0 |          | b0 |                           | z0 |
    +----+          +----+                           +----+
      |               |                                |
      | next          | next                           | next
      V               V                                V
    +----+          +----+                           +----+
    | a1 |          | b1 |                           | z1 |
    +----+          +----+                           +----+
      |               |                                |
      | next          | next                           | next
      V               V                                V

      :               :                                :
      :               :                                :

      |               |                                |
      | next          | next                           | next
      V               V                                V
    +----+          +----+                           +----+
    | an |          | bn |                           | zn |
    +----+          +----+                           +----+
