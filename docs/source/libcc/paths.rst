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

Position a piece have at the very beginning of its ply is its initial position.

Position from which a piece starts its ply is a starting position.

Most of the time initial and starting positions are the same, but not always;
if not the same, a piece is repositioned from initial onto starting position,
before commencing its ply.

Path is a list of steps a piece can take from its starting position to a destination.

Some pieces can have different paths connecting the same starting position and
destination; route is collection of all those paths.

In a single ply, from its starting position a piece can have many legal destinations,
so it can take multiple routes; each route can have a few paths; along each path
a piece can encounter other pieces, each with a few possible interactions, each
leading to different paths.

For instance, one can encounter opponent's Starchild along its path; depending on
a piece moving, possible interactions with the Starchild might include:

    * transparency, piece continues its movement as if nothing happened
    * divergence, piece changes its direction, continues moving
    * capture, thus ending its ply, and a move
    * activation, ending its ply, but starting Starchild's
    * initiating sense-journey

If a piece above was e.g. a Centaur or a Serpent, it could also have multiple paths
towards said Starchild. Even pieces with straight movement (e.g. Bishop) could
encounter Shaman earlier in the ply, and diverge from it towards the Starchild
above.

.. _lbl-libcc-paths-path:

Path
----

Path segment is a list of steps from one position to another, last step can also
have an interaction, i.e. a side-effect.

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
