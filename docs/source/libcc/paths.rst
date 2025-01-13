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

.. _lbl-libcc-paths-segmenttree:

Path segment, tree
------------------

Path segment is a list of all steps taken from one position to another, in order
in which they were visited; last step can also have an interaction, i.e. a
side-effect.

Complete such a path tree is represented by :c:type:`CcPathLink` nodes linked via
:c:member:`CcPathLink.fork`, :c:member:`CcPathLink.alt` and :c:member:`CcPathLink.next`
members; its :c:member:`CcPathLink.steps` contain a path segment, and
:c:member:`CcPathLink.side_effect` what a moving piece had at the end of
this path segment.

.. _lbl-libcc-paths-segmenttree-subsequent:

Subsequent paths
^^^^^^^^^^^^^^^^

Subsequent paths are represented as a list of :c:type:`CcPathLink` nodes connected
via :c:member:`CcPathLink.next`; all their path segments are to be concatenated to
their respective predecessor. For instance::

    +---+   next    +---+   next   +----+
    | A |  ------>  | B |  ------> | C0 |
    +---+           +---+          +----+

Subsequent paths are used when interaction does not produce multiple alternative
paths (e.g. when Serpent displaces own Pawns), or when path segment is continuation
of movement in previous segment (e.g. a piece encounters transparent piece).

.. _lbl-libcc-paths-segmenttree-alternative:

Alternative paths
^^^^^^^^^^^^^^^^^

Alternative paths are represented as a list of :c:type:`CcPathLink` nodes connected
via :c:member:`CcPathLink.alt`; they are "in-situ" segments, i.e. each alternative
path segment is meant to replace originating segment. For instance::

    +---+   next    +---+   next    +----+   next    +---+
    | A |  ------>  | B |  ------>  | C0 |  ------>  | D |
    +---+           +---+           +----+           +---+
                                      |
                                      | alt
                                      V
                                    +----+
                                    | C1 |
                                    +----+
                                      |
                                      | alt
                                      V
                                    +----+
                                    | C2 |
                                    +----+

produces 3 different paths::

    +---+   next    +---+   next    +----+   next    +---+
    | A |  ------>  | B |  ------>  | C0 |  ------>  | D |
    +---+           +---+           +----+           +---+

    +---+   next    +---+   next    +----+
    | A |  ------>  | B |  ------>  | C1 |
    +---+           +---+           +----+

    +---+   next    +---+   next    +----+
    | A |  ------>  | B |  ------>  | C2 |
    +---+           +---+           +----+

Alternative paths are used when there are multiple possible interactions with
encountered piece.

.. _lbl-libcc-paths-segmenttree-forking:

Forking paths
^^^^^^^^^^^^^

Forking paths are represented as a list of :c:type:`CcPathLink` nodes connected
via :c:member:`CcPathLink.fork`; they are "post-node" segments, i.e. each forking
path segment is meant to be concatenated to originating segment. For instance::

    +---+   next    +---+   next    +---+
    | A |  ------>  | B |  ------>  | C |
    +---+           +---+           +---+
                       \
                        \  fork
                         \
                          V
                        +----+
                        | D0 |
                        +----+
                          |
                          | alt
                          V
                        +----+
                        | D1 |
                        +----+
                          |
                          | alt
                          V
                        +----+
                        | D2 |
                        +----+

beside default path::

    +---+   next    +---+   next    +---+
    | A |  ------>  | B |  ------>  | C |
    +---+           +---+           +---+

also produces::

    +---+   next    +---+   next    +----+
    | A |  ------>  | B |  ------>  | D0 |
    +---+           +---+           +----+

    +---+   next    +---+   next    +----+
    | A |  ------>  | B |  ------>  | D1 |
    +---+           +---+           +----+

    +---+   next    +---+   next    +----+
    | A |  ------>  | B |  ------>  | D2 |
    +---+           +---+           +----+

Note that :c:member:`CcPathLink.fork` link was used only once, all other
alternative paths after divergence are linked via :c:member:`CcPathLink.alt`.

It is possible to have subsequent nodes use :c:member:`CcPathLink.fork` link,
if all their path segments end with divergence.

Forking paths are used after divergence; and also to facilitate multiple,
independent paths from a starting position.

.. _lbl-libcc-paths-segmenttree-complete:

Complete paths
^^^^^^^^^^^^^^

A complete path is built by traversing path tree from its origin :c:type:`CcPathLink`
node down to any terminal node; a legal ply is built by stitching path segments
from those nodes, in order in which they were visited.

For instance, this path in a path-tree::

    +---+  next   +---+   alt    +---+   fork    +---+
    | A | ------> | B |  ----->  | C |  ------>  | D |
    +---+         +---+          +---+           +---+

      |             |              |               |
      | steps       | steps        | steps         | steps
      |             |              |               |
      V             V              V               V

    +----+        +----+         +----+          +----+
    | a0 |        | b0 |         | c0 |          | d0 |
    +----+        +----+         +----+          +----+
      |             |              |               |
      | next        | next         | next          | next
      V             V              V               V
    +----+        +----+         +----+          +----+
    | a1 |        | b1 |         | c1 |          | d1 |
    +----+        +----+         +----+          +----+
      |             |              |               |
      | next        | next         | next          | next
      V             V              V               V

      :             :              :               :
      :             :              :               :

      |             |              |               |
      | next        | next         | next          | next
      V             V              V               V
    +----+        +----+         +----+          +----+
    | an |        | bn |         | cn |          | dn |
    +----+        +----+         +----+          +----+

gives a complete ply with steps ordered like so::

    +----+  next   +----+  next        next   +----+  next
    | a0 | ------> | a1 | ------> ... ------> | an | ------> ...
    +----+         +----+                     +----+

         next   +----+  next   +----+  next        next   +----+  next
    ... ------> | b0 | ------> | b1 | ------> ... ------> | bn | ------> ...
                +----+         +----+                     +----+

         next   +----+  next   +----+  next        next   +----+  next
    ... ------> | c0 | ------> | c1 | ------> ... ------> | cn | ------> ...
                +----+         +----+                     +----+

         next   +----+  next   +----+  next        next   +----+
    ... ------> | d0 | ------> | d1 | ------> ... ------> | dn |
                +----+         +----+                     +----+
