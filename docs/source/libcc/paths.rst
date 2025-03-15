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

.. _lbl-libcc-paths-pathsegmenttree:

Path segment, tree
------------------

Path segment is a list of all steps taken from one position to another, in order
in which they were visited; on last step one can also encountered a piece, and its
tag.

There might be a few different interactions possible with encountered piece; the
one chosen for this path segment is stored in :c:member:`CcPathLink.side_effect`.

Complete such a path tree is represented by :c:type:`CcPathLink` nodes linked via
:c:member:`CcPathLink.fork`, :c:member:`CcPathLink.alt`, :c:member:`CcPathLink.next`
and :c:member:`CcPathLink.sub` members; its :c:member:`CcPathLink.steps` contain
a path segment.

Path node side-effect (i.e. :c:member:`CcPathLink.side_effect`) is applied to last
step in path segment (i.e. :c:member:`CcPathLink.steps`) of a parent node, when
complete path is built from root node down to any leaf.

.. note::

    If both side-effects are defined, path node side-effect will override any
    side-effect from steps.

    This is so that e.g. a Shaman can capture opponent's Starchild, continue capturing
    opponent's pieces in the next path node (without its side-effect), and also diverge
    from said Starchild in a forking path node (with its side-effect).

First step in a root node is initial position of a piece. Second step, either in
root node or otherwise, might be repositioning.

.. warning::

    Any :c:type:`CcPathLink` node with its path continued (regardless which
    :c:member:`CcPathLink.fork`, :c:member:`CcPathLink.alt`, :c:member:`CcPathLink.next`,
    :c:member:`CcPathLink.sub` members are present) **must** also have path segment
    (i.e. :c:member:`CcPathLink.steps`) defined.

    .. note::

        Path node without path segment (i.e. if :c:member:`CcPathLink.steps` is
        :c:data:`NULL`) is valid path node, as long as path is not continued.

.. _lbl-libcc-paths-pathsegmenttree-subsequentpaths:

Subsequent paths
^^^^^^^^^^^^^^^^

Subsequent paths are represented as a list of :c:type:`CcPathLink` nodes connected
via :c:member:`CcPathLink.next`; all their path segments are to be concatenated to
their respective predecessor. For instance::

    +---+   next    +---+   next   +----+
    | A |  ------>  | B |  ------> | C0 |
    +---+           +---+          +----+

Subsequent paths are used when interaction does not produce multiple alternative
paths (e.g. when Shaman keeps capturing pieces along predetermined path in a
trance-journey), or when path segment is continuation of movement in previous
segment (e.g. a Shaman continues its capturing-ply, after it encountered divergent
piece, opponent's Starchild).

.. _lbl-libcc-paths-pathsegmenttree-alternativepaths:

Alternative paths
^^^^^^^^^^^^^^^^^

Alternative paths are represented as a list of :c:type:`CcPathLink` nodes connected
via :c:member:`CcPathLink.alt`; they are alternative to the originating segment.
For instance::

    +---+   next    +----+   next    +---+
    | A |  ------>  | B0 |  ------>  | C |
    +---+           +----+           +---+
                      |
                      | alt
                      V
                    +----+
                    | B1 |
                    +----+
                      |
                      | alt
                      V
                    +----+   next    +---+
                    | B2 |  ------>  | D |
                    +----+           +---+

beside default path::

    +---+   next    +----+   next    +---+
    | A |  ------>  | B0 |  ------>  | C |
    +---+           +----+           +---+

also produces 2 additional, alternative paths::

    +---+   next    +----+
    | A |  ------>  | B1 |
    +---+           +----+

    +---+   next    +----+   next    +---+
    | A |  ------>  | B2 |  ------>  | D |
    +---+           +----+           +---+

Alternative paths are used when there are multiple possible interactions with
encountered piece.

For instance, a Bishop encounters opponent's Starchild on its capture-field; it
can capture Starchild (e.g. ``A --> B1``), or use transparency to continue moving
along a diagonal (e.g. ``A --> B0 --> C``). Bishop can also diverge from a Starchild,
but this is covered in :ref:`lbl-libcc-paths-pathsegmenttree-forkingpaths`, below.

.. .. _lbl-libcc-paths-pathsegmenttree-auxiliarypaths:
..
.. Auxiliary paths
.. ^^^^^^^^^^^^^^^
..
.. Auxiliary paths are represented as a list of :c:type:`CcPathLink` nodes connected
.. via :c:member:`CcPathLink.aux`; they are "in-situ" segments, i.e. each auxiliary
.. path segment is meant to replace just an originating segment, and continue path
.. with the remainder of a path segments owned by originating path node.
.. For instance::
..
..     +---+   next    +----+   next    +---+
..     | A |  ------>  | B0 |  ------>  | C |
..     +---+           +----+           +---+
..                       |
..                       | aux
..                       V
..                     +----+
..                     | B1 |
..                     +----+
..                       |
..                       | aux
..                       V
..                     +----+   next    +---+
..                     | B2 |  ------>  | D |
..                     +----+           +---+
..
.. produces 3 different paths::
..
..     +---+   next    +----+   next    +---+
..     | A |  ------>  | B0 |  ------>  | C |
..     +---+           +----+           +---+
..
..     +---+   next    +----+   next    +---+
..     | A |  ------>  | B1 |  ------>  | C |
..     +---+           +----+           +---+
..
..     +---+   next    +----+   next    +---+   next    +---+
..     | A |  ------>  | B2 |  ------>  | D |  ------>  | C |
..     +---+           +----+           +---+           +---+
..
.. Auxiliary paths are used when there are multiple possible interactions with
.. encountered piece, but originating path has to be continued.
..
.. For instance, a Shaman can capture, diverge from, or use transparency of opponent's
.. Starchild, and still continue its ply after all those interactions; in example above,
.. ``B0`` node could be a capture, while ``B1`` node would then be a transparency;
.. divergence is covered later in :ref:`lbl-libcc-paths-pathsegmenttree-forkingpaths`.

.. _lbl-libcc-paths-pathsegmenttree-substitutepaths:

Substitute paths
^^^^^^^^^^^^^^^^

Substitute paths are represented as a list of :c:type:`CcPathLink` nodes connected
via :c:member:`CcPathLink.sub`; they only contain side-effect, and can contain link
to another substitute path (i.e. :c:member:`CcPathLink.sub`), but neither path
segment, nor any other path continuations (i.e. all of :c:member:`CcPathLink.fork`,
:c:member:`CcPathLink.alt`, :c:member:`CcPathLink.next`, and
:c:member:`CcPathLink.steps` are :c:data:`NULL`).

To generate complete paths from a tree containing substitute nodes, every side-effect
from those nodes overrides side-effect of the last step in a starting node.
For instance::

    +---+   next    +----+   next    +---+
    | A |  ------>  | B0 |  ------>  | C |
    +---+           +----+           +---+
                      |
                      | sub
                      V
                    +----+
                    | B1 |
                    +----+
                      |
                      | sub
                      V
                    +----+
                    | B2 |
                    +----+

beside default path::

    +---+   next    +----+   next    +---+
    | A |  ------>  | B0 |  ------>  | C |
    +---+           +----+           +---+

also produces default paths with substituted side-effects::

    +---+   next    +---------+   next    +---+
    | A |  ------>  | B0 < B1 |  ------>  | C |
    +---+           +---------+           +---+

    +---+   next    +---------+   next    +---+
    | A |  ------>  | B0 < B2 |  ------>  | C |
    +---+           +---------+           +---+

Here, starting node of substitute paths is ``B0``. Substitute side-effects node
``B0 < B2`` represents starting node ``B0`` with side-effect of its last step
overridden by side-effect from ``B2`` node.

This is to be used primarily for displacements, when there are many possible
displacement fields, none of which alters current path; e.g. a Shaman displacing
pieces along predetermined path in a trance-journey. Another example, a Serpent
displacing Pawns encountered on its path.

.. Another example, a Shaman can capture, or diverge from opponent's Starchild, and
.. still continue its ply after all those interactions; in example above,
.. ``B0`` node could be a capture, while ``B1`` node would then be a transparency;
.. divergence is covered in :ref:`lbl-libcc-paths-pathsegmenttree-forkingpaths`, below.

.. _lbl-libcc-paths-pathsegmenttree-forkingpaths:

Forking paths
^^^^^^^^^^^^^

Forking paths are represented as a list of :c:type:`CcPathLink` nodes connected
via :c:member:`CcPathLink.fork`; they are "post-node" segments, i.e. each forking
path segment is meant to be concatenated to the originating segment.
For instance::

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
                        +----+   next    +---+
                        | D1 |  ------>  | E |
                        +----+           +---+
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

    +---+   next    +---+   next    +----+   next    +---+
    | A |  ------>  | B |  ------>  | D1 |  ------>  | E |
    +---+           +---+           +----+           +---+

    +---+   next    +---+   next    +----+
    | A |  ------>  | B |  ------>  | D2 |
    +---+           +---+           +----+

Note that :c:member:`CcPathLink.fork` link was used only once, all other
alternative paths after divergence are linked via :c:member:`CcPathLink.alt`.

It is possible to have subsequent nodes use :c:member:`CcPathLink.fork` link,
if all their path segments end with divergence.

Forking paths are used after divergence; and also to facilitate multiple,
independent paths from a starting position.

.. _lbl-libcc-paths-pathsegmenttree-complete:

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

First step in a complete ply is initial position of a piece; second step might
be repositioning.
