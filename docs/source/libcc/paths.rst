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

Position a piece have at the very beginning of its ply (before any movement)
is its initial position.

Position from which a piece starts its ply (its movement) is a starting position.

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
in which they were visited. Each step in a segment can also feature encounter with
another piece (and its tag), and have a side-effect associated with such an encounter,
if it's the only one possible. Terminal side-effects (e.g. most captures) can only
be contained in the very last step of a segment.

Each step in a segment can also feature substitute side-effects, when only one of a
possible few can be chosen. This is used only for displacements, when chosen side-effect
does not alter the path a moving piece is taking.

If there are multiple interactions possible with encountered piece, step onto that
piece is added to path segment as the last one; then, terminal side-effect (e.g. a
capture) is stored in this last step. All other choices are stored in
:c:member:`CcPathNode.side_effect` in subsequent path nodes, either forked or
alternated.

Complete such a path tree is represented by :c:type:`CcPathNode` nodes linked via
:c:member:`CcPathNode.fork`, :c:member:`CcPathNode.alt` members,
:c:member:`CcPathNode.steps` contain a path segment, and each step has its substitute
side-effects stored in :c:member:`CcStep.tentative__d` member.

Path node side-effect (i.e. :c:member:`CcPathNode.side_effect`) is applied to the
last step in path segment (i.e. :c:member:`CcPathNode.steps`) of a *parent* node
(i.e. :c:member:`CcPathNode.back__w`), when complete path is built from root node down
to any leaf.

.. note::

    If both side-effects are defined, path node side-effect will override any
    side-effect from steps.

    This is so that most common terminal side-effect (i.e. capture) can be had
    without having to fork a path for it. For instance, a Shaman capturing opponent's
    pieces in a single path segment can end it by capturing opponent's Starchild,
    and still diverge from said Starchild with side-effect of forking path node
    set to divergence.

First step in a root node is initial position of a piece. Second step, either in
root node or otherwise, might be repositioning.

.. warning::

    Any :c:type:`CcPathNode` node with its path continued by :c:member:`CcPathNode.fork` or
    :c:member:`CcPathNode.alt` members **must** also have path segment
    (i.e. :c:member:`CcPathNode.steps`) defined, except for leaf nodes.

.. note::

    Path node without path segment (i.e. if :c:member:`CcPathNode.steps` is
    :c:data:`NULL`) is valid path node, as long as path is not continued.

.. _lbl-libcc-paths-pathsegmenttree-alternativepaths:

Alternative paths
^^^^^^^^^^^^^^^^^

Alternative paths always replace original path; they represent multiple, independent
paths.

Alternative paths are represented as a list of :c:type:`CcPathNode` nodes connected
via :c:member:`CcPathNode.alt`; they are alternative to the originating segment.
For instance::

    +---+
    | A |
    +---+
      |
      | alt
      V
    +---+
    | B |
    +---+
      |
      | alt
      V
    +---+
    | C |
    +---+

beside default path::

    +---+
    | A |
    +---+

also produces 2 additional, alternative paths::

    +---+
    | B |
    +---+

    +---+
    | C |
    +---+

Alternative paths are almost never used alone, but together with
:ref:`lbl-libcc-paths-pathsegmenttree-forkingpaths`, below.

.. .. _lbl-libcc-paths-pathsegmenttree-auxiliarypaths:
..
.. Auxiliary paths
.. ^^^^^^^^^^^^^^^
..
.. Auxiliary paths are represented as a list of :c:type:`CcPathNode` nodes connected
.. via :c:member:`CcPathNode.aux`; they are "in-situ" segments, i.e. each auxiliary
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

Substitute paths are used to replace side-effect of an origin node (side-effect of
its last step), without altering path segment (i.e. path taken by a piece) in any
other way.

Substitute paths are used primarily for displacements, when there are many possible
displacement fields, none of which alters current path; e.g. a Shaman displacing
pieces along predetermined path in a trance-journey. Another example, a Serpent
displacing Pawns encountered on its path.

Substitute paths are represented as a linked list of :c:type:`CcSideEffectLink`
nodes connected via :c:member:`CcSideEffectLink.next` member.

To generate complete paths from a tree containing substitute nodes, every side-effect
from those nodes overrides side-effect of the last step in an originating node.
For instance::

    +---+
    | A |
    +---+
      |
      | steps
      V
    +---+   next    +---+   next    +---+
    | a |  ------>  | b |  ------>  | c |
    +---+           +---+           +---+
                      |
                      | tentative
                      V
                    +---+
                    | 0 |
                    +---+
                      |
                      | next
                      V
                    +---+
                    | 1 |
                    +---+

beside default path::

    +---+
    | A |
    +---+

with steps forming complete path::

    +---+   next    +---+   next    +---+
    | a |  ------>  | b |  ------>  | c |
    +---+           +---+           +---+

also produces steps making complete paths with substituted side-effects::

    +---+   next    +-------+   next    +---+
    | a |  ------>  | b < 0 |  ------>  | c |
    +---+           +-------+           +---+

    +---+   next    +-------+   next    +---+
    | a |  ------>  | b < 1 |  ------>  | c |
    +---+           +-------+           +---+

Here, starting node is ``A``, with steps ``a``, ``b`` and ``c``; where ``b`` step
also contain a list of substitute side-effects ``0`` and ``1``. So, when complete
path a piece can take is built, beside a list of steps from default path
``a --> b --> c``, there are also two additional possibilities, with one of
substitute side-effects overriding side-effect in ``b`` node.

.. divergence is covered in :ref:`lbl-libcc-paths-pathsegmenttree-forkingpaths`, below.

.. _lbl-libcc-paths-pathsegmenttree-forkingpaths:

Forking paths
^^^^^^^^^^^^^

Forking paths are used to represent multiple, independent paths from a common
position, e.g. after divergence, or from a starting position. Forking paths are
appended to original path.

Forking paths are represented as a list of :c:type:`CcPathNode` nodes connected
via :c:member:`CcPathNode.fork`; they are "post-node" segments, i.e. each forking
path segment is meant to be concatenated to the originating segment.
For instance::

    +---+
    | A |
    +---+
       \
        \  fork
         \
          V
        +---+
        | B |
        +---+
          |
          | alt
          V
        +---+   tentative    +---+
        | C |  ----------->  | 0 |
        +---+                +---+
          |
          | alt
          V
        +---+
        | D |
        +---+

produces::

    +---+   fork    +---+
    | A |  ------>  | B |
    +---+           +---+

    +---+   fork    +---+
    | A |  ------>  | C |
    +---+           +---+

    +---+   fork    +-------+
    | A |  ------>  | C < 0 |
    +---+           +-------+

    +---+   fork    +---+
    | A |  ------>  | D |
    +---+           +---+

Note that :c:member:`CcPathNode.fork` link was used only once, all other
alternative paths after divergence are linked via :c:member:`CcPathNode.alt`.

It is possible to have subsequent nodes use :c:member:`CcPathNode.fork` link,
if their path segments end with divergence.

Path node does not contain substitute side-effects, steps in its segment do.
So, when ``C`` node is linked to substitute side-effect ``0``, it is meant
for its step to have such a link.

For instance, ``A`` path segment could contain only starting position of a
Serpent, different paths are then forks from that starting position; along
a path Serpent could e.g. capture a Pawn (``A --> C``), or displace it
(``A --> C < 0``).

.. _lbl-libcc-paths-pathsegmenttree-complete:

Complete paths
^^^^^^^^^^^^^^

A complete path is built by traversing path tree from its origin :c:type:`CcPathNode`
node down to any terminal node; a legal ply is built by stitching path segments
from those nodes, in order in which they were visited.

For instance, this path-tree::

    +---+  fork   +---+   alt    +---+
    | A | ------> | B |  ----->  | C |
    +---+         +---+          +---+

      |             |              |
      | steps       | steps        | steps
      |             |              |
      V             V              V

    +----+        +----+         +----+
    | a0 |        | b0 |         | c0 |
    +----+        +----+         +----+
      |             |              |
      | next        | next         | next
      V             V              V
    +----+        +----+         +----+
    | a1 |        | b1 |         | c1 |
    +----+        +----+         +----+
      |             |              |
      | next        | next         | next
      V             V              V

      :             :              :
      :             :              :

      |             |              |
      | next        | next         | next
      V             V              V
    +----+        +----+         +----+    tentative     +---+   next    +---+
    | ai |        | bj |         | ck |  ------------->  | 0 |  ------>  | 1 |
    +----+        +----+         +----+                  +---+           +---+

gives a complete ply with steps ordered like so::

    +----+  next   +----+  next        next   +----+  next
    | a0 | ------> | a1 | ------> ... ------> | ai | ------> ...
    +----+         +----+                     +----+

         next   +----+  next   +----+  next        next   +----+
    ... ------> | b0 | ------> | b1 | ------> ... ------> | bj |
                +----+         +----+                     +----+

another one like so::

    +----+  next   +----+  next        next   +----+  next
    | a0 | ------> | a1 | ------> ... ------> | ai | ------> ...
    +----+         +----+                     +----+

         next   +----+  next   +----+  next        next   +----+
    ... ------> | c0 | ------> | c1 | ------> ... ------> | ck |
                +----+         +----+                     +----+

the one with substituted side-effect::

    +----+  next   +----+  next        next   +----+  next
    | a0 | ------> | a1 | ------> ... ------> | ai | ------> ...
    +----+         +----+                     +----+

         next   +----+  next   +----+  next        next   +--------+
    ... ------> | c0 | ------> | c1 | ------> ... ------> | ck < 0 |
                +----+         +----+                     +--------+

and finally, another one with different substituted side-effect::

    +----+  next   +----+  next        next   +----+  next
    | a0 | ------> | a1 | ------> ... ------> | ai | ------> ...
    +----+         +----+                     +----+

         next   +----+  next   +----+  next        next   +--------+
    ... ------> | c0 | ------> | c1 | ------> ... ------> | ck < 1 |
                +----+         +----+                     +--------+

First step in a complete ply is initial position of a piece; second step might
be repositioning.

When stitching nodes into one complete path, side-effect of a current node in sequence
replaces side-effect of a last step in a previous node; i.e. :c:member:`CcPathNode.side_effect`
of a current node replaces side-effect in last step found in :c:member:`CcPathNode.steps`
of the previous node (i.e. the one accessed via :c:member:`CcPathNode.back__w.steps`).

This is always done, regardless if next node has been linked via :c:member:`CcPathNode.fork`,
or :c:member:`CcPathNode.alt`.
