<!-- Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com -->
<!-- Licensed as Public Domain work, see https://en.wikipedia.org/wiki/Public_domain. -->

[<<< prev](memory.md "<<< prev") || [^^^ top](main.md "^^^ top") || [next >>>](organization.md "next >>>")

Design concepts                         {#design_concepts}
===============

Describes main design concepts, mostly related to parsing user AN.

Anchors
-------

An anchor is a chessboard position, which then can serve as a starting point for parsing AN.

### Hard anchors

Hard anchor is a chessboard position with either initial setup, or after any legal move.

### Soft anchors

Soft anchor is a chessboard position after a legal ply is applied to it, while parsing a (cascading) move AN.

End-points
----------

End-point refers to generated paths, path segments, and other data necessary to parse next ply AN.

### Hard end-points

Hard end-point is all generated, complete paths a piece can take at the beginning of a ply. <br />

It is comprised of list of paths, each path is a list of steps. <br />
It provides the same interface for all pieces, regardless if they choose one, or two initial steps, or each step independently.

### Soft end-points

Soft end-point is generated path segment, which can be common for multiple paths. <br />
For instance, path segment can be part of a complete path leading a piece to divergence.

[<<< prev](memory.md "<<< prev") || [^^^ top](main.md "^^^ top") || [next >>>](organization.md "next >>>")
