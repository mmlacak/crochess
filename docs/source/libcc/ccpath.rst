.. Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.
   Included source code files are licensed under GNU GPL v3+ license. See LICENSING, COPYING files in root folder of the project for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccpath:

Path
====

Documents ``cc_path.h`` and ``cc_path.c`` files, which contain various
path definitions, types and functions.

.. seealso::

    Paths are discussed in greater details in :doc:`paths`.

.. _lbl-libcc-ccpath-linkedpathsegments:

Linked path segments
--------------------

.. c:struct:: CcPathNode

    Node containing path segment, and links to other nodes in a path tree.

    First step in a root node is initial position of a piece. Second step either
    in root node or otherwise might be repositioning.

    Forking :term:`path`\s are introduced by single :c:member:`fork` path; all
    other possible :term:`path`\s after the same e.g. divergence are then linked
    by :c:member:`alt`.

    .. warning::

        All :c:member:`fork`, :c:member:`alt`, :c:member:`sub`, and :c:member:`next`
        members are owners of the remainder of a path tree they are pointing to; and
        must always be a singular pointer (owner) within a path tree to their
        respective forking, alternating, substitute, or subsequent path.

        Any two pointers pointing to the same node, any back-references within
        a path tree, or shared among them are not allowed, and will likely cause
        crash.

        .. note::

            Weak back-links are fine, as they are never :c:func:`free()`\-ed.

    .. seealso::

        :ref:`lbl-libcc-paths-pathsegmenttree`

    .. c:member:: CcSideEffect side_effect

        One chosen side-effect from all possible on piece encountered in previous
        (parent) path segment.

    .. c:member:: CcStep * steps

        Fields visited (steps performed) by a moving piece, a path segment.

        Linked path :term:`segment` is a linked list of all fields visited from one
        position to another in order in which they were made; only the last step
        in a :term:`segment` can also have another, encountered piece and its tag.

    .. c:member:: CcPieceTagType encounter

        Piece encountered at the very last field in the :c:member:`steps` list,
        and its tag.

    .. c:member:: CcActivationDesc act_desc

        Activation descriptor for a moving piece; its activator, momentum usage
        and momentum it had after all performed steps.

        This is non-cached, stored data of :c:member:`CcPlyContext.act_desc` as move,
        its plies progresses.

    .. c:member:: struct CcPathNode * fork

        Link to forking paths.

        Forking paths are used after divergence; and also to facilitate multiple,
        independent paths from a starting position.

        Every forking path has this step as its starting position.

        One forking path links to another via :c:member:`alt` member.

        .. seealso::

            :ref:`lbl-libcc-paths-pathsegmenttree-forkingpaths`

    .. c:member:: struct CcPathNode * alt

        Link to alternative to this path segment.

        Alternative paths are used when there are multiple possible interactions with
        encountered piece.

        This link should be set only after divergence, or if part of alternative
        paths, i.e. if this step has been pointed-to by either :c:member:`fork`,
        or :c:member:`alt`.

        .. seealso::

            :ref:`lbl-libcc-paths-pathsegmenttree-alternativepaths`

    .. c:member:: struct CcPathNode * sub

        Link to substitute side-effects to path segment of a substitute-starting
        node.

        Substitute paths are used when there are multiple possible side-effects
        (interactions with encountered piece), which do not alter path of a
        moving piece.

        .. seealso::

            :ref:`lbl-libcc-paths-pathsegmenttree-substitutepaths`

    .. c:member:: struct CcPathNode * next

        Link to subsequent path.

        Subsequent paths are used when interaction does not produce multiple alternative
        paths (e.g. when Serpent displaces Pawns), or when path segment is continuation of
        movement in previous segment (e.g. a piece encounters transparent piece).

        .. seealso::

            :ref:`lbl-libcc-paths-pathsegmenttree-subsequentpaths`

    .. c:member:: struct CcPathNode * back__w

        Weak back-link to parent node, regardless if pointed-to by :c:member:`fork`,
        :c:member:`alt`, or :c:member:`next`.

    :c:`Struct` is tagged with the same :c:struct:`CcPathNode` name.

.. c:function:: CcPathNode * cc_path_node__new( CcSideEffect side_effect, CcStep ** steps__d_n, CcPieceTagType encounter, CcActivationDesc act_desc )

    Function allocates a new path link.

    *Optional* path segment can be :c:data:`NULL`, path link will still be allocated;
    it's used when side-effect is terminating (like capturing), and so no more fields
    are visited.

    Takes ownership of :c:`steps__d_n`, inner pointer will be set to :c:data:`NULL`,
    if valid path link is produced.

    :param side_effect: A possible side-effect on previously encountered piece.
    :param steps__d_n: **Ownership transfer**, *optional*; steps performed, a path segment; can be :c:data:`NULL`.
    :param encounter: Piece (and its tag) encountered at the very last field in the :c:var:`steps__d_n` list.
    :param act_desc: Activation descriptor for a moving piece, momentum it had after all performed steps.
    :returns: Pointer to a newly allocated path link if successful,
        :c:data:`NULL` otherwise.

.. c:function:: CcPathNode * cc_path_node_append( CcPathNode ** pl__iod_a, CcSideEffect side_effect, CcStep ** steps__d_n, CcPieceTagType encounter, CcActivationDesc act_desc )

    Function appends a newly allocated path link to a given path segment,
    as its :c:member:`next` member.

    If path segment :c:`*pl__iod_a` is :c:data:`NULL`, it will be initialized
    with a newly allocated path link as its only element.

    :param pl__iod_a: **Ownership**, *optional* *input/output*; path segment.
    :param side_effect: A possible side-effect on previously encountered piece.
    :param steps__d_n: **Ownership transfer**, *optional*; steps performed, a path segment; can be :c:data:`NULL`.
    :param encounter: Piece (and its tag) encountered at the very last field in the :c:var:`steps__d_n` list.
    :param act_desc: Activation descriptor for a moving piece, momentum it had after all performed steps.
    :returns: A weak pointer to a newly allocated linked position
              if successful, :c:data:`NULL` otherwise.
    :seealso: :c:func:`cc_path_node__new()`

.. c:function:: CcPathNode * cc_path_node_extend( CcPathNode ** pl__iod_a, CcPathNode ** pl__n )

    Extends existing path segment with another one, as its :c:member:`next`
    segment.

    If path segment to extend (:c:`pl__iod_a`) hasn't been allocated yet,
    this will initialize it with content of an extending path segment, i.e.
    :c:`pl__n`.

    .. note::

        Extending path segment :c:`pl__n` has its ownership transferred to
        extended path segment :c:`pl__iod_a`; as a result, inner pointer
        :c:`*pl__n` is :c:data:`NULL`\ed.

    :param pl__iod_a: **Ownership**, *optional* *input/output*; a path segment
        to extend.
    :param pl__n: **Ownership transfer**; path segment with which to extend
        existing segment.
    :returns: Weak pointer to extended portion of a resulting path segment if
        successful, :c:data:`NULL` otherwise.

.. c:function:: CcPathNode * cc_path_node_add_fork( CcPathNode ** pl_step__a, CcPathNode ** pl_fork__n )

    Function extends forking paths of a given path step (:c:`pl_step__a`) with a
    path segment (:c:`pl_fork__n`), as an additional alternative path (i.e. appends
    to :c:`pl_step__a->fork->alt` linked list).

    If a given path step doesn't have forking path yet (i.e. if :c:`pl_step__a->fork == NULL`),
    function initializes it with a given forking path.

    .. note::

        Extending path segment :c:`pl_fork__n` has its ownership transferred to
        path segment :c:`pl_step__a`; as a result, inner pointer :c:`*pl_fork__n`
        is :c:data:`NULL`\ed.

    :param pl_step__a: **Ownership**; a path step from which to fork.
    :param pl_fork__n: **Ownership transfer**; forking path.
    :returns: Weak pointer to alternative path if successful,
        :c:data:`NULL` otherwise.

.. c:function:: CcPathNode * cc_path_node_add_alter( CcPathNode ** pl_step__a, CcPathNode ** pl_alt__n )

    Function extends alternating paths of a given path step (:c:`pl_step__a`) with
    path segment (:c:`pl_alt__n`), i.e. appends to :c:`pl_step__a->alt` linked list.

    If a given path step doesn't have alternating path yet (i.e. if :c:`pl_step__a->alt == NULL`),
    function initializes it with a given alternative path.

    .. note::

        Extending path segment :c:`pl_alt__n` has its ownership transferred to
        path segment :c:`pl_step__a`; as a result, inner pointer :c:`*pl_alt__n`
        is :c:data:`NULL`\ed.

    :param pl_step__a: **Ownership**; a path step from which to fork.
    :param pl_alt__n: **Ownership transfer**; alternating path.
    :returns: Weak pointer to alternative path if successful,
        :c:data:`NULL` otherwise.

.. c:function::CcPathNode * cc_path_node_add_subs( CcPathNode ** pl_step__a, CcPathNode ** pl_sub__n )

    Function extends substitute paths of a given path step (:c:`pl_step__a`) with
    path segment (:c:`pl_sub__n`), i.e. appends to :c:`pl_step__a->sub` linked list.

    If a given path step doesn't have substitute path yet (i.e. if :c:`pl_step__a->sub == NULL`),
    function initializes it with a given substitute path.

    .. note::

        Extending path segment :c:`pl_alt__n` has its ownership transferred to
        path segment :c:`pl_step__a`; as a result, inner pointer :c:`*pl_alt__n`
        is :c:data:`NULL`\ed.

    :param pl_step__a: **Ownership**; a path step to which to add substitute
        side-effect.
    :param pl_sub__n: **Ownership transfer**; substituting path.
    :returns: Weak pointer to substitute path if successful,
        :c:data:`NULL` otherwise.

.. c:function:: CcSideEffect * cc_path_node_last_step_side_effect( CcPathNode * pl_node )

    Function returns weak pointer to side-effect of a last step in a given path link node.

    .. note::

        Path link node is not traversed, i.e. none of :c:member:`CcPathNode.fork`,
        :c:member:`CcPathNode.alt`, :c:member:`CcPathNode.sub`, or
        :c:member:`CcPathNode.next` links are followed.

    :param pl_node: A path link node.
    :returns: Weak pointer to side-effect if successful,
              :c:data:`NULL` otherwise.

.. c:function:: CcMaybeBoolEnum cc_path_node_last_step_side_effect_is_none( CcPathNode * pl_node )

    Function checks if side-effect type of a last step in a given path link node is
    :c:enumerator:`CC_SETE_None`.

    .. note::

        Path link node is not traversed, i.e. none of :c:member:`CcPathNode.fork`,
        :c:member:`CcPathNode.alt`, :c:member:`CcPathNode.sub`, or
        :c:member:`CcPathNode.next` links are followed.

    :param pl_node: A path link node.
    :returns: One of :c:enum:`CcMaybeBoolEnum` values:

        * :c:enumerator:`CC_MBE_True` if side-effect is none,
        * :c:enumerator:`CC_MBE_False` if side-effect is not none
        * :c:enumerator:`CC_MBE_Void` in case of an error, insufficient data given.

.. c:function:: CcMaybeBoolEnum cc_path_node_is_leaf( CcPathNode * pl_node )

    Function checks if a given path link node is a leaf node.

    Leaf node is one without any of  :c:member:`CcPathNode.fork`,
    :c:member:`CcPathNode.alt`, :c:member:`CcPathNode.sub`, or
    :c:member:`CcPathNode.next` valid (non-:c:data:`NULL`) links.

    :param pl_node: A path link node.
    :returns: One of :c:enum:`CcMaybeBoolEnum` values:

        * :c:enumerator:`CC_MBE_True` if given path link node is leaf,
        * :c:enumerator:`CC_MBE_False` if given path link node is not a leaf,
        * :c:enumerator:`CC_MBE_Void` in case of an error, insufficient data given.

.. c:function:: bool cc_path_node_is_valid( CcPathNode * path_link )

    Checks if given path segment is valid; by checking if all positions are valid
    (i.e. not :c:data:`CC_INVALID_COORD`), and if all back-links are valid (e.g.
    if :c:`pl->fork` is non-:c:data:`NULL`, then :c:`pl->fork->back__w` must point
    back to :c:`pl`).

    :param path_link: A path segment.
    :returns: :c:data:`true` if given path segment is valid,
              :c:data:`false` otherwise.

.. c:function:: CcPathNode * cc_path_node_duplicate_all__new( CcPathNode * path_link )

    Duplicates complete linked tree of a given path segment into a newly
    allocated one.

    :param path_link: A path segment.
    :returns: A pointer to newly allocated path segment if successful,
              :c:data:`NULL` otherwise.

.. c:function:: bool cc_path_node_free_all( CcPathNode ** pl__f )

    Frees all path links from complete linked tree of a given path segment.

    :param pl__f: A path segment to :c:func:`free()`.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. c:function:: size_t cc_path_node_len( CcPathNode * path_link, bool count_all )

    Function returns length of a given path segment; optionally also includes
    :c:member:`fork`, :c:member:`alt`, :c:member:`sub` branches.

    :param path_link: A path segment.
    :param count_all: Flag, whether to include :c:member:`fork`, :c:member:`alt`
        path segments (if :c:data:`true`), or only a given path segment
        without branching (if :c:data:`false`).
    :returns: Length of a given path segment if successful, ``0`` otherwise.

.. c:function:: size_t cc_path_node_count_all_segments( CcPathNode * path_link )

    Function returns count of all segments, including :c:member:`fork`,
    :c:member:`alt` ones; substitute paths (i.e. all nodes linked via :c:member:`sub`)
    should not have path segments (i.e. :c:member:`steps`).

    :param path_link: A path segment.
    :returns: Count of all segments if successful, ``0`` otherwise.

.. c:function:: char * cc_path_node_to_string__new( cc_uchar_t depth, CcPathNode * path_link_node )

    Function returns string containing user-readable representation of a given
    path node.

    Path node depth is used to preface returned string with space-padding, which
    corresponds to depth of a given node in a path hierarchy.

    Depth is increased after a fork, but not for alternative, substitute paths,
    or after continuing current path (by taking next node).

    :param depth: Path node depth.
    :param path_link_node: A path segment.
    :returns: A newly allocated, null-terminated (``'\0'``) string if
        successful, :c:data:`NULL` otherwise.
    :seealso: :c:func:`cc_pos_to_string()`

.. .. TODO :: rethink (maybe?)
.. .. c:function:: char * cc_path_node_to_string__new( CcPathNode * path_link )
..
..     Function returns string containing user-readable representation of a complete
..     path tree, including :c:member:`fork`, :c:member:`alt` branches.
..
..     :param path_link: A path segment.
..     :returns: A newly allocated, null-terminated (``'\0'``) string if
..         successful, :c:data:`NULL` otherwise.
..     :seealso: :c:func:`cc_pos_to_string()`

.. _lbl-libcc-ccpath-nodelinkage:

Node linkage
^^^^^^^^^^^^

.. c:enum:: CcPathLinkNodeLinkageEnum

    Enumerates all node links in a path node, i.e. all different pointers a node
    can use to have a link to another node, see :c:struct:`CcPathNode`.

    .. c:enumerator:: CC_PLNLE_NoLinkage

    .. c:enumerator:: CC_PLNLE_Fork

    .. c:enumerator:: CC_PLNLE_Alt

    .. c:enumerator:: CC_PLNLE_Sub

    .. c:enumerator:: CC_PLNLE_Next

    :c:`enum` is tagged with the same :c:enum:`CcPathLinkNodeLinkageEnum` name.

.. c:macro:: CC_PATH_LINK_NODE_LINKAGE_IS_ENUMERATOR(plnle)

    Macro to check if given variant value is an enumerator, i.e. between
    :c:enumerator:`CC_PLNLE_NoLinkage` and :c:enumerator:`CC_PLNLE_Next` values.

    :param plnle: Variant (integer) value.
    :returns: :c:data:`true` if enumerator, :c:data:`false` otherwise.

.. c:macro:: CC_PATH_LINK_NODE_LINKAGE_IS_VALID(plnle)

    Macro to check if given variant value is a valid enumerator, i.e. between
    :c:enumerator:`CC_PLNLE_NoLinkage` and :c:enumerator:`CC_PLNLE_Next` values.

    This macro is the same as :c:macro:`CC_PATH_LINK_NODE_LINKAGE_IS_ENUMERATOR`, since
    :c:enum:`CcPathLinkNodeLinkageEnum` does not feature *null* (or *void*, or *empty*) value.

    :param plnle: Variant (integer) value.
    :returns: :c:data:`true` if valid enumerator, :c:data:`false` otherwise.

.. c:macro:: CC_MAX_LEN_PATH_LINK_NODE_LINKAGE_STRING

    Maximum length a node linkage string can be, equals to ``4``.

.. c:macro:: CC_SIZE_PATH_LINK_NODE_LINKAGE_STRING

    Maximum size a node linkage string can be, equals to
    :c:macro:`CC_MAX_LEN_PATH_LINK_NODE_LINKAGE_STRING` + ``1``.

.. c:function:: char const * cc_path_node_linkage_as_string( CcPathLinkNodeLinkageEnum plnle )

    Function returns string containing user-readable representation of a given
    path node linkage.

    .. note::

        Returned string is not :c:func:`alloc()`\ated, do not :c:func:`free()` it.

    :param plnle: A node linkage, :c:enum:`CcPathLinkNodeLinkageEnum` value.
    :returns: Pointer to a non-allocated, null-terminated (``'\0'``) string if successful,
        :c:data:`NULL` otherwise.

.. c:function:: CcPathLinkNodeLinkageEnum cc_path_node_linkage( CcPathNode * path_link_node )

    Function returns linkage of a given path node.

    :param path_link_node: A path node.
    :returns: Path node linkage, :c:enum:`CcPathLinkNodeLinkageEnum` value.

.. c:function:: char const * cc_path_node_linkage_to_string( CcPathNode * path_link_node )

    Function returns string containing user-readable linkage representation
    of a given path node.

    .. note::

        Returned string is not :c:func:`alloc()`\ated, do not :c:func:`free()` it.

    :param path_link_node: A path node.
    :returns: Pointer to a non-allocated, null-terminated (``'\0'``) string if successful,
        :c:data:`NULL` otherwise.
    :seealso: :c:func:`cc_path_node_linkage_as_string()`

.. _lbl-libcc-ccpath-linkedpathbacktracking:

Linked path backtracking
^^^^^^^^^^^^^^^^^^^^^^^^

For a given path node, :c:member:`next` members simply concatenate to existing path;
other members provide list of alternative, or substitute paths, and so has to find
path node to replace, or alter; this is mostly the first node of a list of alternative,
substitute paths.

Starting node is to be found by using :c:member:`back__w`, and checking if parent
node points to a current node, e.g. if :c:expr:`current->back__w->alt == current`
still holds true; once it doesn't, this is starting node.

If node was in a list of alternative paths, starting node is then replaced by node
from a list, if an alternative list was started with immediate :c:member:`alt`. If
an alternative list was started with :c:member:`fork`, then node from an alternative
list is concatenated to starting node.

Backtracking is also similar for substitute paths, except check for parent node is
:c:expr:`current->back__w->sub == current`. Once found, side-effect of the last step
in starting node is overridden by a side-effect from a node in a substitute list.

.. _lbl-libcc-ccpath-linkedpathsideeffects:

Linked path side-effects
------------------------

.. c:struct:: CcPathSideEffectLink

    Linked side-effects :c:`struct`\ure, linked list.

    .. c:member:: CcPathLinkNodeLinkageEnum link

        Type of path linkage.

    .. c:member:: CcSideEffect side_effect

        A side-effects.

    .. c:member:: struct CcPathSideEffectLink * next

        Next side-effect in a linked list.

    :c:`struct` is tagged with the same :c:struct:`CcPathSideEffectLink` name.

.. c:function:: CcPathSideEffectLink * cc_path_side_effect_link__new( CcPathLinkNodeLinkageEnum link, CcSideEffect side_effect )

    Returns a newly allocated side-effect link.

    :param link: Type of path linkage.
    :param side_effect: A side-effect.
    :returns: A newly allocated side-effect link if successful, :c:data:`NULL` otherwise.

.. c:function:: CcPathSideEffectLink * cc_path_side_effect_link_append( CcPathSideEffectLink ** side_effect_link__iod_a, CcPathLinkNodeLinkageEnum link, CcSideEffect side_effect )

    Appends a newly allocated side-effect link to a given linked list.

    If linked list :c:`*side_effect_link__iod_a` is :c:data:`NULL`, it will be initialized
    with a newly allocated side-effect link as its only element.

    :param side_effect_link__iod_a: **Ownership**, *optional* *input/output* parameter;
        linked list of side-effects to which a new side-effect is appended, inner pointer
        can be :c:data:`NULL`.
    :param link: Type of path linkage.
    :param side_effect: A side-effect.
    :returns: A weak pointer to newly allocated side-effect link if successful,
        :c:data:`NULL` otherwise.

.. c:function:: CcPathSideEffectLink * cc_path_side_effect_link_duplicate_all__new( CcPathSideEffectLink * side_effect_link )

    Duplicates all given side-effects into a newly allocated linked list.

    :param side_effect_link: Linked list to duplicate.
    :returns: A newly allocated linked list if successful, :c:data:`NULL` otherwise.

.. c:function:: CcPathSideEffectLink * cc_path_side_effect_link_extend( CcPathSideEffectLink ** side_effect_link__iod_a, CcPathSideEffectLink ** side_effect_link__n )

    Extends given linked list of side-effects with another.

    If linked list to extend (:c:`side_effect_link__iod_a`) hasn't been allocated yet,
    this will initialize it with content of an extending linked list, i.e.
    :c:`side_effect_link__n`.

    .. note::

        Extending linked list :c:`side_effect_link__n` has its ownership transferred to
        extended linked list :c:`side_effect_link__iod_a`; as a result, inner pointer
        :c:`*side_effect_link__n` is :c:data:`NULL`\ed.

    :param side_effect_link__iod_a: **Ownership**, *optional* *input/output*; linked list to extend.
    :param side_effect_link__n: **Ownership transfer**, *optional*; linked list to extend existing side-effects.
    :returns: Weak pointer to extended portion of a linked list if successful,
              :c:data:`NULL` otherwise.

.. c:function:: bool cc_path_side_effect_link_free_all( CcPathSideEffectLink ** side_effect_link__f )

    Frees all side-effects in a linked list.

    :param side_effect_link__f: Linked list of side-effects.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. c:function:: size_t cc_path_side_effect_link_len( CcPathSideEffectLink * side_effect_link )

    Function returning length of linked list of side-effects.

    :param side_effect_link: Linked list of side-effects.
    :returns: Length if successful, ``0`` otherwise.

.. c:function:: char * cc_path_side_effect_link_to_string__new( CcPathSideEffectLink * side_effect_link )

    Function returns a newly allocated string representing a given linked list of side-effects.

    :param side_effect_link: Linked list of side-effects.
    :returns: Valid pointer if successful, :c:data:`NULL` otherwise.

.. _lbl-libcc-ccpath-sourcecodeheader:

Header file
-----------

Included source header file is ``cc_path.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_path.h
    :language: C
    :linenos:

.. _lbl-libcc-ccpath-sourcecodefile:

Source code file
----------------

Included source code file is ``cc_path.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_path.c
    :language: C
    :linenos:
