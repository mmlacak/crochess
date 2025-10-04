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

        All :c:member:`fork`, :c:member:`alt`, and :c:member:`sub` members are
        owners of the remainder of a path tree they are pointing to; and must
        always be a singular pointer (owner) within a path tree to their
        respective forking, alternating, substitute, or subsequent path.

        Any two pointers pointing to the same node, any back-references within
        a path tree, or shared among them are not allowed, and will likely cause
        crash.

        .. note::

            Weak back-links are fine, as they are never :c:func:`free()`\-ed.

    .. seealso::

        :ref:`lbl-libcc-paths-pathsegmenttree`

    .. c:member:: CcSideEffect side_effect

        One chosen side-effect from all possible on a piece encountered in previous
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

        Link to alternative to this path node.

        Alternative paths are used when there are multiple possible interactions with
        encountered piece.

        This link should be set only after divergence, or if part of alternative
        paths, i.e. if this step has been pointed-to by either :c:member:`fork`,
        or :c:member:`alt`.

        .. seealso::

            :ref:`lbl-libcc-paths-pathsegmenttree-alternativepaths`

    .. c:member:: struct CcPathNode * sub

        Link to substitute side-effect to originating path node.

        Substitute paths are used when there are multiple possible side-effects
        (interactions with encountered piece), which do not alter path of a
        moving piece.

        .. seealso::

            :ref:`lbl-libcc-paths-pathsegmenttree-substitutepaths`

    .. c:member:: struct CcPathNode * back__w

        Weak back-link to parent path node, regardless if pointed-to by
        :c:member:`fork`, :c:member:`alt`, or :c:member:`sub`.

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

.. c:function:: CcPathNode * cc_path_node_add_fork( CcPathNode ** pn_step__a, CcPathNode ** pn_fork__n )

    Function extends forking paths of a given path step (:c:`pn_step__a`) with a
    path node (:c:`pn_fork__n`), as an additional alternative path (i.e. appends
    to :c:`pn_step__a->fork->alt` linked list).

    If a given path step doesn't have forking path yet (i.e. if :c:`pn_step__a->fork == NULL`),
    function initializes it with a given forking path.

    .. note::

        Extending path node :c:`pn_fork__n` has its ownership transferred to
        path node :c:`pn_step__a`; as a result, inner pointer :c:`*pn_fork__n`
        is :c:data:`NULL`\ed.

    :param pn_step__a: **Ownership**; a path step from which to fork.
    :param pn_fork__n: **Ownership transfer**; forking path.
    :returns: Weak pointer to alternative path if successful,
        :c:data:`NULL` otherwise.

.. c:function:: CcPathNode * cc_path_node_add_alter( CcPathNode ** pn_step__a, CcPathNode ** pn_alt__n )

    Function extends alternating paths of a given path step (:c:`pn_step__a`) with
    path node (:c:`pn_alt__n`), i.e. appends to :c:`pn_step__a->alt` linked list.

    If a given path step doesn't have alternating path yet (i.e. if :c:`pn_step__a->alt == NULL`),
    function initializes it with a given alternative path.

    .. note::

        Extending path node :c:`pn_alt__n` has its ownership transferred to
        path node :c:`pn_step__a`; as a result, inner pointer :c:`*pn_alt__n`
        is :c:data:`NULL`\ed.

    :param pn_step__a: **Ownership**; a path step from which to fork.
    :param pn_alt__n: **Ownership transfer**; alternating path.
    :returns: Weak pointer to alternative path if successful,
        :c:data:`NULL` otherwise.

.. c:function::CcPathNode * cc_path_node_add_subs( CcPathNode ** pn_step__a, CcPathNode ** pn_sub__n )

    Function extends substitute paths of a given path step (:c:`pn_step__a`) with
    path node (:c:`pn_sub__n`), i.e. appends to :c:`pn_step__a->sub` linked list.

    If a given path step doesn't have substitute path yet (i.e. if :c:`pn_step__a->sub == NULL`),
    function initializes it with a given substitute path.

    .. note::

        Extending path node :c:`pn_sub__n` has its ownership transferred to
        path node :c:`pn_step__a`; as a result, inner pointer :c:`*pn_sub__n`
        is :c:data:`NULL`\ed.

    :param pn_step__a: **Ownership**; a path step to which to add substitute
        side-effect.
    :param pn_sub__n: **Ownership transfer**; substituting path.
    :returns: Weak pointer to substitute path if successful,
        :c:data:`NULL` otherwise.

.. c:function:: CcSideEffect * cc_path_node_last_step_side_effect( CcPathNode * path_node )

    Function returns weak pointer to side-effect of a last step in a given path node.

    .. note::

        Path node is not traversed, i.e. none of :c:member:`CcPathNode.fork`,
        :c:member:`CcPathNode.alt`, or :c:member:`CcPathNode.sub` links are followed.

    :param path_node: A path node.
    :returns: Weak pointer to side-effect if successful,
              :c:data:`NULL` otherwise.

.. c:function:: CcMaybeBoolEnum cc_path_node_last_step_side_effect_is_none( CcPathNode * path_node )

    Function checks if side-effect type of a last step in a given path node is
    :c:enumerator:`CC_SETE_None`.

    .. note::

        Path node is not traversed, i.e. none of :c:member:`CcPathNode.fork`,
        :c:member:`CcPathNode.alt`, or :c:member:`CcPathNode.sub` links are followed.

    :param path_node: A path node.
    :returns: One of :c:enum:`CcMaybeBoolEnum` values:

        * :c:enumerator:`CC_MBE_True` if side-effect is none,
        * :c:enumerator:`CC_MBE_False` if side-effect is not none
        * :c:enumerator:`CC_MBE_Void` in case of an error, insufficient data given.

.. c:function:: CcMaybeBoolEnum cc_path_node_is_leaf( CcPathNode * path_node )

    Function checks if a given path node is a leaf node.

    Leaf node is one without any of  :c:member:`CcPathNode.fork`,
    :c:member:`CcPathNode.alt`, or :c:member:`CcPathNode.sub`
    valid (non-:c:data:`NULL`) links.

    :param path_node: A path node.
    :returns: One of :c:enum:`CcMaybeBoolEnum` values:

        * :c:enumerator:`CC_MBE_True` if given path node is leaf,
        * :c:enumerator:`CC_MBE_False` if given path node is not a leaf,
        * :c:enumerator:`CC_MBE_Void` in case of an error, insufficient data given.

.. c:function:: bool cc_path_node_is_valid( CcPathNode * path_node )

    Checks if given path node is valid; by checking if all positions are valid
    (i.e. not :c:data:`CC_INVALID_COORD`), and if all back-links are valid (e.g.
    if :c:`pl->fork` is non-:c:data:`NULL`, then :c:`pl->fork->back__w` must point
    back to :c:`pl`).

    :param path_node: A path node.
    :returns: :c:data:`true` if given path node is valid,
              :c:data:`false` otherwise.

.. c:function:: CcPathNode * cc_path_node_duplicate_all__new( CcPathNode * path_node )

    Duplicates complete linked tree of a given path node into a newly
    allocated one.

    :param path_node: A path node.
    :returns: A pointer to newly allocated path node if successful,
              :c:data:`NULL` otherwise.

.. c:function:: bool cc_path_node_free_all( CcPathNode ** pl__f )

    Frees all path links from complete linked tree of a given path node.

    :param pl__f: A path node to :c:func:`free()`.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. c:function:: size_t cc_path_node_count( CcPathNode * path_node )

    Function returns count of all nodes in a given path tree; includes
    :c:member:`fork`, :c:member:`alt`, :c:member:`sub` branches.

    :param path_node: A path node.
    :returns: Length of a given path tree if successful, ``0`` otherwise.

.. c:function:: size_t cc_path_node_count_all_segments( CcPathNode * path_node )

    Function returns count of all segments, including :c:member:`fork`,
    :c:member:`alt` ones; substitute paths (i.e. all nodes linked via :c:member:`sub`)
    should not have path segments (i.e. :c:member:`steps`).

    :param path_node: A path node.
    :returns: Count of all segments if successful, ``0`` otherwise.

.. c:function:: char * cc_path_node_to_string__new( CcPathNode * path_node )

    Function returns string containing user-readable representation of a given
    path node, and all of its :c:member:`fork`, :c:member:`alt`, :c:member:`sub`
    branches.

    :param path_node: A path node.
    :returns: A newly allocated, null-terminated (``'\0'``) string if
        successful, :c:data:`NULL` otherwise.
    :seealso: :c:func:`cc_pos_to_string()`

.. _lbl-libcc-ccpath-nodelinkage:

Node linkage
^^^^^^^^^^^^

.. c:enum:: CcPathNodeLinkageEnum

    Enumerates all node links in a path node, i.e. all different pointers a node
    can use to have a link to another node, see :c:struct:`CcPathNode`.

    .. c:enumerator:: CC_PNLE_NoLinkage

    .. c:enumerator:: CC_PNLE_Fork

    .. c:enumerator:: CC_PNLE_Alt

    .. c:enumerator:: CC_PNLE_Sub

    :c:`enum` is tagged with the same :c:enum:`CcPathNodeLinkageEnum` name.

.. c:macro:: CC_PATH_NODE_LINKAGE_IS_ENUMERATOR(plnle)

    Macro to check if given variant value is an enumerator, i.e. between
    :c:enumerator:`CC_PNLE_NoLinkage` and :c:enumerator:`CC_PNLE_Sub` values.

    :param plnle: Variant (integer) value.
    :returns: :c:data:`true` if enumerator, :c:data:`false` otherwise.

.. c:macro:: CC_PATH_NODE_LINKAGE_IS_VALID(plnle)

    Macro to check if given variant value is a valid enumerator, i.e. between
    :c:enumerator:`CC_PNLE_NoLinkage` and :c:enumerator:`CC_PNLE_Sub` values.

    This macro is the same as :c:macro:`CC_PATH_NODE_LINKAGE_IS_ENUMERATOR`, since
    :c:enum:`CcPathNodeLinkageEnum` does not feature *null* (or *void*, or *empty*) value.

    :param plnle: Variant (integer) value.
    :returns: :c:data:`true` if valid enumerator, :c:data:`false` otherwise.

.. c:macro:: CC_MAX_LEN_PATH_NODE_LINKAGE_STRING

    Maximum length a node linkage string can be, equals to ``4``.

.. c:macro:: CC_SIZE_PATH_NODE_LINKAGE_STRING

    Maximum size a node linkage string can be, equals to
    :c:macro:`CC_MAX_LEN_PATH_NODE_LINKAGE_STRING` + ``1``.

.. c:function:: char const * cc_path_node_linkage_as_string( CcPathNodeLinkageEnum plnle )

    Function returns string containing user-readable representation of a given
    path node linkage.

    .. note::

        Returned string is not :c:func:`alloc()`\ated, do not :c:func:`free()` it.

    :param plnle: A node linkage, :c:enum:`CcPathNodeLinkageEnum` value.
    :returns: Pointer to a non-allocated, null-terminated (``'\0'``) string if successful,
        :c:data:`NULL` otherwise.

.. c:function:: CcPathNodeLinkageEnum cc_path_node_linkage( CcPathNode * path_node )

    Function returns linkage of a given path node.

    :param path_node: A path node.
    :returns: Path node linkage, :c:enum:`CcPathNodeLinkageEnum` value.

.. c:function:: char const * cc_path_node_linkage_to_string( CcPathNode * path_node )

    Function returns string containing user-readable linkage representation
    of a given path node.

    .. note::

        Returned string is not :c:func:`alloc()`\ated, do not :c:func:`free()` it.

    :param path_node: A path node.
    :returns: Pointer to a non-allocated, null-terminated (``'\0'``) string if successful,
        :c:data:`NULL` otherwise.
    :seealso: :c:func:`cc_path_node_linkage_as_string()`

.. _lbl-libcc-ccpath-linkedpathbacktracking:

Linked path backtracking
^^^^^^^^^^^^^^^^^^^^^^^^

For a given path node, :c:member:`fork` member concatenate to existing path; other
members provide list of alternative (:c:member:`alt`), or substitute paths
(:c:member:`sub`), and so has to find path node to replace, or alter; this is mostly
the first node of a list of alternative, substitute paths.

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

Root node can be found by backtracking until :c:member:`back__w` is :c:data:`NULL`.

.. _lbl-libcc-ccpath-linkedpathsideeffects:

Linked path side-effects
------------------------

.. c:struct:: CcPathSideEffectLink

    Linked side-effects :c:`struct`\ure, linked list.

    .. c:member:: CcPathNodeLinkageEnum link

        Type of path linkage.

    .. c:member:: CcSideEffect side_effect

        A side-effects.

    .. c:member:: struct CcPathSideEffectLink * next

        Next side-effect in a linked list.

    :c:`struct` is tagged with the same :c:struct:`CcPathSideEffectLink` name.

.. c:function:: CcPathSideEffectLink * cc_path_side_effect_link__new( CcPathNodeLinkageEnum link, CcSideEffect side_effect )

    Returns a newly allocated side-effect link.

    :param link: Type of path linkage.
    :param side_effect: A side-effect.
    :returns: A newly allocated side-effect link if successful, :c:data:`NULL` otherwise.

.. c:function:: CcPathSideEffectLink * cc_path_side_effect_link_append( CcPathSideEffectLink ** side_effect_link__iod_a, CcPathNodeLinkageEnum link, CcSideEffect side_effect )

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
