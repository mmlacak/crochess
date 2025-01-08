.. Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.
   Included source code files are licensed under GNU GPL v3+ license. See LICENSING, COPYING files in root folder of the project for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccpath:

Path
====

Documents ``cc_path.h`` and ``cc_path.c`` files, which contain various
path definitions, types and functions.

.. _lbl-libcc-ccpath-linkedpathsegments:

Linked path segments
--------------------

.. c:struct:: CcPathLink

    Linked path :term:`segment` is essentially a linked list of traversed positions,
    with possible alternative paths after divergence.

    Each path :term:`segment` is comprised of path links linked only by
    :c:member:`next` member, the same as in ordinary linked list.

    Divergent :term:`path`\s are introduced by single :c:member:`diverge` path;
    all other possible :term:`path`\s after the same divergence are then linked
    by :c:member:`alt`.

    .. warning::

        All :c:member:`diverge`, :c:member:`alt`, and :c:member:`next` members
        must always be a singular pointer to any path link.

        Circular references within a path tree, or shared among them, are not
        allowed, and will likely cause crash.

    .. c:member:: CcPosDesc pos

        A position.

    .. c:member:: cc_uint_t momentum

        Momentum a moving piece had when this position was reached.

    .. c:member:: struct CcPathLink * diverge

        Link to divergent paths.

        Every divergent path has this step as its starting position.

        One divergent path links to another via :c:member:`alt` member.

    .. c:member:: struct CcPathLink * alt

        Link to alternative to this path segment.

        This link should be set only after divergence, or if part of alternative
        paths, i.e. if this step has been pointed-to by either :c:member:`diverge`,
        or :c:member:`alt`.

    .. c:member:: struct CcPathLink * next

        Link to next position descriptor in a straight path segment.

    :c:`Struct` is tagged with the same :c:struct:`CcPathLink` name.

.. c:function:: CcPathLink * cc_path_link__new( CcPos pos, cc_uint_t momentum )

    Function allocates a new path link.

    :param pos: A position.
    :param momentum: A momentum.
    :returns: Pointer to a newly allocated path link if successful,
        :c:data:`NULL` otherwise.

.. c:function:: CcPathLink * cc_path_link_append( CcPathLink ** pl__iod_a, CcPos pos, cc_uint_t momentum )

    Function appends a newly allocated path link to a given path segment,
    as its :c:member:`next` member.

    If path segment :c:`*pl__iod_a` is :c:data:`NULL`, it will be initialized
    with a newly allocated path link as its only element.

    :param pl__iod_a: **Ownership**, *optional* *input/output*; path segment.
    :param pos: A position.
    :param momentum: A momentum.
    :returns: A weak pointer to a newly allocated linked position
              if successful, :c:data:`NULL` otherwise.

.. c:function:: CcPathLink * cc_path_link_extend( CcPathLink ** pl__iod_a, CcPathLink ** pl__n )

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

.. c:function:: CcPathLink * cc_path_link_fork( CcPathLink ** pl_step__a, CcPathLink ** pl_alt__n )

    Function extends divergent paths of a given path step (:c:`pl_step__a`) with
    path segment (:c:`pl_alt__n`) as an additional alternative path.

    If a given path step doesn't have divergent path yet, function initializes it
    with a given alternative path.

    .. note::

        Extending path segment :c:`pl_alt__n` has its ownership transferred to
        diverging path segment :c:`pl_step__a`; as a result, inner pointer
        :c:`*pl_alt__n` is :c:data:`NULL`\ed.

    :param pl_step__a: **Ownership**; a path step from which to diverge.
    :param pl_alt__n: **Ownership transfer**; diverging path.
    :returns: Weak pointer to alternative path if successful,
        :c:data:`NULL` otherwise.

.. c:function:: CcPathLink * cc_path_link_duplicate_all__new( CcPathLink * path_link )

    Duplicates complete linked tree of a given path segment into a newly
    allocated one.

    :param path_link: A path segment.
    :returns: A pointer to newly allocated path segment if successful,
              :c:data:`NULL` otherwise.

.. c:function:: bool cc_path_link_free_all( CcPathLink ** pl__f )

    Frees all path links from complete linked tree of a given path segment.

    :param pl__f: A path segment to :c:func:`free()`.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. c:function:: size_t cc_path_link_len( CcPathLink * path_link, bool count_all )

    Function returns length of a given path segment; optionally also includes
    :c:member:`diverge`, :c:member:`alt` branches.

    :param path_link: A path segment.
    :param count_all: Flag, whether to include :c:member:`diverge`, :c:member:`alt`
        path segments (if :c:data:`true`), or only a given path segment
        without branching (if :c:data:`false`).
    :returns: Length of a given path segment if successful, ``0`` otherwise.

.. c:function:: size_t cc_path_link_count_all_seqments( CcPathLink * path_link )

    Function returns count of all segments, including :c:member:`diverge`,
    :c:member:`alt` ones.

    :param path_link: A path segment.
    :returns: Count of all segments if successful, ``0`` otherwise.

.. c:function:: char * cc_path_link_to_string__new( CcPathLink * path_link )

    Function returns string containing user-readable representation of a complete
    path tree, including :c:member:`diverge`, :c:member:`alt` branches.

    :param path_link: A path segment.
    :returns: A newly allocated, null-terminated (``'\0'``) string if
        successful, :c:data:`NULL` otherwise.
    :seealso: :c:func:`cc_pos_to_string()`

.. _lbl-libcc-ccpath-weaklylinkedpathsegments:

Weakly linked path segments
---------------------------

.. c:struct:: CcPathWeakLink

    Linked list of weak pointers to path segments.

    .. c:member:: CcPathLink * pl__w

        Weak pointer to a path segment.

    .. c:member:: struct CcPathWeakLink * next

        Link to next item in a list.

    :c:`Struct` is tagged with the same :c:struct:`CcPathWeakLink` name.

.. c:function:: CcPathWeakLink * cc_path_weak_link__new( CcPathLink * pl )

    Function allocates a new weak path link.

    :param pl: A path segment.
    :returns: Pointer to a newly allocated weak path link if successful,
        :c:data:`NULL` otherwise.

.. c:function:: CcPathWeakLink * cc_path_weak_link_append( CcPathWeakLink ** pwl__iod_a, CcPathLink * pl )

    Function appends a newly allocated weak path link to a given linked list.

    If linked list :c:`*pwl__iod_a` is :c:data:`NULL`, it will be initialized
    with a newly allocated weak path link as its only element.

    :param pwl__iod_a: **Ownership**, *optional* *input/output* parameter, linked list.
    :param pl: A path segment.
    :returns: Weak pointer to a newly allocated linked typed step if successful,
              :c:data:`NULL` otherwise.

.. c:function:: CcPathWeakLink * cc_path_weak_link_extend( CcPathWeakLink ** pwl__iod_a, CcPathWeakLink ** pwl__n )

    Extends existing linked list with a another one.

    If linked list to extend (:c:`pwl__iod_a`) hasn't been allocated yet,
    this will initialize it with content of an extending linked list, i.e.
    :c:`pwl__n`.

    .. note::

        Extending linked list :c:`pwl__n` has its ownership transferred to
        extended linked list :c:`pwl__iod_a`; as a result, inner pointer
        :c:`*pwl__n` is :c:data:`NULL`\ed.

    :param pwl__iod_a: **Ownership**, *optional* *input/output* parameter, linked list.
    :param pwl__n: **Ownership transfer**; linked list with which to extend existing list.
    :returns: Weak pointer to extended portion of a linked list if successful,
              :c:data:`NULL` otherwise.

.. c:function:: bool cc_path_weak_link_free_all( CcPathWeakLink ** pwl__f )

    Frees all weak path links in a linked list.

    :param pwl__f: Linked list to :c:func:`free()`.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. c:function:: size_t cc_path_weak_link_len( CcPathWeakLink * pwl )

    Function returns length of a linked list.

    :param pwl: A linked list.
    :returns: Count of weak path links if successful, ``0`` otherwise.

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
