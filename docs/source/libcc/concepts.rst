.. Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.

.. include:: ../defines.rst

.. _lbl-libcc-concepts:

Concepts
========

Explanation of some concepts, design choices in ``libcrochess``.

.. _lbl-libcc-concepts-validity:

Validity
--------

For :c:`int`\eger values, one is designated to be invalid, all the others are
then considered valid.

This is mostly relevant to coordiantes, various positions, steps, etc. where one
value (in this case, :c:macro:`CC_INVALID_COORD`) is invalid, all the other are
valid, even if a coordinate may be way off-board.

For :c:`enum`\s, first :c:`macro` checks if a given value is enumerated within
the :c:`enum`; this macro is named ``"is enumerator?"``, e.g.
:c:enumerator:`CC_PARSED_STEP_LINK_IS_ENUMERATOR`.

Second :c:`macro` also checks if given value is different from pre-designed
:c:`None` (or :c:`Void`, or :c:`Empty`) enumeration value; this macro is named
``"is valid?"``, e.g. :c:enumerator:`CC_PARSED_STEP_LINK_IS_VALID`.

.. _lbl-libcc-concepts-positionssteps:

Positions, steps
----------------

Position is a pair of coordinates, used to address location (a field) on a
chessboard.

Position is an absolute address, all positions are coordinated against the same
origin, i.e. ``(0, 0)``, a.k.a. ``a1`` field.

Step is relative position, i.e. difference between two absolute positions.

Steps are applied multiple times to a given position, to produce complete
movement of a piece.

.. _lbl-libcc-concepts-strings:

Strings
-------

All newly allocated strings are zero-terminated (i.e. they end in ``'\0'``).

Array strings are not necessary zero-terminated even though arrays are zeroed,
since it's possible for a string to be exactly the same size as a holding array.

Always do use array size, to limit processing to array proper.

For strings starting, ending, first and last characters, length, and size are
defined like so::

    start <-------- size ----------> end
      |                               |
      v                               v
    +---+---+---+... ...+---+---+---+---+
    | a | b | c |       | x | y | z | 0 |
    +---+---+---+... ...+---+---+---+---+
      ^                           ^
      |                           |
    first <------ length ------> last

Length of a string counts all of its content, i.e. it includes everything from
first to last :c:`char`, without zero-terminating :c:`char`.

Size of a string counts everything from starting to ending :c:`char`,
including zero-terminating :c:`char`.

.. _lbl-libcc-concepts-listlikearrays:

List-like arrays
----------------

List-like arrays have terminating data in it to gracefully stop further processing,
even if its size is not known, or given::

    start <-------- size ----------> end
      |                               |
      v                               v
    +---+---+---+... ...+---+---+---+---+
    | A | B | C |       | X | Y | Z | # |
    +---+---+---+... ...+---+---+---+---+
      ^                           ^
      |                           |
    first <------ length ------> last

Similar to strings, length of such an array does include only content items,
while size also include terminating data.

Also similar to strings, last item in array is content item, while ending item
is always terminating item.

Guard data is defined for array type; for instance, :c:enum:`CcTypedStep` arrays
have :c:data:`CC_TYPED_STEP_INVALID` as a terminating item.

.. _lbl-libcc-concepts-arrays:

Arrays
------

Fixed-size arrays do not include terminating data, one has to use array size
to access items.

Here, all such arrays are 2D, and are used to hold initial setups of various
chessboards, either for pieces or tags.

For instance, :c:data:`CC_SETUP_BOARD_NINETEEN` holds initial piece positions
for Nineteen variant, while :c:data:`CC_SETUP_TAGS_MAYAN_ASCENDANCY` holds
initial tags for Mayan Ascendancy variant.

.. _lbl-libcc-concepts-linkedlists:

Linked lists
------------

All linked lists are :c:`struct`\s holding pointer to its own type; an item in
a linked list is called :c:term:`link`.

Each item in a linked list holds ownership over next item in a sequence, and
-by extension- to the rest of that linked list.

Linked list is simply a pointer to the first item in that list,
:c:data:`NULL`\-pointer is equivalent to an empty list.

For instance, :c:struct:`CcTypedStepLink` defines :c:term:`link` :c:`struct`,
chaining together :c:term:`link`\s builds typed steps, i.e. whole linked list.

All :c:term:`link`\s are newly allocated on the heap; so, all linked lists has
to be freed.

.. warning::

    Do not use standard :c:func:`free()` function on :c:term:`link`\s, it will
    not free any additional allocated resources.

    Use linked list specific function to free all its :c:term:`link`\s together
    with all owned resources, see :ref:`lbl-libcc-concepts-linkedlists-free` for
    details.

.. _lbl-libcc-concepts-linkedlists-macros:

Linked list macros
^^^^^^^^^^^^^^^^^^

Linked list related macros are defined in ``cc_defines.h``.

Macro :c:macro:`CC_FASTFORWARD` takes pointer variable to list (or any other
:c:`struct` having :c:member:`next` member), and fast-forwards it to the last
:c:term:`link` in a given linked list.

Another macro :c:macro:`CC_REWIND_BY` does the same as :c:macro:`CC_FASTFORWARD`,
but also takes member name to rewind, so it's not fixed to :c:member:`next`
anymore.

.. _lbl-libcc-concepts-linkedlists-new:

Linked list new()
^^^^^^^^^^^^^^^^^

All linked lists have :c:`*__new()` function, which takes all data necessary to
fill-in all members of a :c:term:`link`.

Function returns a newly allocated :c:term:`link`.

.. _lbl-libcc-concepts-linkedlists-append:

Linked list append()
^^^^^^^^^^^^^^^^^^^^

All linked lists have :c:`*_append()` function, which takes optional linked list
in addition to all data necessary to fill-in all members of a :c:term:`link`.

Function appends newly allocated :c:term:`link` to a linked list, if given
(i.e. if inner pointer is not :c:data:`NULL`); otherwise, it initializes
linked list with a :c:term:`link` as its only element.

Function returns weak pointer to a newly allocated :c:term:`link`, or
:c:data:`NULL` in case of an error.

.. _lbl-libcc-concepts-linkedlists-extend:

Linked list extend()
^^^^^^^^^^^^^^^^^^^^

Most linked lists have :c:`*_extend()` function, which takes two optional
linked lists.

:c:`*_extend()` function extends first linked list with the second; this also
transfers ownership from second linked list onto the first one.

As a result, inner pointer to second linked list is :c:data:`NULL`\ed.

Both linked lists are optional, if either (or both!) arguments are not given,
function tries to do the most reasonable thing:

    * if first linked list is not given (its inner pointer is :c:data:`NULL`),
      first linked list simply takes over second linked list

    * if second linked list is not given (its inner pointer is :c:data:`NULL`),
      no action is performed

Function returns valid weak pointer to a :c:term:`link` if successful,
otherwise :c:data:`NULL` in case of an error.

Returned weak pointer is either to first :c:term:`link` from the second linked
list, if given; or to first :c:term:`link` in the first linked list.

.. _lbl-libcc-concepts-linkedlists-free:

Linked list free()
^^^^^^^^^^^^^^^^^^

All linked lists have :c:`*_free_all()` function, which takes linked list and
deallocates all its :c:term:`link`\s, and all associated, owned resources.

.. note::

    Function calls standard :c:func:`free()` in the background; in case of
    circular linked list (or some other issue), it'll most likely crash
    ("behavior is undefined", see
    `<https://en.cppreference.com/w/c/memory/free>`_ for details).

Function returns :c:data:`true` if successful, :c:data:`false` otherwise.

.. _lbl-libcc-concepts-queue:

Queue
-----

All queues (double-linked lists) are :c:`struct`\s holding two pointer to its
own type; an item in a queue is called :c:term:`link`.

Each item in a queue holds ownership over next item in a sequence, and
-by extension- to the rest of that queue.

Queue is simply a pointer to the first item in that queue,
:c:data:`NULL`\-pointer is equivalent to an empty queue.

For instance, :c:struct:`CcParsedMove` defines :c:term:`link` :c:`struct`,
chaining together :c:term:`link`\s builds parsed moves, i.e. whole queue.

All :c:term:`link`\s are newly allocated on the heap; so, all queues has
to be freed.

.. warning::

    Do not use standard :c:func:`free()` function on :c:term:`link`\s, it will
    not free any additional allocated resources.

    Use linked list specific function to free all its :c:term:`link`\s together
    with all owned resources, see :ref:`lbl-libcc-concepts-queue-free` for
    details.

.. _lbl-libcc-concepts-queue-macros:

Queue macros
^^^^^^^^^^^^

Queue related macros are also defined in ``cc_defines.h``.

Macro :c:macro:`CC_REWIND` takes pointer variable to queue (or any other
:c:`struct` having :c:member:`prev__w` member), and rewinds it to the first
:c:term:`link` in a given queue.

Also useful is :c:macro:`CC_REWIND_BY` macro, since it takes member name to
rewind, so it's not fixed neither to :c:member:`next` nor to :c:member:`prev__w`.

.. _lbl-libcc-concepts-queue-new:

Queue new()
^^^^^^^^^^^

All queues have :c:`*__new()` function, which takes all data necessary to
fill-in all members of a :c:term:`link`.

Function returns a newly allocated :c:term:`link`.

.. _lbl-libcc-concepts-queue-append:

Queue append()
^^^^^^^^^^^^^^

All queues have :c:`*_append()` function, which takes queue in addition to all
data necessary to fill-in all members of a :c:term:`link`.

Function appends newly allocated :c:term:`link` to a queue, if given (i.e. if
inner pointer is not :c:data:`NULL`); otherwise, it initializes queue with a
:c:term:`link` as its only element.

Function returns weak pointer to a newly allocated :c:term:`link`, or
:c:data:`NULL` in case of an error.

.. _lbl-libcc-concepts-queue-free:

Queue free()
^^^^^^^^^^^^

All queues have :c:`*_free_all()` function, which takes queue and deallocates
all its :c:term:`link`\s, and all associated, owned resources.

.. note::

    Function calls standard :c:func:`free()` in the background; in case of
    circular queue (or some other issue), it'll most likely crash
    ("behavior is undefined", see
    `<https://en.cppreference.com/w/c/memory/free>`_ for details).

Function returns :c:data:`true` if successful, :c:data:`false` otherwise.
