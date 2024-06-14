.. Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
   Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See LICENSING, COPYING files for details.

.. _lbl-memory-management:

Memory management
=================

Entity in this text refers to any
`plain-old-data <https://en.wikipedia.org/wiki/Passive_data_structure>`_
structure, which can be ``alloc()``-ated on the heap.

Here, these are mostly linked ``struct``-s, usually containing ``union``-s.

.. _lbl-memory-management-ownership:

Ownership
---------

Ownership defines who (which pointer) gets to ``free()`` allocated memory.

It refers to the one pointer variable, which is the reference from which all
other usages are borrowed.

.. _lbl-memory-management-ownership-variables:

Variables
^^^^^^^^^

If a pointer, as a standalone variable, has ownership over any entity, ``__a``
is appended to its name, e.g. ``CcChessboard * cb__a``.

If ownership is transferred to a function (its parameter), some entity, or to
another variable, ``__t`` is appended.

So, ``__a`` is appended to a pointer name if that pointer has entity ownership
during its whole scope, this includes a case of a function which returns that
pointer, since it also returns ownership.

Trailing ``__a`` is also appended if pointer is the last one in a chain of
ownership transfers, since that pointer needs to be ``free()``-ed always,
unconditionally.

If pointer has temporary ownership of an entity, ``__t`` is appended to its name,
i.e. if pointer has to be ``free()``-ed sometimes, only under some conditions.

Transfer of ownership ``__t`` indicates what would happen if main line of execution
is followed, i.e. if all data is valid.

It is possible that some condition fails, and ownership was not transferred, in
which case just before bail-out, beside any valid ownership variables, all live
transfer variables must be cleaned as well.

For instance, lets say that up to certain point in code we have instantiated
variables, but we haven't yet transferred ownership of plies and steps to a
larger entity:

.. code-block:: C
    :force:

    CcChessboard * cb__a = cc_chessboard__new( ... ); // ownership (asset)
    ...
    CcParsedPly * plies_0__t = cc_parsed_ply__new( ... ); // transfer
    ...
    CcParsedStep * steps_2__t = cc_parsed_step_none__new( ... ); // ditto

Now, if appending a new step fails, it also has to clean-up all owned and all
alive transfer variables, before it can bail-out:

.. code-block:: C
    :force:

    if ( !cc_parsed_step_none_append( steps_2__t, ... ) ) {
        cc_parsed_step_free_all( &steps_2__t );
        cc_parsed_ply_free_all( &plies_0__t );
        cc_chessboard_free_all( &cb__a );
        return false;
    }

..        return cc_game_move_data_free_all( NULL, &cb__a, NULL, &plies_0__t, &steps_2__t, false );

If a variable is a borrow, nothing needs to be appended to its name.

For variables holding weak pointers returned from a function, append ``__w`` to
variable name, for instance:

.. code-block:: C
    :force:

    CcMoves * moves__w = cc_parsed_move_append( moves__io, an, max_len__d );

Do the same (i.e. append ``__w`` to its name) for weak pointer variables that
will be returned from a function, for example:

.. code-block:: C
    :force:

    CcParsedStep * cc_parsed_step_duplicate_all__new( ... ) {
        // Function returns weak pointer, for read+write borrow, or for checking if append passed ok.
        CcParsedStep * step__w = cc_parsed_step_append( ... );

        if ( !step__w ) {
            // Failed append --> ownership not transferred ...

            // free( step__w ); // Do *not* clean borrowers!

            return NULL; // Indicate failure.
        }

        return step__w; // If everything went ok, return weak pointer.
    }

Ownership transfer indicator has precedence over its weak sibling. So, in a
function returning weak pointer to allocated memory, used pointer gets ``__t``
indicator, since it does *temporary* own allocated memory, even if will become
weak before it gets returned.

.. code-block:: C
    :force:

    CcParseMsgs * cc_parse_msg_append( ... )
    {
        CcParseMsg * pm__t = malloc( ... );

        if ( something_failed ) free( pm__t ); // pm__t is still a temporary owner.

        ... = pm__t; // Ownership transferred, pm__t is now weak pointer.

        return pm__t; // Weak pointer is returned.
    }

.. _lbl-memory-management-ownership-entities:

Entities
^^^^^^^^

Every ``alloc()``-ated entity has implicit ownership over all links (pointers) to
other ``alloc()``-ated entities, and, by extension, over all accessible entities
in a linked structure.

Note that in a linked list, entity in the middle has ownership only over entities
in the tail of that linked list; only the first entity has the complete ownership
of the entire linked list.

If a pointer in an entity does not have ownership over linked entity, ``__w`` is
appended to its name, e.g. ``CcParsedPly * related_ply__w``.

Function(s) ``free()``-ing containing entity does not ``free()`` weak pointers.

For instance, ``CcParsedMove`` contains ``CcParsedPly *``, so it owns all
``CcParsedPly`` items in that linked list.

Now, each ``CcParsedPly`` contains ``CcParsedStep *``, so it owns all ``CcParsedStep``
items in that linked list.

So, ``CcParsedMove`` indirectly owns every ``CcParsedStep`` in the whole structure.

This is evidenced when ``free()``-ing hierarchically complete structure from a single
``CcParsedMove`` pointer.

All ``CcParsedMove``s in a linked list are ``free()``-ed by calling ``cc_move_free_all_moves()``,
which ``free()``-s all linked ``CcParsedPly``s in each ``CcParsedMove`` (by calling ``cc_ply_free_all_plies()``),
which ``free()``-s all linked ``CcParsedStep``s in each ``CcParsedPly`` (by calling ``cc_parsed_step_free_all_steps()``).

.. _lbl-memory-management-ownership-transfer:

Transfer of ownership
^^^^^^^^^^^^^^^^^^^^^

Transfer of ownership from a functions which allocates new memory is indicated by
function name ending in ``__new``, e.g. ``cc_ply_teleport__new()``.

If function name does not end in ``__new``, then returned pointer is borrowed, e.g.
``cc_ply_get_steps()``.

.. _lbl-memory-management-ownership-borrows:

Borrows
^^^^^^^

Whether borrow is mutable or not can be seen in a function return type, if returned
pointer points to ``const`` entity, that is immutable borrow.

Pointers returned from a function usually are mutable borrows
(e.g. ``CcParsedStep * cc_ply_get_steps()``), although there are also read-only
borrows (e.g. ``char const * cc_variant_label()``).

.. _lbl-memory-management-parameters:

Parameters
----------

Pointers as function parameters are usually input, read-only borrows.

Strings (i.e. ``char *``) have their underlying type ``const``-ed
(i.e. ``char const *``), most other types do not have ``const``.

For instance, ``char const * str``, ``CcParsedMove * moves``.

.. _lbl-memory-management-parameters-optional:

Optional parameters
^^^^^^^^^^^^^^^^^^^

Discretional parameters are indicated by appending ``__d`` to their name,
e.g. ``int disamb_i__d``.

For pointers, ``NULL`` is used if optional parameter is not given.

For other types check which value(s) are used to convey absence of a valid value.

In a given example, disambiguation coordinate is optional, with ``CC_INVALID_COORD``
used as an absence value.

Multi-pointer parameters can be optional not just on data (more precisely, inner-most
pointer), but also on any other pointer level.

For each optional pointer an additional ``d`` is appended to existing ``__d`` indicator.

If pointer is not optional, an ``m`` is appended, to keep track of indirection.

Each ``d`` or ``m`` corresponds to one pointer, starting from inner-most pointer
outwards, i.e. in reverse order to pointers declaration.

For instance, ``CcParseMsg ** parse_msgs__dd`` means both data (inner pointer),
and outer pointer are optional.

Another example, ``CcParseMsg ** parse_msgs__md`` means outer pointer is optional,
but inner pointer (data) is mandatory,

i.e. if outer pointer is provided, inner pointer must also be valid (non-``NULL``).

All indicators for the outmost pointers that are mandatory can be omitted.

For instance, ``CcParseMsg ** parse_msgs__d`` is treated the same as
``CcParseMsg ** parse_msgs__dm``.

.. _lbl-memory-management-parameters-output:

Output parameters
^^^^^^^^^^^^^^^^^

Output parameters are indicated by appending ``__o``, e.g. ``char * str__o``.

Output parameter is implicitly void; that is, pointer *must* be ``NULL``, and
then it's up to called function to allocate storage.

.. note::

    If function through output parameter returns pointer to fixed, pre-allocated
    storage (e.g. string constants), then output parameter can to be marked as
    optional by also appending ``__d`` to its name, e.g. ``char * str__od``.

Input / output parameters (mutable borrows) are indicated by appending
``__io`` to their name, e.g. ``char * str__io``.

Input / output parameter is implicitly mandatory (that is, given pointer
*must not* be ``NULL``). So, pointer has to have ``__d`` appended to its
name if its optional, like so ``char * str__iod``.

.. _lbl-memory-management-parameters-transfer:

Ownership transfer parameters
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Ownership transfer parameters are indicated by:

* their type (pointer to pointer to type), e.g. ``CcParseMsg ** parse_msgs``
* appending direction indicator (``__o``, ``__io``) to parameter name if they are
  output, or input + output parameter
* appending ``__n`` if inner pointer is going to be ``NULL``-ed, e.g.
  ``CcParsedPly ** plies__n``
* appending ``__f`` if inner pointer is going to be ``free()``-ed then ``NULL``-ed,
  e.g. ``char ** str__f``
* appending ``__r`` if inner pointer is going to be ``realloc()``-ated, e.g.
  ``char ** str_io__r``
* appending ``__t`` if inner pointer is going to transfer ownership into function,
  e.g. ``char ** str__t``
* appending ``__a`` if inner pointer is going to transfer ownership out of a function,
  e.g. ``char ** str__a``
* appending ``__F`` if inner pointer is going to be *conditionally* ``free()``-ed
  then ``NULL``-ed, e.g. ``CcRoutePin ** route_pin__io_a_F``

If parameter is input only, use ``__t`` to specify that ownership is given into that
function, and remaining pointer is weak after function returns.

Indicator ``__a`` is used when data can be allocated within function, and passed via
output parameters.

For instance, in all append functions linked list can be given just as an address
of a ``NULL``-initialized pointer variable, which can then be initialized with newly
allocated item as its first, and only element.

Another example, if optional output string ``char ** str__iod`` can also be allocated
from within function, it has to have ``__a`` appended, like so ``char ** str__iod_a``.

If parameter is output only, appending ``__a`` to the parameter specifies that
ownership is taken out from the function.

If parameter is input + output, ownership is retained throughout, and after the
call to the function.

Ownership transfer indicator (one of ``__n``, ``__f``, ``__r``, ``__a``) tells what
will happen to inner pointer (i.e. to ``*arg`` if ``arg`` is passed into ``Foo **``
type parameter), if main line is executed; that is to say, if all parameters were
valid, and all sanity checks passed.

Input + output arguments can be allocated within function, and used in multiple
consecutive calls as an external variable.

Function can also deallocate its argument after multiple consecutive calls when
it's done with such an argument, after some conditions are met, or after an error.

In such a case ``__F`` is appended to parameter name, to specify that data can
be freed within function (and (inner) pointer set to ``NULL``) *conditionally*.

For example, iterator ``cc_route_pin_iter()`` traverses over given path tree.

For the first call over a new path tree (i.e. if ``*route_pin__io_a_F`` is ``NULL``),
it allocates a new route, and initializes it with a first one found in a given path
tree.

On each consecutive call, it returns next route from starting to destination field
via input / output parameter ``route_pin__io_a_F``.

When it runs out of routes in a given path tree, it frees allocated route, and sets
its pointer back to ``NULL``, so it's ready to start over again.

.. _lbl-memory-management-parameters-free:

Free parameters
^^^^^^^^^^^^^^^

Free parameters are input parameters which point to an element in a container
(e.g. linked list) that needs to be ``free()``-ed,

either just pointed-to element, or a larger sub-container, but not the whole
container itself.

These are indicated by appending ``__f`` to parameter name, e.g.
``CcRoutePin * rp__f``.

Unlike corresponding ownership transfer parameter with the same ``__f`` indicator,
free parameter pointer is single (i.e. ``CcRoutePin * rp__f`` and not
``CcRoutePin ** rp__f``),

since container continues to live, and thus given pointer to it is not ``NULL``-ed.

.. _lbl-memory-management-parameters-weak:

Weak parameters
^^^^^^^^^^^^^^^

Weak parameters are indicated by appending ``__w`` to their name, e.g.
``ply_start__w``.

They are the same as input, read-only borrows, only they are stored in some
structure, as opposed to just being used within called function; for example,
``char * ply_start__w``.

Since lifetime of a data pointed to by weak pointer depends on external owner,
it's best to be used within hierarchical structure, where weak pointers from
children points to their parents.

.. _lbl-memory-management-summary:

Summary
-------

If multiple indicators are needed, direction indicator (one of ``__o``, ``__io``)
is applied first, followed by discretion indicator (one of ``__d``, ``__m``), finally
followed by ownership transfer indicator (one of ``__w``, ``__t``, ``__a``, ``__n``,
``__f``, ``__r``).

*Static*, direction and discretion indicators can be combined, e.g. ``move__iod``.
Ownership transfer indicator is always kept separated, i.e. if any of direction or
discretion indicators are combined with ownership transfer indicator, they are
separated by one underscore (``_``), e.g. ``str__d_f``. ``move__iod_r``.

.. _lbl-memory-management-summary-functions:

Functions table
^^^^^^^^^^^^^^^

.. .. | Indicator |           ``return`` |                  ``*return`` |
.. .. | --------: | -----------------: | -------------------------: |
.. .. |           |             borrow | read (+ write, if mutable) |
.. .. |   ``__new`` | ownership transfer |    read + write + ``free()`` |

..   :widths: 15 45 35

.. list-table:: Functions table
   :header-rows: 1
   :align: left

   * - Indicator
     - ``return``
     - ``*return``
   * -
     - borrow
     - read (+ write, if mutable)
   * - ``__new``
     - ownership transfer
     - read + write + ``free()``

.. _lbl-memory-management-summary-variables:

Variables table
^^^^^^^^^^^^^^^

.. .. | Indicator |     Variable |              ``var`` |                  ``*var`` |
.. .. | --------: | -----------: | -----------------: | ----------------------: |
.. .. |           |   standalone |             borrow |            read + write |
.. .. |     ``__a`` |   standalone |  asset (ownership) | read + write + ``free()`` |
.. .. |     ``__t`` |   standalone | ownership transfer |            read + write |
.. .. |           | in an entity |          ownership | read + write + ``free()`` |
.. .. |     ``__w`` |         both |               weak |            read + write |

..   :widths: 15 20 35 55

.. list-table:: Variables table
   :header-rows: 1
   :align: left

   * - Indicator
     - Variable
     - ``var``
     - ``*var``
   * -
     - standalone
     - borrow
     - read + write
   * - ``__a``
     - standalone
     - asset (ownership)
     - read + write + ``free()``
   * - ``__t``
     - standalone
     - ownership transfer
     - read + write
   * -
     - in an entity
     - ownership
     - read + write + ``free()``
   * - ``__w``
     - both
     - weak
     - read + write

.. _lbl-memory-management-summary-ioparams:

Input, output parameters table
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. .. | Indicator |               ``arg`` |            ``*arg`` |
.. .. | --------: | ------------------: | ----------------: |
.. .. |           |               input |              read |
.. .. |     ``__s`` |       input, ``NULL`` |    read, *static* |
.. .. |     ``__o`` |              output |             write |
.. .. |    ``__io`` |      input + output |      read + write |
.. .. |     ``__d`` | input, discretional |              read |
.. .. |     ``__e`` |      output, ``NULL`` |             write |
.. .. |     ``__w`` |         input, weak |              read |
.. .. |     ``__f`` |            ``free()`` |               [1] |

   .. :widths: 15 35 35

.. list-table:: Input, output parameters table
   :header-rows: 1
   :align: left

   * - Indicator
     - ``arg``
     - ``*arg``
   * -
     - input
     - read
   * - ``__o``
     - output, ``NULL``
     - write
   * - ``__io``
     - input + output, ``!NULL``
     - read + write
   * - ``__d``
     - input, discretional
     - read
   * - ``__w``
     - input, weak
     - read
   * - ``__f``
     - ``free()``
     - [1]_

..   * - ``__e``
..     - output, ``NULL``
..     - write

.. _lbl-memory-management-summary-transfer:

Ownership transfer parameters table
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. .. | Indicator |              ``arg`` |                                ``*arg`` |                                   ``**arg`` |
.. .. | --------: | -----------------: | ------------------------------------: | ----------------------------------------: |
.. .. |           |            ``!NULL`` |                                 input |                                      read |
.. .. |     ``__s`` |            ``!NULL`` |                         input, ``NULL`` |                            read, *static* |
.. .. |     ``__o`` |            ``!NULL`` |                                output |                                     write |
.. .. |    ``__io`` |            ``!NULL`` |                        input + output |                              read + write |
.. .. |     ``__d`` |                [2] |                   input, discretional |                                      read |
.. .. |     ``__m`` |                [2] |                      input, mandatory |                                      read |
.. .. |     ``__e`` |                [2] |                        output, ``NULL`` |                                     write |
.. .. |     ``__n`` |            ``!NULL`` |                       ``*args = NULL;`` |                           ownership taken |
.. .. |     ``__f`` |            ``!NULL`` |               ``free(); *args = NULL;`` |                                     freed |
.. .. |     ``__r`` |            ``!NULL`` |                  ``*args = realloc();`` |                               reallocated |
.. .. |     ``__t`` |            ``!NULL`` |                                 input |                           ownership given |

.. .. |     ``__a`` |            ``!NULL`` |          output
 input + output | ownership taken
 ownership retained |
.. .. |     ``__F`` |            ``!NULL`` | _conditional_ ``free(); *args = NULL;`` |                     _conditionally_ freed |

.. list-table:: Ownership transfer parameters table
   :header-rows: 1
   :align: left

   * - Indicator
     - ``arg``
     - ``*arg``
     - ``**arg``
   * -
     - ``!NULL``
     - input
     - read
   * - ``__o``
     - ``!NULL``
     - output, ``NULL``
     - write
   * - ``__io``
     - ``!NULL``
     - input + output
     - read + write
   * - ``__d``
     - [2]_
     - input, discretional
     - read
   * - ``__m``
     - [2]_
     - input, mandatory
     - read
   * - ``__n``
     - ``!NULL``
     - ``*args = NULL;``
     - ownership taken
   * - ``__f``
     - ``!NULL``
     - ``free(); *args = NULL;``
     - freed
   * - ``__r``
     - ``!NULL``
     - ``*args = realloc();``
     - reallocated
   * - ``__t``
     - ``!NULL``
     - input
     - ownership given
   * - ``__a``
     - ``!NULL``
     - output
     - ownership taken
   * - ``__a``
     - ``!NULL``
     - input + output
     - ownership retained
   * - ``__F``
     - ``!NULL``
     - *conditional* ``free(); *args = NULL;``
     - *conditionally* freed

.. rubric:: Footnotes

.. [1] Frees one or more elements, but not the whole container.
.. [2] Depends on a level of indirection, i.e. to which pointer ``d``, ``m`` indicator corresponds.
