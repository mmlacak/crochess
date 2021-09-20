<!-- Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com -->
<!-- Licensed as Public Domain work, see https://en.wikipedia.org/wiki/Public_domain. -->

[<<< prev](1_organization.md "<<< prev") ||

Memory management                         {#memory_management}
=================

Entity in this text refers to any
[**plain-old-data**](https://en.wikipedia.org/wiki/Passive_data_structure "plain-old-data") structure,
which can be `alloc()`-ated on the heap. Here, these are mostly linked `struct`s, usually containing
`union`s.

Ownership
---------

Ownership defines who (which pointer) gets to `free()` allocated memory.
It refers to the one pointer variable, which is the reference from which all other usages are borrowed.

### Variables

If a pointer, as a standalone variable, has ownership over any entity, `__o` is appended to its name,
e.g. `CcChessboard * cb__o`. If ownership is transferred to a function (its parameter), some entity,
or to another variable, `__t` is appended.

So, `__o` is appended to a pointer name if that pointer has entity ownership during its whole scope,
or is the last in a chain of ownership transfers. If pointer has temporary ownership of an entity,
`__t` is appended to its name.

Transfer of ownership `__t` indicates what would happen if main line of execution is followed, i.e.
if all data is valid. It is possible that some condition fails, and ownership was not transferred,
in which case just before bail-out, all ownership and transfer variables must be cleaned as well.

For instance, lets say that up to certain point in code we have instantiated variables, but we haven't
yet transferred ownership of plies and steps to a larger entity:
@code{.c}
    CcChessboard * cb__o = cc_chessboard_new( ... );
    ...
    CcPly * plies_0__t = cc_ply_new( ... );
    ...
    CcStep * steps_2__t = cc_step_none_new( ... );
@endcode

Now, if append new step fails, it also has to clean-up all ownership and transfer variables,
before it can bail-out:
@code{.c}
    if ( !cc_step_none_append_new( steps_2__t, ... ) )
        return cc_game_move_data_free_all( NULL, &cb__o, NULL, &plies_0__t, &steps_2__t, false );
@endcode

### Entities

Every `alloc()`-ated entity has implicit ownership over all links (pointers) to other `alloc()`-ated
entities, and, by extension, over all accessible entities in a linked structure.

Note that in a linked list, entity in the middle has ownership only over entities in the tail of that
linked list; only the first entity has the complete ownership of the entire linked list.

If a pointer in an entity does not have ownership over linked entity, `__w` is appended to its name,
e.g. `CcPly * related_ply__w`. Function(s) `free()`-ing containing entity does not `free()` weak pointers.

For instance, `CcMove` contains `CcPly *`, so it owns all `CcPly` items in that linked list.
Now, each `CcPly` contains `CcStep *`, so it owns all `CcStep` items in that linked list.
So, `CcMove` indirectly owns every `CcStep` in the whole structure.

This is evidenced when `free()`-ing hierarchially complete structure from a single `CcMove` pointer.

All `CcMove`s in a linked list are `free()`-ed by calling `cc_move_free_all_moves()`, which
`free()`-s all linked `CcPly`s in each `CcMove` (by calling `cc_ply_free_all_plies()`), which
`free()`-s all linked `CcStep`s in each `CcPly` (by calling `cc_step_free_all_steps()`).

### Transfer of ownership

Transfer of ownership from a functions which allocates new memory is indicated by function name
ending in `_new`, e.g. `cc_ply_teleport_new()`. If function name does not end in `_new`, then
returned pointer is borrowed, e.g. `cc_ply_get_steps()`.

### Borrows

Whether borrow is mutable or not can be seen in a function return type, if returned pointer points
to `const` entity, that is imutable borrow. Borrows are usually mutable (e.g. `CcStep * cc_ply_get_steps()`),
although there are also read-only borrows (e.g. `char const * cc_variant_label()`).

Parameters
----------

Pointers as function parameters are usually input, read-only borrows, as indicated by their type,
`const` pointer to `const` type, e.g. `char const * const restrict str`.

### _Static_ parameters

_Static_ parameters are mostly input, read-only borrows, that initialize local static variables
inside a function body. For a first call in a series such a parameters have to be valid pointers,
afterwards they can be `NULL`s.

Those _static_ parameters are usually used to initialize a kind of an iterator (or generator),
after which said iterator (or generator) continues to return valid values until it runs out of
them, or is initialized again.

_Static_ parameters are indicated by appending `_s` to parameter name, e.g. `char const * const restrict str_s`.

### Output parameters

Output parameters (mutable borrows) are indicated by their type (`const` pointer to type), and
appending either `_o`, or `_io` to their name, depending if they are pure output parameter, or
input+output one, e.g. `char * const restrict str_io`.

### Ownership transfer parameters

Ownership transfer parameters are indicated by:
- their type (pointer to pointer to type), e.g. `CcParseMsg ** parse_msgs`
- appending direction indicator (`_o`, `_io`) to parameter name if they are output, or input+output parameter
- appending `__n` if inner pointer is going to be `NULL`-ed, e.g. `CcPly ** restrict plies__n`
- appending `__f` if inner pointer is going to be `free()`-ed then `NULL`-ed, e.g. `char ** restrict str__f`
- appending `__r` if inner pointer is going to be `realloc()`-ated, e.g. `char ** const restrict str_io__r`

Ownership transfer indicator (one of `__n`, `__f`, `__r`) tells what will happen to inner pointer
(i.e. to `*arg` if `arg` is passed), if main line is executed; that is to say, if all parameters
were valid.

Summary
-------

If multiple indicators are needed, _static_ indicator (`_s`) is apended to parameter name first, followed
by direction indicator (one of `_o`, `_io`), finally followed by ownership transfer indicator (one of
`__w`, `__o`, `__n`, `__f`, `__r`).

_Static_ and direction indicators can be combined, ownership transfer indicator is always kept separated,
e.g. `str_sio__n`.

### Functions

| Indicator |           `return` |                  `*return` |
| --------: | -----------------: | -------------------------: |
|           |             borrow | read (+ write, if mutable) |
|    `_new` | ownership transfer |    read + write + `free()` |

### Variables

| Indicator |     Variable |              `var` |                  `*var` |
| --------: | -----------: | -----------------: | ----------------------: |
|           |   standalone |             borrow |            read + write |
|     `__o` |   standalone |          ownership | read + write + `free()` |
|     `__t` |   standalone | ownership transfer |            read + write |
|           | in an entity |          ownership | read + write + `free()` |
|     `__w` |         both |               weak |            read + write |

### Input, output parameters

| Indicator |          `arg` |            `*arg` |
| --------: | -------------: | ----------------: |
|           |          input |              read |
|      `_s` |  input, `NULL` |    read, _static_ |
|      `_o` |         output |             write |
|     `_io` | input + output |      read + write |

### Ownership transfer parameters

| Indicator |          `arg` |                  `*arg` |         `**arg` |
| --------: | -------------: | ----------------------: | --------------: |
|           |        `!NULL` |                   input |            read |
|      `_s` |        `!NULL` |           input, `NULL` |  read, _static_ |
|      `_o` |        `!NULL` |                  output |           write |
|     `_io` |        `!NULL` |          input + output |    read + write |
|     `__n` |        `!NULL` |         `*args = NULL;` | ownership taken |
|     `__f` |        `!NULL` | `free(); *args = NULL;` |           freed |
|     `__r` |        `!NULL` |    `*args = realloc();` |     reallocated |

[<<< prev](1_organization.md "<<< prev") ||
