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
e.g. `CcChessboard * cb__o`.

`__o` is not appended, if ownership is transferred to a function (its parameter), or to another variable.

So, `__o` is appended to a pointer name if that pointer has entity ownership during its whole scope,
or is the last in a chain of ownership transfers.

### Entities

Every `alloc()`-ated entity has implicit ownership over all links (pointers) to other `alloc()`-ated
entities, and, by extension, over all accessible entities in a linked structure.

Note that in a linked list, entity in the middle has ownership only over entities in the tail of that
linked list; only the first entity has the complete ownership of the entire linked list.

If a pointer in an entity does not have ownership over linked entity, `_w` is appended to its name,
e.g. `CcPly * related_ply_w`. Function(s) `free()`-ing containing entity does not `free()` weak pointers.

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
- appending `_n` if inner pointer is going to be `NULL`-ed, e.g. `CcPly ** restrict plies_n`
- appending `_f` if inner pointer is going to be `free()`-ed then `NULL`-ed, e.g. `char ** restrict str_f`
- appending `_r` if inner pointer is going to be `realloc()`-ated, e.g. `char ** const restrict str_io_r`

Ownership transfer indicator (one of `_n`, `_f`, `_r`) tells what will happen to inner pointer
(i.e. to `*arg` if `arg` is passed), if main line is executed; that is to say, if all parameters
were valid.

Summary
-------

If multiple indicators are needed, _static_ indicator (`_s`) is apended to parameter name first, followed
by direction indicator (one of `_o`, `_io`), finally followed by ownership transfer indicator (one of
`_w`, `__o`, `_n`, `_f`, `_r`).

_Static_ and direction indicators can be combined, ownership transfer indicator is always kept separated,
e.g. `str_sio_n`.

### Functions

| Indicator |           `return` |                  `*return` |
| --------: | -----------------: | -------------------------: |
|           |             borrow | read (+ write, if mutable) |
|    `_new` | ownership transfer |    read + write + `free()` |

### Variables

| Indicator |     Variable |     `var` |                  `*var` |
| --------: | -----------: | --------: | ----------------------: |
|           |   standalone |    borrow |            read + write |
|     `__o` |   standalone | ownership | read + write + `free()` |
|           | in an entity | ownership | read + write + `free()` |
|      `_w` |         both |      weak |            read + write |

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
|      `_n` |        `!NULL` |         `*args = NULL;` | ownership taken |
|      `_f` |        `!NULL` | `free(); *args = NULL;` |           freed |
|      `_r` |        `!NULL` |    `*args = realloc();` |     reallocated |

[<<< prev](1_organization.md "<<< prev") ||
