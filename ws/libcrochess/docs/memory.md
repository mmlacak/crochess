<!-- Copyright (c) 2021, 2024 Mario Mlačak, mmlacak@gmail.com -->
<!-- Licensed as Public Domain work, see https://en.wikipedia.org/wiki/Public_domain. -->

|| [^^^ top](main.md "^^^ top") || [next >>>](design.md "next >>>")

Memory management                         {#memory_management}
=================

Entity in this text refers to any
[**plain-old-data**](https://en.wikipedia.org/wiki/Passive_data_structure "plain-old-data")
structure, which can be `alloc()`-ated on the heap. <br />
Here, these are mostly linked `struct`s, usually containing `union`s.

Ownership
---------

Ownership defines who (which pointer) gets to `free()` allocated memory. <br />
It refers to the one pointer variable, which is the reference from which all other
usages are borrowed.

### Variables

If a pointer, as a standalone variable, has ownership over any entity, `__a` is
appended to its name, e.g. `CcChessboard * cb__a`. <br />
If ownership is transferred to a function (its parameter), some entity, or to another
variable, `__t` is appended.

So, `__a` is appended to a pointer name if that pointer has entity ownership during
its whole scope, this includes a case of a function which returns that pointer, since
it also returns ownership. <br />
Trailing `__a` is also appended if pointer is the last one in a chain of ownership
transfers, since that pointer needs to be `free()`-ed always, unconditionally.

If pointer has temporary ownership of an entity, `__t` is appended to its name, i.e.
if pointer has to be `free()`-ed sometimes, only under some conditions. <br />
Transfer of ownership `__t` indicates what would happen if main line of execution is
followed, i.e. if all data is valid. <br />
It is possible that some condition fails, and ownership was not transferred, in which
case just before bail-out, beside any valid ownership variables, all transfer variables
must be cleaned as well.

For instance, lets say that up to certain point in code we have instantiated variables,
but we haven't yet transferred ownership of plies and steps to a larger entity:
@code{.c}
    CcChessboard * cb__a = cc_chessboard__new( ... ); // ownership (asset)
    ...
    CcPly * plies_0__t = cc_ply__new( ... ); // transfer
    ...
    CcStep * steps_2__t = cc_step_none__new( ... ); // ditto
@endcode

Now, if appending a new step fails, it also has to clean-up all ownership and transfer
variables, before it can bail-out:
@code{.c}
    if ( !cc_step_none_append__new( steps_2__t, ... ) )
        return cc_game_move_data_free_all( NULL, &cb__a, NULL, &plies_0__t, &steps_2__t, false );
@endcode

If a variable is a borrow, nothing needs to be appended to its name. <br />
For variables holding weak pointers returned from a function, append `__w` to variable
name, for instance:
@code{.c}
    CcMoves * moves__w = cc_moves_append_or_init( moves__io, an, max_len__d );
@endcode

Do the same (i.e. append `__w` to its name) for weak pointer variables that will be
returned from a function, for example:
@code{.c}
CcParseMsgs * cc_parse_msgs_append_or_init( ... )
{
    CcParseMsgs * pm__w = NULL;

    /* Do something with pm__w. */

    return pm__w;
}
@endcode

### Entities

Every `alloc()`-ated entity has implicit ownership over all links (pointers) to other
`alloc()`-ated entities, and, by extension, over all accessible entities in a linked
structure.

Note that in a linked list, entity in the middle has ownership only over entities in
the tail of that linked list; only the first entity has the complete ownership of the
entire linked list.

If a pointer in an entity does not have ownership over linked entity, `__w` is appended
to its name, e.g. `CcPly * related_ply__w`. <br />
Function(s) `free()`-ing containing entity does not `free()` weak pointers.

For instance, `CcParsedMove` contains `CcPly *`, so it owns all `CcPly` items in that linked
list. <br />
Now, each `CcPly` contains `CcStep *`, so it owns all `CcStep` items in that linked
list. <br />
So, `CcParsedMove` indirectly owns every `CcStep` in the whole structure.

This is evidenced when `free()`-ing hierarchically complete structure from a single
`CcParsedMove` pointer.

All `CcParsedMove`s in a linked list are `free()`-ed by calling `cc_move_free_all_moves()`,
which `free()`-s all linked `CcPly`s in each `CcParsedMove` (by calling `cc_ply_free_all_plies()`),
which `free()`-s all linked `CcStep`s in each `CcPly` (by calling `cc_step_free_all_steps()`).

### Transfer of ownership

Transfer of ownership from a functions which allocates new memory is indicated by
function name ending in `__new`, e.g. `cc_ply_teleport__new()`. <br />
If function name does not end in `__new`, then returned pointer is borrowed, e.g.
`cc_ply_get_steps()`.

### Borrows

Whether borrow is mutable or not can be seen in a function return type, if returned
pointer points to `const` entity, that is immutable borrow. <br />
Borrows are usually mutable (e.g. `CcStep * cc_ply_get_steps()`), although there are
also read-only borrows (e.g. `char * cc_variant_label()`).

Parameters
----------

Pointers as function parameters are usually input, read-only borrows. <br />
Strings (i.e. `char *`) have their underlying type `const`-ed (i.e. `char const *`),
most other types do not have `const`. <br />
For instance, `char const * str`, `CcParsedMove * moves`.

### _Static_ parameters

_Static_ parameters are mostly input, read-only borrows, that initialize local static
variables inside a function body. <br />
For a first call in a series such a parameters have to be valid pointers, afterwards
they can be `NULL`s.

Those _static_ parameters are usually used to initialize a kind of an iterator (or
generator), after which said iterator (or generator) continues to return valid values
until it runs out of them, or is initialized again.

_Static_ parameters are indicated by appending `__s` to parameter name, e.g.
`char * str__s`.

### Optional parameters

Discretional parameters are indicated by appending `__d` to their name, e.g.
`int disamb_i__d`. <br />
For pointers, `NULL` is used if optional parameter is not given. <br />
For other types check which value(s) are used to convey absence of a valid value. <br />
In a given example, disambiguation coordinate is optional, with `CC_INVALID_COORD`
used as an absence value.

Multi-pointer parameters can be optional not just on data (more precisely, inner-most
pointer), but also on any other pointer level. <br />
For each optional pointer an additional `d` is appended to existing `__d` indicator. <br />
If pointer is not optional, an `m` is appended, to keep track of indirection. <br />
Each `d` or `m` corresponds to one pointer, starting from inner-most pointer outwards,
i.e. in reverse order to pointers declaration.

For instance, `CcParseMsg ** parse_msgs__dd` means both data (inner pointer), and
outer pointer are optional. <br />
Another example, `CcParseMsg ** parse_msgs__md` means outer pointer is optional, but
inner pointer (data) is mandatory, <br />
i.e. if outer pointer is provided, inner pointer must also be valid (non-`NULL`).

All indicators for the outmost pointers that are mandatory can be omitted. <br />
For instance, `CcParseMsg ** parse_msgs__d` is treated the same as
`CcParseMsg ** parse_msgs__dm`.

### Output parameters

Output parameters (mutable borrows) are indicated by appending either `__o`, or `__io`
to their name, depending if they are just output parameter, or input + output one, e.g.
`char * str__io`.

Output parameter, i.e. one named with `__o`, is also implicitly optional, so
`char * str__o` is treated the same as `char * str__od`. <br />
Input / output parameter is implicitly mandatory, and has to have `__d` appended to its
name if its optional, like so `char * str__iod`.

### Empty parameters

Empty parameters are output parameters (i.e. pointers), which must always be empty when calling a function. <br />
They refer to data only, i.e. the innermost pointer must always be `NULL`, pointer to it may be optional or mandatory. <br />
Empty parameters are indicated by appending `__e` to their name, e.g. `CcPos * pos__e`.

When using multi-pointer parameters, empty output parameter indicator precedes any optional/mandatory ones. <br />
For instance, `CcPos ** pos__ed` declares output parameter, which is optional pointer (`__d`) to empty data pointer (`__e`).

### Ownership transfer parameters

Ownership transfer parameters are indicated by:
- their type (pointer to pointer to type), e.g. `CcParseMsg ** parse_msgs`
- appending direction indicator (`__o`, `__io`) to parameter name if they are output, or input + output parameter
- appending `__n` if inner pointer is going to be `NULL`-ed, e.g. `CcPly ** plies__n`
- appending `__f` if inner pointer is going to be `free()`-ed then `NULL`-ed, e.g. `char ** str__f`
- appending `__r` if inner pointer is going to be `realloc()`-ated, e.g. `char ** str_io__r`
- appending `__t` if inner pointer is going to transfer ownership into function, e.g. `char ** str__t`
- appending `__a` if inner pointer is going to transfer ownership out of a function, e.g. `char ** str__a`
- appending `__F` if inner pointer is going to be _conditionally_ `free()`-ed then `NULL`-ed, e.g. `CcRoutePin ** route_pin__io_a_F`

If parameter is input only, use `__t` to specify that ownership is given into that
function, and remaining pointer is weak after function returns.

Indicator `__a` is used when data can be allocated within function, and passed via
output parameters. <br />
For instance, in all append functions linked list can be given just as an address
of a `NULL`-initialized pointer variable, which can then be initialized with newly
allocated item as its first, and only element. <br />
Another example, if optional output string `char ** str__iod` can also be allocated
from within function, it has to have `__a` appended, like so `char ** str__iod_a`.

If parameter is output only, appending `__a` to the parameter specifies that ownership
is taken out from the function. <br />
If parameter is input + output, ownership is retained throughout, and after the call
to the function.

Ownership transfer indicator (one of `__n`, `__f`, `__r`, `__a`) tells what will
happen to inner pointer (i.e. to `*arg` if `arg` is passed into `Foo **` type
parameter), if main line is executed; that is to say, if all parameters were valid,
and all sanity checks passed.

Input + output arguments can be allocated within function, and used in multiple
consecutive calls as an external variable. <br />
Function can also deallocate its argument after multiple consecutive calls when
it's done with such an argument, after some conditions are met, or after an error.  <br />
In such a case `__F` is appended to parameter name, to specify that data can be
freed within function (and (inner) pointer set to `NULL`) _conditionally_.

For example, iterator `cc_route_pin_iter()` traverses over given path tree. <br />
For the first call over a new path tree (i.e. if `*route_pin__io_a_F` is `NULL`), it
allocates a new route, and initializes it with a first one found in a given path tree. <br />
On each consecutive call, it returns next route from starting to destination field via
input / output parameter `route_pin__io_a_F`. <br />
When it runs out of routes in a given path tree, it frees allocated route, and sets its
pointer back to `NULL`, so it's ready to start over again.

### Free parameters

Free parameters are input parameters which point to an element in a container
(e.g. linked list) that needs to be `free()`-ed, <br />
either just pointed-to element, or a larger sub-container, but not the whole
container itself. <br />
These are indicated by appending `__f` to parameter name, e.g.
`CcRoutePin * rp__f`.

Unlike corresponding ownership transfer parameter with the same `__f` indicator,
free parameter pointer is single (i.e. `CcRoutePin * rp__f` and not `CcRoutePin ** rp__f`), <br />
since container continues to live, and thus given pointer to it is not `NULL`-ed.

### Weak parameters

Weak parameters are indicated by appending `__w` to their name, e.g. `ply_start__w`. <br />
They are the same as input, read-only borrows, only they are stored in some structure,
as opposed to just being used within called function. <br />
For example, `char * ply_start__w`.

Since lifetime of a data pointed to by weak pointer depends on external owner, it's
best to be used within hierarchical structure, where weak pointers from children points
to their parents.

Summary
-------

If multiple indicators are needed, _static_ indicator (`__s`) is appended to parameter
name first, followed by direction indicator (one of `__o`, `__io`), <br />
followed by discretion indicator (one of `__d`, `__m`), finally followed by ownership
transfer indicator (one of `__w`, `__t`, `__a`, `__n`, `__f`, `__r`).

_Static_, direction and discretion indicators can be combined, e.g. `move__siod`.
Ownership transfer indicator is always kept separated, i.e. <br />
if any of direction or discretion indicators are combined with ownership transfer
indicator, they are separated by one underscore (`_`), e.g. `str__d_f`. `move__siod_r`.

### Functions

| Indicator |           `return` |                  `*return` |
| --------: | -----------------: | -------------------------: |
|           |             borrow | read (+ write, if mutable) |
|   `__new` | ownership transfer |    read + write + `free()` |

### Variables

| Indicator |     Variable |              `var` |                  `*var` |
| --------: | -----------: | -----------------: | ----------------------: |
|           |   standalone |             borrow |            read + write |
|     `__a` |   standalone |  asset (ownership) | read + write + `free()` |
|     `__t` |   standalone | ownership transfer |            read + write |
|           | in an entity |          ownership | read + write + `free()` |
|     `__w` |         both |               weak |            read + write |

### Input, output parameters

| Indicator |               `arg` |            `*arg` |
| --------: | ------------------: | ----------------: |
|           |               input |              read |
|     `__s` |       input, `NULL` |    read, _static_ |
|     `__o` |              output |             write |
|    `__io` |      input + output |      read + write |
|     `__d` | input, discretional |              read |
|     `__e` |      output, `NULL` |             write |
|     `__w` |         input, weak |              read |
|     `__f` |            `free()` |               [1] |

### Ownership transfer parameters

| Indicator |              `arg` |                                `*arg` |                                   `**arg` |
| --------: | -----------------: | ------------------------------------: | ----------------------------------------: |
|           |            `!NULL` |                                 input |                                      read |
|     `__s` |            `!NULL` |                         input, `NULL` |                            read, _static_ |
|     `__o` |            `!NULL` |                                output |                                     write |
|    `__io` |            `!NULL` |                        input + output |                              read + write |
|     `__d` |                [2] |                   input, discretional |                                      read |
|     `__m` |                [2] |                      input, mandatory |                                      read |
|     `__e` |                [2] |                        output, `NULL` |                                     write |
|     `__n` |            `!NULL` |                       `*args = NULL;` |                           ownership taken |
|     `__f` |            `!NULL` |               `free(); *args = NULL;` |                                     freed |
|     `__r` |            `!NULL` |                  `*args = realloc();` |                               reallocated |
|     `__t` |            `!NULL` |                                 input |                           ownership given |
|     `__a` |            `!NULL` |          output <br /> input + output | ownership taken <br /> ownership retained |
|     `__F` |            `!NULL` | _conditional_ `free(); *args = NULL;` |                     _conditionally_ freed |

[1] Frees one or more elements, but not the whole container.

[2] Depends on a level of indirection, i.e. to which pointer `d`, `m` indicator corresponds.

|| [^^^ top](main.md "^^^ top") || [next >>>](design.md "next >>>")
