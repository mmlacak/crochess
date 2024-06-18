.. Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
   Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See LICENSING, COPYING files for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccdefines:

Defines
=======

Documents ``cc_defines.h`` file, which contains constants and macros used throughout project.

.. _lbl-libcc-ccdefines-shorthandtypes:

Shorthand types
---------------

Types for the convenience of typing less.

.. code-block:: C
    :force:

    typedef unsigned char uchar;
    typedef unsigned short ushort;
    typedef unsigned int uint;

Constant representing minimum value for all :c:`unsigned` types.

.. code-block:: C
    :force:

    #define CC_UNSIGNED_MIN (0)

.. _lbl-libcc-ccdefines-maybebool:

Maybe bool
----------

Maybe bool represents :c:`bool` values, which may undefined or uninitialized, or
has to differentiate error state from valid :c:`true` and :c:`false` response.

It is defined as :c:`enum CcMaybeBoolEnum;`, with values:

.. code-block:: C
    :force:

    typedef enum CcMaybeBoolEnum {
        CC_MBE_Void = -1, /* Void (undefined, uninitialized, or error) value. */
        CC_MBE_False = 0, /* Boolean false value. */
        CC_MBE_True = 1, /* Boolean true value. */
    } CcMaybeBoolEnum;

Macros to convert from and into :c:`bool` value.

.. code-block:: C
    :force:

    #define CC_BOOL_TO_MAYBE(bool_val) /* Converts bool value into CcMaybeBoolEnum. */
    #define CC_MAYBE_IS_TRUE(maybe_bool) /* Checks if CcMaybeBoolEnum value is CC_MBE_True. */
    #define CC_MAYBE_IS_FALSE(maybe_bool) /* Checks if CcMaybeBoolEnum value is CC_MBE_False. */
    #define CC_MAYBE_IS_VOID(maybe_bool) /* Checks if CcMaybeBoolEnum value is CC_MBE_Void. */

.. _lbl-libcc-ccdefines-xor:

XOR
---

Macro to evaluate logical XOR, i.e. to check if one or the other :c:`bool` value
is :c:`true`, but not both.

.. code-block:: C
    :force:

    #define CC_XOR(bool_1,bool_2) /* Both bool_1, bool_2 are converted to Boolean values. Returns either 1 (true) or 0 (false). */

.. note::

    In case of integer(s), one has to be non-zero, while the other has to be zero,
    for :c:`XOR` to return :c:`true`. Arguments are converted to :c:`bool`\s (so,
    non-zero integer is :c:`1`, otherwise it's :c:`0`), then they are compared.

.. seealso::

    `<https://en.wikipedia.org/wiki/Bitwise_operations_in_C#Logical_equivalents>`_,
    `<https://www.reddit.com/r/C_Programming/comments/2cruz3/comment/cjih6wt/>`_
