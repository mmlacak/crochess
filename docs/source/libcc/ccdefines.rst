.. Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
   Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See LICENSING, COPYING files for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccdefines:

Defines
=======

Constants and macros used throughout project.

.. _lbl-libcc-ccdefines-shorthandtypes:

Shorthand types
---------------

Types for the convenience of typing less.

.. list-table:: Shorthand types table
   :header-rows: 1
   :align: left
   :widths: 25 75

   * - Type
     - Shorthand for
   * - :c:`uchar`
     - :c:`unsigned char`
   * - :c:`ushort`
     - :c:`unsigned short`
   * - :c:`uint`
     - :c:`unsigned int`

Constant :c:`CC_UNSIGNED_MIN` representing minimum :c:`unsigned` value, i.e. :c:`0`.

.. _lbl-libcc-ccdefines-maybebool:

Maybe bool
----------

Maybe bool represents :c:`bool` value, which may undefined or uninitialized, or
has to differentiate error state from valid :c:`bool` value.

It is defined as :c:`enum CcMaybeBoolEnum;`, with values:

.. list-table:: Maybe bool values table
   :header-rows: 1
   :align: left
   :widths: 75 25

   * - Enum
     - Value
   * - :c:`CC_MBE_Void`
     - :c:`-1`
   * - :c:`CC_MBE_False`
     - :c:`0`
   * - :c:`CC_MBE_True`
     - :c:`1`

Macros to convert from and into :c:`bool` value.

.. list-table:: Maybe bool macros table
   :header-rows: 1
   :align: left
   :widths: 35 65

   * - Macro
     - Description
   * - :c:`CC_BOOL_TO_MAYBE(bool_val)`
     - converts :c:`bool` into :c:`CcMaybeBoolEnum` value
   * - :c:`CC_MAYBE_IS_TRUE(maybe_bool)`
     - checks if :c:`CcMaybeBoolEnum` value is :c:`CC_MBE_True`
   * - :c:`CC_MAYBE_IS_FALSE(maybe_bool)`
     - checks if :c:`CcMaybeBoolEnum` value is :c:`CC_MBE_False`
   * - :c:`CC_MAYBE_IS_VOID(maybe_bool)`
     - checks if :c:`CcMaybeBoolEnum` value is :c:`CC_MBE_Void`

.. _lbl-libcc-ccdefines-xor:

XOR
---

Macro :c:`CC_XOR(bool_1,bool_2)` to check if one or the other :c:`bool` value
is :c:`true`, but not both.

.. note::

    In case of integer(s), one has to be non-zero, while the other has to be zero,
    for :c:`XOR` to return :c:`true`. Arguments are converted to :c:`bool`\s (so,
    non-zero integer is :c:`1`, otherwise it's :c:`0`), then they are compared.

.. seealso::

    `<https://en.wikipedia.org/wiki/Bitwise_operations_in_C#Logical_equivalents>`_,
    `<https://www.reddit.com/r/C_Programming/comments/2cruz3/comment/cjih6wt/>`_
