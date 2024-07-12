.. Copyright (c) 2021, 2022, 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.
   Included source code files are licensed under GNU GPL v3+ license. See LICENSING, COPYING files in root folder of the project for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccparsedsideeffect:

Parsed side-effect
==================

Documents ``cc_parsed_side_effect.h`` and ``cc_parsed_side_effect.c`` files,
which contain various parsed side-effect definitions and functions.

.. _lbl-libcc-ccparsedsideeffect-data:

Parsed side-effect data
-----------------------

.. c:enum:: CcParsedSideEffectEnum

    Step side-effect enumeration.

    .. c:enumerator:: CC_PSLE_None

        No side effects.

    .. c:enumerator:: CC_PSEE_Capture

        Corresponds to ``*``.

    .. c:enumerator:: CC_PSEE_Displacement

        Corresponds to ``<``.

    .. c:enumerator:: CC_PSEE_EnPassant

        Corresponds to ``:``.

    .. c:enumerator:: CC_PSEE_Castle

        Corresponds to ``&``.

    .. c:enumerator:: CC_PSEE_Promotion

        Corresponds to ``=``, can be omitted.

    .. c:enumerator:: CC_PSEE_TagForPromotion

        Corresponds to ``=``, it's mandatory.

    .. c:enumerator:: CC_PSEE_Conversion

        Corresponds to ``%``.

    .. c:enumerator:: CC_PSEE_FailedConversion

        Corresponds to ``%%``.

    .. c:enumerator:: CC_PSEE_Transparency

        Corresponds to ``^``.

    .. c:enumerator:: CC_PSEE_Divergence

        Corresponds to ``/``.

    .. c:enumerator:: CC_PSEE_DemoteToPawn

        Corresponds to ``>``.

    .. c:enumerator:: CC_PSEE_Resurrection

        Corresponds to ``$``.

    .. c:enumerator:: CC_PSEE_ResurrectingOpponent

        Corresponds to ``$$``.

    .. c:enumerator:: CC_PSEE_FailedResurrection

        Corresponds to ``$$$``.

    :c:`enum` is tagged with the same :c:expr:`CcParsedSideEffectEnum` name.

.. c:macro:: CC_PARSED_SIDE_EFFECT_ENUM_IS_CASTLING(see)

    Macro to check if given side-effect enum is castling.

    :param see: A side-effect enumeration, i.e. one of :c:expr:`CcParsedSideEffectEnum` values.
    :returns: :c:`true` if castling, :c:`false` otherwise.

.. c:macro:: CC_MAX_LEN_PARSED_SIDE_EFFECT_SYMBOL

    Maximum length of a side-effect symbol; equals to :c:`3`.

.. _lbl-libcc-ccparsedsideeffect-functions:

Parsed side-effect functions
----------------------------

.. c:function:: char const * cc_parsed_side_effect_symbol( CcParsedSideEffectEnum see )

    Function returns string symbol, as used in algebraic notation, for a given side-effect.

    Returned string is not allocated, so do not :c:`free()` it.

    :param see: A side-effect enum.
    :returns: String symbol if side-effect enum is valid, :c:`"?"` otherwise.






.. _lbl-libcc-ccparsedsideeffect-sourcecodeheader:

Parsed side-effect source code header
-------------------------------------

Included source code file is ``cc_parsed_side_effect.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_parsed_side_effect.h
    :language: C
    :linenos:

.. _lbl-libcc-ccparsedsideeffect-sourcecodefile:

Parsed side-effect source code file
-----------------------------------

Included source code file is ``cc_parsed_side_effect.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_parsed_side_effect.c
    :language: C
    :linenos:
