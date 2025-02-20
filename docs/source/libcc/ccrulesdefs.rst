.. Copyright (c) 2021, 2022, 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.
   Included source code files are licensed under GNU GPL v3+ license. See LICENSING, COPYING files in root folder of the project for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccrulesdefs:

Definitions of rules
====================

Documents ``cc_rules_defs.h`` and ``cc_rules_defs.c`` files,
which contain various definitions of rules.

.. _lbl-libcc-ccrulesdefs-macros:

Macros
------

.. c:macro:: CC_TRANCE_JOURNEY_TYPE_IS_ENUMERATOR(tjte)

    Macro to check if given trance-journey type is enumeration in :c:enum:`CcTranceJourneyTypeEnum`,
    i.e. between :c:enumerator:`CC_TJTE_None` and :c:enumerator:`CC_TJTE_DoubleCapture`
    values.

    :param tjte: A trance-journey type, integer value.
    :returns: :c:data:`true` if :c:type:`CcTranceJourneyTypeEnum` enumerator,
              :c:data:`false` otherwise.

.. c:macro:: CC_TRANCE_JOURNEY_TYPE_IS_VALID(tjte)

    Macro to check if given trance-journey type is valid :c:enum:`CcTranceJourneyTypeEnum`
    enumerator, and not :c:enumerator:`CC_TJTE_None`.

    :param tjte: A trance-journey type, integer value.
    :returns: :c:data:`true` if valid :c:type:`CcTranceJourneyTypeEnum` enumerator,
              :c:data:`false` otherwise.

.. c:macro:: CC_TRANCE_JOURNEY_TYPE_IS_ANY_CAPTURE(tjte)

    Macro to check if given trance-journey is a capturing one, or a double trance-journey.

    :param tjte: A trance-journey type, integer value.
    :returns: :c:data:`true` if trance-journey is capturing,
              :c:data:`false` otherwise.

.. _lbl-libcc-ccrulesdefs-data:

Trance-journey type
-------------------

.. c:enum:: CcTranceJourneyTypeEnum

    Trance-journey type enumeration.

    .. c:enumerator:: CC_TJTE_None

        No trance-journey.

    .. c:enumerator:: CC_TJTE_Displacement

        Displacement trance-journey.

    .. c:enumerator:: CC_TJTE_Capture

        Capturing trance-journey.

    .. c:enumerator:: CC_TJTE_DoubleCapture

        Double capturing trance-journey.

    :c:`enum` is tagged with the same :c:enum:`CcTranceJourneyTypeEnum` name.


.. _lbl-libcc-ccrulesdefs-sourcecodeheader:

Header file
-----------

Included source header file is ``cc_rules_defs.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_rules_defs.h
    :language: C
    :linenos:

.. .. _lbl-libcc-ccrulesdefs-sourcecodefile:
..
.. Source code file
.. ----------------
..
.. Included source code file is ``cc_rules_defs.c``.
..
.. .. literalinclude:: ../../../ws/libcrochess/src/cc_rules_defs.c
..     :language: C
..     :linenos:
