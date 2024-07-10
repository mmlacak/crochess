.. Copyright (c) 2021, 2022, 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.
   Included source code files are licensed under GNU GPL v3+ license. See LICENSING, COPYING files in root folder of the project for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccparsedstep:

Parsed step
===========

Documents ``cc_parsed_step.h`` and ``cc_parsed_step.c`` files, which contain various
parsed step definitions and functions.

.. _lbl-libcc-ccparsedstep-data:

Parsed step data
----------------

.. c:macro:: CC_IS_STEP_LINK_VALID(sle)

    Macro to check if a given step link is valid.

    :param sle: Step link enumeration, :c:expr:`CcParsedStepLinkEnum` value.
    :returns: :c:`true` if valid, :c:`false` otherwise.

.. c:macro:: CC_IS_STEP_LINK_DESTINATION(sle)

    Macro to check if a given step link is destination.

    :param sle: Step link enumeration, :c:expr:`CcParsedStepLinkEnum` value.
    :returns: :c:`true` if destination, :c:`false` otherwise.

.. c:enum:: CcParsedStepLinkEnum

    Step link enumeration.

    .. c:enumerator:: CC_PSLE_None

        Step link not found, uninitialized, not parsed yet, or error happened.

    .. c:enumerator:: CC_PSLE_Start

        Position from which a piece started moving.

    .. c:enumerator:: CC_PSLE_Reposition

        In trance-journey, dark Shaman's distant starting field; separated by ``,`` (comma).

    .. c:enumerator:: CC_PSLE_Next

        Step immediately following previous, separated by ``.`` (dot).

    .. c:enumerator:: CC_PSLE_Distant

        Step not immediately following previous, separated by ``..`` (double-dot).

    .. c:enumerator:: CC_PSLE_Destination

        Step to destination field, separated by ``-`` (hyphen).

    .. c:enumerator:: CC_PSLE_JustDestination

        Just destination field, no separators, no other steps.

    :c:`enum` is tagged with the same :c:expr:`CcParsedStepLinkEnum` name.






.. _lbl-libcc-ccparsedstep-sourcecodeheader:

Parsed step source code header
------------------------------

Included source code file is ``cc_parsed_step.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_parsed_step.h
    :language: C
    :linenos:

.. _lbl-libcc-ccparsedstep-sourcecodefile:

Parsed step source code file
----------------------------

Included source code file is ``cc_parsed_step.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_parsed_step.c
    :language: C
    :linenos:
