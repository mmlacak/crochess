.. Copyright (c) 2021, 2022, 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.
   Included source code files are licensed under GNU GPL v3+ license. See LICENSING, COPYING files in root folder of the project for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccparsemessage:

Parse message
=============

Documents ``cc_parse_msg.h`` and ``cc_parse_msg.c`` files,
which contain various parse message definitions and functions.

.. _lbl-libcc-ccparsemessage-types:

Parse message types
-------------------

.. c:enum:: CcParseMsgTypeEnum

    Parser message enumeration.

    .. c:enumerator:: CC_PMTE_Debug

    .. c:enumerator:: CC_PMTE_Info

    .. c:enumerator:: CC_PMTE_Warning

    .. c:enumerator:: CC_PMTE_Error

    .. c:enumerator:: CC_PMTE_Fatal

    :c:`enum` is tagged with the same :c:enum:`CcParseMsgTypeEnum` name.





.. _lbl-libcc-ccparsemessage-sourcecodeheader:

Parse message source code header
--------------------------------

Included source code file is ``cc_parse_msg.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_parse_msg.h
    :language: C
    :linenos:

.. _lbl-libcc-ccparsemessage-sourcecodefile:

Parse message source code file
------------------------------

Included source code file is ``cc_parse_msg.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_parse_msg.c
    :language: C
    :linenos:
