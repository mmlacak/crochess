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

.. c:struct:: CcParseMsg

    Parser message :c:`struct`\ure, linked list.

    .. c:member:: CcParseMsgTypeEnum type

        Type of a parser message.

    .. c:member:: char * msg

        Parser message.

    .. c:member:: struct CcParseMsg * next

        Next parser message, in a linked list.

    :c:`struct` is tagged with the same :c:struct:`CcParseMsg` name.

.. _lbl-libcc-ccparsemessage-functions:

Parse message functions
-----------------------

.. c:function:: CcParseMsg * cc_parse_msg__new( CcParseMsgTypeEnum type, char const * msg, size_t max_len__d )

    Returns a newly allocated parser message.

    :param type: Type of a parser message.
    :param msg: Parser message to copy.
    :param max_len__d: *Optional*, maximum length of :c:`msg` to copy, can be :c:macro:`CC_MAX_LEN_ZERO_TERMINATED`.
    :returns: A newly allocated parser message if successful,
              :c:`NULL` otherwise.

.. c:function:: CcParseMsg * cc_parse_msg_append( CcParseMsg ** parse_msgs__iod_a, CcParseMsgTypeEnum type, char const * msg, size_t max_len__d )

    Appends a newly allocated parser message to a given linked list.

    If linked list :c:`*parse_msgs__iod_a` is :c:data:`NULL`, it will be
    initialized with a newly allocated parser message as its only element.

    :param parse_msgs__iod_a: **Ownership**, *optional* *input/output* parameter, linked list.
    :param type: Type of a parser message.
    :param msg: Parser message to copy.
    :param max_len__d: *Optional*, maximum length of :c:`msg` to copy, can be :c:macro:`CC_MAX_LEN_ZERO_TERMINATED`.
    :returns: A newly allocated parser message if successful,
              :c:`NULL` otherwise.

.. c:function:: CcParseMsg * cc_parse_msg_append_fmt_va( CcParseMsg ** parse_msgs__iod_a, CcParseMsgTypeEnum type, size_t max_len__d, char const * fmt, va_list args )

    Appends a newly allocated, formatted parser message to a given linked list.

    If linked list :c:`*parse_msgs__iod_a` is :c:data:`NULL`, it will be
    initialized with a newly allocated parser message as its only element.

    :param parse_msgs__iod_a: **Ownership**, *optional* *input/output* parameter, linked list.
    :param type: Type of a parser message.
    :param max_len__d: *Optional*, maximum length to copy, can be :c:macro:`CC_MAX_LEN_ZERO_TERMINATED`.
    :param fmt: Formatting string, as defined for :c:func:`printf()`.
    :param args: Variadic list, input for a string format.
    :returns: Weak pointer to a newly allocated parser message if successful,
              :c:`NULL` otherwise.
    :seealso: :c:func:`cc_parse_msg_append()`

.. c:function:: CcParseMsg * cc_parse_msg_append_fmt( CcParseMsg ** parse_msgs__iod_a, CcParseMsgTypeEnum type, size_t max_len__d, char const * fmt, ... )

    Appends a newly allocated, formatted parser message to a given linked list.

    If linked list :c:`*parse_msgs__iod_a` is :c:data:`NULL`, it will be
    initialized with a newly allocated parser message as its only element.

    :param parse_msgs__iod_a: **Ownership**, *optional* *input/output* parameter, linked list.
    :param type: Type of a parser message.
    :param max_len__d: *Optional*, maximum length to copy, can be :c:macro:`CC_MAX_LEN_ZERO_TERMINATED`.
    :param fmt: Formatting string, as defined for :c:func:`printf()`.
    :param ...: Variadic format arguments, as used for :c:func:`printf()`.
    :returns: Weak pointer to a newly allocated parser message if successful,
              :c:`NULL` otherwise.
    :seealso: :c:func:`cc_parse_msg_append_fmt_va()`





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
