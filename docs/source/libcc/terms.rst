.. Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
   Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See LICENSING, COPYING files for details.

.. include:: ../defines.rst

.. _lbl-libcc-terms-abbreviations:

Terms, abbreviations
====================

Terms, abbreviations used throughout documentation.

.. _lbl-libcc-terms-abbreviations-glossary:

Glossary
--------

.. glossary::

    AN
        Algebraic notation in general, usually refers to new notation.

    CAN
        Compatibility algebraic notation, as used in Classical Chess.

    XAN
        The long variation of new, extended notation.

    field
        Field is any square on a chessboard.

    position
        Position is pair of coordinates, used to address individual
        fields from chessboard origin.

    step
        Step is relative position, i.e. coordinate difference between
        two fields.

    module
        Collective name for a source file and its header file.

    entity
        Plain-old-data, either data type provided by C, or some user-defined
        :c:`struct`, :c:`union`, :c:`enum`, etc.

    method
        Function which operates primarily on one entity, defined in the same
        module as said entity.

    tag
        Tag is a (usually, delayed) opportunity link between a piece and a
        field at which it stands.
