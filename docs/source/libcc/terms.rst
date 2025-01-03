.. Copyright (c) 2021, 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.

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

    cycle
        Light player's move followed by dark player's move.

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

    cascade
        Cascade is a move in which two or more pieces has been moved.

    activator
        Activator is any non-Wave piece in a cascade. Usually, it refers
        to last such a piece preceding a Wave, from which that Wave
        inherits movement rules.

    private
        Private is Pawn, Scout, or Grenadier.

    weightless piece
        Weightless piece does not spend momentum for its movement.
        These are Wave and Starchild.

    figure
        Figure is any piece, except Pawn.

    rush
        Initial movement of a private, for two or more fields forward.

    link
        Item in a linked list, or a queue.

    path
        Path is a complete list of all fields traversed by a single piece,
        in a single ply, in order in which they were visited.

    segment
        Segment is a straight part of a path, before or after any path alteration;
        e.g. before teleportation, or after divergence.
