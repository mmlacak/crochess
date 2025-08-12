.. Copyright (c) 2025 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.

.. include:: ./defines.rst

.. _lbl-howto:

How to ...
==========

.. _lbl-howto-clonerepository:

Clone repository
----------------

Open terminal in a folder of your choice, then clone repository with::

    git clone https://github.com/mmlacak/crochess.git

for details, see
`GitHub on-line documentation <https://docs.github.com/en/get-started/git-basics/about-remote-repositories>`_.

If you don't plan to contribute to the project, you can just unpack downloaded ``ZIP`` file
from `Croatian Chess repository <https://github.com/mmlacak/crochess>`_ into a folder.

.. _lbl-howto-builddocs:

Build docs
----------

To build docs, open terminal in root folder of cloned/unzipped repository, then type::

    ./build.py --html

In case of error(s) (e.g. after upgrading Sphinx version), you can force complete docs rebuild with::

    ./build.py --clean --html

If everything goes well, now there should be ``index.html`` created under ``docs/build/html/`` subfolder.

.. _lbl-howto-buildthebook:

Build the book
--------------

Open terminal in root folder of cloned/unzipped repository, and type::

    ./gfx.sh

This will generate all the images used in the book. Next, compile ``PDF`` file::

    ./pdf.sh

If everything goes well, the new book will be created under ``book`` folder.

.. _lbl-howto-contribute:

Contribute
----------

After you changed files in cloned repository, use ``push.py`` script in root folder of the project,
instead of committing your changes directly via ``git``; this is to ensure new version is set in
the book and the library, and then propagated to (console) applications, documentation and ``README.md``.

You'll need ``Python 3`` installed and access to GitHub via console ``git`` command (it should also be
in your ``PATH``); there is a `GitHub document <https://docs.github.com/en/get-started/git-basics/set-up-git>`_
which describes how to set it up.

Options to use with ``push.py``::

    ./push.py <script options> -*- <git commit options> -*- <git push options>

For ``<script options>`` use:

    * ``--book`` (or, just ``-b``) when updating anything other then comments in LaTeX, Python files for the book
    * ``--docs`` (``-d``) when updating Sphinx documentation (not comments)
    * ``--commit`` (``-c``) when updating C source code, regardless if in library or in console apps (again, not comments)
    * ``--feature`` (``-f``) when feature is finished in updated C source code
    * ``--minor`` (``-m``) when minor work is finished
    * ``--major`` (``-M``) when major work is finished
    * ``--meta`` (``-meta``) when updating only comments in C source files; this is optional, and not needed if any other
      C source code option is used

For debugging use following ``<script options>``:

    * ``--dry-run`` (``-n``) does not update versions, nor commit changes
    * ``--wet-run`` (``-w``) updates versions, but does not commit changes
    * ``--verbose`` (``-v``) prints project root folder, time-stamps found and generated
    * ``--debug-script`` (``-DS``) prints updated versions, ``git`` arguments, etc.

Options can be mixed and matched as you please, order is not important.
More significant C source code option will override lesser ones, e.g. ``-minor`` will override ``--feature`` and ``--commit``.
Do not forget to ``git --add`` if you add/delete file(s) before running the script.

After ``push.py`` script has finished updating versions, ``git`` is spawned thrice as a separate process,
first with ``add`` arguments::

    git add -- <auto-updated files>

where ``<auto-updated files>`` are all files which had its version updated, e.g. ``README.md``, ``version.c``, etc.;
obviously, this depends on ``<script options>`` used.

``git`` is then spawned with ``commit`` arguments::

    git commit <git commit options>

and finally with ``push`` arguments::

    git push <git push options>

.. _lbl-howto-dependecies:

Dependecies
-----------

Install these:

    * `Python 3 <https://www.python.org/>`_ (3.10.12) for docs, book, contributing
    * `Sphinx <https://www.sphinx-doc.org/en/master/>`_ (8.1.3) for docs
    * `Read the Docs theme <https://github.com/readthedocs/sphinx_rtd_theme>`_ (3.0.2) for docs
    * `pycairo <https://pypi.org/project/pycairo/>`_ (1.20.1) for book
    * `cairo <https://cairographics.org/>`_ (1.16.0) for book
    * `LaTeX <https://www.latex-project.org/get/>`_ \
      (3.141592653-2.6-1.40.22) for book; minimal installation should suffice, with following packages:

        - inputenc
        - charter
        - helvet
        - geometry
        - graphicx
        - wrapfig
        - hyperref
        - multirow
        - booktabs
        - alltt

Versions in brackets are those I currently use, but nothing too fancy was used,
so older versions might also work.

Subprojects which needs them are listed for each dependency; so, for instance,
if you'd like to compile the book yourself, you do need ``cairo``, but do not need
``Sphinx``.
