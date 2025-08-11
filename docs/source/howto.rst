.. Copyright (c) 2025 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.

.. include:: ./defines.rst

.. _lbl-howto:

How to
======

.. _lbl-howto-clonerepository:

Clone repository
----------------

Go to `Croatian Chess repository <https://github.com/mmlacak/crochess>`_, then clone it with::

    git clone https://github.com/mmlacak/crochess.git

Or, you could just unpack downloaded zip file into a folder.

.. _lbl-howto-builddocs:

Build docs
----------

.. _lbl-howto-builddocs-prerequsites:

To build docs, open terminal in folder where you've cloned/unzipped repository, then type::

    ./build.py --html

In case of error(s) (e.g. after upgrading Sphinx version), you can force complete docs rebuild with::

    ./build.py --clean --html

If everything goes well, now there should be `index.html` created under `docs/build/html/` subfolder.

Dependecies
-----------

Install these:

    * `Python 3 <https://www.python.org/>`_ (3.10.12)
    * `Sphinx <https://www.sphinx-doc.org/en/master/>`_ (8.1.3)
    * `Read the Docs theme <https://github.com/readthedocs/sphinx_rtd_theme>`_ (3.0.2)

Versions in brackets are those I currently use, but nothing too fancy was used, so older versions should also work.
