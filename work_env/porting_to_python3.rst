.. _PortingToPython3:

************************
Porting Code to Python 3
************************

While there are many differences between Python 2 and Python 3 few of them impact most of the code that we write in the Salish Sea project.
This section describes the types of changes that had to be made in order to convert (also known as "port") the :ref:`SalishSeaToolsPackage` to Python 3.

If you encounter other changes that you need to make to port our code to Python 3,
please feel free to add them below.

If you are interested in the details of the differences between Python 2 and Python 3 they can be found in the `What's New in Python`_ documentation.

.. _What's New in Python: https://docs.python.org/3/whatsnew/

Part of the move to Python 3 was a reorganization of the standard library.
That means that some :kbd:`import` need to be changed when code is ported from Python 2 to Python 3.
Specific instances of that
(like the :py:mod:`StringIO` module)
are described below.
The description of all of the standard library changes is contained in `PEP 3108`_.

.. _PEP 3108: https://www.python.org/dev/peps/pep-3108/


.. _PortingTheSalishSeaToolsPackage:

Porting the :py:obj:`SalishSeaTools` Package
============================================

This section describes the types of changes that had to be made to port the :ref:`SalishSeaToolsPackage`
(including the :py:obj:`nowcast` codebase)
from Python 2.7 to Python 3.5 in October 2015.


Mixed TABs and Spaces for Indentation
-------------------------------------

While mixing TABs and spaces for indentation in a Python module was never a good idea,
it causes a :py:exc:`TabError` exception to be raised when such a module is imported in Python 3.

All Python code should use spaces for indentation and the indentation levels should be 4 spaces.


Change :py:obj:`print` Statements to :py:func:`print` Functions
---------------------------------------------------------------

:py:obj:`print` was a statement in Python 2.
It is a function in Python 3.
So,
code like:

.. code-block:: python

    print 'red is clockwise'

has to be changed to:

.. code-block:: python

    print('red is clockwise')


Change :py:mod:`cStringIO.StringIO` Imports to :py:mod:`io.StringIO`
--------------------------------------------------------------------

In Python 2 :py:class:`StringIO` class in the standard library has two implementations,
one in Python,
and a faster one in C.

The former was imported like:

.. code-block:: python

    from StringIO import StringIO

and the latter like:

.. code-block:: python

    from cStringIO import StringIO

In Python 3 the :py:class:`StringIO` class has been moved to the :py:mod:`io` module and the interpreter takes care of first trying to import the faster C version or falling back to the Python version if necessary.
So,
those imports need to be changes to:

.. code-block:: python

    from io import StringIO


:py:mod:`mock` Library is in the Standard Library
-------------------------------------------------

.. note:: This is only applicable to test suite code.

The :py:mod:`mock` library that was developed as a separate,
stand-alone library for Python 2 is included in the standard library in Python 3.
So,
instead from it like:

.. code-block:: python

    from mock import (
         Mock,
         patch,
     )

the Python 3 import looks like:

.. code-block:: python

    from unittest.mock import (
         Mock,
         patch,
     )

Also,
because :py:mod:`mock` is now part of the standard library,
it no longer needs to be installed separately or included in :file:`setup.py` or environment descriptions files
(:file:`requirements.txt`,
:file:`requirements.pip`,
:file:`environment.yaml`,
etc.).
