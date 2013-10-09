Documentation with Sphinx
=========================

We use Sphinx_ for most documentation in the Salish Sea MEOPAR project.
Sphinx_ provides:

* direct rendering to HTML for online publication
* easy inclusion of LaTeX_ math syntax with in-browser rendering via MathJax_
* easy inclusion of figures, graphs, and images
* deep linkability
* optional PDF rendering

.. _Sphinx: http://sphinx-doc.org/
.. _LaTeX: http://www.latex-project.org/
.. _MathJax: http://www.mathjax.org/

LaTeX_ should be used for manuscripts of publications,
for which PDFs must be rendered,
uploaded,
and linked into other documentation to make them available online.

All documentation is under :ref:`vc-with-hg` and stored in either the docs_ or `private-docs`_ project documentation repos,
or in the docs directory of another appropriate project repo
(see :ref:`team-repos`).

.. _docs: https://bitbucket.org/salishsea/docs/
.. _private-docs: https://bitbucket.org/salishsea/private-docs/

Sphinx_ uses reStructuredText
(reST),
a simple,
unobtrusive markup language.
The `Sphinx documentation`_ provides a brief `introduction to reST concepts and syntax`_.
Sphinx extends reST with a `collection of directives and interpreted text roles`_ for
cross-referencing,
tables of contents,
code examples,
and specially formatted paragraphs like
notes,
alerts,
warnings,
etc.

.. _Sphinx documentation: http://sphinx-doc.org/contents.html
.. _introduction to reST concepts and syntax: http://sphinx-doc.org/rest.html
.. _collection of directives and interpreted text roles: http://sphinx-doc.org/markup/index.html


Installing Sphinx
-----------------

The easiest way to install Sphinx and the packages that it depends on is to install the Anaconda_ Python distribution.
Follow the `installation instructions`_ for your operating system and accept the option at the end of the installation to make Anaconda your default Python.

.. _Anaconda: https://store.continuum.io/cshop/anaconda/
.. _installation instructions: http://www.continuum.io/downloads

Experienced Python developers may wish to install Sphinx in other ways:

* In a Python virtual environment via :command:`virtualenv` and :command:`pip`
* In :file:`$HOME/.local/` via :command:`pip install --user sphinx`
