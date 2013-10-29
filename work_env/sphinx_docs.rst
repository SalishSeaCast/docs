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

Sphinx and the packages that it depends on are included in the :ref:`AnacondaPythonDistro` that you should already have installed.

Experienced Python developers may wish to install Sphinx in other ways:

* In a Python virtual environment via :command:`virtualenv` and :command:`pip`
* In :file:`$HOME/.local/` via :command:`pip install --user sphinx`


Building and Previewing Documentation
-------------------------------------

As you are writing and editing Sphinx documentation you can build the HTML rendered docs locally and preview them in your browser to ensure that there are no reST syntax errors and that the docs look the way you want them to.

In the top level :file:`docs/` directory
(e.g. :file:`docs/` in the :ref:`docs-repo` repo,
or :file:`tools/docs/` in the :ref:`tools-repo` repo)
use the command:

.. code-block:: bash

    make html

to build the docs.
You will be notified of any syntax or consistency errors.
The HTML pages produced byt the :command:`make html` command are stored in the :file:`_build/html/` subdirectory and you can use your browser to open the :file:`index.html` file in that directory to preview them.
You can keep a browser tab open to the rendered docs and refresh after each build to see updates.

.. note::

    The top level :file:`docs/` directory contains
    (at minimum)
    the files
    :file:`conf.py`,
    :file:`Makefile`,
    and :file:`index.rst`,
    and the directory :file:`_static/`.
    After the docs have been built it will also contain the :file:`_build/` directory.

The result of running :command:`make html` should look something link::

  sphinx-build -b html -d _build/doctrees   . _build/html
  Running Sphinx v1.1.3
  loading pickled environment... done
  building [html]: targets for 9 source files that are out of date
  updating environment: 0 added, 0 changed, 0 removed
  looking for now-outdated files... none found
  preparing documents... done
  writing output... [100%] sphinx_docs
  writing additional files... search
  copying static files... done
  dumping search index... done
  dumping object inventory... done
  build succeeded.

  Build finished. The HTML pages are in _build/html.

.. note::

    Building the :ref:`docs-repo` repo results in 1 consistency warning::

      docs/README.rst:: WARNING: document isn't included in any toctree

    that can be ignored.


Links and Cross-references
--------------------------

External Links
~~~~~~~~~~~~~~

The preferred way to including external links is via markup like::

  This is a paragraph that contains `a link`_.

  .. _a link: http://example.com/

If the link text should be the web address,
you don't need special markup at all,
the parser finds links and mail addresses in ordinary text.


Internal Links
~~~~~~~~~~~~~~

To support cross-referencing to arbitrary locations in any document,
the standard reST labels are used.
For this to work label names must be unique throughout the entire documentation.  There are two ways in which you can refer to labels:

* If you place a label directly before a section title,
  you can reference to it with ``:ref:`label-name```.
  Example::

    .. _my-reference-label:

    Section to cross-reference
    --------------------------

    This is the text of the section.

    It refers to the section itself, see :ref:`my-reference-label`.

  The ``:ref:`` role would then generate a link to the section,
  with the link title being "Section to cross-reference".
  This works just as well when section and reference are in different source files.

  Automatic labels also work with figures: given::

    .. _my-figure:

    .. figure:: whatever

       Figure caption

  a reference ``:ref:`my-figure``` would insert a reference to the figure
  with link text "Figure caption".

  The same works for tables that are given an explicit caption using the
  :kbd:`table` directive.

* Labels that aren't placed before a section title can still be referenced to,
  but you must give the link an explicit title,
  using this syntax: ``:ref:`Link title <label-name>```.

* Inter-Sphinx links are enabled between the :ref:`tools-repo` docs and the :ref:`docs-repo` repo;
  i.e.
  whenever Sphinx encounters a cross-reference that has no matching target in the :ref:`tools-repo` docs,
  it looks for targets in the :ref:`docs-repo`.

Using :rst:role:`ref` is advised over standard reStructuredText links to sections
(like ```Section title`_``) because it works across files,
when section headings are changed,
and for all builders that support cross-references.


Links to Rendered IPython Notebooks
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To link to a rendered representation of an IPython Notebook that has been pushed to a Bitbucket repo use markup like::

  * `SalishSeaBathy.ipynb`_: Documents the full domain bathymetry used for the Salish Sea NEMO runs.

  .. _SalishSeaBathy.ipynb: http://nbviewer.ipython.org/urls/bitbucket.org/salishsea/tools/raw/tip/bathymetry/SalishSeaBathy.ipynb
