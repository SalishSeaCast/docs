.. _team-repos:

Organization of Mercurial Repositories
======================================

General
-------

* The central location for all repos will be the `SalishSea-MEOPAR team account`_ on Bitbucket

  .. _SalishSea-MEOPAR team account: https://bitbucket.org/salishsea/

* People are free to push changes to code and docs that they own,
  but they should fork repos and create pull requests to provide changes to stuff owned by others so that changes are reviewed

* All repos will have issue trackers enabled on Bitbucket

* Bitbucket wikis will be disabled on all repos to force documentation into repo docs directories

* To the extent that is practical docs use Sphinx_ and are themed to match `salishsea.eos.ubc.ca`_

  .. _Sphinx: http://sphinx-doc.org/
  .. _salishsea.eos.ubc.ca: http://salishsea.eos.ubc.ca/

* Docs from public repos will be rendered as HTML under the `salishsea.eos.ubc.ca`_ domain and on `readthedocs.org`_ as an alternative;
  readthedocs also provides PDF rendering

  .. _readthedocs.org: https://readthedocs.org/profiles/salishsea/

* Docs from private repos will be rendered as HTML in password protected areas under the `salishsea.eos.ubc.ca`_ domain


Repositories
------------

.. _docs-repo:

docs
~~~~

* public
* Creative Commons Attribution license, copyright project contributors and UBC
* project level documentation, reports, etc.
* rendered at https://salishsea-meopar-docs.readthedocs.org/
* Bitbucket: https://bitbucket.org/salishsea/docs/

private-docs
~~~~~~~~~~~~

* private to SalishSea-MEOPAR team members
* documentation from project codebase initialization tarballs received from J-P Paquin on 2013-10-02

  * contents of :file:`DOCUMENTATION` tarball and :file:`OPA_notes_2013.pdf` that Doug generated

* meeting notes, plans, work-in-progress papers, etc.
* reports that might go in docs repo but are not ready for release,
  or which cannot be released for some reason
* ideally at least some of this repo’s will eventually be moved to the public docs repo


.. _tools-repo:

tools
~~~~~
* public
* Apache v2.0 license, copyright project contributors and UBC
* scripts and docs for
  preparing, running, and post-processing NEMO runs,
  and any other support task that we write software for
* documentation is rendered at http://salishsea-meopar-tools.readthedocs.org/
* Bitbucket: https://bitbucket.org/salishsea/tools/


private-tools
~~~~~~~~~~~~~

* private to SalishSea-MEOPAR team members
* scripts from project codebase initialization tarballs received from J-P Paquin on 2013-10-02

  * contents of :file:`NEMO_PREPARATION` tarball

* scripts and docs that might go in tools repo but are not ready for release,
  or which cannot be released for some reason
* ideally this repo will eventually empty out as its contents are moved to the public tools repo
* Bitbucket: https://bitbucket.org/salishsea/private-tools/


NEMO-code
~~~~~~~~~

* private
  (because NEMO project required sign-in to access code)
* CeCILL license, copyright Centre National de la Recherche Scientifique CNRS
* NEMO code that we run

  * Initially,
    an :command:`svn` checkout of the :kbd:`nemo_v3_1` tag from https://forge.ipsl.jussieu.fr/nemo/tags/nemo_v3_1/

  * Merge the CONCEPTS model changes from the :file:`CODE` tarball received received from J-P Paquin on 2013-10-02

  * Port the CONCEPTS changes to NEMO 3.4 or trunk to get the coastal shelf features in 3.4 that we need to properly model the Salish Sea



salishsea-site
~~~~~~~~~~~~~~

* public
* Apache v2.0 license, copyright project contributors and UBC
* content and tool chain for the `salishsea.eos.ubc.ca`_ domain site static content
* Bitbucket: https://bitbucket.org/salishsea/nemo-code/


NEMO-3.1
~~~~~~~~

* private
  (because NEMO project required sign-in to access code)
* CeCILL license, copyright Centre National de la Recherche Scientifique CNRS
* NEMO-3.1 reference repo
* a Mercurial repo of SVN checkouts of modipsl trunk, the NEMO-3.1 tag, and supporting repos that are believed to be the basis on which the 2-Oct-2013 CONCEPTS-110 CODE tarball was built
* documentation rendered at http://salishsea-meopar-docs.readthedocs.org/en/latest/code-notes/nemo31-concepts110.html
* Bitbucket: https://bitbucket.org/salishsea/nemo-3.1/


CONCEPTS-110
~~~~~~~~~~~~

* private
  (because NEMO project required sign-in to access code)
* CeCILL license, copyright Centre National de la Recherche Scientifique CNRS
* CONCEPTS-110 reference repo
* a Mercurial repo of the CODE.tar tarball received from J-P Paquin on 2-Oct-2013
* documentation rendered at http://salishsea-meopar-docs.readthedocs.org/en/latest/code-notes/nemo31-concepts110.html
* Bitbucket: https://bitbucket.org/salishsea/concepts-110/
