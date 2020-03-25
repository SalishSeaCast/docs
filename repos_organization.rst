.. _team-repos:

Organization of Mercurial Repositories
======================================

General
-------

* The central location for all repos is the `SalishSea-MEOPAR team account`_ on Bitbucket

  .. _SalishSea-MEOPAR team account: https://bitbucket.org/salishsea/

* People are free to push changes to code and docs that they own,
  but they should fork repos and create pull requests to provide changes to stuff owned by others so that changes are reviewed

* All repos have issue trackers enabled on Bitbucket

* Bitbucket wikis are disabled on all repos to force documentation into repo docs directories

* Docs use Sphinx_

  .. _Sphinx: http://sphinx-doc.org/

* Docs from public repos will be rendered as HTML on `readthedocs.org`_.
  readthedocs also provides PDF rendering.

  .. _readthedocs.org: https://readthedocs.org/profiles/salishsea/

* There is no provision for rendering docs from private repos.


Repositories
------------

.. _docs-repo:

docs
~~~~

* public
* Creative Commons Attribution license, copyright project contributors and UBC
* project level documentation, reports, etc.
* rendered at https://salishsea-meopar-docs.readthedocs.io/en/latest/
* Bitbucket: https://bitbucket.org/salishsea/docs/


.. _private-docs-repo:

private-docs
~~~~~~~~~~~~

* private to SalishSea-MEOPAR team members
* meeting notes, plans, work-in-progress reports and papers, presentations, etc.
* anything that might go in docs repo but are not ready for release,
  or which cannot be released for some reason
* ideally at least some of this repo will eventually be moved to the public docs repo


.. _tools-repo:

tools
~~~~~

* public
* Apache v2.0 license, copyright project contributors and UBC
* scripts and docs for
  preparing, running, and post-processing NEMO runs,
  and any other support task that we write software for
* documentation is rendered at https://salishsea-meopar-tools.readthedocs.io/en/latest/
* Bitbucket: https://bitbucket.org/salishsea/tools/


.. _private-tools-repo:

private-tools
~~~~~~~~~~~~~

* private to SalishSea-MEOPAR team members
* scripts and docs that might go in tools repo but are not ready for release,
  or which cannot be released for some reason
* ideally this repo will eventually empty out as its contents are moved to the public tools repo
* Bitbucket: https://bitbucket.org/salishsea/private-tools/


.. _SalishSeaCmd-repo:

SalishSeaCmd
~~~~~~~~~~~~

* public
* Apache v2.0 license, copyright project contributors and UBC
* The Salish Sea NEMO command processor,
  :command:`salishsea`,
  a command line tool for doing various operations associated with the Salish Sea NEMO model.
  It is a domain-specific command processor tool for the Salish Sea model that uses plug-ins from the :ref:`NEMO-Cmd-repo` package.
* documentation is rendered at https://salishseacmd.readthedocs.io/en/latest/
* Bitbucket: https://bitbucket.org/salishsea/salishseacmd/


.. _NEMO-Cmd-repo:

NEMO-Cmd
~~~~~~~~

* public
* Apache v2.0 license, copyright project contributors and UBC
* The NEMO command processor, :command:`nemo`,
  a command line tool for doing various operations associated with running the NEMO ocean model.
* documentation is rendered at https://nemo-cmd.readthedocs.io/en/latest/
* GitHub: https://github.com/SalishSeaCast/NEMO-Cmd


.. _SalishSeaNowcast-repo:

SalishSeaNowcast
~~~~~~~~~~~~~~~~

* public
* Apache v2.0 license, copyright project contributors and UBC
* The :kbd:`SalishSeaNowcast` package is a collection of Python modules associated with running the Salish Sea NEMO model in a daily nowcast/forecast mode.
  The runs use as-recent-as-available
  (typically previous day)
  forcing data for the western boundary sea surface height and the Fraser River flow,
  and atmospheric forcing from the four-times-daily produced forecast results from the Environment Canada High Resolution Deterministic Prediction System (HRDPS) operational GEM 2.5km resolution model.
* documentation is rendered at https://salishsea-nowcast.readthedocs.io/en/latest/
* Bitbucket: https://bitbucket.org/salishsea/salishseanowcast/


.. _analysis-repo:

analysis
~~~~~~~~

* **Read-only** legacy repo
* public
* Apache v2.0 license, copyright project contributors and UBC
* analyses of the results of the Salish Sea MEOPAR NEMO model;
  most of the files are Jupyter Notebooks
* Bitbucket: https://bitbucket.org/salishsea/analysis/

.. note::
    This was the project's original analysis repo.
    It got so big that it was in danger of hitting the Bitbucket 2Gb limit at which changesets would no longer be accepted.
    In April 2016 it was split into a collection of personal analysis repos for each team member,
    as well as some other application-specific analysis repos.
    They are all public with the same license and copyright a the original analysis repo.
    They can be found in the `SalishSea-MEOPAR Analysis project`_ collection of repos on Bitbucket.

    .. _SalishSea-MEOPAR Analysis project: https://bitbucket.org/account/user/salishsea/projects/SSM_ANALYSIS


.. _NEMO-3.6-code-repo:

NEMO-3.6-code
~~~~~~~~~~~~~

* private
  (because NEMO project requires sign-in to access code)
* CeCILL license, copyright Centre National de la Recherche Scientifique CNRS
* NEMO-3.6 code that we run
* a merge of the :kbd:`http://forge.ipsl.jussieu.fr/nemo/svn/branches/2015/nemo_v3_6_STABLE` and our local code
* maintenance of the repo to handle the merges is decribed in :ref:`NEMO-3.6CodeRepoMaintenance`
* Bitbucket: https://bitbucket.org/salishsea/nemo-3.6-code/
* documentation: https://salishsea-meopar-docs.readthedocs.io/en/latest/code-notes/salishsea-nemo/repos-maint/nemo-3.6.html


.. _XIOS-2-repo:

XIOS-2 (XML I/O Server)
~~~~~~~~~~~~~~~~~~~~~~~

* private
  (because the canonical source for this code is :kbd:`http://forge.ipsl.jussieu.fr/ioserver/wiki`)
* CeCILL_V2 license, copyright Centre National de la Recherche Scientifique CNRS
* XIOS-2.0 code that we run with NEMO-3.6
* a checkout of :kbd:`http://forge.ipsl.jussieu.fr/ioserver/svn/XIOS/trunk`
* maintenance of the repo to handle the merging in changes from upstream is decribed in :ref:`XIOS-2CodeRepoMaintenance`
* Bitbucket: https://bitbucket.org/salishsea/xios-2/
* documentation: https://salishsea-meopar-docs.readthedocs.io/en/latest/code-notes/salishsea-nemo/repos-maint/xios-2.html


.. _XIOS-ARCH-repo:

XIOS-ARCH
~~~~~~~~~

* public
* Apache v2.0 license, copyright project contributors and UBC
* XIOS build configuration arch files for use with NEMO-3.6
* GitHub: https://github.com/SalishSeaCast/XIOS-ARCH
* documentation: :ref:`moaddocs:XIOS-2-docs`


.. _grid-repo:

grid
~~~~

* public
* Apache v2.0 license, copyright project contributors and UBC
* Coordinates,
  bathymetry,
  mesh mask,
  land processor elimination,
  and atmospheric forcing weights files for the Salish Sea NEMO model.
  Also included is the file that imposed the "no snow" atmospheric forcing condition.
  Files for AGRIF sub-grids are in appropriately names sub-directories of the :file:`subgrids/` directory.

  Many of these files originated in the (private) :ref:`NEMO-forcing-repo` repository.
  The commit message history of files from that repository is included in their initial commit messages.
* Bitbucket: https://bitbucket.org/salishsea/grid


.. _rivers-repo:

rivers-climatology
~~~~~~~~~~~~~~~~~~

* public
* Apache v2.0 license, copyright project contributors and UBC
* Rivers runoff climatology files for the Salish Sea NEMO model.
  Included are discharge, temperature, nutrients & biological tracers, and depths over which river quantities are introduced into model grid.
  Files for AGRIF sub-grids are in appropriately names sub-directories of the :file:`subgrids/` directory.

  Many of these files originated in the (private) :ref:`NEMO-forcing-repo` repository.
  The commit message history of files from that repository is included in their initial commit messages.
* Bitbucket: https://bitbucket.org/salishsea/rivers-climatology


.. _tides-repo:

tides
~~~~~

* public
* Apache v2.0 license, copyright project contributors and UBC
* Tide boundary condition files for the Salish Sea NEMO model.

  Many of these files originated in the (private) :ref:`NEMO-forcing-repo` repository.
  The commit message history of files from that repository is included in their initial commit messages.
* GitHub: https://github.com/SalishSeaCast/tides


.. _tracers-repo:

tracers
~~~~~~~

* public
* Apache v2.0 license, copyright project contributors and UBC
* Tracer boundary condition climatology files and domain initialization files for the Salish Sea NEMO model.
  Included are temperature, salinity, nutrients, and biological tracers.

  Many of these files originated in the (private) :ref:`NEMO-forcing-repo` repository.
  The commit message history of files from that repository is included in their initial commit messages.
* Bitbucket: https://bitbucket.org/salishsea/tracers


.. _NEMO-forcing-repo:

NEMO-forcing
~~~~~~~~~~~~

* **Read-only** legacy repo
* private to SalishSea-MEOPAR team members
* private because the files are from project initialization tarballs received from J-P Paquin on 2013-10-02
* domain-specific set-up,
  initial conditions,
  forcing,
  etc.
  files used to run NEMO for the Salish Sea:

  * coordinates
  * bathymetry
  * initial temperature and salinity
  * restart files from spin-up runs
  * tidal forcing
  * open boundary condition forcing
  * etc.

* Bitbucket: https://bitbucket.org/salishsea/nemo-forcing/
* documentation: https://salishsea-meopar-docs.readthedocs.io/en/latest/code-notes/salishsea-nemo/nemo-forcing/index.html

.. note::
    This was the project's original repo of initial conditions, forcing, etc. files.
    It got so big that it was in danger of hitting the Bitbucket 2Gb limit at which changesets would no longer be accepted.
    It also contained multiple copies large binary files which are no longer used.
    Together,
    these factors make this repo very time consuming to clone and update.
    In August 2017 the repo was split into 4 smaller repos:

    * :ref:`grid-repo`
    * :ref:`rivers-repo`
    * :ref:`tides-repo`
    * :ref:`tracers-repo`

    They are all public,
    licensed under Apache v2.0 license,
    and copyright by the project contributors and UBC.
    They can be found in the `SalishSea-MEOPAR NEMO Model Runs`_ collection of repos on Bitbucket.

    .. _SalishSea-MEOPAR NEMO Model Runs: https://bitbucket.org/account/user/salishsea/projects/SSM_NEMO_RUNS


.. _SS-run-sets-repo:

SS-run-sets
~~~~~~~~~~~

* public
* Apache v2.0 license, copyright project contributors and UBC
* a collection of namelists and run description files for various sets of NEMO runs
* Bitbucket: https://bitbucket.org/salishsea/ss-run-sets/


.. _salishsea-site-repo:

salishsea-site
~~~~~~~~~~~~~~

* public
* Apache v2.0 license, copyright project contributors and UBC
* code and documentation for the dynamic web app based on the Pyramid framework that serves the `salishsea.eos.ubc.ca`_ domain
* Bitbucket: https://bitbucket.org/salishsea/salishsea-site/
* documentation: https://salishsea-site.readthedocs.io/en/latest/

  .. _salishsea.eos.ubc.ca: https://salishsea.eos.ubc.ca/


.. _results-repo:

results
~~~~~~~

* public
* all rights reserved, copyright project contributors and UBC
* a collection of model results and analysis produced by the Salish Sea MEOPAR project
* Bitbucket: https://bitbucket.org/salishsea/results/


.. _storm-surge-repo:

Storm-Surge
~~~~~~~~~~~

* public
* Apache v2.0 license,
  copyright project contributors and UBC,
  except the manuscript files which are copyright Taylor and Francis
* Salish Sea MEOPAR storm surge paper
* Soontiens, N., Allen, S., Latornell, D., Le Souef, K., Machuca, I., Paquin, J.-P., Lu, Y., Thompson, K., Korabel, V. (2016).
  Storm surges in the Strait of Georgia simulated with a regional model.
  Atmosphere-Ocean, volume 54, issue 1.
  https://dx.doi.org/10.1080/07055900.2015.1108899
* Bitbucket: https://bitbucket.org/salishsea/storm-surge/


.. _barotropic-tides:

Barotropic-Tides
~~~~~~~~~~~~~~~~

* private until paper is published
* development of the Salish Sea NEMO barotropic tides paper
* Bitbucket: https://bitbucket.org/salishsea/barotropic-tides


.. _internal-tides:

internal-tides
~~~~~~~~~~~~~~

* private until paper is published
* development of the Salish Sea NEMO interal tides paper
* Bitbucket: https://bitbucket.org/salishsea/internal-tides


.. _mixing-paper:

mixing-paper
~~~~~~~~~~~~

* private until paper is published
* development of the Salish Sea NEMO mixing paper
* Bitbucket: https://bitbucket.org/salishsea/mixing-paper


.. _plume-paper:

plume-paper
~~~~~~~~~~~

* private until paper is published
* development of the Salish Sea NEMO plume paper
* Bitbucket: https://bitbucket.org/salishsea/plume-paper


.. _XIOS-repo:

XIOS (XML I/O Server)
~~~~~~~~~~~~~~~~~~~~~

* private
  (because the canonical source for this code is :kbd:`http://forge.ipsl.jussieu.fr/ioserver/wiki`)
* CeCILL_V2 license, copyright Centre National de la Recherche Scientifique CNRS
* XIOS-1.0 code that we run with NEMO-3.6
* a checkout of the :kbd:`http://forge.ipsl.jussieu.fr/ioserver/svn/XIOS/branchs/xios-1.0` branch
* maintenance of the repo to handle the merging in changes from upstream is decribed in :ref:`NEMO-MirrorMaintenance`
* Bitbucket: https://bitbucket.org/salishsea/xios/
* documentation: https://salishsea-meopar-docs.readthedocs.io/en/latest/code-notes/salishsea-nemo/nemo-code.html

.. note::
    This is the XIOS-1.0 code.
    It is archival.
    The Salish Sea model now uses :ref:`XIOS-2-repo`,
    and that is the recommended code for any new projects.


.. _NEMO-code-repo:

NEMO-code
~~~~~~~~~

* private
  (because NEMO project requires sign-in to access code)
* CeCILL license, copyright Centre National de la Recherche Scientifique CNRS
* NEMO-3.4 code that we run
* a merge of the :kbd:`http://forge.ipsl.jussieu.fr/nemo/svn/branches/2012/dev_v3_4_STABLE_2012` branch and our local code
* maintenance of the repo to handle the merges is decribed in :ref:`NEMO-MirrorMaintenance`
* Bitbucket: https://bitbucket.org/salishsea/nemo-code/
* documentation: https://salishsea-meopar-docs.readthedocs.io/en/latest/code-notes/salishsea-nemo/nemo-code.html

.. note::
    This is the NEMO-3.4 code.
    It is archival.
    The Salish Sea model is now based on :ref:`NEMO-3.6-code-repo`,
    and that is the recommended code for any new projects.


.. _SoG-obs-repo:

SoG-obs
~~~~~~~

* private
* a collection of observations made in the Salish Sea
* This repo is a vestige of an experiment that didn't work out because storing large binary files under Mercurial version control is not a good idea.
* Bitbucket: https://bitbucket.org/salishsea/sog-obs/


.. _NEMO_EastCoast-repo:

NEMO_EastCoast
~~~~~~~~~~~~~~

* public
* a collection of files for pre-processing,
  running,
  and post-processing of numerical simulations with NEMO for MEOPAR project
  with primary focus on the Scotia Shelf deployment of the model
* Bitbucket: https://bitbucket.org/salishsea/nemo_eastcoast/


.. _NEMO-3.1-repo:

NEMO-3.1
~~~~~~~~

* private
  (because NEMO project required sign-in to access code)
* CeCILL license, copyright Centre National de la Recherche Scientifique CNRS
* NEMO-3.1 reference repo
* a Mercurial repo of SVN checkouts of modipsl trunk, the NEMO-3.1 tag, and supporting repos that are believed to be the basis on which the 2-Oct-2013 CONCEPTS-110 CODE tarball was built
* documentation rendered at https://salishsea-meopar-docs.readthedocs.io/en/latest/code-notes/salishsea-nemo/nemo31-concepts110.html
* Bitbucket: https://bitbucket.org/salishsea/nemo-3.1/


.. _CONCEPTS-110-repo:

CONCEPTS-110
~~~~~~~~~~~~

* private
  (because NEMO project required sign-in to access code)
* CeCILL license, copyright Centre National de la Recherche Scientifique CNRS
* CONCEPTS-110 reference repo
* a Mercurial repo of the CODE.tar tarball received from J-P Paquin on 2-Oct-2013
* documentation rendered at https://salishsea-meopar-docs.readthedocs.io/en/latest/code-notes/salishsea-nemo/nemo31-concepts110.html
* Bitbucket: https://bitbucket.org/salishsea/concepts-110/
