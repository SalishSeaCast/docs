********************************
:kbd:`salishsea.eos.ubc.ca` Site
********************************

This section documents the `salishsea.eos.ubc.ca`_ web site.
Included are a description of the design of the site,
instructions on how to build the site locally to test new content,
and how the site is deployed to EOAS department web server.

.. _salishsea.eos.ubc.ca: http://salishsea.eos.ubc.ca/

The source files for the site are in the :ref:`salishsea-site-repo` repo.


Site Design
===========

The `salishsea.eos.ubc.ca`_ web site is based on `reStructuredText`_ files rendered to HTML by `Sphinx`_,
the same tools as are used for the :ref:`docs-repo` repo and the :ref:`tools-repo` repo docs.
The vastly different appearance of the salishsea site from the renderings of those repos on `readthedocs.org`_ is thanks to CSS and Javascript support files provided by the `sphinx-bootstrap-theme`_ package and the `Bootswatch`_ `Superhero`_ theme.

.. _reStructuredText: http://docutils.sourceforge.net/rst.html
.. _Sphinx: http://sphinx-doc.org/
.. _sphinx-bootstrap-theme: http://ryan-roemer.github.io/sphinx-bootstrap-theme/README.html
.. _readthedocs.org: http://salishsea-meopar-docs.readthedocs.org/
.. _Bootswatch: http://bootswatch.com/
.. _Superhero: http://bootswatch.com/superhero/

The directory structure of the repo looks like::

  salishsea-site/
  |-- CONTRIBUTORS.rst
  |-- LICENSE
  |-- README.rst
  |-- images/
  |   |-- README
  |   `-- index/
  |       |-- AragoniteTimeSeries.png
  |       |-- README
  |       |-- SalishSeaBathy.png
  |       `-- nitrate_diatoms_timeseries.png
  |-- requirements.txt
  `-- site/
      |-- Makefile
      |-- _static/
      |   |-- bloomcast/
      |   |   `-- spring_diatoms/
      |   |       `-- README
      |   |-- custom.css
      |   |-- index.svg
      |   `-- nemo/
      |       |-- SalishSeaImage.png
      |       `-- results_figures/
      |           |-- forecast/
      |           |   `-- README
      |           |-- forecast2/
      |           |   `-- README
      |           `-- nowcast/
      |               `-- README
      |-- _templates/
      |-- bloomcast/
      |   `-- README
      |-- bloomcast.rst
      |-- conf.py
      |-- contributors.rst
      |-- index.rst
      |-- license.rst
      |   `-- nemo/
      |       |-- index.rst
      |       `-- results\
      |           |-- index.rst
      |           |-- forecast/
      |           |   `-- README
      |           |-- forecast2/
      |           |   `-- README
      |           `-- nowcast/
      |               `-- README
      |-- nemo.rst
      |-- sog.rst
      `-- storm-surge/
          `-- index.rst

.. note::

    The above directory tree does not show all of the sub-directories -
    just the "interesting" ones.

There are several important things to understand about this directory structure:

* The :file:`images/` directory tree contains "source" files for images that are in the site content.
  For example,
  the files in the :file:`images/index/` directory are the base images that were used to compose the "circles" image on the main site index page.
  Files in the :file:`images/` directory should be organized in sub-directories that reflect the site structure.
  The content images themselves are stored in the :file:`site/_static/` directory tree
  (see below).
* The :file:`site/` directory tree contains the rst files, images, etc. that are processed by Sphinx to render the site.
* The theme options are all specified in the :file:`salishsea-site/site/conf.py` file.
* The rendered site is built in :file:`site/_build/html/`.
* The :file:`site/_static/` directory tree contains the site's "static assets" such as image,
  CSS,
  and Javascript files.
* The :file:`site/_static/bloomcast/spring_diatoms/` directory is managed by the SoG-bloomcast project's automation scripts.
  As indicated in the :file:`README` in that directory,
  *please do not add files or change this part of the directory tree* without corresponding changes in the SoG-bloomcast codebase.
* Likewise,
  the :file:`site/_static/nemo/results_figures/` directory tree is managed by the :ref:`salishsea_tools.nowcast` automation scripts.
  As indicated in the :file:`README` files in that directory tree,
  *please do not add files or change this part of the directory tree* without corresponding changes in the :ref:`salishsea_tools.nowcast` codebase.
* The automation scripts in the SoG-bloomcast project and the :ref:`salishsea_tools.nowcast` also generate files that are stored in the :file:`site/bloomcast/` and the :file:`site/nemo/results/` and :file:`site/storm-surge/` directories,
  respectively.
  After those automation scripts have generated their files,
  they:

  * execute the :command:`hg update` command on a clone of the :ref:`salishsea-site-repo` repo to pull in any changes from other sources
  * execute the equivalent of :command:`make html` in the :file:`site/` directory to build the new/changed pages of the site (see :ref:`BuildingAndPreviewingTheSite`)
  * execute an :command:`rsync` command to push the changes to the web server (see :ref:`DeploymentOfTheSite`)

* The :file:`site/nemo.rst` file is an artifact of the initial deployment of the site,
  but it has been referenced in grant proposals so it must be maintained to avoid a "published" broken link.
  Its content should be kept in sync with :file:`site/nemo/index.rst`.
  Unfortunately,
  :file:`site/nemo.rst` cannot simply be a symlink to :file:`site/nemo/index.rst` because the relative paths to the :file:`site/_static/` directory have to be different in the two files.


Building the Site Locally
=========================

Prerequisite: A clone of the :ref:`salishsea-site-repo` repo


The Build Environment
---------------------

You can install the packages required to build the site into your default :ref:`AnacondaPythonDistro` environment,
but the recommended practice is to work in a project-specific environment that contains just the packaged required for working with the :ref:`salishsea-site-repo` repo.
Doing so helps to avoid package dependency conflicts and makes it easire to debug issues related to Python packages.

Create a new :command:`conda` environment with `Sphinx`_ and `pip`_ installed in it:

.. code-block:: bash

    $ conda create -n salishsea-site python=3.4 sphinx pip

which should produce output like::

  Fetching package metadata: ..
  Solving package specifications: .
  Package plan for installation in environment ~/anaconda/envs/salishsea-site:

  The following NEW packages will be INSTALLED:

      docutils:   0.12-py34_0
      jinja2:     2.7.3-py34_1
      markupsafe: 0.23-py34_0
      openssl:    1.0.1j-5
      pip:        6.0.6-py34_0
      pygments:   1.6-py34_0
      python:     3.4.2-0
      readline:   6.2-2
      setuptools: 11.3.1-py34_0
      sphinx:     1.2.3-py34_0
      sqlite:     3.8.4.1-0
      system:     5.8-1
      tk:         8.5.15-0
      xz:         5.0.5-0
      zlib:       1.2.8-0

  Proceed ([y]/n)? y

  Linking packages ...
  [      COMPLETE      ] |################################################################################| 100%
  #
  # To activate this environment, use:
  # $ source activate salishsea-site
  #
  # To deactivate this environment, use:
  # $ source deactivate
  #

.. _pip: https://pip.pypa.io/en/latest/

The package version numbers you see may be different than above,
and you may see additional messages about packages being downloaded and installed --
that's all normal and nothing to worry about.

Note that we're using Python 3.4 in this environment because all of the tools in the salishsea site build chain work under both Python 3 and Python 2,
so why not use the shiny new Python?!

Once the environment has been created,
activate it and use :command:`pip` to install the :kbd:`sphinx-bootstrap-theme` package that isn't available from the conda package repository but is available from the Python Package Index:

.. code-block:: bash

    $ source activate salishsea-site
    (salishsea-site)$ pip install sphinx-bootstrap-theme

which should produce output like::

    Collecting sphinx-bootstrap-theme
      Using cached sphinx-bootstrap-theme-0.4.5.tar.gz
    Requirement already satisfied (use --upgrade to upgrade): setuptools in ./anaconda/envs/salishsea-site/lib/python3.4/site-packages/setuptools-11.3.1-py3.4.egg (from sphinx-bootstrap-theme)
    Installing collected packages: sphinx-bootstrap-theme
      Running setup.py install for sphinx-bootstrap-theme
        Fixing build/lib/sphinx_bootstrap_theme/__init__.py
        Fixing build/lib/sphinx_bootstrap_theme/__init__.py
    Successfully installed sphinx-bootstrap-theme-0.4.5

You now have a conda environment in which you can build the content of the :kbd:`salishsea.eos.ubc.ca` site.

As time passes,
new version of the packages installed above will be released.
To update the packages in the environment use:

.. code-block:: bash

    (salishsea-site)$ conda update --all
    (salishsea-site)$ pip install --upgrade sphinx-bootstrap-theme


.. _BuildingAndPreviewingTheSite:

Building and Previewing the Site
--------------------------------

With your build environment activated:

.. code-block:: bash

    $ source activate salishsea-site

navigate to the :file:`salishsea-site/site/` directory:

.. code-block:: bash

    (salishsea-site)$ cd salishsea-site/site/

and build the pages that have changed since your most recent build with:

.. code-block:: bash

    (salishsea-site)$ make html

You can delete the :file:`salishsea-site/site/_build/html/` tree and build the entire site from scratch with:

.. code-block:: bash

    (salishsea-site)$ make clean html

Preview the site by opening to :file:`salishsea-site/site/_build/html/index.html`
(or any other page)
in your web browser.
On Ubuntu you should be able to do that from the command-line with:

.. code-block:: bash

    (salishsea-site)$ firefox _build/html/index.html

On OS/X:

.. code-block:: bash

    (salishsea-site)$ open _build/html/index.html


.. _DeploymentOfTheSite:

Deployment of the Site
======================

The site is deployed on the EOAS department web server,
:kbd:`shelob` in the :file:`/www/salishsea/data/` directory.
:kbd:`shelob` is accessible only via :command:`ssh` from other machines within the :kbd:`eos.ubc.ca` domain.

The :kbd:`rsync-shelob` target in the :file:`site/Makefile` uses `rsync`_ to transfer the file changes from a build of the site on an EOAS machine to the web server:

.. code-block:: bash

    (salishsea-site)$ cd salishsea-site/site/
    (salishsea-site)$ make rsync-shelob

which should produce output like::

  # This target can only be used from a machine on the EOAS network.
  #
  # A shelob-salishsea host must be defined in your .ssh/config file.

  chmod -R g+w _build/html
  rsync -rltpDvh \
    --exclude bloomcast/spring_diatoms.html \
    --exclude _static/bloomcast/spring_diatoms/ \
    --exclude nemo/results/ \
    --exclude storm-surge/forecast*.html \
    --exclude _static/nemo/results_figures/nowcast/ \
    --exclude _static/nemo/results_figures/forecast/ \
    --exclude _static/nemo/results_figures/forecast2/ \
    _build/html/ shelob-salishsea:/www/salishsea/data/
  sending incremental file list
  rsync: failed to set times on "/www/salishsea/data/.": Operation not permitted (1)
  ./
  _static/
  _static/nemo/
  _static/nemo/results_figures/
  storm-surge/
  storm-surge/index.html

  sent 6.45K bytes  received 303 bytes  13.51K bytes/sec
  total size is 6.05M  speedup is 895.36
  rsync error: some files/attrs were not transferred (see previous errors) (code 23) at main.c(1183) [sender=3.1.0]
  make: *** [rsync-shelob] Error 23

.. _rsync: http://rsync.samba.org/

The incremental file list that you see will,
undoubtedly,
be different,
as will the values in the statistics report.
The message about failing to set times on :file:`/www/salishsea/data/.`,
and "some files/attrs were not transferred[...]" are due to lacking permission to set the modification time attribute of the :file:`/www/salishsea/data/` directory.
Don't worry about them, they are inconsequential, but unavoidable.

To use the :command:`make rsync-salishsea` command you need to have a :kbd:`Host` entry for :kbd:`shelob-salishsea` in your :file:`$HOME/.ssh/config` file:

.. code-block:: none

    Host shelob-salishsea
         HostName shelob
         User     dlatorne
         IdentityFile ~/.ssh/SalishSeaNEMO-nowcast_id_rsa

Your :kbd:`User` and :kbd:`IdentityFile` values may differ.
The ones shown are those required for the :ref:`salishsea_tools.nowcast` automation.
The :kbd:`User` value must be a user account on :kbd:`shelob` created by EOAS compstaff,
and the :kbd:`IdentityFile` value must be an ssh private key whose corresponding public key is in the :file:`$HOME/.ssh/authorized_keys` file of the :kbd:`User` account on :kbd:`shelob`.
