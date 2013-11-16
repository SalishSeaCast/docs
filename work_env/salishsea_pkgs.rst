.. _SalishSeaReposPackages:

Salish Sea Repos and Packages
=============================

Once you have your :ref:`MercurialConfiguration` set up and the :ref:`AnacondaPythonDistro` installed and activated you need to set up a working area,
clone some Mercurial repositories,
and install some Salish Sea MEOPAR project tools.

On :kbd:`salish` you should create a work space on the :file:`/data/` partition:

.. code-block:: bash

    ssh salish
    mkdir -p /data/<userid>/MEOPAR

where :kbd:`<userid>` is your EOAS userid.

On an :kbd:`ocean` cluster workstation list :kbd:`tyee` you should create a work space on the :kbd:`/ocean/` partition:

.. code-block:: bash

    mkdir -p /ocean/<userid>/MEOPAR

On your own laptop,
well,
the choice of where you put files is up to you...

In the work space you need to clone the Salish Sea MEOPAR project repos from Bitbucket.
Assuming you're working on :kbd:`salish`:

.. code-block:: bash

    cd /data/<userid>/MEOPAR/
    hg clone ssh://hg@bitbucket.org/salishsea/nemo-code NEMO-code
    hg clone ssh://hg@bitbucket.org/salishsea/nemo-forcing NEMO-forcing
    hg clone ssh://hg@bitbucket.org/salishsea/ss-run-sets SS-run-sets
    hg clone ssh://hg@bitbucket.org/salishsea/docs
    hg clone ssh://hg@bitbucket.org/salishsea/tools

Next,
install the :ref:`SalishSeaTools` and the :ref:`SalishSeaCmdProcessor` by following the installation instructions in those links.
