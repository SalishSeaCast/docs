************************
Working on :kbd:`jasper`
************************

This section describes *very* briefly the steps to set up and run the Salish Sea NEMO code on the `jasper.westgrid.ca`_ HPC cluster.
This guide assumes that your :ref:`WorkingEnvironment` is set up,
and that you are familiar with :ref:`WorkingOnSalish`.

.. _jasper.westgrid.ca: https://www.westgrid.ca/jasper

Follow the instructions in :ref:`LoadingModulesOnWestgridClusters` to manually load the necessary software component modules or edit your :kbd:`jasper` :file:`$HOME/.bashrc` to make them load automatically when you :program:`ssh` into :kbd:`jasper`.

Create a Workspace and Clone the Repos
======================================

.. code-block:: bash

    mkdir -p $HOME/MEOPAR/SalishSea/results

Clone the repos needed to run the model:

.. code-block:: bash

    cd $HOME/MEOPAR
    hg clone ssh://hg@bitbucket.org/salishsea/nemo-code NEMO-code
    hg clone ssh://hg@bitbucket.org/salishsea/nemo-forcing NEMO-forcing
    hg clone ssh://hg@bitbucket.org/salishsea/ss-run-sets SS-run-sets
    hg clone ssh://hg@bitbucket.org/salishsea/tools

There is no need to clone the :file:`docs` or :file:`analysis` repos on :kbd:`jasper`.


Install Tools and Command Processor Packages
============================================

Create a :file:`$HOME/.local/` file space for per-user installation of Python packages and install the :ref:`SalishSeaToolsPackage` and :ref:`SalishSeaCmdProcessor` Python packages:

.. code-block:: bash

    mkdir -p $HOME/.local
    cd tools
    pip install --user -e SalishSeaTools
    pip install --user -e SalishSeaCmd

Edit your :file:`$HOME/.bashrc` to add :file:`$HOME/.local/bin` to your :envvar:`PATH`:

.. code-block:: bash

    export PATH=$HOME/.local/bin:$PATH


Compile the Code
================

Compile the Salish Sea NEMO configuration,
and the :program:`rebuild_nemo` tool:

.. code-block:: bash

    cd NEMO-code/NEMOGCM/CONFIG
    ./makenemo -n SalishSea -m mpif90_jasper -j8
    cd ../TOOLS
    ./maketools -n REBUILD_NEMO  -m mpif90_jasper


Prepare and Execute Runs
========================

The :file:`SS-run-sets/` :ref:`SS-run-sets-SalishSea` contains version controlled sample run description files and namelist segment files.
In your own directory in that repo copy,
edit,
and version control those files to define the runs that you want to execute.

The run description file is described in the :ref:`RunDescriptionFileStructure` section of the :ref:`project tools documentation <SalishSeaToolsDocs>`.
The namelists are described in the `NEMO-3.4 Book`_.

.. _NEMO-3.4 Book: http://www.nemo-ocean.eu/content/download/21612/97924/file/NEMO_book_3_4.pdf

Use :program:`salishsea` :ref:`salishsea-run` to prepare,
execute,
and gather the results for a run:

.. code-block:: bash

    salishsea run SalishSea.yaml iodef.xml $HOME/MEOPAR/SalishSea/results/my_excellent_results

:command:`salishsea run` returns the relative path and name of the temporary run directory,
and the job identifier assigned by the queue manager,
something like:

.. code-block:: bash

    salishsea_cmd.prepare INFO: Created run directory ../../SalishSea/38e87e0c-472d-11e3-9c8e-0025909a8461
    salishsea_cmd.run INFO: 4792276.jasper-usradm.westgrid.ca

You can use the job identifier with :program:`qstat`,
:program:`showstart`,
and :program:`checkjob` to monitor the execution status of your job.

When the job completes the results should have been gathered in the directory you specified in the :command:`salishsea run` command and the temporary run directory should have been deleted.

To view and analyze the run results copy them to your :file:`/ocean/` workspace with :program:`scp` or :program:`sftp`.
