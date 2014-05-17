*****************
Quick Start Guide
*****************

This section describes *very* briefly the steps to set up and run the Salish Sea NEMO code.
Details of what this all means and why the steps below are what they are can be found in subsequent sections.


Create a Workspace
==================

You can all your workspace directory whatever you want so here we're going to call it :file:`MEOPAR`.

Most runs of the model will be done on :kbd:`salish` and the :file:`/data/` partition should be used there:

.. code-block:: bash

    mkdir -p /data/<userid>/MEOPAR

Development and analysis work will probably be done on one of the :kbd:`ocean`-cluster workstations where storage is under the :file:`/ocean/` NFS mount:

.. code-block:: bash

    mkdir -p /ocean/<userid>/MEOPAR

On a :kbd:`westgrid.ca` machine or your laptop you would similarly create a :file:`MEOPAR/` directory in an appropriate file system location.

All of the following steps assume that you are in the :file:`MEOPAR/` directory
(or its equivalent if you have chosen a different name).

Create directories for the temporary directories that are created to run the model,
and to receive results files from runs.
We'll call them :file:`SalishSea/` and :file:`Salishsea/results/`:

.. code-block:: bash

    mkdir -p SalishSea/results


Clone the Repos
===============

Assuming that you are using SSH key authentication on Bitbucket
(see :ref:`vc-with-hg`),
clone the :ref:`NEMO-code <NEMO-code>`,
:ref:`NEMO-forcing <NEMO-forcing>`,
:ref:`SS-run-sets <SS-run-sets>`,
:ref:`tools <tools-repo>`,
:ref:`analysis <analysis-repo>`,
and :ref:`docs <docs-repo>` repos:

.. code-block:: bash

    hg clone ssh://hg@bitbucket.org/salishsea/nemo-code NEMO-code
    hg clone ssh://hg@bitbucket.org/salishsea/nemo-forcing NEMO-forcing
    hg clone ssh://hg@bitbucket.org/salishsea/ss-run-sets SS-run-sets
    hg clone ssh://hg@bitbucket.org/salishsea/tools
    hg clone ssh://hg@bitbucket.org/salishsea/analysis
    hg clone ssh://hg@bitbucket.org/salishsea/docs


Install Tools and Command Processor Packages
============================================

Assuming that you have the :ref:`AnacondaPythonDistro` installed,
install the :ref:`SalishSeaTools` and :ref:`SalishSeaCmdProcessor` Python packages:

.. code-block:: bash

    cd tools/SalishSeaTools
    pip install -e .
    cd ../SalishSeaCmd
    pip install -e .


Compile the Code
================

Assuming that you are working on :kbd:`salish`,
compile and link the full domain Salish Sea NEMO configuration and the IOM output server with the :kbd:`salish` architecture definitions with the compilation distributed over 8 cores.

.. code-block:: bash

    cd NEMO-code/NEMOGCM/CONFIG
    ./makenemo -n SalishSea -m salish -j8

The resulting executables are located in :file:`NEMO-code/NEMOGCM/CONFIG/SalishSea/BLD/bin/`.

Compile and link the :program:`rebuild_nemo` tool:

.. code-block:: bash

    cd NEMO-code/NEMOGCM/TOOLS
    ./maketools -m salish -n REBUILD_NEMO

See :ref:`rebuild-nemo-tool` for more information about it.


.. _PrepareRun:

Prepare a Run
=============

The :file:`SS-run-sets/` :ref:`SS-run-sets-SalishSea` contains version controlled sample run description files and namelist segment files.
Edit or copy and edit those files to define the run that you want to execute then use :program:`salishsea` :ref:`salishsea-prepare` to create a temporary run directory containing all of the appropriate files and symbolic links:

.. code-block:: bash

    salishsea prepare SalishSea.yaml iodef.xml

That command will provide the relative path and name of the temporary run directory,
something like:

.. code-block:: bash

    Created run directory ../../SalishSea/4361797c-530f-11e3-ae1d-0025909a8461


Run the Model
=============

Assuming that you are running the model with a 4x4 MPI domain decomposition,
go to the temporary run directory and run the model:

.. code-block:: bash

    cd ../../SalishSea/4361797c-530f-11e3-ae1d-0025909a8461
    mpiexec -n 16 ./nemo.exe > stdout 2> stderr &

A convenient command to monitor the memory use of a run and its time step progress is:

.. code-block:: bash

    watch -n 5 "(free -m; cat time.step)"


Collect the Run Results
=======================

When the run is finished,
and assuming that you are still in the temporary run directory,
combine the pre-processor netCDF run results files and gather the rest of the run results files into a directory for analysis:

.. code-block:: bash

    salishsea gather --no-compress SalishSea.yaml ../results/my_excellent_results

Unless you have a reason to keep it around,
the
(now empty)
temporary run directory can be deleted at this point.


Look at the Results
===================

A number of IPython notebooks that look at NetCDF files are available in :file:`analysis_tools/` directory of the :ref:`tools-repo`.
To view those notebooks,
go to https://bitbucket.org/salishsea/tools/src/tip/analysis_tools/ and follow the links in the text below the files list.

To work in your own notebook,
go to your clone of the :ref:`analysis-repo` on your local machine (not :kbd:`salish`),
create a directory with your name to work in,
and start the IPython Notebook server:

.. code-block:: bash

    cd MEOPAR/analysis/
    mkdir Susan
    cd Susan
    ipython notebook

At this points a lot of information will appear in your terminal.
This terminal session is now running a server and cannot be used for anything else until you are finished with the notebooks.
At that point you need to type :kbd:`CTRL-C` to shut down the server.

At the same time a window should open in your browser.
If it doesn't,
look at your terminal,
find the IP address,
something like:

.. code-block:: bash

    The IPython Notebook is running at: http://127.0.0.1:8888/

and put that number in your browser.
From this initial window you can navigate the :ref:`analysis-repo`,
open notebooks,
and look around.
The links to the various files will probably not work.
Change them to point to your file.
You will eventually want to build your own notebooks in your personal :file:`analysis/` directory,
but the example notebooks in https://bitbucket.org/salishsea/tools/src/tip/analysis_tools/,
and the other notebooks in the :ref:`analysis-repo` give you lots of examples to copy from.


Working on :kbd:`jasper`
========================

There are some minor differences to keep in mind when working on :kbd:`jasper.westgrid.ca` or other WestGrid_ machines.
This section is an even more condensed version of the above guide.

.. _WestGrid: https://www.westgrid.ca/

Follow the instructions in :ref:`LoadingModulesOnWestgridClusters` to manually load the necessary software component modules or edit your :kbd:`jasper` :file:`$HOME/.bashrc` to make them load automatically when you :program:`ssh` into :kbd:`jasper`.

Create a workspace:

.. code-block:: bash

    mkdir -p $HOME/MEOPAR/SalishSea/results

Clone the repos:

.. code-block:: bash

    cd $HOME/MEOPAR
    hg clone ssh://hg@bitbucket.org/salishsea/nemo-code NEMO-code
    hg clone ssh://hg@bitbucket.org/salishsea/nemo-forcing NEMO-forcing
    hg clone ssh://hg@bitbucket.org/salishsea/ss-run-sets SS-run-sets
    hg clone ssh://hg@bitbucket.org/salishsea/tools

There is probably no need to clone the :file:`docs` repo on :kbd:`jasper`.

Create a :file:`$HOME/.local/` file space for per-user installation of Python packages,
install pip_,
upgrade setuptools_ to the current release,
and install the :ref:`SalishSeaTools` and :ref:`SalishSeaCmdProcessor` Python packages:

.. code-block:: bash

    mkdir -p $HOME/.local/bin $HOME/.local/lib/python2.7/site-packages
    easy_install --install-dir $HOME/.local/lib/python2.7/site-packages --script-dir $HOME/.local/bin "pip==1.4.1"
    $HOME/.local/bin/pip install --user --upgrade setuptools
    cd tools/SalishSeaTools
    $HOME/.local/bin/pip install --user .
    cd ../SalishSeaCmd
    $HOME/.local/bin/pip install --user .

.. _pip: https://pypi.python.org/pypi/pip
.. _setuptools: https://pypi.python.org/pypi/setuptools

.. note::

    The above method of installing the :ref:`SalishSeaTools` and :ref:`SalishSeaCmdProcessor` packages means that they must be updated any time changes to them are pulled in from the :ref:`tools-repo` repo.
    The command to do that is:

    .. code-block:: bash

        pip install --upgrade --no-deps --user .

Edit your :file:`$HOME/.bashrc` to add :file:`$HOME/.local/bin` to your :envvar:`PATH`:

.. code-block:: bash

    export PATH=$HOME/.local/bin:$PATH

Compile the Salish Sea NEMO configuration,
and the :program:`rebuild_nemo` tool:

.. code-block:: bash

    cd NEMO-code/NEMOGCM/CONFIG
    ./makenemo -n SalishSea -m mpif90_jasper -j8
    cd ../TOOLS
    ./maketools -n REBUILD_NEMO  -m mpif90_jasper

Run preparation is the same as that for running on :kbd:`salish`
(described in :ref:`PrepareRun`).
The recommended MPI domain decomposition for :kbd:`jasper` runs is 6x14.
A namelist for that configuration is included in :ref:`SS-run-sets-SalishSea`.
One additional file is required for :kbd:`jasper` runs:
a TORQUE batch job script;
an example can be found in :file:`SS-run-sets/SalishSea/jasper/SalishSea.40d.pbs`.
You should create your own copy with appropriate values for:

* :kbd:`#PBS -N`
* :kbd:`#PBS -l walltime=`
* :kbd:`#PBS -M`
* :kbd:`#PBS -o`
* :kbd:`#PBS -e`
* :kbd:`RESULTS_DIR=`

See the `WestGrid TORQUE directives docs`_ for details of the :kbd:`#PBS` directives.
Set :kbd:`RESULTS_DIR` to the name of the directory where you want your run results to be gathered.

.. _WestGrid TORQUE directives docs: https://www.westgrid.ca/support/running_jobs#directives

Run the model by copying the TORQUE batch job script to the temporary run directory
(returned by the :command:`salishsea prepare` command),
going to that directory,
and submitting the job script:

.. code-block:: bash

    cp SalishSea.40d.pbs ../../SalishSea/4361797c-530f-11e3-ae1d-0025909a8461/
    cd ../../SalishSea/4361797c-530f-11e3-ae1d-0025909a8461
    qsub SalishSea.40d.pbs

The :program:`qsub` command returns a job identifier string that you can use with :program:`qstat`,
:program:`showstart`,
and :program:`checkjob` to monitor the execution status of your job.

When the job completes the results should have been gathered in the directory you specified in the jobs script and the temporary run directory should be empty.
You can delete the run directory.

To view and analyze the run results copy them to your :file:`/ocean/` workspace with :program:`scp` or :program:`sftp`.
