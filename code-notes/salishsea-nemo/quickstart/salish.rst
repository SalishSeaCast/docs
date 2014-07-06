.. _WorkingOnSalish:

************************
Working on :kbd:`salish`
************************

This section describes *very* briefly the steps to set up and run the Salish Sea NEMO code.
Details of what this all means and why the steps below are what they are can be found in subsequent sections.


Create a Workspace
==================

You can call your workspace directory whatever you want but for consistency across systems we're going to call it :file:`MEOPAR`.

:kbd:`salish` has a large local storage partition mounted at :file:`/data/` which is where we will put the code and run configuration file repos:

.. code-block:: bash

    mkdir -p /data/$USER/MEOPAR

Create directories for the temporary directories that are created to run the model,
and to receive results files from runs.
We'll call them :file:`SalishSea/` and :file:`SalishSea/results/`:

.. code-block:: bash

    mkdir -p /data/$USER/MEOPAR/SalishSea/results

The :kbd:`ocean.eos.ubc.ca` storage storage server space that you use on your Waterhole workstation is mounted at :file:`/ocean/` on :kbd:`salish` so you should be able to see your :ref:`SalishSeaReposPackages` at :file:`/ocean/$USER/MEOPAR/`.

You may want to open an EOAS help desk ticket requesting that the :kbd:`salish` :file:`/data/` partition be mounted on your Waterhole workstation so that you can easily view and copy files when you are not logged into :kbd:`salish` without having to use :command:`scp` or :command:`sftp`.


Clone the Repos
===============

Assuming that you are using SSH key authentication on Bitbucket
(see :ref:`vc-with-hg`),
clone the :ref:`NEMO-code <NEMO-code>`,
:ref:`NEMO-forcing <NEMO-forcing>`,
and :ref:`SS-run-sets <SS-run-sets>` repos into your workspace on :file:`/data/`:

.. code-block:: bash

    cd /data/$USER/MEOPAR/
    hg clone ssh://hg@bitbucket.org/salishsea/nemo-code NEMO-code
    hg clone ssh://hg@bitbucket.org/salishsea/ss-run-sets SS-run-sets


Compile the Code
================

Compile and link the full domain Salish Sea NEMO configuration and the IOM output server with the :kbd:`salish` architecture definitions with the compilation distributed over 8 cores.

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
Create your own directory in that repo where you can copy,
edit,
and version control those files to define the run that you want to execute.
Use :program:`salishsea` :ref:`salishsea-prepare` to create a temporary run directory containing all of the appropriate files and symbolic links:

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

A number of notebooks that look at NetCDF files are available in :file:`analysis/analysis_tools/`.
To start these,
go to the top level directory of the :file:`analysis` repo on your local machine
(not on :kbd:`salish`) and type:

.. code-block:: bash

    ipython notebook

At this points alot of information will appear in your terminal.
This terminal session is now running a server and cannot be used for anything else until you are finished with the notebooks.
At that point you need to CTRL-C to get out.

At the same time a window should open in your browser.
If it doesn't,
look at your terminal,
find the ip address, something like:

.. code-block:: bash

    The IPython Notebook is running at: http://127.0.0.1:8888/

and put that number in your browser.
From this initial window you can open the notebooks in :file:`analysis_tools` directory and look around.
The links to the various files will probably not work.
Change them to point to your file space.
You will probably want to build your own notebook but these notebooks give you lots of examples to copy from.
