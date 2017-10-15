.. _WorkingOnComputeCanada:

******************************************
Working on :kbd:`computecanada`: NEMO v3.6
******************************************

This section describes the steps to set up and run the Salish Sea NEMO version 3.6 code on the ComputeCanada machines `graham.computecanada.ca`_ and `cedar.computecanada.ca`_.

This guide assumes that your :ref:`WorkingEnvironment` is set up and that you are familiar with :ref:`WorkingOnSalish`.

.. _graham.computecanada.ca: https://docs.computecanada.ca/wiki/Graham
.. _cedar.computecanada.ca: https://docs.computecanada.ca/wiki/Cedar


Modules setup
=============

When working on the ComputeCanada clusters, the :command:`module load` command must be used to load extra software components.

You can manually load the modules each time you log in,
or you can add the lines to your :file:`$HOME/.bashrc` file so that they are automatically loaded upon login.

At present, :kbd:`cedar` and :kbd:`graham` are configured similarly.
The modules needed are:

.. code-block:: bash

    module load python27-scipy-stack/2017a
    module load perl/5.22.2
    module load intel/2016.4
    module load hdf5-mpi/1.8.18
    module load netcdf-c++4-mpi/4.3.0
    module load netcdf-fortran-mpi/4.4.4
    module load netcdf-mpi/4.4.1.1


Create a Workspace and Clone the Repositories
=============================================

:kbd:`cedar` and :kbd:`graham` provide `several different types of file storage`_.
We use project space for our working environments because it is large,
high performance,
and backed up.
Scratch space is even larger,
also high performance,
but not backed up,
so we use that as the space to execute NEMO runs in,
but generally move the run results to project space.

Both systems provide environment variables that are more convenient that remembering full paths to access your project and scratch spaces:

* Your project space is at :file:`$PROJECT/$USER/`
* Your scratch space is at :file:`$SCRATCH/`
* Daily atmospheric,
  river,
  and west boundary forcing files are in the :file:`$PROJECT/SalishSea/forcing/` tree

.. _several different types of file storage: https://docs.computecanada.ca/wiki/Storage_and_file_management

Create a :file:`MEOPAR/` directory tree in your project space:

.. code-block:: bash

    mkdir -p $PROJECT/$USER/MEOPAR/SalishSea/results

Clone the repos needed to run the model:

.. code-block:: bash

    cd $HOME/MEOPAR
    hg clone ssh://hg@bitbucket.org/salishsea/nemo-3.6-code NEMO-3.6-code
    hg clone ssh://hg@bitbucket.org/salishsea/xios-2 XIOS-2
    hg clone ssh://hg@bitbucket.org/salishsea/xios-arch XIOS-ARCH
    hg clone ssh://hg@bitbucket.org/salishsea/ss-run-sets SS-run-sets
    hg clone ssh://hg@bitbucket.org/salishsea/grid
    hg clone ssh://hg@bitbucket.org/salishsea/rivers-climatology
    hg clone ssh://hg@bitbucket.org/salishsea/tides
    hg clone ssh://hg@bitbucket.org/salishsea/tracers
    hg clone ssh://hg@bitbucket.org/salishsea/nemo-cmd NEMO-Cmd
    hg clone ssh://hg@bitbucket.org/salishsea/salishseacmd SalishSeaCmd


Install the Command Processor Packages
======================================

Install the :ref:`NEMO-CommandProcessor` and :ref:`SalishSeaCmdProcessor` Python packages:

.. code-block:: bash

    cd $PROJECT/$USER/MEOPAR/
    pip install --user --editable NEMO-Cmd
    pip install --user --editable SalishSeaCmd


.. _CompileXIOS-computecanada:

Compile XIOS-2
==============

First symlink the XIOS-2 build configuration files for the machine that you are working on from the :file:`XIOS-ARCH` repo clone into the :file:`XIOS-2/arch/` directory,
then compile XIOS-2:

:kbd:`cedar`:
-------------

.. code-block:: bash

    cd $PROJECT/$USER/MEOPAR/XIOS-2/arch
    ln -sf $PROJECT/$USER/MEOPAR/XIOS-ARCH/WESTGRID/arch-X64_CEDAR.env
    ln -sf $PROJECT/$USER/MEOPAR/XIOS-ARCH/WESTGRID/arch-X64_CEDAR.fcm
    ln -sf $PROJECT/$USER/MEOPAR/XIOS-ARCH/WESTGRID/arch-X64_CEDAR.path
    cd $PROJECT/$USER/MEOPAR/XIOS-2
    ./make_xios --arch X64_CEDAR --job 8


:kbd:`graham`:
--------------

.. code-block:: bash

    cd $PROJECT/$USER/MEOPAR/XIOS-2/arch
    ln -sf $PROJECT/$USER/MEOPAR/XIOS-ARCH/WESTGRID/arch-X64_GRAHAM.env
    ln -sf $PROJECT/$USER/MEOPAR/XIOS-ARCH/WESTGRID/arch-X64_GRAHAM.fcm
    ln -sf $PROJECT/$USER/MEOPAR/XIOS-ARCH/WESTGRID/arch-X64_GRAHAM.path
    cd $PROJECT/$USER/MEOPAR/XIOS-2
    ./make_xios --arch X64_GRAHAM --job 8


Compile NEMO-3.6
================

Compile the :kbd:`SalishSea` NEMO configuration and the :program:`rebuild_nemo` tool:

:kbd:`cedar`:
-------------

.. code-block:: bash

    cd $PROJECT/$USER/MEOPAR/NEMO-3.6-code/NEMOGCM/CONFIG
    ./makenemo -n SalishSea -m X64_CEDAR -j 8
    cd $PROJECT/$USER/MEOPAR/NEMO-3.6-code/NEMOGCM/TOOLS
    ./maketools -n REBUILD_NEMO -m X64_CEDAR


:kbd:`graham`:
--------------

.. code-block:: bash

    cd $PROJECT/$USER/MEOPAR/NEMO-3.6-code/NEMOGCM/CONFIG
    ./makenemo -n SalishSea -m X64_GRAHAM -j 8
    cd $PROJECT/$USER/MEOPAR/NEMO-3.6-code/NEMOGCM/TOOLS
    ./maketools -n REBUILD_NEMO -m X64_GRAHAM


To build a configuration other than :kbd:`SalishSea`, replace :kbd:`SalishSea` with the name of the configuration to be built, e.g. :kbd:`SMELT`:

.. code-block:: bash

    cd $PROJECT/$USER/MEOPAR/NEMO-3.6-code/NEMOGCM/CONFIG
    ./makenemo -n SMELT -m X64_CEDAR -j 8


Prepare and Execute Runs
========================

The :file:`SS-run-sets/v201702/` directory in the :ref:`SS-run-sets-SalishSea` repo contains version controlled sample run description files and namelist segment files.
In your own directory in that repo copy, edit,
and version control those files to define the runs that you want to execute.

The run description file is described in the :ref:`RunDescriptionFileStructure` section of the :ref:`project tools documentation <SalishSeaToolsDocs>`.
The namelists are described in the `NEMO-3.6 Book`_.

.. _NEMO-3.6 Book: https://www.nemo-ocean.eu/wp-content/uploads/NEMO_book.pdf

Use :program:`salishsea` :ref:`salishsea-run` to prepare,
execute,
and gather the results for a run:

.. code-block:: bash

    salishsea run SalishSea.yaml $PROJECT/$USER/MEOPAR/SalishSea/results/my_excellent_results

:command:`salishsea run` returns the path and name of the temporary run directory,
and the job identifier assigned by the queue manager,
something like:

.. code-block:: bash

    salishsea_cmd.run INFO: Created run directory /scratch/dlatorne/20mar17nowcast16x34_2017-10-06T101548.694389-0700
    salishsea_cmd.run INFO: Submitted batch job 1578481

You can use the batch job number with :command:`squeue --job` and :command:`sacct --job` to monitor the execution status of your job.

When the job completes the results should have been gathered in the directory you specified in the :command:`salishsea run` command and the temporary run directory should have been deleted.

To view and analyze the run results copy them to your :file:`/data/$USER/results/` workspace with :program:`scp`, :program:`sftp` or :program:`rsync`.
