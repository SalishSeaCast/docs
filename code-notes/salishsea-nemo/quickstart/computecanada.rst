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

At present, :kbd:`beluga`,
:kbd:`cedar`,
and :kbd:`graham` are configured similarly.
The modules needed are:

.. code-block:: bash

    module load netcdf-fortran-mpi/4.4.4
    module load perl/5.22.4
    module load python/3.7.0


.. _CreateWorkspaceAndCloneRepositories:

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

:kbd:`beluga`
-------------

.. code-block:: bash

    mkdir -p ~/projects/def-allen/$USER/MEOPAR/SalishSea/results

:kbd:`cedar`
-------------

.. code-block:: bash

    mkdir -p $PROJECT/$USER/MEOPAR/SalishSea/results

:kbd:`graham`
-------------

.. code-block:: bash

    mkdir -p $PROJECT/$USER/MEOPAR/SalishSea/results

Clone the repos needed to run the model:

.. code-block:: bash

    cd $PROJECT/$USER/MEOPAR
    git clone git@github.com:SalishSeaCast/tides.git

    hg clone ssh://hg@bitbucket.org/salishsea/nemo-3.6-code NEMO-3.6-code
    hg clone ssh://hg@bitbucket.org/salishsea/xios-2 XIOS-2
    hg clone ssh://hg@bitbucket.org/salishsea/xios-arch XIOS-ARCH
    hg clone ssh://hg@bitbucket.org/salishsea/ss-run-sets SS-run-sets
    hg clone ssh://hg@bitbucket.org/salishsea/grid
    hg clone ssh://hg@bitbucket.org/salishsea/rivers-climatology
    hg clone ssh://hg@bitbucket.org/salishsea/tracers
    hg clone ssh://hg@bitbucket.org/salishsea/nemo-cmd NEMO-Cmd
    hg clone ssh://hg@bitbucket.org/salishsea/salishseacmd SalishSeaCmd


.. _InstallCommandProcessorPackages:

Install the Command Processor Packages
======================================

Install the :ref:`NEMO-CommandProcessor` and :ref:`SalishSeaCmdProcessor` Python packages:

.. code-block:: bash

    cd $PROJECT/$USER/MEOPAR/
    python3.7 -m pip install --user --editable NEMO-Cmd
    python3.7 -m pip install --user --editable SalishSeaCmd


.. _CompileXIOS-computecanada:

Compile XIOS-2
==============

Please see the :ref:`moaddocs:XIOS-2-docs` section of the :ref:`UBC-EOAS-MOAD-docs`.


.. _CompileNEMO-3.6-computecanada:

Compile NEMO-3.6
================

Compile the :kbd:`SalishSeaCast` NEMO configuration and link it to XIOS-2, and compile the :program:`rebuild_nemo` tool.
The NEMO ARCH files use the :envvar:`XIOS_HOME` environment variable to find the XIOS-2 library you built above.
:envvar:`XIOS_HOME` *must* be an absolute path to your XIOS-2 clone directory.
You can set :envvar:`XIOS_HOME` on the command-line before the :command:`makenemo` and :command:`maketools` commands as shown below,
or you can set and export the value of :envvar:`XIOS_HOME` in your :file:`$HOME/.bashrc` file.


:kbd:`beluga`
-------------

.. code-block:: bash

    cd $PROJECT/$USER/MEOPAR/NEMO-3.6-code/NEMOGCM/CONFIG
    XIOS_HOME=$PROJECT/$USER/MEOPAR/XIOS-2/ ./makenemo -n SalishSeaCast -m X64_BELUGA -j 8
    cd $PROJECT/$USER/MEOPAR/NEMO-3.6-code/NEMOGCM/TOOLS
    XIOS_HOME=$PROJECT/$USER/MEOPAR/XIOS-2/ ./maketools -n REBUILD_NEMO -m X64_BELUGA


:kbd:`cedar`
------------

.. code-block:: bash

    cd $PROJECT/$USER/MEOPAR/NEMO-3.6-code/NEMOGCM/CONFIG
    XIOS_HOME=$PROJECT/$USER/MEOPAR/XIOS-2/ ./makenemo -n SalishSeaCast -m X64_CEDAR -j 8
    cd $PROJECT/$USER/MEOPAR/NEMO-3.6-code/NEMOGCM/TOOLS
    XIOS_HOME=$PROJECT/$USER/MEOPAR/XIOS-2/ ./maketools -n REBUILD_NEMO -m X64_CEDAR


:kbd:`graham`
-------------

.. code-block:: bash

    cd $PROJECT/$USER/MEOPAR/NEMO-3.6-code/NEMOGCM/CONFIG
    XIOS_HOME=$PROJECT/$USER/MEOPAR/XIOS-2/ ./makenemo -n SalishSeaCast -m X64_GRAHAM -j 8
    cd $PROJECT/$USER/MEOPAR/NEMO-3.6-code/NEMOGCM/TOOLS
    XIOS_HOME=$PROJECT/$USER/MEOPAR/XIOS-2/ ./maketools -n REBUILD_NEMO -m X64_GRAHAM


To build a configuration other than :kbd:`SalishSeaCast`, replace :kbd:`SalishSeaCast` with the name of the configuration to be built, e.g. :kbd:`SMELT`:

.. code-block:: bash

    cd $PROJECT/$USER/MEOPAR/NEMO-3.6-code/NEMOGCM/CONFIG
    XIOS_HOME=$PROJECT/$USER/MEOPAR/XIOS-2/ ./makenemo -n SMELT -m X64_CEDAR -j 8


Prepare and Execute Runs
========================

The :file:`SS-run-sets/v201702/` directory in the :ref:`SS-run-sets-SalishSea` repo contains version controlled sample run description files and namelist segment files.
In your own directory in that repo copy, edit,
and version control those files to define the runs that you want to execute.

The run description file is described in the :ref:`RunDescriptionFileStructure` section of the :ref:`project tools documentation <SalishSeaToolsDocs>`.
The namelists are described in the `NEMO-3.6 Book`_.

.. _NEMO-3.6 Book: https://www.nemo-ocean.eu/wp-content/uploads/NEMO_book.pdf

Please see the sections below for details of using forcing climatology and shared daily forcing files in your runs,
and examples of run description and namelist file sections.

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

:command:`salishsea run` has a number of command-line option flags that are useful for controlling details of how runs are executed,
and for debugging your YAML files and the symlinks in the temporary run directory.
Please see :command:`salishsea help run` or the :ref:`SalishSeaCmd package docs <salishseacmd:salishsea-run>`.

You can use the batch job number with :command:`squeue --job` and :command:`sacct --job` to monitor the execution status of your job.

When the job completes the results should have been gathered in the directory you specified in the :command:`salishsea run` command and the temporary run directory should have been deleted.

To view and analyze the run results copy them to your EOAS :file:`/data/$USER/results/` workspace with :program:`scp`, :program:`sftp` or :program:`rsync`.


Forcing Climatology and Daily Files
===================================

Model runs use a mixture of climatologies and daily forcing from other operational models or observations:

* Atmospheric forcing is almost always from the Environment and Climate Change Canada (ECCC) `High Resolution Deterministic Prediction System`_ (HRDPS) model hourly forecasts.

.. _High Resolution Deterministic Prediction System: https://weather.gc.ca/grib/grib2_HRDPS_HR_e.html

* Tides are,
  by definition,
  climatological.

* Most of the river run-offs are climatological,
  but daily average discharge and turbidity for the Fraser River may also be used.

* Tracers at the northern boundary in Johnstone Strait are climatological.
  At the western boundary at the mouth of the Juan de Fuca Strait we have hourly tracer fields from the University of Washington `LiveOcean model`_ since 4-Feb-2017 as well as climatologies.

  .. _LiveOcean model: https://faculty.washington.edu/pmacc/LO/LiveOcean.html
