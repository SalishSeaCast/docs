.. _WorkingOnComputeCanada:

********************************
Working on ``graham``: NEMO v3.6
********************************

This section describes the steps to set up and run the SalishSeaCast NEMO version 3.6 code on the 
Digital Research Alliance of Canada `graham.computecanada.ca`_ HPC cluster.

.. _graham.computecanada.ca: https://docs.alliancecan.ca/wiki/Graham

The process described here should be applicable with little or no change to the other
`Alliance HPC clusters`_.

.. _Alliance HPC clusters: https://docs.alliancecan.ca/wiki/Getting_started#What_resources_are_available?

Before going through the process described here,
you should have completed the steps in :ref:`moaddocs:InitialSetupOnGraham` 


Modules setup
=============

Alliance clusters use the :command:`module load` command to load software components.
On ``graham`` the module loads that are required to build and run NEMO are:

.. code-block:: bash

    module load netcdf-fortran-mpi/4.6.0
    module load perl/5.30.2

You can manually load the modules each time you log in,
or you can add the above lines to your :file:`$HOME/.bashrc` file so that they are
automatically loaded upon login.


.. _CreateWorkspaceAndCloneRepositories:

Create a Workspace and Clone the Repositories
=============================================

``graham`` provides `several different types of file storage`_.
We use project space for our working environments because it is large,
high performance,
and backed up.
Scratch space is even larger,
also high performance,
but not backed up,
so we use that as the space to execute NEMO runs in.
Scratch space is purged monthly to delete files that have not been accessed in the preceding 60 days.
So,
if you have run results that want to preserve longer than that
(e.g. results that are used in a your thesis or a publication),
you should move them to project space or to MOAD storage at UBC.

.. _several different types of file storage: https://docs.alliancecan.ca/wiki/Storage_and_file_management

We use environment variables to access your project and scratch spaces:

* Your project space is at :file:`$PROJECT/$USER/`
* Your scratch space is at :file:`$SCRATCH/`
* Daily atmospheric,
  river,
  and west boundary forcing files are in the :file:`$PROJECT/SalishSea/forcing/` tree

:envvar:`PROJECT` and :envvar:`SCRATCH` are more convenient that remembering full paths,
and using them in run description YAML files makes those files reusable by other users.
The :envvar:`USER` variable always evaluates to your userid.

Create a :file:`MEOPAR/` directory trees in your project and scratch spaces:

.. code-block:: bash

    mkdir -p $PROJECT/$USER/MEOPAR
    mkdir -p $SCRATCH/MEOPAR

Clone the repos needed to run the model:

.. code-block:: bash

    cd $PROJECT/$USER/MEOPAR
    git clone git@github.com:SalishSeaCast/grid.git
    git clone git@github.com:SalishSeaCast/NEMO-Cmd.git
    git clone git@github.com:SalishSeaCast/SalishSeaCmd.git
    git clone git@github.com:SalishSeaCast/SS-run-sets.git
    git clone git@github.com:SalishSeaCast/tides.git
    git clone git@github.com:SalishSeaCast/tracers.git
    git clone git@github.com:SalishSeaCast/rivers-climatology.git
    git clone git@github.com:SalishSeaCast/NEMO-3.6-code.git
    git clone git@github.com:SalishSeaCast/XIOS-ARCH.git
    git clone git@github.com:SalishSeaCast/XIOS-2.git


.. _InstallCommandProcessorPackages:

Install the Command Processor Packages
======================================

Download and install the Miniforge distribution of :program:`conda`:

.. code-block:: bash

    wget https://github.com/conda-forge/miniforge/releases/latest/download/Miniforge3-Linux-x86_64.sh
    bash Miniforge3-Linux-x86_64.sh

Accept the defaults offered for all of the settings.
Exit your terminal session on ``graham`` with :command:`exit` and start a new session to ensure that
the Miniforge configuration takes effect and the :command:`conda` command is available.

Create a ``salishsea-cmd`` conda environment:

.. code-block:: bash

    cd $PROJECT/$USER/MEOPAR/
    conda env create -f SalishSeaCmd/envs/environment-hpc.yaml

Install the :ref:`NEMO-CommandProcessor` and :ref:`SalishSeaCmdProcessor` Python packages:

.. code-block:: bash

    conda activate salishsea-cmd
    python3 -m pip install --user --editable NEMO-Cmd
    python3 -m pip install --user --editable SalishSeaCmd

Confirm that the :ref:`SalishSeaCmdProcessor` works in your base environment
(i.e. without the ``salishsea-cmd`` environment activated):

.. code-block:: bash

    conda deactivate
    salishsea --help

You should see output like:

.. code-block:: text

    usage: salishsea [--version] [-v | -q] [--log-file LOG_FILE] [-h] [--debug]

    SalishSeaCast NEMO Command Processor

    options:
    --version             show program's version number and exit
    -v, --verbose         Increase verbosity of output. Can be repeated.
    -q, --quiet           Suppress output except warnings and errors.
    --log-file LOG_FILE
                            Specify a file to log output. Disabled by default.
    -h, --help            Show help message and exit.
    --debug               Show tracebacks on errors.

    Commands:
    combine  Combine per-processor files from an MPI NEMO run into single files (NEMO-Cmd)
    complete  print bash completion command (cliff)
    deflate  Deflate variables in netCDF files using Lempel-Ziv compression. (NEMO-Cmd)
    gather  Gather results from a NEMO run. (NEMO-Cmd)
    help  print detailed help for another command (cliff)
    prepare  Prepare a SalishSeaCast NEMO run.
    run  Prepare, execute, and gather results from a SalishSeaCast NEMO model run.
    split-results  Split the results of a multi-day SalishSeaCast NEMO model run (e.g. a hindcast run)


Compile XIOS-2
==============

Please see the :ref:`moaddocs:XIOS-2-docs` section of the :ref:`UBC-EOAS-MOAD-docs`.


.. _CompileNEMO-3.6-graham:

Compile NEMO-3.6
================

Compile the ``SalishSeaCast`` NEMO configuration and link it to XIOS-2, 
and compile the :program:`rebuild_nemo` tool.
The NEMO ARCH files use the :envvar:`XIOS_HOME` environment variable to find the XIOS-2 library 
you built above.
:envvar:`XIOS_HOME` *must* be an absolute path to your XIOS-2 clone directory.
You can set :envvar:`XIOS_HOME` on the command-line before the :command:`makenemo` 
and :command:`maketools` commands as shown below,
or you can set and export the value of :envvar:`XIOS_HOME` in your :file:`$HOME/.bashrc` file.

.. code-block:: bash

    cd $PROJECT/$USER/MEOPAR/NEMO-3.6-code/NEMOGCM/CONFIG
    XIOS_HOME=$PROJECT/$USER/MEOPAR/XIOS-2/ ./makenemo -n SalishSeaCast -m X64_GRAHAM -j 8
    cd $PROJECT/$USER/MEOPAR/NEMO-3.6-code/NEMOGCM/TOOLS
    XIOS_HOME=$PROJECT/$USER/MEOPAR/XIOS-2/ ./maketools -n REBUILD_NEMO -m X64_GRAHAM

It typically takes about 3.5 minutes to build a NEMO configuration on ``graham``,
and about 15 seconds to build ``REBUILD_NEMO``.

To build a configuration other than ``SalishSeaCast``, 
replace ``SalishSeaCast`` with the name of the configuration to be built, e.g. ``SMELT``:

.. code-block:: bash

    cd $PROJECT/$USER/MEOPAR/NEMO-3.6-code/NEMOGCM/CONFIG
    XIOS_HOME=$PROJECT/$USER/MEOPAR/XIOS-2/ ./makenemo -n SMELT -m X64_GRAHAM -j 8


If you need to do a clean build of a NEMO configuration,
you can use:

.. code-block:: bash

    cd $PROJECT/$USER/MEOPAR/NEMO-3.6-code/NEMOGCM/CONFIG
    ./makenemo -n SalishSeaCast clean
    XIOS_HOME=$PROJECT/$USER/MEOPAR/XIOS-2/ ./makenemo -n SalishSeaCast -m X64_GRAHAM -j 8

to clear away all artifacts of the previous build and do a fresh one.
To clean and rebuild a different configuration,
replace ``SalishSeaCast`` with the name of the configuration.


Prepare and Execute Runs
========================

The :file:`SS-run-sets/v202111/` directory in the :ref:`SS-run-sets-SalishSea` repo contains 
version controlled sample run description files and namelist segment files.
In your own directory in that repo copy, edit,
and version control those files to define the runs that you want to execute.

The run description file is described in the :ref:`salishseacmd:RunDescriptionFileStructure` section
of the :ref:`salishseacmd:SalishSeaCmdProcessor` documentation.
The namelists are described in the `NEMO-3.6 Book`_.

.. _NEMO-3.6 Book: https://zenodo.org/record/3248739

Please see the sections below for details of using forcing climatology and 
shared daily forcing files in your runs,
and examples of run description and namelist file sections.

Use :program:`salishsea` :ref:`salishseacmd:salishsea-run` to prepare,
execute,
and gather the results for a run:

.. code-block:: bash

    salishsea run SalishSea.yaml $SCRATCH/MEOPAR/my_excellent_results

:command:`salishsea run` returns the path and name of the temporary run directory,
and the job identifier assigned by the queue manager,
something like:

.. code-block:: text

    salishsea_cmd.run INFO: Created run directory /scratch/dlatorne/20mar17nowcast16x34_2017-10-06T101548.694389-0700
    salishsea_cmd.run INFO: Submitted batch job 1578481

:command:`salishsea run` has a number of command-line option flags that are useful for controlling 
details of how runs are executed,
and for debugging your YAML files and the symlinks in the temporary run directory.
Please see :command:`salishsea help run` or the 
:ref:`SalishSeaCmd package docs <salishseacmd:salishsea-run>`.

You can use the batch job number with :command:`squeue --job` and :command:`sacct --job` 
to monitor the execution status of your job.

The command alias:

.. code-block:: bash

    alias sq='squeue -o "%.12i %.8u %.9a %.22j %.2t %.10r %.19S %.10M %.10L %.6D %.5C %P %N"'

provides more informative output from :command:`squeue`.
Add the alias to your :file:`$HOME/.bashrc` file so that it is available in every terminal session.
You can use as:

* :command:`sq -u $USER` to see all of your queued jobs
* :command:`sq -A rrg-allen,def-allen` to see all of the group's queued jobs
* :command:`sq --job job-number`,
  where `job-number` is the batch job number provided in the output of a :command:`salishsea run` 
  command,
  to see the information about a specific job

When the job completes the results should have been gathered in the directory you specified 
in the :command:`salishsea run` command and the temporary run directory should have been deleted.


Forcing Climatology and Daily Files
===================================

Model runs use a mixture of climatology and daily forcing from other operational models 
or observations:

* Atmospheric forcing is almost always from the Environment and Climate Change Canada (ECCC) 
  `High Resolution Deterministic Prediction System`_ (HRDPS) model hourly forecasts.

  .. _High Resolution Deterministic Prediction System: https://weather.gc.ca/grib/grib2_HRDPS_HR_e.html

* Tides are,
  by definition,
  climatological.

* Most of the river run-offs use day-averaged discharges from gauged rivers.
  Turbidity for the Fraser River is also day-averaged buoy observations.
  Temperature and chemistry of the river run-offs are climatological.

* Tracers at the northern boundary in Johnstone Strait are climatological.
  On the western boundary at the mouth of the Juan de Fuca Strait we have hourly tracer fields 
  from the University of Washington `LiveOcean model`_ since 4-Feb-2017
  for temperature,
  salinity,
  and chemistry.
  Biology there is climatological.

  .. _LiveOcean model: https://faculty.washington.edu/pmacc/LO/LiveOcean.html
