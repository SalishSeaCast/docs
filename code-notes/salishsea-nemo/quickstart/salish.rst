.. _WorkingOnSalish:

************************************
Working on :kbd:`salish` : NEMO v3.6
************************************

This section describes *very* briefly the steps to set up and run the NEMO version 3.6 code on out group's development machine,
:kbd:`salish`.
To set up NEMO version 3.4,
go :ref:`here<SalishNEMO34>`.
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
clone the :ref:`NEMO-3.6-code-repo`,
:ref:`XIOS-repo`,
:ref:`NEMO-forcing-repo`,
and :ref:`SS-run-sets-repo` repos into your workspace on :file:`/data/`:

.. code-block:: bash

    cd /data/$USER/MEOPAR/
    hg clone ssh://hg@bitbucket.org/salishsea/nemo-3.6-code NEMO-3.6-code
    hg clone ssh://hg@bitbucket.org/salishsea/xios XIOS
    hg clone ssh://hg@bitbucket.org/salishsea/xios-arch XIOS-ARCH
    hg clone ssh://hg@bitbucket.org/salishsea/nemo-forcing NEMO-forcing
    hg clone ssh://hg@bitbucket.org/salishsea/ss-run-sets SS-run-sets


Compile XIOS
============

Symlink the XIOS build configuration files for :kbd:`salish` from the :file:`XIOS-ARCH` repo clone into the :file:`XIOS/arch/` directory:

.. code-block:: bash

    cd /data/$USER/MEOPAR/XIOS/arch
    ln -sf /data/$USER/MEOPAR/XIOS-ARCH/UBC-EOAS/arch-GCC_SALISH.fcm
    ln -sf /data/$USER/MEOPAR/XIOS-ARCH/UBC-EOAS/arch-GCC_SALISH.path

Compile the XIOS input/output server:

.. code-block:: bash

    cd /data/$USER/MEOPAR/XIOS
    ./make_xios --arch GCC_SALISH --netcdf_lib netcdf4_seq --job 8

.. note::
    If you have chosen to store XIOS on :file:`ocean`
    (i.e. :file:`/ocean/$USER/MEOPAR/XIOS` )
    then you need to create a symbolic link to :file:`/data/$USER/MEOPAR/XIOS`:

    .. code-block:: bash

        cd /data/$USER/MEOPAR
        ln -s /ocean/$USER/MEOPAR/XIOS

    This is because the NEMO code attempts to find XIOS in :file:`/data/$USER/MEOPAR/XIOS`.
    Alternatively,
    you can use the NEMO :file:`GCC_SALISH_ocean` arch file,
    which will look for XIOS on :file:`ocean`.

    If you have XIOS stored under :file:`/data/$USER/MEOPAR`,
    you don't have to worry about this.


Compile NEMO-3.6
================

Compile and link the Salish Sea NEMO configuration and the XIOS server with the :kbd:`salish` architecture definitions,
distributing the compilation over 8 cores.

.. code-block:: bash

    cd NEMO-3.6-code/NEMOGCM/CONFIG
    ./makenemo -n SalishSea -m GCC_SALISH -j8

The resulting executables are located in :file:`NEMO-3.6-code/NEMOGCM/CONFIG/SalishSea/BLD/bin/`.

Compile and link the :program:`rebuild_nemo` tool:

.. code-block:: bash

    cd NEMO-3.6-code/NEMOGCM/TOOLS
    ./maketools -m GCC_SALISH -n REBUILD_NEMO

See :ref:`rebuild-nemo-tool` for more information about it.


.. _PrepareRun:

Prepare and Execute Runs
========================

The :file:`SS-run-sets/` :ref:`SS-run-sets-SalishSea` contains version controlled sample run description files and namelist segment files.
Create your own directory in that repo where you can copy,
edit,
and version control those files to define the run that you want to execute.

The run description file is described in the :ref:`RunDescriptionFileStructure` section of the :ref:`project tools documentation <SalishSeaToolsDocs>`.
The namelists are described in the `NEMO-3.6 Book`_.

.. _NEMO-3.6 Book: http://www.nemo-ocean.eu/content/download/178055/725078/file/NEMO_book_V36stable.pdf

Use :program:`salishsea` :ref:`salishsea-run` to prepare,
execute,
and gather the results for a run:

.. code-block:: bash

    salishsea run SalishSea.yaml iodef.xml /data/$USER/MEOPAR/SalishSea/results/my_excellent_results

:command:`salishsea run` returns the relative path and name of the temporary run directory,
and the job identifier assigned by the queue manager,
something like:

.. code-block:: bash

    salishsea_cmd.prepare INFO: Created run directory ../../SalishSea/38e87e0c-472d-11e3-9c8e-0025909a8461
    salishsea_cmd.run INFO: 57.master


You can use the :program:`qstat` command to monitor the execution status of your job.

A convenient command to monitor the memory use of a run and its time step progress is:

.. code-block:: bash

    watch -n 5 "(free -m; cat time.step)"

When the job completes the results should have been gathered in the directory you specified in the :command:`salishsea run` command and the temporary run directory should have been deleted.


Look at the Results
===================

A number of notebooks that look at NetCDF files are available in :file:`tools/analysis_tools/`.
To start these,
go to the top level directory of the :file:`analysis` repo on your local machine
(not on :kbd:`salish`) and type:

.. code-block:: bash

    jupyter notebook

At this point lots of information will appear in your terminal.
This terminal session is now running a server and cannot be used for anything else until you are finished with the notebooks.
At that point you need to CTRL-C to get out.

At the same time a window should open in your browser.
If it doesn't,
look at your terminal,
find the ip address, something like:

.. code-block:: bash

    The Jupyter Notebook is running at: http://127.0.0.1:8888/

and put that UTL into your browser.
From this initial window you can open the notebooks in :file:`analysis_tools` directory and look around.
The links to the various files will probably not work.
Change them to point to your file space.
You will probably want to build your own notebook but these notebooks give you lots of examples to copy from.


Profiling with the GNU Profiler
===============================

The GNU profiler allows you to find out which parts of the code are taking the longest to run.

1. Compile the code with the -pg flag.

This requires adding -pg to the two lines in your arch file that start with %FCFLAGS and %LDFLAGS (as in the following excerpt from :file:`NEMO-3.6-code/NEMOGCM/ARCH/UBC_EOAS/arch-GCC_SALISH_ocean_gprof.fcm`):

.. code-block:: bash

    %XIOS_HOME           /ocean/$USER/MEOPAR/XIOS

    %NCDF_INC            -I/usr/include
    %NCDF_LIB            -L/usr/lib -lnetcdff -lnetcdf

    %XIOS_INC            -I%XIOS_HOME/inc
    %XIOS_LIB            -L%XIOS_HOME/lib -lxios -lstdc++

    %CPP	             cpp
    %FC                  mpif90
    %FCFLAGS             -cpp -O3 -fdefault-real-8 -funroll-all-loops -fcray-pointer -ffree-line-length-none -pg
    %FFLAGS              %FCFLAGS
    %LD                  mpif90
    %LDFLAGS             -lstdc++ -pg
    %FPPFLAGS            -P -C -traditional
    %AR                  ar
    %ARFLAGS             -rs
    %MK                  make
    %USER_INC            %XIOS_INC %NCDF_INC
    %USER_LIB            %XIOS_LIB %NCDF_LIB

Using the modified arch file, compile your NEMO configuration, e.g.:

.. code-block:: bash

    ./makenemo -n SalishSea -m GCC_SALISH_ocean_gprof


2. Run the model as usual from your prepared run directory.
\*Not tested with the salishsea run command.

.. code-block:: bash

    mpirun -n 7 ./nemo.exe : -n 1 ./xios_server.exe > stdout 2>stderr &

A file called gmon.out will be created in your run directory.


3. Use the :program:`gprof` command with the executable name and gmon.out as input to create a readable summary of the timing output. Redirect the output to a text file to save it; you can then view this file using :program:`less`.

.. code-block:: bash

    gprof nemo.exe gmon.out > gprof_out.txt
    less gprof_out.txt

For more information, see http://www.ibm.com/developerworks/library/l-gnuprof.html
