.. _WorkingOnComputeCanada:

******************************************
Working on :kbd:`computecanada`: NEMO v3.6
******************************************

This section describes the steps to set up and run the Salish Sea NEMO version 3.6 code on the new compute canada machines `graham.computecanada.ca`_ and `cedar.computecanada.ca`_.

This guide assumes that your :ref:`WorkingEnvironment` is set up and that you are familiar with :ref:`WorkingOnSalish`.

.. _graham.computecanada.ca: https://docs.computecanada.ca/wiki/Graham
.. _cedar.computecanada.ca: https://docs.computecanada.ca/wiki/Cedar

Modules setup
=============

When working on the Compute Canada clusters, the :command:`module load` command must be used to load extra software components.
The required modules (and their names) typically vary from cluster to cluster.

You can manually load the modules each time you log in,
or you can add the lines to your :file:`.bashrc` file so that they are automatically loaded upon login.

Lastly, you need to modify your search path such that your shell can find python scripts installed with :kbd:`pip --user`.
Change the :kbd:`lpath` line in the :kbd:`modify search path` section of :file:`.bash_profile` to include :file:`$HOME/.local/bin` and :file:`$HOME/bin` in your search path:

.. code-block:: bash

    lpath=$HOME/.local/bin:$HOME/bin

At present, both Graham and Cedar are configured similarly. The modules needed are:

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

.. code-block:: bash

    mkdir -p $HOME/MEOPAR/SalishSea/results

Clone the repos needed to run the model:

.. code-block:: bash

    cd $HOME/MEOPAR
    hg clone ssh://hg@bitbucket.org/salishsea/nemo-3.6-code NEMO-3.6-code
    hg clone ssh://hg@bitbucket.org/salishsea/xios XIOS
    hg clone ssh://hg@bitbucket.org/salishsea/xios-arch XIOS-ARCH
    hg clone ssh://hg@bitbucket.org/salishsea/nemo-forcing NEMO-forcing
    hg clone ssh://hg@bitbucket.org/salishsea/ss-run-sets SS-run-sets
    hg clone ssh://hg@bitbucket.org/salishsea/tools
    hg clone ssh://hg@bitbucket.org/salishsea/nemo-cmd NEMO-Cmd
    hg clone ssh://hg@bitbucket.org/salishsea/salishseacmd SalishSeaCmd

There is no need to clone the :file:`docs` or :file:`analysis` repos at WestGrid.


Install Tools and Command Processor Packages
============================================

Install the :ref:`SalishSeaToolsPackage` and :ref:`SalishSeaCmdProcessor` Python packages:

.. code-block:: bash

    mkdir -p $HOME/.local
    cd $HOME/MEOPAR/
    pip install --user --editable tools/SalishSeaTools
    pip install --user --editable NEMO-Cmd
    pip install --user --editable SalishSeaCmd


.. _CompileXIOS-computecanada:

Compile XIOS
============

First symlink the XIOS build configuration files for the machine that you are working on from the :file:`XIOS-ARCH` repo clone into the :file:`XIOS/arch/` directory, then compile XIOS:

:kbd:`graham`:
---------------

.. code-block:: bash

    cd $HOME/MEOPAR/XIOS/arch
    ln -sf $HOME/MEOPAR/XIOS-ARCH/WESTGRID/arch-X64_GRAHAM.env
    ln -sf $HOME/MEOPAR/XIOS-ARCH/WESTGRID/arch-X64_GRAHAM.fcm
    ln -sf $HOME/MEOPAR/XIOS-ARCH/WESTGRID/arch-X64_GRAHAM.path
    cd $HOME/MEOPAR/XIOS
    ./make_xios --arch X64_GRAHAM --netcdf_lib netcdf4_par --job 8

:kbd:`cedar`:
--------------

.. code-block:: bash

    cd $HOME/MEOPAR/XIOS/arch
    ln -sf $HOME/MEOPAR/XIOS-ARCH/WESTGRID/arch-X64_CEDAR.env
    ln -sf $HOME/MEOPAR/XIOS-ARCH/WESTGRID/arch-X64_CEDAR.fcm
    ln -sf $HOME/MEOPAR/XIOS-ARCH/WESTGRID/arch-X64_CEDAR.path
    cd $HOME/MEOPAR/XIOS
    ./make_xios --arch X64_CEDAR --netcdf_lib netcdf4_seq --job 8

Compile NEMO-3.6
================

Compile the Salish Sea NEMO configuration and the :program:`rebuild_nemo` tool:

:kbd:`graham`:
--------------

.. code-block:: bash

    cd $HOME/MEOPAR/NEMO-3.6-code/NEMOGCM/CONFIG
    ./makenemo -n SalishSea -m X64_GRAHAM -j 8
    cd $HOME/MEOPAR/NEMO-3.6-code/NEMOGCM/TOOLS
    ./maketools -n REBUILD_NEMO -m X64_GRAHAM

:kbd:`cedar`:
---------------

.. code-block:: bash

    cd $HOME/MEOPAR/NEMO-3.6-code/NEMOGCM/CONFIG
    ./makenemo -n SalishSea -m X64_CEDAR -j 8
    cd $HOME/MEOPAR/NEMO-3.6-code/NEMOGCM/TOOLS
    ./maketools -n REBUILD_NEMO -m X64_CEDAR


To build a configuration other than :kbd:`SalishSea`, replace :kbd:`SalishSea` with the name of the configuration to be built, e.g. :kbd:`SOG`:

.. code-block:: bash

    cd $HOME/MEOPAR/NEMO-3.6-code/NEMOGCM/CONFIG
    ./makenemo -n SOG -m X64_CEDAR -j 8


Prepare and Execute Runs
========================

The :file:`SS-run-sets/nemo3.6` :ref:`SS-run-sets-SalishSea` contains version controlled sample run description files and namelist segment files.
In your own directory in that repo copy, edit,
and version control those files to define the runs that you want to execute.

The run description file is described in the :ref:`RunDescriptionFileStructure` section of the :ref:`project tools documentation <SalishSeaToolsDocs>`.
The namelists are described in the `NEMO-3.6 Book`_.

.. _NEMO-3.6 Book: https://www.nemo-ocean.eu/wp-content/uploads/NEMO_book.pdf

Use :program:`salishsea` :ref:`salishsea-run` to prepare,
execute,
and gather the results for a run:

.. code-block:: bash

    salishsea run SalishSea.yaml $HOME/MEOPAR/SalishSea/results/my_excellent_results

:command:`salishsea run` returns the relative path and name of the temporary run directory,
and the job identifier assigned by the queue manager,
something like:

.. code-block:: bash

    salishsea_cmd.prepare INFO: Created run directory /home/dlatorne/MEOPAR/SalishSea/a90d391c-0e1e-11e4-aa4e-6431504adba6
    salishsea_cmd.run INFO: 3544250.orca2.ibb

You can use the job identifier with :program:`qstat`,
:program:`showstart`,
and :program:`checkjob` to monitor the execution status of your job.

When the job completes the results should have been gathered in the directory you specified in the :command:`salishsea run` command and the temporary run directory should have been deleted.

To view and analyze the run results copy them to your :file:`/ocean/` workspace with :program:`scp`, :program:`sftp` or :program:`rsync`.
