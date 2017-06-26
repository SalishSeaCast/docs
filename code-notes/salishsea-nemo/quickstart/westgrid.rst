.. _WorkingOnWestGrid:

*************************************
Working on :kbd:`westgrid`: NEMO v3.6
*************************************

This section describes the steps to set up and run the Salish Sea NEMO version 3.6 code on WestGrid machines `bugaboo.westgrid.ca`_, `jasper.westgrid.ca`_, `nestor.westgrid.ca`_ and `orcinus.westgrid.ca`_.

This guide assumes that your :ref:`WorkingEnvironment` is set up and that you are familiar with :ref:`WorkingOnSalish`.

.. _bugaboo.westgrid.ca: https://www.westgrid.ca/support/systems/bugaboo
.. _jasper.westgrid.ca: https://www.westgrid.ca/support/systems/jasper
.. _nestor.westgrid.ca: https://www.westgrid.ca/support/systems/hermesnestor
.. _orcinus.westgrid.ca: https://www.westgrid.ca/support/systems/orcinus


Modules setup
=============

When working on WestGrid clusters the :command:`module load` command must be used to load extra software components.
The required modules (and their names) vary from cluster to cluster.

You can manually load the modules each time you log in,
or you can add the lines to your :file:`.bashrc` file so that they are automatically loaded upon login.

Lastly, you need to modify your search path such that your shell can find python scripts installed with :kbd:`pip --user`.
Change the :kbd:`lpath` line in the :kbd:`modify search path` section of :file:`.bash_profile` to include :file:`$HOME/.local/bin` and :file:`$HOME/bin` in your search path:

.. code-block:: bash

    lpath=$HOME/.local/bin:$HOME/bin

The modules needed for each cluster are:

:kbd:`bugaboo`:
---------------

.. code-block:: bash

    module load python
    module load intel/15.0.2

:kbd:`jasper`:
--------------

.. code-block:: bash

    module load application/python/2.7.8
    module load compiler/intel/13.0.1
    module load library/hdf5/1.8.9
    module load library/netcdf/4.1.3

:kbd:`nestor`:
--------------

.. code-block:: bash

    module load python
    module load gcc/5.1.0
    module load intel/12.0.2.137

:kbd:`orcinus`:
---------------

.. code-block:: bash

    module load python
    module load intel/14.0.2
    module load intel/14.0/netcdf-4.3.3.1_mpi
    module load intel/14.0/netcdf-fortran-4.4.0_mpi
    module load intel/14.0/hdf5-1.8.15p1_mpi
    module load intel/14.0/nco-4.5.2


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


.. _CompileXIOS-westgrid:

Compile XIOS
============

First symlink the XIOS build configuration files for the machine that you are working on from the :file:`XIOS-ARCH` repo clone into the :file:`XIOS/arch/` directory, then compile XIOS:

:kbd:`bugaboo`:
---------------

.. code-block:: bash

    cd $HOME/MEOPAR/XIOS/arch
    ln -sf $HOME/MEOPAR/XIOS-ARCH/WESTGRID/arch-X64_BUGABOO.env
    ln -sf $HOME/MEOPAR/XIOS-ARCH/WESTGRID/arch-X64_BUGABOO.fcm
    ln -sf $HOME/MEOPAR/XIOS-ARCH/WESTGRID/arch-X64_BUGABOO.path
    cd $HOME/MEOPAR/XIOS
    ./make_xios --arch X64_BUGABOO --netcdf_lib netcdf4_par --job 8

:kbd:`jasper`:
--------------

.. code-block:: bash

    cd $HOME/MEOPAR/XIOS/arch
    ln -sf $HOME/MEOPAR/XIOS-ARCH/WESTGRID/arch-X64_JASPER.env
    ln -sf $HOME/MEOPAR/XIOS-ARCH/WESTGRID/arch-X64_JASPER.fcm
    ln -sf $HOME/MEOPAR/XIOS-ARCH/WESTGRID/arch-X64_JASPER.path
    cd $HOME/MEOPAR/XIOS
    ./make_xios --arch X64_JASPER --netcdf_lib netcdf4_seq --job 8

:kbd:`nestor`:
--------------

.. code-block:: bash

    cd $HOME/MEOPAR/XIOS/arch
    ln -sf $HOME/MEOPAR/XIOS-ARCH/WESTGRID/arch-X64_NESTOR.env
    ln -sf $HOME/MEOPAR/XIOS-ARCH/WESTGRID/arch-X64_NESTOR.fcm
    ln -sf $HOME/MEOPAR/XIOS-ARCH/WESTGRID/arch-X64_NESTOR.path
    cd $HOME/MEOPAR/XIOS
    ./make_xios --arch X64_NESTOR --netcdf_lib netcdf4_seq --job 8

:kbd:`orcinus`:
---------------

.. code-block:: bash

    cd $HOME/MEOPAR/XIOS/arch
    ln -sf $HOME/MEOPAR/XIOS-ARCH/WESTGRID/arch-X64_ORCINUS.env
    ln -sf $HOME/MEOPAR/XIOS-ARCH/WESTGRID/arch-X64_ORCINUS.fcm
    ln -sf $HOME/MEOPAR/XIOS-ARCH/WESTGRID/arch-X64_ORCINUS.path
    cd $HOME/MEOPAR/XIOS
    ./make_xios --arch X64_ORCINUS --netcdf_lib netcdf4_par --job 8


Compile NEMO-3.6
================

Compile the Salish Sea NEMO configuration and the :program:`rebuild_nemo` tool:

:kbd:`bugaboo`:
---------------

.. code-block:: bash

    cd $HOME/MEOPAR/NEMO-3.6-code/NEMOGCM/CONFIG
    ./makenemo -n SalishSea -m X64_BUGABOO -j 8
    cd $HOME/MEOPAR/NEMO-3.6-code/NEMOGCM/TOOLS
    ./maketools -n REBUILD_NEMO -m X64_BUGABOO

:kbd:`jasper`:
--------------

.. code-block:: bash

    cd $HOME/MEOPAR/NEMO-3.6-code/NEMOGCM/CONFIG
    ./makenemo -n SalishSea -m X64_JASPER -j 8
    cd $HOME/MEOPAR/NEMO-3.6-code/NEMOGCM/TOOLS
    ./maketools -n REBUILD_NEMO -m X64_JASPER

:kbd:`nestor`:
--------------

.. code-block:: bash

    cd $HOME/MEOPAR/NEMO-3.6-code/NEMOGCM/CONFIG
    ./makenemo -n SalishSea -m X64_NESTOR -j 8
    cd $HOME/MEOPAR/NEMO-3.6-code/NEMOGCM/TOOLS
    ./maketools -n REBUILD_NEMO -m X64_NESTOR

:kbd:`orcinus`:
---------------

.. code-block:: bash

    cd $HOME/MEOPAR/NEMO-3.6-code/NEMOGCM/CONFIG
    ./makenemo -n SalishSea -m X64_ORCINUS -j 8
    cd $HOME/MEOPAR/NEMO-3.6-code/NEMOGCM/TOOLS
    ./maketools -n REBUILD_NEMO -m X64_ORCINUS

To build a configuration other than :kbd:`SalishSea`, replace :kbd:`SalishSea` with the name of the configuration to be built, e.g. :kbd:`SOG`:

.. code-block:: bash

    cd $HOME/MEOPAR/NEMO-3.6-code/NEMOGCM/CONFIG
    ./makenemo -n SOG -m X64_ORCINUS -j 8


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
