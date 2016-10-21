.. _WorkingOnOrcinus:

*************************************
Working on :kbd:`orcinus`: NEMO v3.6
*************************************

This section describes *very* briefly the steps to set up and run the Salish Sea NEMO version 3.6 code on the `orcinus.westgrid.ca`_ HPC cluster.
To set up NEMO version 3.4, go :ref:`here<OrcinusNEMO34>`.
This guide assumes that your :ref:`WorkingEnvironment` is set up,
and that you are familiar with :ref:`WorkingOnSalish`.

.. _orcinus.westgrid.ca: https://www.westgrid.ca/orcinus


Set-up SSH key-forwarding
=========================

See orcinus instructions at the bottom of :ref:`sshConfiguration`.


:file:`.bash_profile` and :file:`.bashrc`
=========================================

:kbd:`orcinus` uses 2 files for :program:`bash` settings: :file:`.bash_profile` and :file:`.bashrc`.
In both files the location at which to add your personal customizations is indicated in comments in the default version of the files.
Environment variables go in :file:`.bash_profile`,
for example:

.. code-block:: bash

    #
    #
    #  Include your own tailored environment below
    #
    # Pager setup
    export PAGER=less
    export LESS=-r

    # Make emacs the default full-screen editor (to ward off vi)
    export EDITOR=emacs
    export VISUAL=emacs

Shell variables,
module loading (see below),
and aliases go in :file:`.bashrc`:

.. code-block:: bash

    #
    #  Include any personal modifications below
    #  modification can include the aliases functions etc.
    #
    # Prompts:
    PS1="\h:\W$ "
    PS2=" > "

    # Modules:
    module load python

    # Aliases:
    alias df="df -h"
    alias du="du -h"
    alias ls="ls --color=auto -F"
    alias la="ls -a"
    alias ll="ls -al"
    alias rm="rm -i"

Please see :ref:`bashConfiguration` for explanations of the above settings.

When working on Westgrid clusters the :command:`module` command must be used to load extra software components.
The required modules vary from cluster to cluster.
On :kbd:`orcinus` only the :kbd:`python` module should be loaded when you log in:

.. code-block:: bash

    module load python

Doing so makes Python,
Mercurial,
and the netCDF4 library available to you.

You can manually load the :kbd:`python` module each time you log in,
or you can add it to your :file:`.bashrc` file (as shown above)
so that it is automatically loaded when you :program:`ssh` into :kbd:`orcinus`.

Change the :kbd:`lpath` line in the :kbd:`modify search path` section of :file:`.bash_profile` to include :file:`$HOME/.local/bin` and :file:`$HOME/bin` in your search path:

.. code-block:: bash

    lpath=$HOME/.local/bin:$HOME/bin


Create a Workspace and Clone the Repos
======================================

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

There is no need to clone the :file:`docs` or :file:`analysis` repos on :kbd:`orcinus`.


Install Tools and Command Processor Packages
============================================

Install the :ref:`SalishSeaToolsPackage` and :ref:`SalishSeaCmdProcessor` Python packages:

.. code-block:: bash

    cd tools
    pip install --user pathlib
    pip install --user -e SalishSeaTools
    pip install --user -e SalishSeaCmd

Load modules
============

Load the following modules (specific to orcinus):

.. code-block:: bash

    module load intel/14.0.2
    module load intel/14.0/netcdf-4.3.3.1
    module load intel/14.0/netcdf-fortran-4.4.0
    module load intel/14.0/hdf5-1.8.15p1

Compile XIOS
============

Symlink the XIOS build configuration files for :kbd:`orcinus` from the :file:`XIOS-ARCH` repo clone into the :file:`XIOS/arch/` directory:

.. code-block:: bash

    cd $HOME/MEOPAR/XIOS/arch
    ln -sf $HOME/MEOPAR/XIOS-ARCH/WESTGRID/arch-X64_ORCINUS.env
    ln -sf $HOME/MEOPAR/XIOS-ARCH/WESTGRID/arch-X64_ORCINUS.fcm
    ln -sf $HOME/MEOPAR/XIOS-ARCH/WESTGRID/arch-X64_ORCINUS.path

Compile XIOS:

.. code-block:: bash

    cd $HOME/MEOPAR/XIOS
    ./make_xios --arch X64_ORCINUS --netcdf_lib netcdf4_seq --job 8


Compile NEMO-3.6
================

Compile the Salish Sea NEMO configuration,
and the :program:`rebuild_nemo` tool:

.. code-block:: bash

    cd $HOME/MEOPAR/NEMO-3.6-code/NEMOGCM/CONFIG
    ./makenemo -n SalishSea -m X64_ORCINUS -j 8
    cd $HOME/MEOPAR/NEMO-3.6-code/NEMOGCM/TOOLS/REBUILD_NEMO
    ./maketools -n REBUILD_NEMO -m X64_ORCINUS

To build a configuration other than :kbd:`SalishSea`, replace :kbd:`SalishSea` with the name of the configuration to be built, e.g. :kbd:`SOG`:

.. code-block:: bash

    cd $HOME/MEOPAR/NEMO-3.6-code/NEMOGCM/CONFIG
    ./makenemo -n SOG -m X64_ORCINUS -j 8

Prepare and Execute Runs
========================

The :file:`SS-run-sets/` :ref:`SS-run-sets-SalishSea` contains version controlled sample run description files and namelist segment files.
In your own directory in that repo copy,
edit,
and version control those files to define the runs that you want to execute.

The run description file is described in the :ref:`RunDescriptionFileStructure` section of the :ref:`project tools documentation <SalishSeaToolsDocs>`.
The namelists are described in the `NEMO-3.6 Book`_.

.. _NEMO-3.6 Book: http://www.nemo-ocean.eu/content/download/178055/725078/file/NEMO_book_V36stable.pdf

Use :program:`salishsea` :ref:`salishsea-run` to prepare,
execute,
and gather the results for a run:

.. code-block:: bash

    salishsea run SalishSea.yaml iodef.xml $HOME/MEOPAR/SalishSea/results/my_excellent_results

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

To view and analyze the run results copy them to your :file:`/ocean/` workspace with :program:`scp` or :program:`sftp`.
