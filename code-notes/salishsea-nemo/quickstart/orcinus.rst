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


Compile XIOS
============

Symlink the XIOS build configuration files for :kbd:`orcinus` from the :file:`XIOS-ARCH` repo clone into the :file:`XIOS/arch/` directory:

.. code-block:: bash

    cd $HOME/MEOPAR/XIOS/arch
    ln -sf $HOME/MEOPAR/XIOS-ARCH/WESTGRID/arch-X64_ORCINUS.env
    ln -sf $HOME/MEOPAR/XIOS-ARCH/WESTGRID/arch-X64_ORCINUS.fcm
    ln -sf $HOME/MEOPAR/XIOS-ARCH/WESTGRID/arch-X64_ORCINUS.path

Compile the XIOS input/output server:

.. code-block:: bash

    cd $HOME/MEOPAR/XIOS
    ./make_xios --arch X64_ORCINUS --netcdf_lib netcdf4_seq --job 8


Compile NEMO-3.6
================

Compile the Salish Sea NEMO configuration,
and the :program:`rebuild_nemo` tool:

.. code-block:: bash

    cd $HOME/MEOPAR/NEMO-3.6-code/NEMOGCM/CONFIG
    source orcinus_build.sh SalishSea
    cd $HOME/MEOPAR/NEMO-3.6-code/NEMOGCM/TOOLS/REBUILD_NEMO
    source orcinus_build.sh

Both invocations of the :file:`orcinus_build.sh` script will produce lots of output that mentions build failures,
but they should end with success messages and show the newly created executables.

To build a configuration other than :kbd:`SalishSea`, replace :kbd:`SalishSea` with the name of the configuration to be built, e.g. :kbd:`SOG`:

.. code-block:: bash

    cd $HOME/MEOPAR/NEMO-3.6-code/NEMOGCM/CONFIG
    source orcinus_build.sh SOG

For :file:`CONFIG/orcinus_build.sh` the output should end something like:

.. code-block:: bash

    mpif90 -o nemo.o -I/home/dlatorne/MEOPAR/NEMO-code/NEMOGCM/CONFIG/SalishSea/BLD/inc -c -fpp -r8 -O3 -assume byterecl -heap-arrays -I/global/software/lib64/intel/ncsa-tools/include /home/dlatorne/MEOPAR/NEMO-code/NEMOGCM/CONFIG/SalishSea/WORK/nemo.f90
    fcm_internal load:F nemo nemo.o nemo.exe
    mpif90 -o server.exe /home/dlatorne/MEOPAR/NEMO-code/NEMOGCM/CONFIG/SalishSea/BLD/obj/server.o -L/home/dlatorne/MEOPAR/NEMO-code/NEMOGCM/CONFIG/SalishSea/BLD/lib -l__fcm__server -shared-intel -lnetcdf -lnetcdff -lhdf5 -lhdf5_hl -lz -lsz
    mpif90 -o nemo.exe /home/dlatorne/MEOPAR/NEMO-code/NEMOGCM/CONFIG/SalishSea/BLD/obj/nemo.o -L/home/dlatorne/MEOPAR/NEMO-code/NEMOGCM/CONFIG/SalishSea/BLD/lib -l__fcm__nemo -shared-intel -lnetcdf -lnetcdff -lhdf5 -lhdf5_hl -lz -lsz
    /global/software/lib64/intel/ncsa-tools/lib/libnetcdf.so: undefined reference to `__intel_cpu_feature_indicator_x'
    /global/software/lib64/intel/ncsa-tools/lib/libnetcdf.so: undefined reference to `__intel_cpu_features_init_x'
    /global/software/lib64/intel/ncsa-tools/lib/libnetcdf.so: undefined reference to `__intel_ssse3_memmove'
    fcm_internal load failed (256)
    make: *** [server.exe] Error 1
    make: *** Waiting for unfinished jobs....
    /global/software/lib64/intel/ncsa-tools/lib/libnetcdf.so: undefined reference to `__intel_cpu_feature_indicator_x'
    /global/software/lib64/intel/ncsa-tools/lib/libnetcdf.so: undefined reference to `__intel_cpu_features_init_x'
    /global/software/lib64/intel/ncsa-tools/lib/libnetcdf.so: undefined reference to `__intel_ssse3_memmove'
    fcm_internal load failed (256)
    make: *** [nemo.exe] Error 1
    make -f /home/dlatorne/MEOPAR/NEMO-code/NEMOGCM/CONFIG/SalishSea/BLD/Makefile -j 8 all failed (2) at /global/home/dlatorne/MEOPAR/NEMO-code/NEMOGCM/EXTERNAL/fcm/bin/../lib/Fcm/Build.pm line 597
    ->Make: 56 seconds
    ->TOTAL: 86 seconds
    Build failed on Mon Aug 18 12:34:21 2014.
    /home/dlatorne/MEOPAR/NEMO-code/NEMOGCM/CONFIG
    /home/dlatorne/MEOPAR/NEMO-code/NEMOGCM/CONFIG/SalishSea
    I/O server build succeeded at Mon Aug 18 12:34:23 PDT 2014
    -rwxr-x--- 1 dlatorne dlatorne 9935884 Aug 18 12:34 /home/dlatorne/MEOPAR/NEMO-code/NEMOGCM/CONFIG/SalishSea/BLD/bin/server.exe*
    NEMO executable build succeeded at Mon Aug 18 12:34:25 PDT 2014
    -rwxr-x--- 1 dlatorne dlatorne 16102827 Aug 18 12:34 /home/dlatorne/MEOPAR/NEMO-code/NEMOGCM/CONFIG/SalishSea/BLD/bin/nemo.exe*
    I/O server executable symlinked in SalishSeaEXP00/ at Mon Aug 18 12:34:25 PDT 2014
    lrwxrwxrwx 1 dlatorne dlatorne 75 Aug 18 12:34 /home/dlatorne/MEOPAR/NEMO-code/NEMOGCM/CONFIG/SalishSea/EXP00/server.exe -> /home/dlatorne/MEOPAR/NEMO-code/NEMOGCM/CONFIG/SalishSea/BLD/bin/server.exe*
    NEMO executable symlinked in SalishSea/EXP00/ as opa at Mon Aug 18 12:34:25 PDT 2014
    lrwxrwxrwx 1 dlatorne dlatorne 73 Aug 18 12:34 /home/dlatorne/MEOPAR/NEMO-code/NEMOGCM/CONFIG/SalishSea/EXP00/opa -> /home/dlatorne/MEOPAR/NEMO-code/NEMOGCM/CONFIG/SalishSea/BLD/bin/nemo.exe*

The :kbd:`Build failed...` message on the 10th to last line is deceptive.
It is an artifact of the slightly convoluted build process that is required on :kbd:`orcinus`.
The important result is that the last line shows a newly created symbolic link between :file:`.../EXP00/opa` and :file:`.../BLD/bin/nemo.exe`.

Similarly,
the output of :file:`TOOLS/REBUILD_NEMO/orcinus_build.sh` should end like:

.. code-block:: bash

    mpif90 -o rebuild_nemo.o -I/home/dlatorne/MEOPAR/NEMO-code/NEMOGCM/TOOLS/REBUILD_NEMO/BLD/inc -c -fpp -r8 -O3 -assume byterecl -heap-arrays -I/global/software/lib64/intel/ncsa-tools/include /home/dlatorne/MEOPAR/NEMO-code/NEMOGCM/TOOLS/REBUILD_NEMO/src/rebuild_nemo.f90
    touch /home/dlatorne/MEOPAR/NEMO-code/NEMOGCM/TOOLS/REBUILD_NEMO/BLD/flags/LD.flags
    touch /home/dlatorne/MEOPAR/NEMO-code/NEMOGCM/TOOLS/REBUILD_NEMO/BLD/flags/LD__nemo.flags
    touch /home/dlatorne/MEOPAR/NEMO-code/NEMOGCM/TOOLS/REBUILD_NEMO/BLD/flags/LD__nemo__rebuild_nemo.flags
    touch /home/dlatorne/MEOPAR/NEMO-code/NEMOGCM/TOOLS/REBUILD_NEMO/BLD/flags/LDFLAGS.flags
    touch /home/dlatorne/MEOPAR/NEMO-code/NEMOGCM/TOOLS/REBUILD_NEMO/BLD/flags/LDFLAGS__nemo.flags
    touch /home/dlatorne/MEOPAR/NEMO-code/NEMOGCM/TOOLS/REBUILD_NEMO/BLD/flags/LDFLAGS__nemo__rebuild_nemo.flags
    fcm_internal load:F nemo rebuild_nemo.o rebuild_nemo.exe
    Use of uninitialized value in split at /home/dlatorne/MEOPAR/NEMO-code/NEMOGCM/EXTERNAL/fcm/bin/fcm_internal line 377.
    mpif90 -o rebuild_nemo.exe /home/dlatorne/MEOPAR/NEMO-code/NEMOGCM/TOOLS/REBUILD_NEMO/BLD/obj/rebuild_nemo.o -L/home/dlatorne/MEOPAR/NEMO-code/NEMOGCM/TOOLS/REBUILD_NEMO/BLD/lib -shared-intel -lnetcdf -lnetcdff -lhdf5 -lhdf5_hl -lz -lsz
    /global/software/lib64/intel/ncsa-tools/lib/libnetcdf.so: undefined reference to `__intel_cpu_feature_indicator_x'
    /global/software/lib64/intel/ncsa-tools/lib/libnetcdf.so: undefined reference to `__intel_cpu_features_init_x'
    /global/software/lib64/intel/ncsa-tools/lib/libnetcdf.so: undefined reference to `__intel_ssse3_memmove'
    fcm_internal load failed (256)
    make: *** [rebuild_nemo.exe] Error 1
    make -f /home/dlatorne/MEOPAR/NEMO-code/NEMOGCM/TOOLS/REBUILD_NEMO/BLD/Makefile -j 1 all failed (2) at /global/home/dlatorne/MEOPAR/NEMO-code/NEMOGCM/EXTERNAL/fcm/bin/../lib/Fcm/Build.pm line 597
    ->Make: 4 seconds
    ->TOTAL: 4 seconds
    Build failed on Mon Aug 18 12:56:07 2014.
    /home/dlatorne/MEOPAR/NEMO-code/NEMOGCM/TOOLS
    ls: cannot access /home/dlatorne/MEOPAR/NEMO-code/NEMOGCM/TOOLS/REBUILD_NEMO/BLD/bin/*.exe: No such file or directory
    /home/dlatorne/MEOPAR/NEMO-code/NEMOGCM/TOOLS/REBUILD_NEMO
    rebuild_nemo build succeeded at Mon Aug 18 12:56:08 PDT 2014
    -rwxr-x--- 1 dlatorne dlatorne 108611 Aug 18 12:56 /home/dlatorne/MEOPAR/NEMO-code/NEMOGCM/TOOLS/REBUILD_NEMO/BLD/bin/rebuild_nemo.exe*
    rebuild_nemo executable symlinked in /home/dlatorne/MEOPAR/NEMO-code/NEMOGCM/TOOLS/REBUILD_NEMO/ at Mon Aug 18 12:56:08 PDT 2014
    lrwxrwxrwx 1 dlatorne dlatorne 83 Aug 18 12:56 /home/dlatorne/MEOPAR/NEMO-code/NEMOGCM/TOOLS/REBUILD_NEMO/rebuild_nemo.exe -> /home/dlatorne/MEOPAR/NEMO-code/NEMOGCM/TOOLS/REBUILD_NEMO/BLD/bin/rebuild_nemo.exe*

As was the case for the NEMO build above,
the :kbd:`Build failed...` message on the 10th to last line is a deceptive artifact of the build process that is required on :kbd:`orcinus`.
The important result is that the last line shows a newly created symbolic link between :file:`.../REBUILD_NEMO/rebuild_nemo.exe` and :file:`.../BLD/bin/rebuild_nemo.exe`.

The command :program:`./makenemo clean` works as usual.


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
