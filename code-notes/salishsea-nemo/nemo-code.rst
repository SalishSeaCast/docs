.. _NEMO-code:

********************
NEMO-code Repository
********************

These notes describe the Salish Sea MEOPAR project `NEMO-code`_ repository and its maintenance.
They are a narrative guide to the Mercurial log and diffs that can be obtained from the repository itself or via the Bitbucket interface.

The `NEMO-code`_ repo is a Mercurial repository in which is maintained the merger of the trunk of the main NEMO :command:`svn` repository and the changes made by the Salish Sea MEOPAR project team.

.. note::

    The `NEMO-code`_ repository is a private repository for members of the Salish Sea MEOPAR project team.
    That is because it contains parts of the NEMO_ codebase.
    Although that codebase is openly licensed it's developers require registration_ to access the code.

    If you have completed that registration and would like access to the `NEMO-code`_,
    please contact `Susan Allen`_,
    the Salish Sea MEOPAR project leader.

    .. _NEMO: http://www.nemo-ocean.eu/
    .. _registration: http://www.nemo-ocean.eu/user/register
    .. _Susan Allen: mailto://sallen@eos.ubc.ca


Getting the Code
================

Team members using SSH key authentication on Bitbucket may clone the `NEMO-code`_ repo with:

.. code-block:: bash

    hg clone ssh://hg@bitbucket.org/salishsea/nemo-code NEMO-code

For password authentication use:

.. code-block:: bash

    hg clone https://<you>@bitbucket.org/salishsea/nemo-code NEMO-code

where :kbd:`<you>` is your Bitbucket user id.


Managing Configurations
=======================

Building an Existing Configuration
----------------------------------

To build the executable code for an already defined configurations,
for example,
:kbd:`SalishSea`,
use:

.. code-block:: bash

    cd NEMO-code/NEMOGCM/CONFIG
    ./makenemo -n SalishSea -m salish -j8

That will compile the full domain Salish Sea NEMO configuration and the IOM output server with the :kbd:`salish` architecture definitions with the compilation distributed over 8 cores.
The resulting executables are located in :file:`NEMO-code/NEMOGCM/CONFIG/SalishSea/BLD/bin/`.


Available Configurations
~~~~~~~~~~~~~~~~~~~~~~~~

In addition to the configurations from the NEMO :program:`svn` checkout,
the repo contains:

* :file:`SalishSea`: The initial full domain Salish Sea development configuration
* :file:`SalishSea_no_IOM`: A copy of the :file:`SalishSea` configuration compiled without the :kbd:`key_iom_put` CPP key
* :file:`SALISH_amm`: A very early stepping-stone configuration created during the learning process of creating a Salish Sea configuration based on the :file:`AMM12` configuration
* :file:`SALISH_JPP`: The Salish Sea sub-domain configuration used to compare with CONCEPTS-110


Available Architecture Definitions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In addition to the collection of architecture definitions that the NEMO consortium provides,
the `NEMO-code`_ repo includes definitions for:

* :kbd:`mpif90_jasper`: OpenMPI builds on :kbd:`jasper.westgrid.ca`
* :kbd:`ifort_jasper`: Intel MPI builds on :kbd:`jasper.westgrid.ca`
* :kbd:`salish`: MPI builds on :kbd:`salish.eos.ubc.ca`
* :kbd:`ocean`: single processor builds on UBC-EOAS :kbd:`ocean`
  cluster workstations and :kbd:`salish`
* :kbd:`gfortran_osx`: single processor builds on Mac-OSX


Creating a New Configuration
----------------------------

To create a new configuration based on,
for example,
`AMM12`_ use:

.. _AMM12: http://www.nemo-ocean.eu/Using-NEMO/Configurations/AMM

.. code-block:: bash

    cd NEMO-code/NEMOGCM/CONFIG
    ./makenemo -r AMM12 -n MY_AMM12 -m salish -j8 add_key "key_netcdf4 key_nosignedzero"

That will use the existing :kbd:`AMM12` configuration as a basis to build a new configuration called :kbd:`MY_AMM12` with the :kbd:`salish` architecture definitions and with compilation distributed over 8 cores.
The C Pre-Processor (CPP) keys :kbd:`key_netcdf4` and :kbd:`key_nosignedzero` will be added to configurations.
The resulting configuration,
including a compiled and link NEMO executable,
is located in :file:`NEMO-code/NEMOGCM/CONFIG/MY_AMM12`.

See :command:`./makenemo -h` for details of options and sub-commands.


Running the Model
=================

We don't want to clutter the :ref:`NEMO-code-repo` repo with files from development and exploration run-sets
(aka experiments),
run results,
etc.,
so runs are done in directories outside the :file:`NEMO-code/` tree.

The :ref:`SS-run-sets` repo contains example run description,
namelist,
and output server control files that define a run.
Run description files define the names and locations of files and directories that the :ref:`SalishSeaCmdProcessor` tool uses to manage Salish Sea NEMO runs and their results.
See :ref:`RunDescriptionFileStructure` for details of the run description file syntax.

After copying and/or editing the run description,
namelist,
and output server control files to define a run use the :program:`salishsea` :ref:`salishsea-prepare` to set up a temporary run directory from which to execute the run.
For example:

.. code-block:: bash

    salishsea prepare SalishSea.yaml iodef.xml

Use the run description and output server definitions files names that are appropriate for your run.
See the :ref:`salishsea-prepare` docs for details of the files and symbolic links that are created in the run directory.
The path to the run directory is printed upon completion of the command.

Go to the run directory and start the run with a command like:

.. code-block:: bash

    mpiexec -n 16 ./nemo.exe > stdout 2> stderr &

That command runs the model in the background on 16 processors,
redirecting the stdout and stderr streams to :file:`stdout` and :file:`stderr` files.
The number of processors to run is must match the domain decomposition defined in the :file:`namelist.compute` file.

A convenient command to monitor the memory use of a run and its time step progress is:

.. code-block:: bash

    watch -n 5 "(free -m; cat time.step)"

The :command:`salishsea gather` sub-command collects the run definition and results files from a Salish Sea NEMO run into a results directory.
The collection process includes combining the per-processors netCDF results files into files in the results directory.
It has a number of option flags to define how it works;
see :command:`salishsea gather -h` for details.
A typical use on :kbd:`salish` is:

.. code-block:: bash

    salishsea gather --no-compress SalishSea.yaml ../results/15-21Sep2002

The files that define the run,
and the non-netCDF results files produced by the run are also moved to the results directory by :command:`salishsea gather`.


.. _NEMO-MirrorMaintenance:

NEMO :command:`svn` Repo Mirror Maintenance
===========================================

The :file:`/ocean/sallen/hg_repos/NEMO-hg-mirror` repository is an :command:`svn` checkout of http://forge.ipsl.jussieu.fr/nemo/svn/branches/2012/dev_v3_4_STABLE_2012 and also a read-only Mercurial repository.
It was initialized with:

.. code-block:: bash

    $ cd /ocean/sallen/hg_repos
    $ svn --username "dlatornell" co -r 3819 http://forge.ipsl.jussieu.fr/nemo/svn/branches/2012/dev_v3_4_STABLE_2012 NEMO-hg-mirror
    $ hg init NEMO-hg-mirror
    $ cd NEMO-hg-mirror
    $ cat > .hgignore
    .svn
    DOC/NEMO_book.pdf
    ctrl-d
    $ hg add
    $ hg ci -m"Initialize NEMO svn mirror at r3819 of ^/branches/2012/dev_v3_4_STABLE_2012."

:command:`svn` v1.7.5 was used on :kbd:`salish` for the :command:`svn` part of the initialization.

.. figure:: images/NEMO-CodeReposMaint.png

   NEMO code repositories and workflow to update and merge SVN and local changes


.. _PullChangesFromNEMOsvn:

Workflow to Pull Changes from NEMO :command:`svn` Repo
------------------------------------------------------

The workflow to pull changes from the master NEMO :command:`svn` repo and commit them to our :file:`NEMO-hg-mirror` repo is somewhat automated by the :ref:`Marlin`.

#. Review the upstream changes in the source browser at https://forge.ipsl.jussieu.fr/nemo/log/branches/2012/dev_v3_4_STABLE_2012 to select a range of changes to be pulled into our :file:`NEMO-hg-mirror` repo.

   .. note::

      Pay special attention to changes in the :file:`OPA_SRC/` tree that involve files that have been copied into :file:`NEMOGCM/CONFIG/SalishSea/MY_SRC/` or team members' :file:`MY_SRC/` directories.
      Those files must be *manually* merged with their :file:`MY_SRC/` counterparts.

#. Working on :kbd:`salish` in the :file:`/ocean/sallen/hg_repos/NEMO-hg-mirror` repo with an activated virtualenv in which :command:`marlin` is installed:

   .. code-block:: bash

       $ ssh salish
       $ workon marlin
       (marlin)$ cd /ocean/sallen/hg_repos/NEMO-hg-mirror

#. Use :kbd:`marlin incoming` information about the next SVN revision that will be pulled from upstream and confirm that it is the expected revision:

   .. code-block:: bash

       (marlin)$ marlin incoming
       r3843 2013-03-20 09:29:58 UTC
         correct ice output filename for limwri.F90

   The :kbd:`--limit` option can be used to see more incoming revisions;
   see :command:`marlin help incoming` for details.

#. Use :kbd:`marlin update` to update the working copy to the next upstream commit and commit the SVN update as a Mercurial changeset with the SVN commit message as the body of the Mercurial commit message and echo that message:

   .. code-block:: bash

       (marlin)$ marlin update
       Update to svn r3843.

       correct ice output filename for limwri.F90

   The :kbd:`--to-rev` option can be used to apply a series of upstream updates,
   committing them to Mercurial one at a time;
   see :command:`marlin help update` for details.


Workflow to Merge NEMO :command:`svn` Repo and Salish Sea Revisions
-------------------------------------------------------------------

Merging changes from NEMO :command:`svn` and the Salish Sea central `NEMO-code` repo on Bitbucket is done in a repo that is used for only that purpose.
Doug does the merges on his laptop.
The repo in which the merging is done was created by cloning the :file:`/ocean/sallen/hg_repos/NEMO-hg-mirror` repo:

.. code-block:: bash

    hg clone ssh://sable.eos.ubc.ca//ocean/sallen/hg_repos/NEMO-hg-mirror NEMO-mirror-merge

and setting the paths in its :file:`.hg/hgrc` to:

.. code-block:: ini

    [paths]
    bb = ssh://hg@bitbucket.org/salishsea/nemo-code
    default-push = ssh://hg@bitbucket.org/salishsea/nemo-code
    mirror = ssh://sable.eos.ubc.ca//ocean/sallen/hg_repos/NEMO-hg-mirror

Those paths mean that the repo for :command:`hg pull` and :command:`hg incoming` commands must be specified explicitly.
The :kbd:`bb` and :kbd:`mirror` paths are provided to facilitate pulling from `NEMO-code`_ on Bitbucket and :file:`/ocean/sallen/hg_repos/NEMO-hg-mirror`,
respectively.
:command:`hg push` and :command:`hg outgoing` commands will act on the `NEMO-code`_ repo,
unless otherwise specified.

After the :ref:`PullChangesFromNEMOsvn` has been completed the workflow to merge those changes with Salish Sea MEOPAR project revisions is:

#. Pull and update recent changes from `NEMO-code`_ into :kbd:`NEMO-mirror-merge`:

   .. code-block:: bash

       cd NEMO-mirror-merge
       hg pull --update bb

#. Pull and update the changes from :file:`/ocean/sallen/hg_repos/NEMO-hg-mirror` into :kbd:`NEMO-mirror-merge`:

   .. code-block:: bash

       hg pull mirror

#. Because the changesets pulled from `NEMO-code`_ are public a branch merge is necessary:

   .. code-block:: bash

       hg merge
       hg commit -m"Merge svn updates."

#. Manually merge and commit changes that involve files that have been copied into :file:`NEMOGCM/CONFIG/SalishSea/MY_SRC/` or team members' :file:`MY_SRC/` directories.
   Those files are most likely to be in :file:`OPA_SRC/`.

#. Push the result of the updates and merges to `NEMO-code`_:

   .. code-block:: bash

       hg push bb

   If other users have pushed changes to `NEMO-code`_ while merge conflicts were being handled :command:`hg pull --rebase` can be used to bring in those changes and deal with any additional merge conflicts.

#. Notify team members of the upstream merge,
   especially if manual merges of :file:`MY_SRC/` files were required,
   so that they can manage merging changes into any untracked :file:`MY_SRC/` files they may have.


.. _NEMO-3.6Migration:

Migration to NEMO-3.6
=====================

In February 2015 the process of migrating the Salish Sea NEMO model from NEMO-3.4 to NEMO-3.6 was started.
A collection of mirror repos similar to that described in :ref:`NEMO-MirrorMaintenance` was created.
NEMO-3.6 uses a separately distributed output server package called `XIOS`_ so the maintenance of Mercurial mirror repos for the Salish Sea NEMO project is expanded to deal with 2 upstream SVN repos.
For NEMO:

* :file:`/ocean/sallen/hg_repos/NEMO-3.6-hg-mirror`
* :file:`/Users/doug/MEOPAR/NEMO-3.6-mirror-merge`
* :file:`https://bitbucket.org/salishsea/nemo-3.6-code`

and for XIOS:

* :file:`/ocean/sallen/hg_repos/XIOS-hg-mirror`
* :file:`/Users/doug/MEOPAR/XIOS-mirror-merge`
* :file:`https://bitbucket.org/salishsea/xios`

.. _XIOS: http://forge.ipsl.jussieu.fr/ioserver/

The :file:`/ocean/sallen/hg_repos/NEMO-hg-mirror` repository is an :command:`svn` checkout of http://forge.ipsl.jussieu.fr/nemo/svn/branches/2012/dev_v3_4_STABLE_2012 and also a read-only Mercurial repository.
It was initialized with:

.. code-block:: bash

    $ cd /ocean/sallen/hg_repos
    $ svn --username "dlatornell" co http://forge.ipsl.jussieu.fr/nemo/svn/trunk NEMO-3.6-hg-mirror
    $ hg init NEMO-3.6-hg-mirror
    $ cd NEMO-3.6-hg-mirror
    $ cat > .hgignore
    .svn
    DOC/NEMO_book.pdf
    ctrl-d
    $ hg add
    $ hg ci -m"Initialize NEMO-3.6 svn mirror at r5072 of ^/trunk."

The :file:`/ocean/sallen/hg_repos/XIOS-hg-mirror` repository is an :command:`svn` checkout of http://forge.ipsl.jussieu.fr/ioserver/svn/XIOS/branchs/xios-1.0 and also a read-only Mercurial repository.
It was initialized with:

.. code-block:: bash

    $ cd /ocean/sallen/hg_repos
    $ http://forge.ipsl.jussieu.fr/ioserver/svn/XIOS/branchs/xios-1.0 XIOS-hg-mirror
    $ hg init XIOS-hg-mirror
    $ cd XIOS-hg-mirror
    $ cat > .hgignore
    .svn
    ctrl-d
    $ hg add
    $ hg ci -m"Initialize XIOS svn mirror at r549 of ^/branchs/xios-1.0."

:command:`svn` v1.8.8 was used on :kbd:`salish` for the :command:`svn` part of the initialization.


Building and Testing XIOS
-------------------------

Building XIOS_ fails with a confusing collection of errors about missing :kbd:`main` modules when the `XIOS build instructions`_ are followed.
The root cause of those errors appears to be the fact that the :kbd:`parse_xml.exe` in :file:`bld.cfg` is based on a C++ :kbd:`main` function while the :kbd:`xios_server.exe` and other targets are based on Fortran :kbd:`main` subroutines.
That means that building :kbd:`parse_xml.exe` requires a :kbd:`-nofor-main` linker argument while building the other targets *must not* use that argument.
It is unclear how the build system is meant to handle those incompatible requirements.
For the purposes of the SalishSea MEOPAR project NEMO model it appear that :kbd:`parse_xml.exe` is not required,
so it is removed from line 33 of :file:`bld.cfg`::

  bld::target xios_server.exe test_client.exe test_complete.exe test_xios_interface.exe

In fact,
the :kbd:`test_*` targets could probably also be eliminated.

.. _XIOS build instructions: http://forge.ipsl.jussieu.fr/ioserver/wiki/documentation

On :kbd:`jasper`,
XIOS_ was successfully built with the following :file:`arc/arch-*` files:

:file:`arch/arch-X64_JASPER.env`:

.. code-block:: bash

    module load library/openmpi/1.6.4-intel
    module load library/netcdf/4.1.3
    module load library/hdf5/1.8.8

:file:`arch/arch-X64_JASPER.path`:

.. code-block:: bash

    NETCDF_LIB="-lnetcdf"
    HDF5_LIB="-lhdf5_hl -lhdf5 -lz"

:file:`arch/arch-X^4_JASPER.fcm`:

.. code-block:: bash

    %CCOMPILER      mpicc
    %FCOMPILER      mpif90
    %LINKER         mpif90

    %BASE_CFLAGS    -diag-disable 1125 -diag-disable 279
    %PROD_CFLAGS    -O3 -D BOOST_DISABLE_ASSERTS
    %DEV_CFLAGS     -g -traceback
    %DEBUG_CFLAGS   -DBZ_DEBUG -g -traceback -fno-inline

    %BASE_FFLAGS    -D__NONE__
    %PROD_FFLAGS    -O3
    %DEV_FFLAGS     -g -O2 -traceback
    %DEBUG_FFLAGS   -g -traceback

    %BASE_INC       -D__NONE__
    %BASE_LD        -lstdc++

    %CPP            mpicc -EP
    %FPP            cpp -P
    %MAKE           gmake

using the command:

.. code-block:: bash

    ./make_xios --arch X64_JASPER --netcdf_lib netcdf4_seq --job 4

At present,
:kbd:`jasper` lacks the parallel versions of the netCDF4 library that is required to build XIOS_ so that it produces a single output file,
hence the :kbd:`--netcdf_lib netcdf4_seq` option.

The :file:`test_client.exe` executable that is built with :file:`xios_server.exe` and the :file:`inputs/iodef.xml` file can be used to test XIOS.
This was done by creating a :file:`test-XIOS/` directory,
copying :file:`inputs/iodef.xml` into it,
and symlinking :file:`xios_server.exe` and :file:`test_client.exe` into it.

In contrast to NEMO-3.4 where the IO server configuration is specified in the :file:`xmlio_server.def` file,
the configuration for XIOS is included as a stanza in :file:`iodef.xml`.
As copied,
:file:`iodef.xml` configures XIOS to run in "attached" mode,
similar to how the IO server is used in NEMO-3.4.
The relevant stanza is:

.. code-block:: xml

    <context id="xios">
        <variable_definition>
          <variable_group id="buffer">
              buffer_size = 80000000
              buffer_server_factor_size = 2
           </variable_group>

          <variable_group id="parameters" >
            <variable id="using_server" type="boolean">false</variable>
            <variable id="info_level" type="int">50</variable>
          </variable_group>
        </variable_definition>
    </context>

and the line:

.. code-block:: xml

    <variable id="using_server" type="boolean">false</variable>

sets "attached" mode.

Using :command:`qsub` to submit a file containing the following shell script with PBS directives runs :file:`test_client.exe` on 10 processors and produces 10 netCDF4 output files,
:file:`output_0.nc` through :file:`output_9.nc`:

.. code-block:: bash

    #!/bin/bash

    #PBS -N test-XIOS
    #PBS -S /bin/bash
    #PBS -l procs=10
    #PBS -l walltime=0:10:00
    #PBS -m bea
    #PBS -M dlatornell@eos.ubc.ca
    #PBS -o stdout
    #PBS -e stderr

    cd $PBS_O_WORKDIR
    echo working dir: $(pwd)

    module load library/netcdf/4.1.3
    module load library/hdf5/1.8.8

    mpirun -np 10 ./test_client.exe
    echo done!

Changing the XIOS server configuration in :file:`iodef.xml` to:

.. code-block:: xml

    <variable id="using_server" type="boolean">true</variable>

creating an MPI application file (let's call it test-XIOS.app) containing:

.. code-block:: bash

    -np 8 ./test_client.exe
    -np 2 ./xios_server.exe

and submitting a PBS script with the :command:`mpirun` line changed to:

.. code-block:: bash

    mpirun --app ./test-XIOS.app

results in :file:`test_client.exe` running on 8 processors and :file:`xios_server.exe` running on 2 and produces 2 netCDF4 output files,
:file:`output_0.nc` and :file:`output_1.nc`.

The netCDF4 files that XIOS produces are not deflated.
Running:

.. code-block:: bash

    ncks -4 -L4 output_0.nc output_0.nc

on one of the files produces by the above test reduces the file size to 33% or its original size.
However,
the build of NCO on :kbd:`jasper` is against the netCDF3 library so it cannot be used to do this deflation.
