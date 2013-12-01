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

* :kbd:`ifort_jasper`: MPI builds on :kbd:`jasper.westgrid.ca`
* :kbd:`salish`: MPI builds on :kbd:`salish.eos.ubc.ca`
* :kbd:`ocean`: single processor builds on UBC-EOAS :kbd:`ocean` cluster workstations and :kbd:`salish`


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

The :command:`salishsea combine` sub-command combines the per-processors netCDF results files from a run into files in a results directory.
It has a number of option flags to define how it works;
see :command:`salishsea combine -h` for details.
A typical use on :kbd:`salish` is:

.. code-block:: bash

    salishsea combine --no-compress SalishSea.yaml ../results/15-21Sep2002

The files that define the run,
and the non-netCDF results files produced by the run can be moved to the results directory with a command like:

.. code-block:: bash

    mv layout.dat namelist NEMO-code_tip.txt NEMO-forcing_tip.txt ocean.output SalishSea.yaml solver.stat stderr stdout time.step ../results/15-21Sep2002


.. _NEMO-MirrorMaintenance:

NEMO :command:`svn` Repo Mirror Maintenance
===========================================

The :file:`/ocean/sallen/hg_repos/NEMO-hg-mirror` repository is an :command:`svn` checkout of http://forge.ipsl.jussieu.fr/nemo/svn/branches/2012/dev_v3_4_STABLE_2012 and also a read-only Mercurial repository.
It was initialized with:

.. code-block:: bash

    cd /ocean/sallen/hg_repos
    svn --username "dlatornell" co -r 3819 http://forge.ipsl.jussieu.fr/nemo/svn/branches/2012/dev_v3_4_STABLE_2012
    hg init NEMO-hg-mirror
    cd NEMO-hg-mirror
    cat > .hgignore
    .svn
    DOC/NEMO_book.pdf
    ctrl-d
    hg add
    hg ci -m"Initialize NEMO svn mirror at r3819 of ^/branches/2012/dev_v3_4_STABLE_2012."

:command:`svn` v1.7.5 was used on :kbd:`salish` for the :command:`svn` part of the initialization.


.. _PullChangesFromNEMOsvn:

Workflow to Pull Changes from NEMO :command:`svn` Repo
------------------------------------------------------

.. todo::

    Write Workflow to Pull Changes from NEMO :command:`svn` Repo


Workflow to Merge :kbd:`trunk` and Salish Sea Revisions
-------------------------------------------------------

Merging changes from NEMO :kbd:`trunk` and the Salish Sea central `NEMO-code` repo on Bitbucket is done in a repo that is used for only that purpose.
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

After the :ref:`PullChangesFromNEMOsvn` has been completed those changes from `NEMO-code`_ are pulled and updated into :kbd:`NEMO-mirror-merge`,

.. code-block:: bash

    cd NEMO-mirror-merge
    hg pull --update bb

The changes from :file:`/ocean/sallen/hg_repos/NEMO-hg-mirror` are also pulled and updated into :kbd:`NEMO-mirror-merge`,
resolving any merge conflicts as necessary:

.. code-block:: bash

    hg pull --update mirror

Finally,
the result of the updates and merges is pushed to `NEMO-code`_:

.. code-block:: bash

    hg push bb

If other users have pushed changes to `NEMO-code`_ while merge conflicts were being handled :command:`hg pull --rebase` can be used to bring in those changes and deal with any additional merge conflicts.
