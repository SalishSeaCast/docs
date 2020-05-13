.. _bashConfiguration:

*****************************
:command:`bash` Configuration
*****************************

All of the Salish Sea MEOPAR documentation assumes that you are using the :command:`bash` shell.
You can check which shell you are using with:

.. code-block:: bash

    echo $SHELL

If the result is not something like :kbd:`/bin/bash` you may be able to start :command:`bash` with:

.. code-block:: bash

    bash

You may want to request that your IT support change your default shell to :command:`bash` to avoid having to start :command:`bash` every time that you open a new shell window/tab.
For the Waterhome workstations and :kbd:`salish` you can open a ticket with the request on the `EOAS Compstaff Helpdesk system`_.
On Westgrid,
send an email request to support@westgrid.ca.

.. _EOAS Compstaff Helpdesk system: https://my.eos.ubc.ca/portal.html

If you are new to :command:`bash` or the Linux command line the copy of "The Linux Command Line" by William E. Shotts, Jr. in the Waterhole will help you,
as will this `Unix Shell Quick Reference`_ page from `Software Carpentry`_.

.. _Unix Shell Quick Reference: https://douglatornell.github.io/2013-09-26-ubc/lessons/ref/shell.html
.. _Software Carpentry: https://software-carpentry.org/


.. _.bashrc-snippets:

:command:`.bashrc` Snippets
===========================

:command:`bash` executes the commands in :file:`$HOME/.bashrc` every time a new shell window/tab is opened.
If you do not have a :file:`$HOME/.bashrc` file you can create it using your favourite editor.

The rest of this section described various snippets of :command:`bash` code that you may want to include in your :file:`$HOME/.bashrc` file.

To shorten your prompt so that it shows just the name of the machine that you are on and the directory that you are currently in instead of the whole path to that directory use:

.. code-block:: bash

    PS1="\h:\W$ "

To force programs and commands that want to display output page by page to use :command:`less` as their pager use:

.. code-block:: bash

    export PAGER=less

To force :command:`less` to allow control sequences that change the colour of output to work use:

.. code-block:: bash

    export LESS=-R

If you are not a fan of the :command:`vi` editor you can set the :envvar:`EDITOR` and :envvar:`VISUAL` environment variables to the command for your favourite editor and export them.
For :command:`emacs` use:

.. code-block:: bash

    export EDITOR=emacs
    export VISUAL=emacs

The :file:`$HOME/bin/` directory is the conventional place keep your own scripts so it is a good idea to add that directory to the end of your :envvar:`PATH`:

.. code-block:: bash

    export PATH=$PATH:$HOME/bin

The :file:`$HOME/.local/bin/` directory is where Python scripts installed via the :kbd:`--user` option are stored so it should be near the beginning of your :envvar:`PATH`:

.. code-block:: bash

    export PATH=$HOME/.local/bin:$PATH

If you are using the Anaconda Python distribution you should add its :file:`bin/` directory to the beginning of your :envvar:`PATH`:

.. code-block:: bash

    export PATH=$HOME/anaconda3/bin:$PATH

Aliases allow you to run commands with different names or with particular option flags set.

To make the :command:`ls` command use different colours for
regular files,
executable files,
directories,
symbolic links,
etc.,
and post-fix characters to indicate those file types use:

.. code-block:: bash

    alias ls="ls --color=auto -F"

To make :command:`la` include hidden files in file listings use:

.. code-block:: bash

    alias la="ls -a"

To make :command:`ll` display long file listings that include
permissions,
owner and group,
and last modification date/time,
and also include hidden files use:

.. code-block:: bash

    alias ll="ls -al"

Aliases are cumulative,
so if the above three aliases are all defined in the order shown,
:command:`ls`,
:command:`la`,
and :command:`ll` will all produce coloured,
post-fixed file listings.

To always be prompted to confirm file removals use:

.. code-block:: bash

    alias rm="rm -i"

Some things cannot be easily accomplished with aliases and so :command:`bash` also provides a way of writing functions.
One good use for :command:`bash` functions is creating commands that change directories for you to particular locations without having to type long paths.
For example:

.. code-block:: bash

    go_results() {
        cd /ocean/$USER/MEOPAR/SalishSea/results;
    }

creates the :command:`go_results` command that will :command:`cd` from wherever you are to the directory where your Salish Sea NEMO model run results are stored.

:command:`.bash_profile`
--------------------------------

To ensure that :command:`.bashrc` is executed when you login via ssh create a file :file:`$HOME/.bash_profile` with the following lines:

.. code-block:: bash

    if [ -f ~/.bashrc ]; then
        . ~/.bashrc;
    fi


.. _LoadingModulesOnHPCClusters:

Loading Modules on HPC Clusters
-------------------------------

When working on ComputeCanada or Westgrid clusters the :command:`module` command must be used to load several software components required to
compile,
run,
and work with the results of NEMO.
The required modules vary from machine to machine:

.. _LoadingModulesOnCedarGraham:

* On :kbd:`graham`,
  :kbd:`cedar`,
  or :kbd:`beluga` the :command:`module load` commands you should include in your :file:`$HOME/.bashrc` are:

  .. code-block:: bash

      module load python/3.8.2
      module load netcdf-fortran-mpi/4.4.4

.. _LoadingModulesOnOrcinus:

* On :kbd:`orcinus` the :command:`module load` commands you should include in your :file:`$HOME/.bashrc` are:

  .. code-block:: bash

      module load python
      module load intel
      module load intel/14.0/netcdf-4.3.3.1_mpi
      module load intel/14.0/netcdf-fortran-4.4.0_mpi
      module load intel/14.0/hdf5-1.8.15p1_mpi
      module load intel/14.0/nco-4.5.2

You can inspect the collection of modules that are loaded with the :command:`module list` command.
The response looks something like this example from :kbd:`graham`::

  Currently Loaded Modules:
    1) nixpkgs/16.09     (S)      6) intel/2016.4       (t)   11) netcdf-fortran-mpi/4.4.4 (io)
    2) imkl/11.3.4.258   (math)   7) openmpi/2.1.1      (m)   12) perl/5.22.4              (t)
    3) gcccore/.5.4.0    (H)      8) StdEnv/2016.4      (S)   13) python/3.8.2             (t)
    4) icc/.2016.4.258   (H)      9) hdf5-mpi/1.8.18    (io)
    5) ifort/.2016.4.258 (H)     10) netcdf-mpi/4.4.1.1 (io)

    Where:
     S:     Module is Sticky, requires --force to unload or purge
     m:     MPI implementations / Implémentations MPI
     math:  Mathematical libraries / Bibliothèques mathématiques
     io:    Input/output software / Logiciel d'écriture/lecture
     t:     Tools for development / Outils de développement
     H:                Hidden Module
