:command:`bash` Configuration
=============================

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


.. _.bashrc-snippets:

:command:`.bashrc` Snippets
---------------------------

:command:`bash` executes the commands in :file:`$HOME/.bashrc` every time a new shell window/tab is opened.
If you do not have a :file:`$HOME/.bashrc` file you can create it using your favourite editor.

The rest of this section described various snippets of :command:`bash` code that you may want to include in your :file:`$HOME/.bashrc` file.

To shorten your prompt so that it shows just the name of the machine that you are on and the directory that you are currently in instead of the whole path to that directory use:

.. code-block:: bash

    PS1="\h:\W$ "

To force programs and command that want to display output page by page to use :command:`less` as their pager use:

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

If you are using the Anaconda Python distribution you should add its :file:`bin/` directory to the beginning of your :envvar:`PATH`:

.. code-block:: bash

    export PATH=$HOME/anaconda/bin:$PATH

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


.. _LoadingModulesOnJasper:

Loading modules on :kbd:`jasper`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When working on :kbd:`jasper.westgrid.ca` the :command:`module` command must be used to load several software components required to
compile,
run,
and work with the results of NEMO.
The following :command:`module load` commands should be added to your :kbd:`jasper` :file:`$HOME/.bashrc` file:

.. code-block:: bash

    module load application/python/2.7.3
    module load library/netcdf/4.1.3
    module load application/nco/4.3.9
    module load library/szip/2.1

The Intel Fortran compiler,
OpenMPI,
and HDF5 modules will be loaded as side-effects.
You can inspect the collection of modules that are loaded with the :command:`module list` command:

.. .. code-block:: bash

    module list
    Currently Loaded Modulefiles:
      1) compiler/intel/12.1           3) application/python/2.7.3      5) library/netcdf/4.1.3
      2) library/openmpi/1.6.4-intel   4) library/hdf5/1.8.8            6) library/szip/2.1
