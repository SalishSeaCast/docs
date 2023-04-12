.. _SalishSeaReposPackages:

Salish Sea Repos and Packages
=============================

Once you have your :ref:`MercurialConfiguration` set up and the :ref:`AnacondaPythonDistro` installed and activated you need to set up a working area,
clone some Mercurial repositories,
and install some Salish Sea MEOPAR project tools.

In the working environment on your Waterhole workstation you should create a work space on the :kbd:`/ocean/` partition:

.. code-block:: bash

    mkdir -p /ocean/$USER/MEOPAR

On your own laptop,
well,
the choice of where you put files is up to you...

In the work space you need to clone a number of Salish Sea MEOPAR project repos from Bitbucket.
The repos that you need depends whether the environment is for running the model,
doing analysis,
etc.;
the :ref:`QuickStartGuide` provides guidance for the repos to install in model run environments.
Here,
we'll assume that you are setting up your Waterhole machine working environment where you will be doing analysis,
documentation,
etc.

.. code-block:: bash

    cd /ocean/$USER/MEOPAR/
    git clone git@github.com:SalishSeaCast/docs.git
    git clone git@github.com:SalishSeaCast/tools.git
    git clone git@github.com:SalishSeaCast/NEMO-Cmd.git
    git clone git@github.com:SalishSeaCast/SalishSeaCmd.git

Next,
install the :ref:`SalishSeaToolsPackage` and the :ref:`SalishSeaCmdProcessor`:

.. code-block:: bash

    python3 -m pip install --user --editable tools/SalishSeaTools
    python3 -m pip install --user --editable NEMO-Cmd
    python3 -m pip install --user --editable SalishSeaCmd

The links above contain information about the contents of those packages.

Next,
you need a repo to store you analysis notebooks and other bits of code,
text,
etc. in.
We used to all share a single `analysis`_ repo,
but it got too big.
So,
we broke it up so that each team member has their own analysis repo,
and there are a few special purpose analysis repos.
The original analysis repo is now read-only.

.. _analysis: https://github.com/SalishSeaCast/analysis

If you are joining the team as a researcher there should be an analysis repo already set up for you.
Its name will be something like :kbd:`analysis-james`,
but with your name.
Go ahead and clone that repo too:

.. code-block:: bash

    cd /ocean/$USER/MEOPAR/
    git clone git@github.com:SalishSeaCast/analysis-james.git

If you are joining the team for a sprint,
please clone the :kbd:`analysis-sprints` repo:

.. code-block:: bash

    cd /ocean/$USER/MEOPAR/
    git clone git@github.com:SalishSeaCast/analysis-sprints.git

and create a directory in it named after yourself to work in.
