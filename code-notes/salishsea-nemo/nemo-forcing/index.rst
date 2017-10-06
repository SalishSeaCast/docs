.. _NEMO-forcing:

***********************************************
Set-up, Initial Conditions, Forcing, etc. Files
***********************************************

The collection of domain-specific set-up,
initial conditions, open boundary conditions,
forcing,
etc.
files used to run NEMO for the Salish Sea are maintained in 4 repositories:

* :ref:`grid-repo`
* :ref:`rivers-repo`
* :ref:`tides-repo`
* :ref:`tracers-repo`

They can be found in the `SalishSea-MEOPAR NEMO Model Runs`_ collection of repos on Bitbucket.

.. _SalishSea-MEOPAR NEMO Model Runs: https://bitbucket.org/account/user/salishsea/projects/SSM_NEMO_RUNS

If you have set up SSH key authentication on Bitbucket,
you can clone the :ref:`grid-repo`,
:ref:`rivers-repo`,
:ref:`tides-repo`,
and :ref:`tracers-repo` repos with:

.. code-block:: bash

    hg clone ssh://hg@bitbucket.org/salishsea/grid
    hg clone ssh://hg@bitbucket.org/salishsea/rivers-climatology
    hg clone ssh://hg@bitbucket.org/salishsea/tides
    hg clone ssh://hg@bitbucket.org/salishsea/tracers

For password authentication use:

.. code-block:: bash

    hg clone https://<you>@bitbucket.org/salishsea/grid
    hg clone https://<you>@bitbucket.org/salishsea/rivers-climatology
    hg clone https://<you>@bitbucket.org/salishsea/tides
    hg clone https://<you>@bitbucket.org/salishsea/tracers

where :kbd:`<you>` is your Bitbucket user id.


.. toctree::
   :maxdepth: 2

   atmospheric
   TSrestart
   obc
   netcdf4
