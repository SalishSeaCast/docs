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

They can be found in the `SalishSeaCast organization`_ collection of repos on GitHub.

.. _SalishSeaCast organization: https://github.com/SalishSeaCast

If you have set up SSH key authentication on GitHub and Bitbucket,
you can clone the :ref:`grid-repo`,
:ref:`rivers-repo`,
:ref:`tides-repo`,
and :ref:`tracers-repo` repos with:

.. code-block:: bash

    git clone git@github.com:SalishSeaCast/grid.git
    git clone git@github.com:SalishSeaCast/rivers-climatology.git
    git clone git@github.com:SalishSeaCast/tides.git
    git clone git@github.com:SalishSeaCast/tracers.git

For password authentication use:

.. code-block:: bash

    git clone https://github.com/SalishSeaCast/grid.git
    git clone https://github.com/SalishSeaCast/rivers-climatology.git
    git clone https://github.com/SalishSeaCast/tides.git
    git clone https://github.com/SalishSeaCast/tracers.git

where :kbd:`<you>` is your Bitbucket user id.


.. toctree::
   :maxdepth: 2

   atmospheric
   TSrestart
   obc
   netcdf4
