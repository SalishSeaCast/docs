***********************************************
Set-up, Initial Conditions, Forcing, etc. Files
***********************************************

The collection of domain-specific set-up,
initial conditions,
forcing,
etc.
files used to run NEMO for the Salish Sea are maintained in the `NEMO-forcing`_ repo.

.. _NEMO-forcing: https://bitbucket.org/salishsea/nemo-forcing/

.. note::

    The `NEMO-forcing`_ repository is a private repository for members of the Salish Sea MEOPAR project team.
    That is because it contains files for which permission and/or licensing for public release have not been obtained.

    If you would like access to `NEMO-forcing`_,
    please contact `Susan Allen`_,
    the Salish Sea MEOPAR project leader.

    .. _Susan Allen: mailto://sallen@eos.ubc.ca


Getting the Repo
================

Team members using SSH key authentication on Bitbucket may clone the `NEMO-forcing`_ repo with:

.. code-block:: bash

    hg clone ssh://hg@bitbucket.org/salishsea/nemo-forcing NEMO-forcing

For password authentication use:

.. code-block:: bash

    hg clone https://<you>@bitbucket.org/salishsea/nemo-forcing NEMO-forcing

where :kbd:`<you>` is your Bitbucket user id.


Repo Contents
=============

:file:`grid/` Directory
-----------------------

The :file:`grid/` directory contains coordinates and bathymetry files.

NEMO has the file names of the coordinates and bathymetry files hard-coded as :file:`coordinates.nc` and :file:`bathy_meter.nc` so the files used for a particular run-set need to be copied or symlinked to those names.

Coordinates and bathymetry for the initial sub-domain test case known as :kbd:`JPP` or :kbd:`WCSD_RUN_tide_M2_OW_ON_file_DAMP_ANALY`:

* :file:`SubDom_coordinates_seagrid_WestCoast.nc`
* :file:`SubDom_bathy_meter_NOBCchancomp.nc`
