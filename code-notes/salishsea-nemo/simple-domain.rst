.. _Simple-Domain:

****************************
Simplified Model Domain - 2D
****************************

This page provides instructions on how to build a simplified two-dimensional version of the Salish Sea model.
The two-dimensional model is based on a simplifed bathymetry profile along the domain's thalweg.
A simplified model allows for quick sensitivity tests of mixing parametrizations and resolution.


New NEMO Configuration
----------------------
The first step is to create a new NEMO configuration.
Here we will define parameters such as number of grid points and resolution.
These instructions explain how to create a new configuration called :kbd:`SalishSea2D`

1. In :kbd:`NEMO-code/NEMOGCM/CONFIG`, create directory for the new configuration.

.. code-block:: bash

    cd NEMO-code/NEMOGCM/CONFIG
    mkdir SalishSea2D

2. Create a :file:`cpp_SalishSea2D.fcm` file and place it the configuration directory.
Add a key for the new configuration and any other keys you require.
For example,

.. code-block:: bash

    cd SalishSea2D
    ls cpp_SalishSea2D.fcm
    bld::tool::fppkeys  key_bdy key_vectopt_loop   key_dynspg_ts key_ldfslp    key_vvl
    key_diainstant key_mpp_mpi key_netcdf4 key_nosignedzero key_traldf_c2d key_dynldf_c3d
    key_tide key_zdfgls key_iomput key_salishsea2D

3. Add configuration source files to :kbd:`SalishSea2D/MY_SRC`.
You need at least two modified source files: :file:`par_oce.F90` and :file:`par_SalishSea2D.h90`.
In :file:`par_oce.F90`, add a case for dealing with the new configuration key:

.. code-block:: fortran

    #elif defined key_salishsea2D
       !!---------------------------------------------------------------------
       !!   'key_salishsea2D':                    Strait of Georgia: 2D
       !!---------------------------------------------------------------------
    #             include "par_SalishSea2D.h90"

4. Specify choices in domain size, grid and resolution by editing :file:`par_SalishSea2D.h90`.
For example, these choices will set up a simplified grid 1100 by 10 by 40 grid points with 500 m resolution.
See the NEMO documentation for details on parameter choices.

.. code-block:: fortran

      jpidta  = 1100,        &  !: first horizontal dimension > or = to jpi
      jpjdta  = 10,          &  !: second                     > or = to jpj
      jpkdta  = 40,          &  !: number of levels           > or = to jpk

      jphgr_msh = 2          &  !: type of horizontal mesh
      !                         !  = 2 f-plane with regular grid-spacing

      ppgphi0  = 0.0_wp,         &  !: latitude for the Coriolis or Beta parameter (jphgr_msh = 2 or 3)

      ppe1_m   = 500,   &  !: zonal      grid-spacing (meters )
      ppe2_m   = 500       !: meridional grid-spacing (meters )

5. Add :kbd:`SalishSea2D OPA_SRC` to the last line of :file:`NEMO-code/NEMOGCM/CONFIG/cfg.txt`

6. Try compiling with this new configuration.
For example, on :kbd:`salish`

.. code-block:: bash

    cd NEMO-code/NEMOGCM/CONFIG
    ./makenemo -n SalishSea2D -m salish -j8

Note: This configuration has been added to the :kbd:`NEMO-code` repository.


Bathymetry
-----------

A simplifed 2D bathymetry was created by smoothing the bathymetry along the thalweg.
See `Generate_2D_bathy.ipynb`_

.. _Generate_2D_bathy.ipynb: http://nbviewer.ipython.org/urls/bitbucket.org/salishsea/2d-domain/raw/tip/notebooks/Generate_2D_bathy.ipynb

Initial Conditions
-------------------

Initial conditions for temperature and salinity were taken from model 2003 spin-up.
The velocities are initialized to zero values.
Winter and summer stratifications were created using `Generate_2D_T+S.ipynb`_.
Note that a test run with basic namelists was used to create the 2D :file:`mesh_mask.nc`.
This file was used to ensure the initial temperature and salinity covered the full 2D domain.

.. _Generate_2D_T+S.ipynb: http://nbviewer.ipython.org/urls/bitbucket.org/salishsea/2d-domain/raw/tip/notebooks/Generate_2D_T+S.ipynb

Tides
------

Tidal elevations are based on the 3D model tides, averaged across the mouth of the Strait of Juan de Fuca.
Tidal currents for U are also based on 3D tidal currents, averaged across the boundary.
Tidal currents for V are ignored.
Some adjustments to the tidal currents are made to enforce rapid velocties over the sill since the horizontal constriction is not present in the 2D domain.
See `Generate_2D_Tides.ipynb`_ for generation of tide forcing files.

.. _Generate_2D_Tides.ipynb: http://nbviewer.ipython.org/urls/bitbucket.org/salishsea/2d-domain/raw/tip/notebooks/Generate_2D_Tides.ipynb

River
------

A simple representation of the Fraser River was added to replace mixed stratification.
A constant flow rate was used.
Details in `Generate_rivers_forcing.ipynb`_.

.. _Generate_rivers_forcing.ipynb: http://nbviewer.ipython.org/urls/bitbucket.org/salishsea/2d-domain/raw/tip/notebooks/Generate_rivers_forcing.ipynb


Namelists
---------

The namelists need to be modified to reflect the new forcing files and boundary conditions.
See :file:`/data/nsoontie/MEOPAR/2Ddomain/namelists`.

Changes in Resolution
---------------------
To be added
