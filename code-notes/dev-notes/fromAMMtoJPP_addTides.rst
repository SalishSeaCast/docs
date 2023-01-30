****************************************************************************
Moving an AMM12 Configuration to JPP Configuration : Part Two: Add the Tides
****************************************************************************

At the end of part one, we had working code with nothing but noise.  Now we
need to add the tides.

Tide Forcing Files
------------------

CONCEPTS 110 uses tide files that give the tides over the whole domain as amplitude and phase. Flow amplitude is in vertically integrated flux.

NEMO 3.4 uses tide files that give the tides only on the open boundary as cosine and sine components. Flow amplitudes are velocities.

In `Prepare Forcing Files`_ I calculate the latter from the former.

.. _Prepare Forcing Files: https://nbviewer.org/github/SalishSeaCast/tools/blob/main/I_ForcingFiles/Tides/Prepare%20Tide%20Files.ipynb

This produces three files : JPP_bdytide_M2_grid_X.nc where X = T, U or V.  These files are available in the `nemo-forcing repository`_ in the bdydta folder.

.. _nemo-forcing repository: https://github.com/SalishSeaCast/NEMO-Forcing

cpp Keys
--------

 We have

.. code-block:: bash

   key_bdy key_vectopt_loop key_dynspg_ts key_ldfslp key_zdfgls key_vvl key_diainstant key_mpp_mpi key_netcdf4 key_nosignedzero key_jpp

We need to

.. code-block:: bash

   add_key "key_diaharm key_tide key_zdftke key_traldf_c2d key_dynldf_c3d" del_key "key_zdfgls"

So we rebuild (on Salish):

.. code-block:: bash

   ./makenemo -m salish -r SALISH_amm -n SALISH_JPP -j8 add_key "key_diaharm key_tide key_zdftke key_traldf_c2d key_dynldf_c3d" del_key "key_zdfgls"

namelist
--------

Change:

.. code-block:: bash

    nb_bdy = 1                            !  number of open boundary sets
    ln_coords_file = .false.              !  =T : read bdy coordinates from file
    cn_coords_file = ''                   !  bdy coordinates files

    !-----------------------------------------------------------------------
    &nambdy_index ! open boundaries - definition ("key_bdy")
    !-----------------------------------------------------------------------
        nbdysege = 0
        nbdysegw = -1
        nbdysegn = -1
        nbdysegs = 0
    /

        filtide      = 'bdydta/JPP_bdytide_'                !  file name root of tidal forcing files

Making it Work
==============

The above changes gave a code that ran for 295 time steps.  It blew up with a large horizontal velocity error located at i=217, j=97 which is on the ridge north of the Chatham Island.  Much work ensued but the final working version had the following changes. All this testing was done on Salish using 16 processors (4x4)

Turbulence Scheme
-----------------

Turbulence scheme was changed back to GLS from TKE.  This requires a change in key from key_zdftke to key_zdfgls.

Horizontal Laplacian Viscosity
------------------------------

This was increased from 20.5 m2/s to 200 m2/s.  Variable is called rn_ahm_0_lap.  Increasing bottom friction (rn_bfeb2 and rn_bfri2) seems to increase the instability.

Topography Smoothing
--------------------

Topography was hand smoothed in Chatham Islands region, Porlier Pass and in a deep basin south of Victoria.  Details available in `SalishSeaSubdomainBathy.ipynb`_.

.. _SalishSeaSubdomainBathy.ipynb: https://nbviewer.org/github/SalishSeaCast/tools/blob/main/bathymetry/SalishSeaSubdomainBathy.ipynb
