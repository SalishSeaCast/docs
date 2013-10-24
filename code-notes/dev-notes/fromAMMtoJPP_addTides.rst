****************************************************************************
Moving an AMM12 Configuration to JPP Configuration : Part Two: Add the Tides
****************************************************************************

At the end of part one, we had working code with nothing but noise.  Now we
need to add the tides.

Tide Forcing Files
------------------

CONCEPTS 110 uses tide files that give the tides over the whole domain as amplitude and phase. Flow amplitude is in vertically integrated flux.

NEMO 3.4 uses tide files that give the tides only on the open boundary as cosine and sine components. Flow amplitudesa are velocities.

In `Prepare Forcing Files`_ I calculate the latter from the former.

.. _Prepare Forcing Files: https://bitbucket.org/salishsea/tools/src/tip/I_ForcingFiles/Prepare%20Tide%20Files.ipynb?at=default

This produces three files : JPP_bdytide_M2_grid_X.nc where X = T, U or V.  These files are available in the `nemo-forcing repository`_ in the bdydta folder.

.. _nemo-forcing repository: https://bitbucket.org/salishsea/nemo-forcing

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
