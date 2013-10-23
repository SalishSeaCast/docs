Moving an AMM12 Configuration to JPP Configuration
==================================================

par_oce
-------

.. note::

   Bottom Line: you need par_JPP.h90 and the new par_oce.F90 from the SALISH_amm/MY_SRC in your MY_SRC


The grid shape for a run is in a par_***.h90. So we need to create a par_JPP.h90 with the correct grid.  These are just the changes

 .. code-block:: fortran

   cp_cfg = "WCSD"         !: name of the configuration
   jp_cfg = 120  ,        &  !: resolution of the configuration (degrees) -- 1/120

   jpidta  = 398,        &  !: first horizontal dimension > or = to jpi
   jpjdta  = 345,        &  !: second                     > or = to jpj
   jpkdta  = 40,         &  !: number of levels           > or = to jpk

   jpisl = 150,           &  !: number of islands
   jpnisl = 3000             !: maximum number of points per island

   ppkth =  25.00_wp        ,  &  !: (non dimensional): gives the approximate
   ppacr =    3.00000000000_wp     !: (non dimensional): stretching factor
                                   !: layer number above which  stretching will
                                   !: be maximum. Usually of order jpk/2.

   ppdzmin = 1._wp           ,  &  !: (meters) vertical thickness of the top layer
   pphmax  = 500._wp              !: (meters) Maximum depth of the ocean gdepw(jpk)

Notes:

 * jp_cfg has no effect as we are reading the coordinate files
 * Islands copied from JPP
 * pp's define vertical grid

And this needs to be referenced in par_oce.F90.  So add:

 .. code-block:: fortran

  #elif defined key_jpp
    !!---------------------------------------------------------------------
    !!   'key_jpp':                        Strait of Georgia Subdomain: JPP 
    !!---------------------------------------------------------------------
  #             include "par_JPP.h90"


Recompile
---------

In your CONFIG directory (assuming you are using SALISH_amm as your config name)

 .. code-block:: bash

    ./makenemo -n SALISH_amm add_key "key_jpp" del_key "key_amm_12km"



Initialization Files
--------------------

.. note::

   Bottom Line: you need files: SubDom_bathy_meter_NOBCchancomp.nc linked to bathy_meter.nc and
   SubDom_coordinates_seagrid_WestCoast.nc linked to coordinates.nc

More details later when we get this repo set-up
