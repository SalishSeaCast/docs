*************************************************************
Moving an AMM12 Configuration to JPP Configuration : Part One
*************************************************************

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

Clone the nemo-forcing repository from here https://bitbucket.org/salishsea/nemo-forcing

.. note::

   Bottom Line: you need files: SubDom_bathy_meter_NOBCchancomp.nc linked to bathy_meter.nc and
   SubDom_coordinates_seagrid_WestCoast.nc linked to coordinates.nc

Namelist File
-------------

Changes from AMM12 namelist file

* experience name **cn_exp**
* last time step **nn_itend**
* start date **nn_date0**
* frequency of restart file **nn_stock**
* frequency of write in output file **nn_write** *probably superceded*
* DIMG file format **ln_dimgnnn** *probably superceded*

* vertical coordinate **ln_zps** **ln_sco**
* minimum depth of ocean **rn_hmin**
* partial step thickenss **rn_e3sps_min** **rn_e3zps_rat**

* time step for the dynamics **rn_rdt**
* zero surface forcing **ln_ana** **ln_flx**  **rn_utau0** *to be changed later!!*
* zero run off **ln_rnf** *to be changed later!!*
* light penetration **ln_traqsr**

* amount of slip on lateral boundaries **rn_shlat**

* climatological obs files **ln_obc_clim**
* total volume conservation **ln_vol_cst**

* use initial state for open boundary conditions **nn_obcdta** *to be changed later!!*
* time scales for open boundary relaxation **rn_dpeXob**

* turn tidal potential off **ln_tide_pot**
* number of tidal harmonics (for potential) & names **nb_harmo** **clname()** 

* number of open boundary sets **nb_bdy** *to be changed next!*
* name of tide files, component names and speed **filtid** **tide_cpt()** **tide_speed()**

* bottom drag coefficient **rn_bfri2**
* bottom turbulent kinetic energy background **rn_bfeb2**
* geothermal heat flux **nn_geoflx**
* lateral mixing coefficient in the bbl **rn_ahtbbl**
* horizontal eddy diffusivity for tracers **rn_aht_0**

* treatment of hydrostatic pressure **ln_phg_sco** **ln_phg_prj**
* bilaplacian operator **ln_dynldf_bilap**
* horizontal laplacian eddy viscosity **rn_ahm_0_lap**
* vertical eddy viscosity and diffusitivity **rn_avm0** **rn_avt0**
* enhanced vertical diffusion and coefficient **ln_zdfevd** **rn_avevd**
* surface tke input coefficient **rn_ebb**
* surface mixing length scale, Langmuir parameterization **ln_mx10**, **ln_lc**
* penetration of tke below mixed layer and its exponential decrease **nn_etau** **nn_htau**

* maximum interations for the SOR solver and its coefficient **nn_nmax** **rn_sor**

* number of processors in various directions **jpni** **jpnj** **jpnij** 
* control sumes **nn_ictlX** **nn_jctlX**

* time step frequency for dynamics and tracer trends **nn_trd**
* time steps used for harmonic analysis **nit000_han** **nitend_han** **nstep_han**
* names for harmonic analysis **tname()**

.. note::

   Bottom Line: you need to clone the namelist from https://bitbucket.org/salishsea/ss-run-sets see JPP.  This run is based on hg changeset: b501af941889

Run
---

From inside your EXP00 directory, where you have linked to your forcing files you can run

.. code-block:: bash

    mpiexec .opa

Put it Together
---------------


