CPP Keys
========

CPP keys used in C Pre-Processor directives in the source code that cause sections of code to be included/excluded from NEMO at the compile stage.


Keys Common to AMM12 and CONCEPTS 110
-------------------------------------

* **key_mpp_mpi** Use multiprocessors
* **key_vvl** Use variable volume coordinates.  This means a nonlinear free surface.
* **key_ldfslp** Computes direction of lateral mixing.


Keys in AMM12 that we need/want
-------------------------------

* **key_bdy** Use the unstructured open boundary conditions.  We need these ones because they include tides.  This replaces **key_obc_mer** the mercator open boundary conditions in CONCEPTS 110
* **key_vectopt_loop** Enables vector optimization.  Good idea!
* **key_netcdf_4** Gives us chunking etc.


Keys in CONCEPTS 110 that we need/want
--------------------------------------

* **key_diaharm** calculate amplitude and phase of tidal components
* **key_tide** Required in addition to **key_diaharm** to enable calculation of amplitude and phase of tidal components.
  Also causes code for tidal potential to be included in the model and we are excluding that effect,
  so it is important to set :kbd:`ln_tide_pot` to :kbd:`.false.` in the :kbd:`nam_tide` namelist.


Decisions
---------
* AMM12 uses **key_dynspg_ts** Time splitting free surface.  CONCEPTS 110 uses **key_dynspg_ts2**.  We my need to return here.
* Turbulence scheme.  CONCEPTS uses **key_zdftke**.  AMM12 uses **key_zdfgls**.  Start with tke and consider later.
* AMM12 uses **key_diainstant** which gives instantaeous fields rather than averages.
* CONCEPTS uses **key_dtatem/key_dtasal** Read climatology initial temperature and salinity fields.  Might be the best way to put in initial profile.
* **key_flx_core** Use short/long wave radiation forcing.  Need to return to this issue later.
* Start with **key_traldf_c2d** horizontal eddy mixing of tracers from CONCEPTS
* Start with **key_dynldf_c3d** 3-dimensional mixing of momentum from CONCEPTS


CONCEPTS 110 that we don't want
-------------------------------
* **key-tide** Use tide potential.  Not likely important in such a small domain.
* **key_zrefsurf** Use surface parameter set by Mercator
* **key_dtatem_month/key_dtasal_month** Read one time frame initial and temperature and salinity fields.  Use with **key_dtatem/key_dtasal**.
