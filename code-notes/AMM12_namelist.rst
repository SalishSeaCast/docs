Understanding the AMM12 Namelist
================================

The file is in sections:

  .. code-block :: bash

   !!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
   !! NEMO/OPA  :  1 - run manager      (namrun)
   !! namelists    2 - Domain           (namzgr, namzgr_sco, namdom, namtsd)
   !!              3 - Surface boundary (namsbc, namsbc_ana, namsbc_flx, namsbc_clio, namsbc_core
   !!                                    namsbc_cpl, namtra_qsr, namsbc_rnf,
   !!                                    namsbc_apr, namsbc_ssr, namsbc_alb)
   !!              4 - lateral boundary (namlbc, namcla, namobc, namagrif, nambdy, nambdy_tide)
   !!              5 - bottom  boundary (nambfr, nambbc, nambbl)
   !!              6 - Tracer           (nameos, namtra_adv, namtra_ldf, namtra_dmp)
   !!              7 - dynamics         (namdyn_adv, namdyn_vor, namdyn_hpg, namdyn_spg, namdyn_ldf)
   !!              8 - Verical physics  (namzdf, namzdf_ric, namzdf_tke, namzdf_kpp, namzdf_ddm, namzdf_tmx)
   !!              9 - diagnostics      (namnc4, namtrd, namspr, namflo, namptr, namhsb)
   !!             10 - miscellaneous    (namsol, nammpp, nammpp_dyndist, namctl)
   !!             11 - Obs & Assim      (namobs, nam_asminc)
   !!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

The first part is the Run Management (note MPI directives are further down, under misc)
  * nn_intend : Number of time steps
  * nn_date0 : Date to start by
  * nn_write : frequency of write in the NetCDF output files
  * nn_stock : restart file frequency
and restart controls.

  .. code-block :: bash

   !!======================================================================
   !!                   ***  Run management namelists  ***
   !!======================================================================
   !!   namrun        parameters of the run
   !!======================================================================
   !
   !-----------------------------------------------------------------------
   &namrun        !   parameters of the run
   !-----------------------------------------------------------------------
      nn_no       =       0   !  job number (no more used...)
      cn_exp      =  "AMM12"  !  experience name
      nn_it000    =       1   !  first time step
      nn_itend    =     576   !  last  time step (std 1 day = 576)
      nn_date0    =  20070101 !  date at nit_0000 (format yyyymmdd) used if ln_rstart=F or (ln_rstart=T and nn_rstctl=0 or 1)
      nn_leapy    =       1   !  Leap year calendar (1) or not (0)
      ln_rstart   =  .false.  !  start from rest (F) or from a restart file (T)
      nn_rstctl   =       0   !  restart control => activated only if ln_rstart = T
                              !    = 0 nn_date0 read in namelist ; nn_it000 : read in namelist
                              !    = 1 nn_date0 read in namelist ; nn_it000 : check consistancy between namelist and restart
                              !    = 2 nn_date0 read in restart  ; nn_it000 : check consistancy between namelist and restart
      cn_ocerst_in  = "restart"   !  suffix of ocean restart name (input)
      cn_ocerst_out = "restart"   !  suffix of ocean restart name (output)
      nn_istate   =       0   !  output the initial state (1) or not (0)
      nn_stock    =     576   !  frequency of creation of a restart file (modulo referenced to 1)
      nn_write    =      12   !  frequency of write in the output file   (modulo referenced to nit000)
      ln_dimgnnn  = .false.   !  DIMG file format: 1 file for all processors (F) or by processor (T)
      ln_mskland  = .false.   !  mask land points in NetCDF outputs (costly: + ~15%)
      ln_clobber  = .false.   !  clobber (overwrite) an existing file
      nn_chunksz  =       0   !  chunksize (bytes) for NetCDF file (works only with iom_nf90 routines)
   /

The next part is setting up the grid and bathymetry
 * vertical coordinate, switch between z and s-coordinates
 * rn_hmin : set minimum ocean depth
 * rn_e3* : to do with partial steps
 * rn_rdt : time step
 * rn_baro : barotropic time steps
 * rn_rd : tracer time steps
 * initialization of TS with input data, and damping back to that data.

  .. code-block :: bash

   !!======================================================================
   !!                      ***  Domain namelists  ***
   !!======================================================================
   !!   namzgr       vertical coordinate
   !!   namzgr_sco   s-coordinate or hybrid z-s-coordinate
   !!   namdom       space and time domain (bathymetry, mesh, timestep)
   !!   namtsd       data: temperature & salinity
   !!======================================================================
   !
   !-----------------------------------------------------------------------
   &namzgr        !   vertical coordinate
   !-----------------------------------------------------------------------
      ln_zco      = .false.   !  z-coordinate - full    steps   (T/F)      ("key_zco" may also be defined)
      ln_zps      = .false.   !  z-coordinate - partial steps   (T/F)
      ln_sco      = .true.    !  s- or hybrid z-s-coordinate    (T/F)
   /
   !-----------------------------------------------------------------------
   &namzgr_sco    !   s-coordinate or hybrid z-s-coordinate
   !-----------------------------------------------------------------------
   NOT IMPORTANT FOR Z-COORDINATES
   /
   !-----------------------------------------------------------------------
   &namdom        !   space and time domain (bathymetry, mesh, timestep)
   !-----------------------------------------------------------------------
      nn_bathy    =    1      !  compute (=0) or read (=1) the bathymetry file
      nn_closea    =   0      !  remove (=0) or keep (=1) closed seas and lakes (ORCA)
      nn_msh      =    0      !  create (=1) a mesh file or not (=0)
      rn_hmin     =   -3.     !  min depth of the ocean (>0) or min number of ocean level (<0)
      rn_e3zps_min=   20.     !  partial step thickness is set larger than the minimum of
      rn_e3zps_rat=    0.1    !  rn_e3zps_min and rn_e3zps_rat*e3t, with 0<rn_e3zps_rat<1
                              !
      rn_rdt      =  150.     !  time step for the dynamics (and tracer if nn_acc=0)
      nn_baro     =   30      !  number of barotropic time step            ("key_dynspg_ts")
      rn_atfp     =    0.1    !  asselin time filter parameter
      nn_acc      =    0      !  acceleration of convergence : =1      used, rdt < rdttra(k)
                                    !                          =0, not used, rdt = rdttra
      rn_rdtmin   =   300.          !  minimum time step on tracers (used if nn_acc=1)
      rn_rdtmax   =   300.          !  maximum time step on tracers (used if nn_acc=1)
      rn_rdth     =  300.           !  depth variation of tracer time step  (used if nn_acc=1)
   /
   !-----------------------------------------------------------------------
   &namtsd    !   data : Temperature  & Salinity
   !-----------------------------------------------------------------------
   !          ! file name ! frequency (hours)    ! variable ! time interp. ! clim  !'yearly' or ! weights  ! rotation !
   !          !           !  (if <0  months)     !   name   !  (logical)   ! (T/F) ! 'monthly'  ! filename ! pairing  !
      sn_tem  = 'data_1m_potential_temperature_nomask', -1,'votemper',  .true.  , .true., 'yearly'   , ' '      , ' '
      sn_sal  = 'data_1m_salinity_nomask'             , -1,'vosaline',  .true.  , .true., 'yearly'   , ''       , ' '
      !
      cn_dir        = './'     !  root directory for the location of the runoff files
      ln_tsd_init   = .false.   !  Initialisation of ocean T & S with T &S input data (T) or not (F)
      ln_tsd_tradmp = .false.   !  damping of ocean T & S toward T &S input data (T) or not (F)
   /

Part 3 is the surface boundary conditions

* ln_flx true sets fluxes from files, in namsbc_flx set file names and characteristics
* ln_rnf sets runoffs, in namsbc_rnf set files and configure river inflow
* left in the penetrative light formulation.  Suspect it is turned off here, but docs not clear.

  .. code-block :: bash

   !!======================================================================
   !!            ***  Surface Boundary Condition namelists  ***
   !!======================================================================
   !!   namsbc          surface boundary condition
   !!   namsbc_ana      analytical         formulation
   !!   namsbc_flx      flux               formulation
   !!   namsbc_clio     CLIO bulk formulae formulation
   !!   namsbc_core     CORE bulk formulae formulation
   !!   namsbc_mfs      MFS  bulk formulae formulation
   !!   namsbc_cpl      CouPLed            formulation                     ("key_coupled")
   !!   namtra_qsr      penetrative solar radiation
   !!   namsbc_rnf      river runoffs
   !!   namsbc_apr      Atmospheric Pressure
   !!   namsbc_ssr      sea surface restoring term (for T and/or S)
   !!   namsbc_alb      albedo parameters
   !!======================================================================
   !
   !-----------------------------------------------------------------------
   &namsbc        !   Surface Boundary Condition (surface module)
   !-----------------------------------------------------------------------
      nn_fsbc     = 1         !  frequency of surface boundary condition computation
                              !     (also = the frequency of sea-ice model call)
      ln_ana      = .false    !  analytical formulation (T => fill namsbc_ana )
      ln_flx      = .true.    !  flux formulation       (T => fill namsbc_flx )
      ln_blk_clio = .false.   !  CLIO bulk formulation                     (T => fill namsbc_clio)
      ln_blk_core = .false.   !  CORE bulk formulation                     (T => fill namsbc_core)
      ln_blk_mfs  = .false.   !  MFS bulk formulation                      (T => fill namsbc_mfs )
      ln_cpl      = .false.   !  Coupled formulation                       (T => fill namsbc_cpl )
      ln_apr_dyn  = .false.   !  Patm gradient added in ocean & ice Eqs.   (T => fill namsbc_apr )
      nn_ice      = 0         !  =0 no ice boundary condition   ,
                              !  =1 use observed ice-cover      ,
                              !  =2 ice-model used                         ("key_lim3" or "key_lim2)
      ln_dm2dc    = .false.   !  daily mean to diurnal cycle on short wave
      ln_rnf      = .true.    !  runoffs                                   (T => fill namsbc_rnf)
      ln_ssr      = .false.   !  Sea Surface Restoring on T and/or S       (T => fill namsbc_ssr)
      nn_fwb      = 0         !  FreshWater Budget: =0 unchecked
                              !     =1 global mean of e-p-r set to zero at each time step
                              !     =2 annual global mean of e-p-r set to zero
                              !     =3 global emp set to zero and spread out over erp area
      ln_cdgw = .false.       !  Neutral drag coefficient read from wave model (T => fill namsbc_wave)
   /
   !-----------------------------------------------------------------------
   &namsbc_ana    !   analytical surface boundary condition
   !-----------------------------------------------------------------------
   NOT USED
   /
   !-----------------------------------------------------------------------
   &namsbc_flx    !   surface boundary condition : flux formulation
   !-----------------------------------------------------------------------
   !              !  file name  ! frequency (hours) ! variable  ! time interp. !  clim  ! 'yearly'/ ! weights  ! rotation !
   !              !             !  (if <0  months)  !   name    !   (logical)  !  (T/F) ! 'monthly' ! filename ! pairing  !
      sn_utau     = 'amm12_utau'     ,          1        ,  'utau'      , .false.      , .false. , 'daily'   ,  ''      ,  ''
      sn_vtau     = 'amm12_vtau'     ,          1        ,  'vtau'      , .false.      , .false. , 'daily'   ,  ''      ,  ''
      sn_qtot     = 'amm12_flx'      ,          3        ,  'sonsfldo'  ,  .true.      , .false. , 'daily'   ,  ''      ,  ''
      sn_qsr      = 'amm12_flx'      ,          3        ,  'soshfldo'  ,  .true.      , .false. , 'daily'   ,  ''      ,  ''
      sn_emp      = 'amm12_flx'      ,          3        ,  'sowafldo'  ,  .true.      , .false. , 'daily'   ,  ''      ,  ''
      cn_dir      = './fluxes/'        !  root directory for the location of the flux files
   /
   !-----------------------------------------------------------------------
   &namsbc_clio   !   namsbc_clio  CLIO bulk formulae
   !-----------------------------------------------------------------------
   NOT USED
   !-----------------------------------------------------------------------
   &namsbc_core   !   namsbc_core  CORE bulk formulae
   !-----------------------------------------------------------------------
   NOT USED
   /
   !-----------------------------------------------------------------------
   &namsbc_mfs   !   namsbc_mfs  MFS bulk formulae
   !-----------------------------------------------------------------------
   NOT USED
   /
   !-----------------------------------------------------------------------
   &namsbc_cpl    !   coupled ocean/atmosphere model                       ("key_coupled")
   !-----------------------------------------------------------------------
   NOT USED
   /
   !-----------------------------------------------------------------------
   &namtra_qsr    !   penetrative solar radiation
   !-----------------------------------------------------------------------
   !              !  file name  ! frequency (hours) ! variable  ! time interp. !  clim  ! 'yearly'/ ! weights  ! rotation !
   !              !             !  (if <0  months)  !   name    !   (logical)  !  (T/F) ! 'monthly' ! filename ! pairing  !
      sn_chl      ='chlorophyll',        -1         , 'CHLA'    ,   .true.     , .true. , 'yearly'  , ''       , ''

      cn_dir      = './'      !  root directory for the location of the runoff files
      ln_traqsr   = .false.   !  Light penetration (T) or not (F)
      ln_qsr_rgb  = .true.    !  RGB (Red-Green-Blue) light penetration
      ln_qsr_2bd  = .false.   !  2 bands              light penetration
      ln_qsr_bio  = .false.   !  bio-model light penetration
      nn_chldta   =      0    !  RGB : Chl data (=1) or cst value (=0)
      rn_abs      =   0.58    !  RGB & 2 bands: fraction of light (rn_si1)
      rn_si0      =   0.35    !  RGB & 2 bands: shortess depth of extinction
      rn_si1      =   23.0    !  2 bands: longest depth of extinction
   /
   !-----------------------------------------------------------------------
   &namsbc_rnf    !   runoffs namelist surface boundary condition
   !-----------------------------------------------------------------------
   !              !  file name           ! frequency (hours) ! variable  ! time interp. !  clim  ! 'yearly'/ ! weights  ! rotation !
   !              !                      !  (if <0  months)  !   name    !   (logical)  !  (T/F) ! 'monthly' ! filename ! pairing  !
      sn_rnf      = 'amm12_rivers'       ,        24         , 'rorunoff',   .false.    , .true. , 'yearly'  , ''       , ''
      sn_cnf      = 'runoff_1m_nomask'   ,         0         , 'socoefr0',   .false.    , .true. , 'yearly'  , ''       , ''
      sn_s_rnf    = 'amm12_rivers'       ,        24         , 'rosaline',   .false.    , .true. , 'yearly'  , ''       , ''
      sn_t_rnf    = 'amm12_rivers'       ,        24         , 'rotemper',   .false.    , .true. , 'yearly'  , ''       , ''
      sn_dep_rnf  = 'amm12_rivers'       ,        24         , 'rodepth' ,   .false.    , .true. , 'yearly'  , ''       , ''

      cn_dir       = './'      !  root directory for the location of the runoff files
      ln_rnf_emp   = .false.   !  runoffs included into precipitation field (T) or into a file (F)
      ln_rnf_mouth = .false.   !  specific treatment at rivers mouths
      rn_hrnf      =  15.e0    !  depth over which enhanced vertical mixing is used
      rn_avt_rnf   =   1.e-3   !  value of the additional vertical mixing coef. [m2/s]
      rn_rfact     =   1.e0    !  multiplicative factor for runoff
      ln_rnf_depth = .true.    !  read in depth information for runoff
      ln_rnf_tem   = .true.    !  read in temperature information for runoff
      ln_rnf_sal   = .true.    !  read in salinity information for runoff
   /
   !-----------------------------------------------------------------------
   &namsbc_apr    !   Atmospheric pressure used as ocean forcing or in bulk
   !-----------------------------------------------------------------------
   NOT USED
   /
   !-----------------------------------------------------------------------
   &namsbc_ssr    !   surface boundary condition : sea surface restoring
   !-----------------------------------------------------------------------
   NOT USED
   /
   !-----------------------------------------------------------------------
   &namsbc_alb    !   albedo parameters
   !-----------------------------------------------------------------------
   FOR ICE, NOT USED
   /

Section Four, Boundary Conditions

* Free slip along coasts rn_shlat = 0
* No cross land advection through thin pennisulas nn_cla = 0
* Open Boundaries: things to change : time relaxation for the different open boundaries
* Check AGRIF conditions: sponge layer in particular
* Unstructured open boundaries, may be able to greatly simplify or even remove
* nambdy_dta open boundary files
* nambdy_tide tide files 

  .. code-block :: bash

   !!======================================================================
   !!               ***  Lateral boundary condition  ***
   !!======================================================================
   !!   namlbc        lateral momentum boundary condition
   !!   namcla        cross land advection
   !!   namobc        open boundaries parameters                           ("key_obc")
   !!   namagrif      agrif nested grid ( read by child model only )       ("key_agrif")
   !!   nambdy        Unstructured open boundaries                         ("key_bdy")
   !!   namtide       Tidal forcing at open boundaries                     ("key_bdy_tides")
   !!======================================================================
   !
   !-----------------------------------------------------------------------
   &namlbc        !   lateral momentum boundary condition
   !-----------------------------------------------------------------------
      rn_shlat    =     0     !  shlat = 0  !  0 < shlat < 2  !  shlat = 2  !  2 < shlat
                              !  free slip  !   partial slip  !   no slip   ! strong slip
      ln_vorlat   = .false.   !  consistency of vorticity boundary condition with analytical eqs.
   /
   !-----------------------------------------------------------------------
   &namcla        !   cross land advection
   !-----------------------------------------------------------------------
      nn_cla      =    0      !  advection between 2 ocean pts separates by land
   /
   !-----------------------------------------------------------------------
   &namobc        !   open boundaries parameters                           ("key_obc")
   !-----------------------------------------------------------------------
      ln_obc_clim = .false.   !  climatological obc data files (T) or not (F)
      ln_vol_cst  = .true.    !  impose the total volume conservation (T) or not (F)
      ln_obc_fla  = .false.   !  Flather open boundary condition
      nn_obcdta   =    1      !  = 0 the obc data are equal to the initial state
                              !  = 1 the obc data are read in 'obc.dta' files
      cn_obcdta   = 'annual'  !  set to annual if obc datafile hold 1 year of data
                              !  set to monthly if obc datafile hold 1 month of data
      rn_dpein    =    1.     !  damping time scale for inflow at east  open boundary
      rn_dpwin    =    1.     !     -           -         -       west    -      -
      rn_dpnin    =    1.     !     -           -         -       north   -      -
      rn_dpsin    =    1.     !     -           -         -       south   -      -
      rn_dpeob    = 3000.     !  time relaxation (days) for the east  open boundary
      rn_dpwob    =   15.     !     -           -         -     west    -      -
      rn_dpnob    = 3000.     !     -           -         -     north   -      -
      rn_dpsob    =   15.     !     -           -         -     south   -      -
      rn_volemp   =    1.     !  = 0 the total volume change with the surface flux (E-P-R)
                              !  = 1 the total volume remains constant
   /
   !-----------------------------------------------------------------------
   &namagrif      !  AGRIF zoom                                            ("key_agrif")
   !-----------------------------------------------------------------------
      nn_cln_update =    3    !  baroclinic update frequency
      ln_spc_dyn    = .true.  !  use 0 as special value for dynamics
      rn_sponge_tra = 2880.   !  coefficient for tracer   sponge layer [m2/s]
      rn_sponge_dyn = 2880.   !  coefficient for dynamics sponge layer [m2/s]
   /
   !-----------------------------------------------------------------------
   &nam_tide      !   tide parameters (#ifdef key_tide)
   !-----------------------------------------------------------------------
      ln_tide_pot   = .true.   !  use tidal potential forcing
      nb_harmo      =    11    !  number of constituents used
      clname(1)     =   'M2'   !  name of constituent
      clname(2)     =   'S2'
      clname(3)     =   'N2'
      clname(4)     =   'K1'
      clname(5)     =   'O1'
      clname(6)     =   'Q1'
      clname(7)     =   'M4'
      clname(8)     =   'K2'
      clname(9)     =   'P1'
      clname(10)    =   'Mf'
      clname(11)    =   'Mm'
   /
   !-----------------------------------------------------------------------
   &nambdy        !  unstructured open boundaries                          ("key_bdy")
   !-----------------------------------------------------------------------
       nb_bdy = 1                            !  number of open boundary sets
       ln_coords_file = .true.               !  =T : read bdy coordinates from file
       cn_coords_file = 'coordinates.bdy.nc' !  bdy coordinates files
       ln_mask_file = .false.                !  =T : read mask from file
       cn_mask_file = ''                     !  name of mask file (if ln_mask_file=.TRUE.)
       nn_dyn2d      =  2                    !  boundary conditions for barotropic fields
       nn_dyn2d_dta  =  2                    !  = 0, bdy data are equal to the initial state
                                             !  = 1, bdy data are read in 'bdydata   .nc' files
                                             !  = 2, use tidal harmonic forcing data from files
                                             !  = 3, use external data AND tidal harmonic forcing
       nn_dyn3d      =  0                    !  boundary conditions for baroclinic velocities
       nn_dyn3d_dta  =  0                    !  = 0, bdy data are equal to the initial state
                              !  = 1, bdy data are read in 'bdydata   .nc' files
       nn_tra        =  1                    !  boundary conditions for T and S
       nn_tra_dta    =  0                    !  = 0, bdy data are equal to the initial state
                              !  = 1, bdy data are read in 'bdydata   .nc' files
       nn_rimwidth  = 10                      !  width of the relaxation zone
       ln_vol     = .false.                  !  total volume correction (see nn_volctl parameter)
       nn_volctl  = 1                        !  = 0, the total water flux across open boundaries is zero
   /
   !-----------------------------------------------------------------------
   &nambdy_dta      !  open boundaries - external data           ("key_bdy")
   !-----------------------------------------------------------------------
   !              !   file name    ! frequency (hours) !  variable  ! time interpol. !  clim   ! 'yearly'/ ! weights  ! rotation !
   !              !                !  (if <0  months)  !    name    !    (logical)   !  (T/F)  ! 'monthly' ! filename ! pairing  !
      bn_ssh =     'amm12_bdyT_u2d' ,         24        , 'sossheig' ,     .true.     , .false. ,  'daily'  ,    ''    ,   ''
      bn_u2d =     'amm12_bdyU_u2d' ,         24        , 'vobtcrtx' ,     .true.     , .false. ,  'daily'  ,    ''    ,   ''
      bn_v2d =     'amm12_bdyV_u2d' ,         24        , 'vobtcrty' ,     .true.     , .false. ,  'daily'  ,    ''    ,   ''
      bn_u3d  =    'amm12_bdyU_u3d' ,         24        , 'vozocrtx' ,     .true.     , .false. ,  'daily'  ,    ''    ,   ''
      bn_v3d  =    'amm12_bdyV_u3d' ,         24        , 'vomecrty' ,     .true.     , .false. ,  'daily'  ,    ''    ,   ''
      bn_tem  =    'amm12_bdyT_tra' ,         24        , 'votemper' ,     .true.     , .false. ,  'daily'  ,    ''    ,   ''
      bn_sal  =    'amm12_bdyT_tra' ,         24        , 'vosaline' ,     .true.     , .false. ,  'daily'  ,    ''    ,   ''
      cn_dir  =    'bdydta/'
      ln_full_vel = .false.
   /
   !-----------------------------------------------------------------------
   &nambdy_tide     ! tidal forcing at open boundaries
   !-----------------------------------------------------------------------
      filtide      = 'bdydta/amm12_bdytide_'         !  file name root of tidal forcing files
       tide_cpt(1)   ='Q1'  !  names of tidal components used
       tide_cpt(2)   ='O1'  !  names of tidal components used
       tide_cpt(3)   ='P1'  !  names of tidal components used
       tide_cpt(4)   ='S1'  !  names of tidal components used
       tide_cpt(5)   ='K1'  !  names of tidal components used
       tide_cpt(6)   ='2N2' !  names of tidal components used
       tide_cpt(7)   ='MU2' !  names of tidal components used
       tide_cpt(8)   ='N2'  !  names of tidal components used
       tide_cpt(9)   ='NU2' !  names of tidal components used
       tide_cpt(10)   ='M2'  !  names of tidal components used
       tide_cpt(11)   ='L2'  !  names of tidal components used
       tide_cpt(12)   ='T2'  !  names of tidal components used
       tide_cpt(13)   ='S2'  !  names of tidal components used
       tide_cpt(14)   ='K2'  !  names of tidal components used
       tide_cpt(15)   ='M4'  !  names of tidal components used
       tide_speed(1)   = 13.398661 !  phase speeds of tidal components (deg/hour)
       tide_speed(2)   = 13.943036 !  phase speeds of tidal components (deg/hour)
       tide_speed(3)   = 14.958932 !  phase speeds of tidal components (deg/hour)
       tide_speed(4)   = 15.000001 !  phase speeds of tidal components (deg/hour)
       tide_speed(5)   = 15.041069 !  phase speeds of tidal components (deg/hour)
       tide_speed(6)   = 27.895355 !  phase speeds of tidal components (deg/hour)
       tide_speed(7)   = 27.968210 !  phase speeds of tidal components (deg/hour)
       tide_speed(8)   = 28.439730 !  phase speeds of tidal components (deg/hour)
       tide_speed(9)   = 28.512585 !  phase speeds of tidal components (deg/hour)
       tide_speed(10)   = 28.984106 !  phase speeds of tidal components (deg/hour)
       tide_speed(11)   = 29.528479 !  phase speeds of tidal components (deg/hour)
       tide_speed(12)   = 29.958935 !  phase speeds of tidal components (deg/hour)
       tide_speed(13)   = 30.000002 !  phase speeds of tidal components (deg/hour)
       tide_speed(14)   = 30.082138 !  phase speeds of tidal components (deg/hour)
       tide_speed(15)   = 57.968212 !  phase speeds of tidal components (deg/hour)
       ln_tide_date = .true.               !  adjust tidal harmonics for start date of run
   /


Section 5 : bottom boundaries

* probably don't have to change anything here on first cut.  May need to do later to get tides correct

  .. code-block :: bash

   !!======================================================================
   !!                 ***  Bottom boundary condition  ***
   !!======================================================================
   !!   nambfr        bottom friction
   !!   nambbc        bottom temperature boundary condition
   !!   nambbl        bottom boundary layer scheme                         ("key_trabbl")
   !!======================================================================
   !
   !-----------------------------------------------------------------------
   &nambfr        !   bottom friction
   !-----------------------------------------------------------------------
      nn_bfr      =    2      !  type of bottom friction :   = 0 : free slip,  = 1 : linear friction
                              !                              = 2 : nonlinear friction
      rn_bfri1    =    4.e-4  !  bottom drag coefficient (linear case)
      rn_bfri2    =    2.5e-3 !  bottom drag coefficient (non linear case)
      rn_bfeb2    =    0.0e0  !  bottom turbulent kinetic energy background  (m2/s2)
      ln_bfr2d    = .false.   !  horizontal variation of the bottom friction coef (read a 2D mask file )
      rn_bfrien   =    50.    !  local multiplying factor of bfr (ln_bfr2d=T)
      ln_bfrimp   = .true.    !  implicit bottom friction (requires ln_zdfexp = .false. if true)
   /
   !-----------------------------------------------------------------------
   &nambbc        !   bottom temperature boundary condition
   !-----------------------------------------------------------------------
      ln_trabbc   = .false.   !  Apply a geothermal heating at the ocean bottom
      nn_geoflx   =    2      !  geothermal heat flux: = 0 no flux
                              !     = 1 constant flux
                              !     = 2 variable flux (read in geothermal_heating.nc in mW/m2)
      rn_geoflx_cst = 86.4e-3 !  Constant value of geothermal heat flux [W/m2]
   /
   !-----------------------------------------------------------------------
   &nambbl        !   bottom boundary layer scheme
   !-----------------------------------------------------------------------
      nn_bbl_ldf  =  0      !  diffusive bbl (=1)   or not (=0)
      nn_bbl_adv  =  0      !  advective bbl (=1/2) or not (=0)
      rn_ahtbbl   =  1000.  !  lateral mixing coefficient in the bbl  [m2/s]
      rn_gambbl   =  10.    !  advective bbl coefficient                 [s]
   /

Section 6 : Tracers

* probably nothing to change here (No TEOS!)

  .. code-block :: bash

   !!======================================================================
   !!                        Tracer (T & S ) namelists
   !!======================================================================
   !!   nameos        equation of state
   !!   namtra_adv    advection scheme
   !!   namtra_ldf    lateral diffusion scheme
   !!   namtra_dmp    T & S newtonian damping
   !!======================================================================
   !
   !-----------------------------------------------------------------------
   &nameos        !   ocean physical parameters
   !-----------------------------------------------------------------------
      nn_eos      =   0       !  type of equation of state and Brunt-Vaisala frequency
                              !     = 0, UNESCO (formulation of Jackett and McDougall (1994) and of McDougall (1987) )
                              !     = 1, linear: rho(T)   = rau0 * ( 1.028 - ralpha * T )
                              !     = 2, linear: rho(T,S) = rau0 * ( rbeta * S - ralpha * T )
      rn_alpha    =   2.0e-4  !  thermal expension coefficient (nn_eos= 1 or 2)
      rn_beta     =   7.7e-4  !  saline  expension coefficient (nn_eos= 2)
   /
   !-----------------------------------------------------------------------
   &namtra_adv    !   advection scheme for tracer
   !-----------------------------------------------------------------------
      ln_traadv_cen2   =  .false.  !  2nd order centered scheme
      ln_traadv_tvd    =  .true.   !  TVD scheme
      ln_traadv_muscl  =  .false.  !  MUSCL scheme
      ln_traadv_muscl2 =  .false.  !  MUSCL2 scheme + cen2 at boundaries
      ln_traadv_ubs    =  .false.  !  UBS scheme
      ln_traadv_qck    =  .false.  !  QUICKEST scheme
   /
   !----------------------------------------------------------------------------------
   &namtra_ldf    !   lateral diffusion scheme for tracers
   !----------------------------------------------------------------------------------
      !                       !  Operator type:
      ln_traldf_lap    =  .true.   !  laplacian operator
      ln_traldf_bilap  =  .false.  !  bilaplacian operator
      !                       !  Direction of action:
      ln_traldf_level  =  .false.  !  iso-level
      ln_traldf_hor    =  .true.   !  horizontal (geopotential)   (needs "key_ldfslp" when ln_sco=T)
      ln_traldf_iso    =  .false.  !  iso-neutral                 (needs "key_ldfslp")
      !		       	   !  Griffies parameters              (all need "key_ldfslp")
      ln_traldf_grif   =  .false.  !  use griffies triads
      ln_traldf_gdia   =  .false.  !  output griffies eddy velocities
      ln_triad_iso     =  .false.  !  pure lateral mixing in ML
      ln_botmix_grif   =  .false.  !  lateral mixing on bottom
      !                       !  Coefficients
      ! Eddy-induced (GM) advection always used with Griffies; otherwise needs "key_traldf_eiv"
      ! Value rn_aeiv_0 is ignored unless = 0 with Held-Larichev spatially varying aeiv
      !                                  (key_traldf_c2d & key_traldf_eiv & key_orca_r2, _r1 or _r05)
      rn_aeiv_0        =     0.    !  eddy induced velocity coefficient [m2/s]
      rn_aht_0         =    50.    !  horizontal eddy diffusivity for tracers [m2/s]
      rn_ahtb_0        =     0.    !  background eddy diffusivity for ldf_iso [m2/s]
      !                                           (normally=0; not used with Griffies)
   /
   !-----------------------------------------------------------------------
   &namtra_dmp    !   tracer: T & S newtonian damping
   !-----------------------------------------------------------------------
      ln_tradmp   =  .false.  !  add a damping termn (T) or not (F)
   REST NOT USED
   /

7. Dynamics

* note: cpp keys matter here too
* AMM12 has split-explicit free surface key_dynspg_ts
* key_ldfslp for s-coordinates, won't need in z
* key_zdfgls for GLS vertical mixing
* Hydrostatic pressure depends on z coordinate ln_hpg 
* Horz Eddy viscosity set here rn_ahm_0_lap = 60.0 m2/s
* Vert Eddy viscosity/diffusivity rn_avt0, rn_avm0 = 0.1e-6 m2/s

  .. code-block :: bash

   !!======================================================================
   !!                      ***  Dynamics namelists  ***
   !!======================================================================
   !!   namdyn_adv    formulation of the momentum advection
   !!   namdyn_vor    advection scheme
   !!   namdyn_hpg    hydrostatic pressure gradient
   !!   namdyn_spg    surface pressure gradient                            (CPP key only)
   !!   namdyn_ldf    lateral diffusion scheme
   !!======================================================================
   !
   !-----------------------------------------------------------------------
   &namdyn_adv    !   formulation of the momentum advection
   !-----------------------------------------------------------------------
      ln_dynadv_vec = .true.  !  vector form (T) or flux form (F)
      ln_dynadv_cen2= .false. !  flux form - 2nd order centered scheme
      ln_dynadv_ubs = .false. !  flux form - 3rd order UBS      scheme
   /
   !-----------------------------------------------------------------------
   &namdyn_vor    !   option of physics/algorithm (not control by CPP keys)
   !-----------------------------------------------------------------------
      ln_dynvor_ene = .false. !  energy    conserving scheme  
      ln_dynvor_ens = .false. !  enstrophy conserving scheme    
      ln_dynvor_mix = .false. !  mixed scheme
      ln_dynvor_een = .true.  !  energy & enstrophy scheme
   /
   !-----------------------------------------------------------------------
   &namdyn_hpg    !   Hydrostatic pressure gradient option
   !-----------------------------------------------------------------------
      ln_hpg_zco  = .false.   !  z-coordinate - full steps
      ln_hpg_zps  = .false.   !  z-coordinate - partial steps (interpolation)
      ln_hpg_sco  = .true.    !  s-coordinate (standard jacobian formulation)
      ln_hpg_djc  = .false.   !  s-coordinate (Density Jacobian with Cubic polynomial)
      ln_hpg_prj  = .false.   !  s-coordinate (Pressure Jacobian scheme)
      ln_dynhpg_imp = .false. !  time stepping: semi-implicit time scheme  (T)
                                    !           centered      time scheme  (F)
   /
   !-----------------------------------------------------------------------
   !namdyn_spg    !   surface pressure gradient   (CPP key only)
   !-----------------------------------------------------------------------
   !                          !  explicit free surface                     ("key_dynspg_exp")
   !                          !  filtered free surface                     ("key_dynspg_flt")
   !                          !  split-explicit free surface               ("key_dynspg_ts")

   !-----------------------------------------------------------------------
   &namdyn_ldf    !   lateral diffusion on momentum
   !-----------------------------------------------------------------------
      !                       !  Type of the operator :
      ln_dynldf_lap    =  .true.   !  laplacian operator
      ln_dynldf_bilap  =  .true.   !  bilaplacian operator
                              !  Direction of action  :
      ln_dynldf_level  =  .false.  !  iso-level
      ln_dynldf_hor    =  .true.   !  horizontal (geopotential)            (require "key_ldfslp" in s-coord.)
      ln_dynldf_iso    =  .false.  !  iso-neutral                          (require "key_ldfslp")
                              !  Coefficient
      rn_ahm_0_lap     = 60.0      !  horizontal laplacian eddy viscosity   [m2/s]
      rn_ahmb_0        =  0.0      !  background eddy viscosity for ldf_iso [m2/s]
      rn_ahm_0_blp     = -1.0e+10  !  horizontal bilaplacian eddy viscosity [m4/s]
   /

   !!======================================================================
   !!             Tracers & Dynamics vertical physics namelists
   !!======================================================================
   !!    namzdf        vertical physics
   !!    namzdf_ric    richardson number dependent vertical mixing         ("key_zdfric")
   !!    namzdf_tke    TKE dependent vertical mixing                       ("key_zdftke")
   !!    namzdf_kpp    KPP dependent vertical mixing                       ("key_zdfkpp")
   !!    namzdf_ddm    double diffusive mixing parameterization            ("key_zdfddm")
   !!    namzdf_tmx    tidal mixing parameterization                       ("key_zdftmx")
   !!======================================================================
   !
   !-----------------------------------------------------------------------
   &namzdf        !   vertical physics
   !-----------------------------------------------------------------------
      rn_avm0     =   0.1e-6  !  vertical eddy viscosity   [m2/s]          (background Kz if not "key_zdfcst")
      rn_avt0     =   0.1e-6  !  vertical eddy diffusivity [m2/s]          (background Kz if not "key_zdfcst")
      nn_avb      =    0      !  profile for background avt & avm (=1) or not (=0)
      nn_havtb    =    0      !  horizontal shape for avtb (=1) or not (=0)
      ln_zdfevd   = .false.   !  enhanced vertical diffusion (evd) (T) or not (F)
      nn_evdm     =    1      !  evd apply on tracer (=0) or on tracer and momentum (=1)
      rn_avevd    =  100.     !  evd mixing coefficient [m2/s]
      ln_zdfnpc   = .false.   !  Non-Penetrative Convective algorithm (T) or not (F)
      nn_npc      =    1            !  frequency of application of npc
      nn_npcp     =  365            !  npc control print frequency
      ln_zdfexp   = .false.   !  time-stepping: split-explicit (T) or implicit (F) time stepping
      nn_zdfexp   =    3            !  number of sub-timestep for ln_zdfexp=T
   /
   !-----------------------------------------------------------------------
   &namzdf_ric    !   richardson number dependent vertical diffusion       ("key_zdfric" )
   !-----------------------------------------------------------------------
   NOT USED
   /
   !-----------------------------------------------------------------------
   &namzdf_tke    !   turbulent eddy kinetic dependent vertical diffusion  ("key_zdftke")
   !-----------------------------------------------------------------------
   NOT USED
   /
   !------------------------------------------------------------------------
   &namzdf_kpp    !   K-Profile Parameterization dependent vertical mixing  ("key_zdfkpp", and optionally:
   !------------------------------------------------------------------------ "key_kppcustom" or "key_kpplktb")
   NOT USED
   /
   !-----------------------------------------------------------------------
   &namzdf_gls                !   GLS vertical diffusion                   ("key_zdfgls")
   !-----------------------------------------------------------------------
      rn_emin       = 1.e-6   !  minimum value of e   [m2/s2]
      rn_epsmin     = 1.e-12  !  minimum value of eps [m2/s3]
      ln_length_lim = .true.  !  limit on the dissipation rate under stable stratification (Galperin et al., 1988)
      rn_clim_galp  = 0.53    !  galperin limit
      ln_crban      = .true.  !  Use Craig & Banner (1994) surface wave mixing parametrisation
      ln_sigpsi     = .true.  !  Activate or not Burchard 2001 mods on psi schmidt number in the wb case
      rn_crban      = 100.    !  Craig and Banner 1994 constant for wb tke flux
      rn_charn =  100000.     !  Charnock constant for wb induced roughness length
      nn_tkebc_surf =   1     !  surface tke condition (0/1/2=Dir/Neum/Dir Mellor-Blumberg)
      nn_tkebc_bot  =   1     !  bottom tke condition (0/1=Dir/Neum)
      nn_psibc_surf =   1     !  surface psi condition (0/1/2=Dir/Neum/Dir Mellor-Blumberg)
      nn_psibc_bot  =   1     !  bottom psi condition (0/1=Dir/Neum)
      nn_stab_func  =   2     !  stability function (0=Galp, 1= KC94, 2=CanutoA, 3=CanutoB)
      nn_clos       =   1     !  predefined closure type (0=MY82, 1=k-eps, 2=k-w, 3=Gen)
   /
   !-----------------------------------------------------------------------
   &namzdf_ddm    !   double diffusive mixing parameterization             ("key_zdfddm")
   !-----------------------------------------------------------------------
   NOT USED 
   /
   !-----------------------------------------------------------------------
   &namzdf_tmx    !   tidal mixing parameterization                        ("key_zdftmx")
   !-----------------------------------------------------------------------
   NOT USED
   /

9. Diagnostics (see below (switched order in this namelist)

10. Misc.

* mpi settings for blocks are here, jpni, jpnj, jpnij

  .. code-block :: bash

   !!======================================================================
   !!                  ***  Miscellaneous namelists  ***
   !!======================================================================
   !!   nammpp            Massively Parallel Processing                    ("key_mpp_mpi)
   !!   namctl            Control prints & Benchmark
   !!   namsol            elliptic solver / island / free surface
   !!======================================================================
   !
   !-----------------------------------------------------------------------
   &namsol        !   elliptic solver / island / free surface
   !-----------------------------------------------------------------------
      nn_solv     =      1    !  elliptic solver: =1 preconditioned conjugate gradient (pcg)
                              !                   =2 successive-over-relaxation (sor)
      nn_sol_arp  =      0    !  absolute/relative (0/1) precision convergence test
      rn_eps      =  1.e-6    !  absolute precision of the solver
      nn_nmin     =    300    !  minimum of iterations for the SOR solver
      nn_nmax     =    800    !  maximum of iterations for the SOR solver
      nn_nmod     =     10    !  frequency of test for the SOR solver
      rn_resmax   =  1.e-10   !  absolute precision for the SOR solver
      rn_sor      =  1.92     !  optimal coefficient for SOR solver (to be adjusted with the domain)
   /
   !-----------------------------------------------------------------------
   &nammpp        !   Massively Parallel Processing                        ("key_mpp_mpi)
   !-----------------------------------------------------------------------
      cn_mpi_send =  'I'      !  mpi send/recieve type   ='S', 'B', or 'I' for standard send,
                              !  buffer blocking send or immediate non-blocking sends, resp.
      nn_buffer   =   0       !  size in bytes of exported buffer ('B' case), 0 no exportation
      ln_nnogather=  .false.  !  activate code to avoid mpi_allgather use at the northfold
      jpni        =   0       !  jpni   number of processors following i (set automatically if < 1)
      jpnj        =   0       !  jpnj   number of processors following j (set automatically if < 1)
      jpnij       =   0       !  jpnij  number of local domains (set automatically if < 1)
   /
   !-----------------------------------------------------------------------
   &namctl        !   Control prints & Benchmark
   !-----------------------------------------------------------------------
      ln_ctl      = .false.   !  trends control print (expensive!)
      nn_print    =    0      !  level of print (0 no extra print)
      nn_ictls    =    0      !  start i indice of control sum (use to compare mono versus
      nn_ictle    =    0      !  end   i indice of control sum        multi processor runs
      nn_jctls    =    0      !  start j indice of control               over a subdomain)
      nn_jctle    =    0      !  end   j indice of control
      nn_isplt    =    1      !  number of processors in i-direction
      nn_jsplt    =    1      !  number of processors in j-direction
      nn_bench    =    0      !  Bench mode (1/0): CAUTION use zero except for bench
                              !     (no physical validity of the results)
      nn_timing   =    1      !  timing by routine activated (=1) creates timing.output file, or not (=0)
   /

9. Diagnostics

* NetCDF chunking and compressions set here nn_nchunks
* Float parameters would be set here too
* Harmonic analysis of tidal constituents set here!

  .. code-block :: bash


   !!======================================================================
   !!                  ***  Diagnostics namelists  ***
   !!======================================================================
   !!   namnc4       netcdf4 chunking and compression settings             ("key_netcdf4")
   !!   namtrd       dynamics and/or tracer trends                         ("key_trddyn","key_trdtra","key_trdmld")
   !!   namflo       float parameters                                      ("key_float")
   !!   namptr       Poleward Transport Diagnostics
   !!   namhsb       Heat and salt budgets
   !!======================================================================
   !
   !-----------------------------------------------------------------------
   &namnc4        !   netcdf4 chunking and compression settings            ("key_netcdf4")
   !-----------------------------------------------------------------------
      nn_nchunks_i=   4       !  number of chunks in i-dimension
      nn_nchunks_j=   4       !  number of chunks in j-dimension
      nn_nchunks_k=   31      !  number of chunks in k-dimension
                              !  setting nn_nchunks_k = jpk will give a chunk size of 1 in the vertical which
                              !  is optimal for postprocessing which works exclusively with horizontal slabs
      ln_nc4zip   = .true.    !  (T) use netcdf4 chunking and compression
                              !  (F) ignore chunking information and produce netcdf3-compatible files
   /
   !-----------------------------------------------------------------------
   &namtrd        !   diagnostics on dynamics and/or tracer trends         ("key_trddyn" and/or "key_trdtra")
   !              !       or mixed-layer trends or barotropic vorticity    ("key_trdmld" or     "key_trdvor")
   !-----------------------------------------------------------------------
   NOT USED
   /
   !-----------------------------------------------------------------------
   &namgap       !   level mean model-data gap                             ('key_diagap')
   !-----------------------------------------------------------------------
   NOT USED   
   /
   !-----------------------------------------------------------------------
   &namflo       !   float parameters                                      ("key_float")
   !-----------------------------------------------------------------------
   NOT USED
   /
   !-----------------------------------------------------------------------
   &namptr       !   Poleward Transport Diagnostic
   !-----------------------------------------------------------------------
      ln_diaptr  = .false.    !  Poleward heat and salt transport (T) or not (F)
      ln_diaznl  = .false.    !  Add zonal means and meridional stream functions
      ln_subbas  = .false.    !  Atlantic/Pacific/Indian basins computation (T) or not
                              !  (orca configuration only, need input basins mask file named "subbasins.nc"
      ln_ptrcomp = .false.    !  Add decomposition : overturning
      nn_fptr    =  1         !  Frequency of ptr computation [time step]
      nn_fwri    =  15        !  Frequency of ptr outputs [time step]
   /
   !-----------------------------------------------------------------------
   &namhsb       !  Heat and salt budgets
   !-----------------------------------------------------------------------
      ln_diahsb  = .false.    !  check the heat and salt budgets (T) or not (F)
   /
   !-----------------------------------------------------------------------
   &nam_diaharm   !   Harmonic analysis of tidal constituents ('key_diaharm')
   !-----------------------------------------------------------------------
       nit000_han = 1         ! First time step used for harmonic analysis
       nitend_han = 75        ! Last time step used for harmonic analysis
       nstep_han  = 15        ! Time step frequency for harmonic analysis
       tname(1)   = 'M2'      ! Name of tidal constituents
       tname(2)   = 'K1'
   /
   !-----------------------------------------------------------------------
   &namdct        ! transports through sections
   !-----------------------------------------------------------------------
       nn_dct      = 15       !  time step frequency for transports computing
       nn_dctwri   = 15       !  time step frequency for transports writing
       nn_secdebug = 112      !      0 : no section to debug
                              !     -1 : debug all section
                              !  0 < n : debug section number n

11. Assimilation and Observation

* no changes here

  .. code-block :: bash

   /
   !!======================================================================
   !!            ***  Observation & Assimilation namelists ***
   !!======================================================================
   !!   namobs       observation and model comparison                      ('key_diaobs')
   !!   nam_asminc   assimilation increments                               ('key_asminc')
   !!======================================================================
   !
   !-----------------------------------------------------------------------
   &namobs       !  observation usage switch                               ('key_diaobs')
   !-----------------------------------------------------------------------
   NOT USED
   /
   !-----------------------------------------------------------------------
   &nam_asminc   !   assimilation increments                               ('key_asminc')
   !-----------------------------------------------------------------------
   NOT USED
   /
   !-----------------------------------------------------------------------
   &namsbc_wave   ! External fields from wave model
   !-----------------------------------------------------------------------
   NOT USED
   /
   !-----------------------------------------------------------------------
   &namdyn_nept  !   Neptune effect (simplified: lateral and vertical diffusions removed)
   !-----------------------------------------------------------------------
      ! Suggested lengthscale values are those of Eby & Holloway (1994) for a coarse model
      ln_neptsimp       = .false.  ! yes/no use simplified neptune
   REST NOT USED
   /
   !-----------------------------------------------------------------------
   &namtrj ! Handling non-linear trajectory for TAM (output for direct model, input for TAM)
   !-----------------------------------------------------------------------
   NOT USED
   /

