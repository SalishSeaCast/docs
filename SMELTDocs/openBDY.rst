.. _openBDY:

Open Boundary Conditions for Tracers
====================================

Initially, several adaptations to the NEMO code were necessary in order to use BDY open boundary conditions on tracers.
These were carried out using the NEMO development branch, `dev_r5144_CMCC5_BDY_for_TOP
<https://forge.ipsl.jussieu.fr/nemo/browser/branches/2015/dev_r5144_CMCC5_BDY_for_TOP>`_, as a template. The `ticket
<https://forge.ipsl.jussieu.fr/nemo/ticket/1441>`_ associated
with that branch has since been closed.

Code Changes
------------

Currently, our code differs from the distribution in the following ways. The output of diff operations
between our configuration version and the standard version of each file is shown.

MY_SRC$ diff bdy_oce.F90 ../../../NEMO/OPA_SRC/BDY/bdy_oce.F90

.. code-block:: diff

    73,78d72
    < #if defined key_top
    < 	  CHARACTER(LEN=20)                   :: cn_obc  !: type of boundary condition to apply
    < 	  REAL(wp)                            :: rn_fac  !: multiplicative scaling factor
    < 	  REAL(wp), POINTER, DIMENSION(:,:)   :: trc     !: now field of the tracer
    < 	  LOGICAL                             :: dmp     !: obc damping term
    < #endif
    111d104
    <    REAL(wp),    DIMENSION(jp_bdy) ::   rn_max_sponge          !: Maximum viscosity for sponge (m2/s)

MY_SRC$ diff trc.F90 ../../../NEMO/TOP_SRC/trc.F90
::

    16,18d15
    < #if defined key_bdy
    <    USE bdy_oce, only: nb_bdy, OBC_DATA
    < #endif
    96,100d92
    <        !#if defined  key_bdy
    <        LOGICAL              :: llobc   !: read in a file or not
    <        LOGICAL              :: llsbc   !: read in a file or not
    <        LOGICAL              :: llcbc   !: read in a file or not
    <        !#endif
    201,209d192
    < #if defined key_bdy
    <    CHARACTER(len=20), PUBLIC, ALLOCATABLE,  SAVE,  DIMENSION(:)   ::  cn_trc_dflt          ! Default OBC condition for all tracers
    <    CHARACTER(len=20), PUBLIC, ALLOCATABLE,  SAVE,  DIMENSION(:)   ::  cn_trc               ! Choice of boundary condition for tracers
    <    INTEGER,           PUBLIC, ALLOCATABLE,  SAVE,  DIMENSION(:)   ::  nn_trcdmp_bdy        !: =T Tracer damping
    <    ! External data structure of BDY for TOP. Available elements: cn_obc, ll_trc, trcnow, dmp
    <    TYPE(OBC_DATA),    PUBLIC, ALLOCATABLE, DIMENSION(:,:), TARGET ::  trcdta_bdy           !: bdy external data (local process)
    < #endif
    232,238c215,216
    <          &      ln_trc_ini(jptra)     , ln_trc_wri(jptra)     , qsr_mean(jpi,jpj)     ,       &
    <          &      ln_trc_sbc(jptra)     , ln_trc_cbc(jptra)     , ln_trc_obc(jptra)     ,       &
    < #if defined key_bdy
    <          &      cn_trc_dflt(nb_bdy)   , cn_trc(nb_bdy)        , nn_trcdmp_bdy(nb_bdy) ,       &
    <          &      trcdta_bdy(jptra,nb_bdy)                                              ,       &
    < #endif
    <          &      STAT = trc_alloc  )
    ---
    >          &      ln_trc_ini(jptra)     , ln_trc_wri(jptra)     , qsr_mean(jpi,jpj)     ,  STAT = trc_alloc  )
    >


MY_SRC$ diff trcini.F90 ../../../NEMO/TOP_SRC/trcini.F90
::

    34d33
    <    USE trcbc,   only : trc_bc_init ! generalized Boundary Conditions
    113,116c112
    < #if defined key_bdy
    <       ! Initialisation of tracers Boundary Conditions
    <       CALL trc_bc_init(jptra)
    < #endif

MY_SRC$ diff trcnam.F90 ../../../NEMO/TOP_SRC/trcnam.F90
::

    306,310d305
    <           !#if defined key_bdy
    <          ln_trc_sbc(jn) =       sn_tracer(jn)%llsbc
    <          ln_trc_cbc(jn) =       sn_tracer(jn)%llcbc
    <          ln_trc_obc(jn) =       sn_tracer(jn)%llobc
    <            !#endif

MY_SRC$ diff trcnxt.F90 ../../../NEMO/TOP_SRC/TRP/trcnxt.F90
::

    35,38d34
    < # if defined key_bdy
    <    USE trcbdy          ! BDY open boundaries
    <    USE bdy_par, only: lk_bdy
    < # endif
    118c114
    <       CALL trc_bdy( kt )               ! BDY open boundaries
    ---
    > !!      CALL bdy_trc( kt )               ! BDY open boundaries

MY_SRC$ diff trctrp.F90 ../../../NEMO/TOP_SRC/TRP/trctrp.F90
::

    29,32c29
    < #if defined key_bdy
    <    USE trcbdy          ! BDY open boundaries
    <    USE bdy_par, only: lk_bdy
    < #endif
    ---
    >
    72,75d68
    <          IF( ln_trcdmp_clo )    CALL trc_dmp_clo( kstp )        ! internal damping trends on closed seas only
    < #if defined key_bdy
    <                                 CALL trc_bdy_dmp( kstp )        ! BDY damping trends
    < #endif
    85a79
    >          IF( ln_trcdmp_clo )    CALL trc_dmp_clo( kstp )        ! internal damping trends on closed seas only


We also use a modified trcbc.F90 file and additional file trcbdy.F90 (March 8, 2017 versions).

.. toctree::
   :maxdepth: 1

   showtrcbcF90
   showtrcbdyF90

Namelist contents
-----------------

We have two open boundaries, one northern and one western.

Boundary-related sections from our :file:`namelist_top_cfg` are reproduced below.

namtrc (note additional columns)

.. code-block:: fortran

   !'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
   &namtrc     !   tracers definition
   !,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
   !                !    name   !           title of the field              ! initial data ! initial data ! obc    !    sbc  !  cbc    ! save   !
   !                !           !                                           !  units       ! from file    !                             ! or not !
   !                !           !                                           !              ! or not       !                             !        !
     sn_tracer(1)  = 'NO3     ' , 'Nitrates Concentration                 ',  'mol-C/L' ,  .true.     , .true.   ,  .false. ,  .false.  ,  .true.
     sn_tracer(2)  = 'NH4     ' , 'Ammonium Concentration                 ',  'mol-C/L' ,  .true.     , .true.   ,  .false. ,  .false.  ,  .true.
     sn_tracer(3)  = 'Si      ' , 'Silicate Concentration                 ',  'mol-C/L' ,  .true.     , .true.   ,  .false. ,  .false.  ,  .true.
     sn_tracer(4)  = 'DIAT    ' , 'Diatoms Concentration                  ',  'mol-C/L' ,  .true.     , .true.   ,  .false. ,  .false.  ,  .true.
     sn_tracer(5)  = 'PHY     ' , 'Nanophytoplankton Concentration        ',  'mol-C/L' ,  .true.     , .true.   ,  .false. ,  .false.  ,  .true.
     sn_tracer(6)  = 'MYRI    ' , 'Mesozooplankton Concentration          ',  'mol-C/L' ,  .true.     , .true.   ,  .false. ,  .false.  ,  .true.
     sn_tracer(7)  = 'MICZ    ' , 'Microzooplankton Concentration         ',  'mol-C/L' ,  .true.     , .true.   ,  .false. ,  .false.  ,  .true.
     sn_tracer(8)  = 'DON     ' , 'Dissolved organic Concentration        ',  'mol-C/L' ,  .true.     , .true.   ,  .false. ,  .false.  ,  .true.
     sn_tracer(9)  = 'PON     ' , 'Small organic carbon Concentration     ',  'mol-C/L' ,  .true.     , .true.   ,  .false. ,  .false.  ,  .true.
     sn_tracer(10) = 'bSi     ' , 'biogenic Silicate Concentration        ',  'mol-C/L' ,  .true.     , .true.   ,  .false. ,  .false.  ,  .true.
     sn_tracer(11) = 'TRA     ' , 'River Tracer                           ',  'mol-C/L' ,  .false.    , .true.   ,  .false. ,  .false.  ,  .true.
     ln_trcdta = .true.
     ln_trcdmp = .false.
   /

nambdy_bc (first boundary)::

 !----------------------------------------------------------------------
 ! nambdy_bc       !   data for BDY boundary conditions: 1st boundary
 !-----------------------------------------------------------------------
 &nambdy_bc
 !
 !              !  file name      ! frequency (hours) ! variable   ! time interp.   !  clim   ! 'yearly'/ ! weights  ! rotation ! land/sea mask !
 !              !                 !  (if <0  months)  !   name     !   (logical)    !  (T/F ) ! 'monthly' ! filename ! pairing  ! filename      !
   sn_trcobc(1) = 'bioOBC_full.nc',              168,        'NO3',      .true.,  .true.,  'yearly',   '',        '',   ''
   sn_trcobc(2) = 'bioOBC_full.nc',              168,        'NH4',      .true.,  .true.,  'yearly',   '',        '',   ''
   sn_trcobc(3) = 'bioOBC_full.nc',              168,        'Si',       .true.,  .true.,  'yearly',   '',        '',   ''
   sn_trcobc(4) = 'bioOBC_full.nc',              168,        'DIA',      .true.,  .true.,  'yearly',   '',        '',   ''
   sn_trcobc(5) = 'bioOBC_full.nc',              168,        'CRY',      .true.,  .true.,  'yearly',   '',        '',   ''
   sn_trcobc(6) = 'bioOBC_full.nc',              168,        'MYRI',     .true.,  .true.,  'yearly',   '',        '',   ''
   sn_trcobc(7) = 'bioOBC_full.nc',              168,        'MICZ',     .true.,  .true.,  'yearly',   '',        '',   ''
   sn_trcobc(8) = 'bioOBC_full.nc',              168,        'DON',      .true.,  .true.,  'yearly',   '',        '',   ''
   sn_trcobc(9) = 'bioOBC_full.nc',              168,        'PON',      .true.,  .true.,  'yearly',   '',        '',   ''
   sn_trcobc(10) = 'bioOBC_full.nc',             168,        'bSi',      .true.,  .true.,  'yearly',   '',        '',   ''
   sn_trcobc(11) = 'bioOBC_full.nc',             168,        'O2',       .true.,  .true.,  'yearly',   '',        '',   ''
   cn_dir_obc        =  './open_boundaries/west/'      !  root directory for the location of OPEN data files
   rn_trofac(1)   =  1.0  !  -      -      -     -
   rn_trofac(2)   =  1.0  !  -      -      -     -
   rn_trofac(3)   =  1.0  !  multiplicative factor
   rn_trofac(4)   =  1.0  !  -      -      -     -
   rn_trofac(5)   =  1.0  !  -      -      -     -
   rn_trofac(6)   =  1.0  !  -      -      -     -
   rn_trofac(7)   =  1.0  !  -      -      -     -
   rn_trofac(8)   =  1.0  !  -      -      -     -
   rn_trofac(9)   =  1.0  !  -      -      -     -
   rn_trofac(10)  =  1.0  !  -      -      -     -
   rn_trofac(11)  =  1.0  !  -      -      -     -
 /

nambdy_bc (second boundary)::

 !----------------------------------------------------------------------
 ! nambdy_bc       !   data for BDY boundary conditions: 2nd boundary
 !-----------------------------------------------------------------------
 &nambdy_bc
 !
 !              !  file name      ! frequency (hours) ! variable   ! time interp.   !  clim   ! 'yearly'/ ! weights  ! rotation ! land/sea mask !
 !              !                 !  (if <0  months)  !   name     !   (logical)    !  (T/F ) ! 'monthly' ! filename ! pairing  ! filename      !
   sn_trcobc(1)  = 'bioOBC_full_north.nc',               4380,        'NO3',      .true.,  .true.,  'yearly',   '',      '',   ''
   sn_trcobc(2)  = 'bioOBC_full_north.nc',               4380,        'NH4',      .true.,  .true.,  'yearly',   '',      '',   ''
   sn_trcobc(3)  = 'bioOBC_full_north.nc',               4380,        'Si',       .true.,  .true.,  'yearly',   '',      '',   ''
   sn_trcobc(4)  = 'bioOBC_full_north.nc',               4380,        'DIA',      .true.,  .true.,  'yearly',   '',      '',   ''
   sn_trcobc(5)  = 'bioOBC_full_north.nc',               4380,        'CRY',      .true.,  .true.,  'yearly',   '',      '',   ''
   sn_trcobc(6)  = 'bioOBC_full_north.nc',               4380,        'MYRI',     .true.,  .true.,  'yearly',   '',      '',   ''
   sn_trcobc(7)  = 'bioOBC_full_north.nc',               4380,        'MICZ',     .true.,  .true.,  'yearly',   '',      '',   ''
   sn_trcobc(8)  = 'bioOBC_full_north.nc',               4380,        'DON',      .true.,  .true.,  'yearly',   '',      '',   ''
   sn_trcobc(9)  = 'bioOBC_full_north.nc',               4380,        'PON',      .true.,  .true.,  'yearly',   '',      '',   ''
   sn_trcobc(10) = 'bioOBC_full_north.nc',               4380,        'bSi',      .true.,  .true.,  'yearly',   '',      '',   ''
   sn_trcobc(11) = 'bioOBC_full_north.nc',               4380,        'O2',       .true.,  .true.,  'yearly',   '',      '',   ''
   cn_dir_obc        =  './open_boundaries/north/'      !  root directory for the location of OPEN data files
   rn_trofac(1)   =  1.0  !  -      -      -     -
   rn_trofac(2)   =  1.0  !  -      -      -     -
   rn_trofac(3)   =  1.0  !  multiplicative factor
   rn_trofac(4)   =  1.0  !  -      -      -     -
   rn_trofac(5)   =  1.0  !  -      -      -     -
   rn_trofac(6)   =  1.0  !  -      -      -     -
   rn_trofac(7)   =  1.0  !  -      -      -     -
   rn_trofac(8)   =  1.0  !  -      -      -     -
   rn_trofac(9)   =  1.0  !  -      -      -     -
   rn_trofac(10)  =  1.0  !  -      -      -     -
   rn_trofac(11)  =  1.0  !  -      -      -     -
 /


Corresponding boundary options in namelist_cfg::

 &nambdy        !  unstructured open boundaries                          ("key_bdy")
 !-----------------------------------------------------------------------
    nb_bdy         = 2                     !  number of open boundary sets According to merge namelist, only 1
    ln_coords_file = .false., .false.      !  =T : read bdy coordinates from file
    cn_dyn2d       = 'flather', 'flather'            !
    nn_dyn2d_dta   =  3, 2                 !  = 0, bdy data are equal to the initial state
                                           !  = 1, bdy data are read in 'bdydata   .nc' files
                                           !  = 2, use tidal harmonic forcing data from files
                                           !  = 3, use external data AND tidal harmonic forcing
    cn_dyn3d      =  'orlanski', 'orlanski'
    nn_dyn3d_dta  =  0, 0                  !  = 0, bdy data are equal to the initial state
                                           !  = 1, bdy data are read in 'bdydata   .nc' files
    cn_tra        =  'frs','frs'           !
    nn_tra_dta    =  1, 1                  !  = 0, bdy data are equal to the initial state
                                           !  = 1, bdy data are read in 'bdydata   .nc' files
    ln_tra_dmp    =.false., .false.        !  open boudaries conditions for tracers
    ln_dyn3d_dmp  =.false., .false.        !  open boundary condition for baroclinic velocities
    rn_time_dmp   =  1., 1.                ! Damping time scale in days (nudging on orlanski inflow)
    rn_time_dmp_out =  1., 1.              ! Outflow damping time scale (nudging on orlanski outflow)
    nn_rimwidth   = 10, 10
 &end
 &nambdy_index ! open boundaries - definition ("key_bdy")
 !-----------------------------------------------------------------------
    ctypebdy = 'W'
    nbdyind   = 2     ! i-index for segment
    nbdybeg   = 385   ! j-index for segment beginning
    nbdyend   = 471   ! j-index for segment end
 &end
 &nambdy_dta      !  open boundaries - external data           ("key_bdy")
 !-----------------------------------------------------------------------
 !        !  file name             ! frequency (hours) ! variable  ! time interp. !  clim   ! 'yearly'/ ! weights  ! rotation !
 !        !                        !  (if <0  months)  !   name    !   (logical)  !  (T/F ) ! 'monthly' ! filename ! pairing  !
  bn_ssh = 'ssh/ssh',                1,                'sossheig', .true.,        .false.,  'daily',    '',        ''
  bn_u2d = 'ssh/ssh',                1,                'vobtcrtx', .true.,        .false.,  'daily',    '',        ''
  bn_v2d = 'ssh/ssh',                1,                'vobtcrty', .true.,        .false.,  'daily',    '',        ''
  bn_u3d = '',                      24,                'vozocrtx', .true.,        .false.,  'daily',    '',        ''
  bn_v3d = '',                      24,                'vomecrty', .true.,        .false.,  'daily',    '',        ''
  bn_tem = 'SalishSea_west_TEOS10', 168,                'votemper', .true.,        .true.,   'yearly',   '',        ''
  bn_sal = 'SalishSea_west_TEOS10', 168,                'vosaline', .true.,        .true.,   'yearly',   '',        ''

  cn_dir = 'open_boundaries/west/'
 &end
 &nambdy_tide     ! tidal forcing at open boundaries
 !-----------------------------------------------------------------------
   filtide          = 'open_boundaries/west/tides/DownbyOne2_N36_J_west_tide_'
   ln_bdytide_2ddta = .false.
   ln_bdytide_conj  = .false.
 &end
 &nambdy_index ! open boundaries - definition ("key_bdy")
 !-----------------------------------------------------------------------
    ctypebdy  = 'N'
    nbdyind   = 896  ! i-index for segment
    nbdybeg   = 33   ! j-index for segment beginning
    nbdyend   = 62   ! j-index for segment end
 &end
 &nambdy_dta      !  open boundaries - external data           ("key_bdy")
 !-----------------------------------------------------------------------
 !        !  file name             ! frequency (hours) ! variable  ! time interp. !  clim   ! 'yearly'/ ! weights  ! rotation !
 !        !                        !  (if <0  months)  !   name    !   (logical)  !  (T/F ) ! 'monthly' ! filename ! pairing  !
  bn_ssh = 'ssh/sshNorth',	           1,               'sossheig', .true.,        .false.,  'monthly',  '',        ''
  bn_u2d = 'ssh/sshNorth',	           1,               'vobtcrtx', .true.,        .false.,  'monthly',  '',        ''
  bn_v2d = 'ssh/sshNorth',	           1,               'vobtcrty', .true.,        .false.,  'monthly',  '',        ''
  bn_u3d = '',                        24,               'vozocrtx', .true.,        .false.,  'daily',    '',        ''
  bn_v3d = '',                        24,               'vomecrty', .true.,        .false.,  'daily',    '',        ''
  bn_tem = 'SalishSea_north_TEOS10',  4380,               'votemper', .true.,        .true.,   'yearly',   '',        ''
  bn_sal = 'SalishSea_north_TEOS10',  4380,               'vosaline', .true.,        .true.,   'yearly',   '',        ''

  cn_dir = 'open_boundaries/north/'
  ln_full_vel = .false.
 &end
 &nambdy_tide     ! tidal forcing at open boundaries
 !-----------------------------------------------------------------------
   filtide          = 'open_boundaries/north/tides/DownbyOne2_N36_J_North_tide_'
   ln_bdytide_2ddta = .false.
   ln_bdytide_conj  = .false.
 &end


Boundary condition file formats
-------------------------------

Our western and northern boundary forcing files for passive tracers contain variables of shape (52, 40, 1, 870) and (2, 40, 1, 300), respectively.
The first dimension is time; the second is depth; the third is always 1; and the fourth is the length of the boundary multiplied by the rimwidth,
both of which are defined in namelist_cfg. The boundary data is repeated nn_rimwidth times in the along-boundary direction.

Sample files:

:download:`bioOBC_full.nc <exFiles/bioOBC_full.nc>`

:download:`bioOBC_full_north.nc <exFiles/bioOBC_full.nc>`
