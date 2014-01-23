************************************
Initializing T+S with a restart file
************************************

This section describes how to initialize the salinity and temperature fields with data from a restart file. 
This is useful for taking the stratification from a spin up run. 

First, place the restart file (eg. :file:`SalishSea_00120960_restart.nc`) in the :file:`NEMO-forcing/initial_strat` directory. 

Next, modify the :file:`&namtsd` section of :file:`namelist.domain`. 
For example:

 .. code-block:: fortran

        &namtsd    !   data : Temperature  & Salinity
	!-----------------------------------------------------------------------
	!          ! file name                          ! freq (hr)  ! variable  !  time  !  clim  ! period  ! weights  ! rotation !
	!          !                                    ! (<0 == mo) !   name    ! interp !  (T/F) !         ! filename ! pairing  !
	sn_tem  = 'SalishSea_00120960_restart.nc',          -12,         'tb',     .false., .true.,  'yearly', '',        ''
	sn_sal  = 'SalishSea_00120960_restart.nc',          -12,         'sb',     .false., .true.,  'yearly', '',        ''
	cn_dir        = 'initial_strat/'  ! directory containing initial condition files
	ln_tsd_init   = .true.            ! Initialisation of ocean T & S with T &S input data (T) or not (F)
	ln_tsd_tradmp = .false.           ! damping of ocean T & S toward T &S input data (T) or not (F)
	/
	
The file name must be modified to the restart file's name.
Additionally, since restart files have a different naming convention for the field variables, ensure that :file:`'tb'` and :file:`'sb'` are used for temperature and salinity in the variable name section.

Considerations
==================

* Bathymetry should be consistent between the restart file and the run you are initializing.
* There may be issues of continuity between the restart T + S data and the boundary conditions at Juan de Fuca, depending on the date chosen for a restart.  
