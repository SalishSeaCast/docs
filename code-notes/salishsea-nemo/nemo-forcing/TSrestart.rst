************************************
Initializing T+S with a restart file
************************************

This section describes how to initialize the salinity and temperature fields with data from a restart file. 
This is useful for taking the stratification from a spin up run. 

First, in the :file:`SalishSea.yaml` file edit the initial conditions section to the path of the directory that contains the restart file. For example, the restart file :file:`SalishSea_00069120_restart.nc` is in :file:`/ocean/dlatorne/MEOPAR/SalishSea/results/spin-up/18oct25oct/` so the forcing section of the :file:`.yaml` files should look like: 

.. code-block:: fortran

	forcing:
	# If relative, paths are taken from forcing path above 
	atmospheric: /ocean/dlatorne/MEOPAR/CGRF/NEMO-atmos/
	initial conditions: /ocean/dlatorne/MEOPAR/SalishSea/results/spin-up/18oct25oct/
	open boundaries: open_boundaries/
	rivers: rivers/

Next, modify the :file:`&namtsd` section of :file:`namelist.domain` so that NEMO reads in the restart file. 
For example:

 .. code-block:: fortran

        &namtsd    !   data : Temperature  & Salinity
	!-----------------------------------------------------------------------
	!          ! file name                          ! freq (hr)  ! variable  !  time  !  clim  ! period  ! weights  ! rotation !
	!          !                                    ! (<0 == mo) !   name    ! interp !  (T/F) !         ! filename ! pairing  !
	sn_tem  = 'SalishSea_00069120_restart.nc',          -12,         'tb',     .false., .true.,  'yearly', '',        ''
	sn_sal  = 'SalishSea_00069120_restart.nc',          -12,         'sb',     .false., .true.,  'yearly', '',        ''
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
