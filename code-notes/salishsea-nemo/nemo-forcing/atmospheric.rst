.. _AtmosphericForcing:

*******************
Atmospheric Forcing
*******************

NEMO includes a CORE bulk formulae interface that allows the :ref:`CGRF-Dataset` to be used as atmospheric forcing
(aka surface boundary conditions; SBC).
The namelist that controls that is :kbd:`namsbc_core`.
That NEMO interface requires that all of the files
(including the :ref:`AtmosphericForcingInterpolationWeights`)
be in a single directory and have a prescribed name pattern.
To satisfy that requirement the following file management strategy has been established:

* The Salish Sea MEOPAR repository of CGRF product files and their NEMO CORE bulk interface representation reside in :file:`/ocean/dlatorne/MEOPAR/CGRF/`

* Within that directory is 1 sub-directory:

  * :file:`NEMO-atmos/` is a collection of CGRF files using the names required by the NEMO CORE bulk interface.
    The :file:`NEMO-atmos/` directory also contains symbolic links to the :ref:`AtmosphericForcingInterpolationWeights` file and the :ref:`NoSnowConstraint` file in the :ref:`grid-repo` repo.

* The files in the :file:`/ocean/dlatorne/MEOPAR/CGRF/` directory are managed by the :command:`salishsea get_cgrf` tool.

* The namelist that directs NEMO to use the files in :file:`NEMO-atmos/` looks like:

  .. code-block:: fortran

      !-----------------------------------------------------------------------
      &namsbc_core   !   namsbc_core  CORE bulk formulae
      !-----------------------------------------------------------------------
      !        !  file name  ! freq (hr)  ! variable  !  time   !  clim  ! period ! weights             ! rotation !
      !        !             ! (<0 == mo) !   name    ! interp  !  (T/F) !        ! filename            ! pairing  !
        sn_wndi = 'u10',       1,          'u_wind',    .true.,  .false., 'daily', 'met_gem_weight.nc',  'Uwnd'
        sn_wndj = 'v10',       1,          'v_wind',    .true.,  .false., 'daily', 'met_gem_weight.nc',  'Vwnd'
        sn_qsr  = 'qsw',       1,          'solar',     .true.,  .false., 'daily', 'met_gem_weight.nc',  ''
        sn_qlw  = 'qlw',       1,          'therm_rad', .true.,  .false., 'daily', 'met_gem_weight.nc',  ''
        sn_tair = 't2',        1,          'tair',      .true.,  .false., 'daily', 'met_gem_weight.nc',  ''
        sn_humi = 'q2',        1,          'qair',      .true.,  .false., 'daily', 'met_gem_weight.nc',  ''
        sn_prec = 'precip',    1,          'precip',    .true.,  .false., 'daily', 'met_gem_weight.nc',  ''
        sn_snow = 'no_snow', -12,          'snow',      .false., .true.,  'yearly','met_gem_weight.nc',  ''

        cn_dir      = 'NEMO-atmos/'  ! root directory for the location of the bulk files
        ln_2m       = .true.         ! air temperature and humidity referenced at 2m (T) instead 10m (F)
      /

  The file name roots and the variable names come from the CGRF files.

* The directory given in as value associated with the :kbd:`atmospheric` key in the :kbd:`forcing` section of the run description file is symlinked as :file:`NEMO-atmos` in the run directory to complete the chain from the namelist to the CGRF products files:

  .. code-block:: yaml

      forcing:
        atmospheric: /ocean/dlatorne/MEOPAR/CGRF/NEMO-atmos/


.. _NoSnowConstraint:

No-Snow Constraint
==================

The NEMO CORE bulk interface requires a snow file but it has been decided to impose a constraint on the Salish Sea model whereby all precipitation falls in liquid phase.
That constraint is provided by the :file:`no_snow.nc` file in the :ref:`grid-repo` repo.
That file ensures that the solid phase precipitation is zero at all times and locations.
It is used as annual climatology forcing in the :kbd:`namsbc_core` name list.


.. _AtmosphericForcingInterpolationWeights:

Interpolation Weights
=====================

JP Paquin created a weights file that allows NEMO's Interpolation On the Fly
(IOF)
feature to be used to take atmospheric forcing values from the :ref:`CGRF-Dataset`.
It is in the :file:`4_weights_ATMOS/` directory of the 2-Oct-2013 WC3_PREP tarball and has been added to the :file:`grid/` of the :ref:`NEMO-forcing-repo` repo.
Those weight values were subsequently used to create a netCDF4 weights file with :kbd:`zlib` compression enabled and CF-1.6 conventions compliant attributes using the `I_ForcingFiles/Atmos/netCDF4weights-CGRF.ipynb`_ notebook.

.. _I_ForcingFiles/Atmos/netCDF4weights-CGRF.ipynb: https://nbviewer.org/github/SalishSeaCast/tools/blob/master/I_ForcingFiles/Atmos/netCDF4weights-CGRF.ipynb


Creating New Weights Files
--------------------------

The :program:`NEMO_Preparation/4_weights_ATMOS/get_weight_nemo` program in the 
:ref:`NEMO_EastCoast-repo` repo can be used in conjunction with a bathymetry file and atmospheric 
forcing file(s) to create a weights file that allows NEMO's Interpolation On the Fly
(IOF)
feature to use the atmospheric forcing values.
An example of the use of :program:`get_weight_nemo` to create a weights file for datasets from the 
operational West deployment of Environment Canada's `High Resolution Deterministic Prediction 
System`_ (HRDPS) is presented here:

.. _High Resolution Deterministic Prediction System: https://weather.gc.ca/grib/grib2_HRDPS_HR_e.html

Clone the :ref:`NEMO_EastCoast-repo` repo on :kbd:`salish` and edit the 
:file:`NEMO_Preparation/4_weights_ATMOS/make.sh` file to comment out the default build commands 
and enable the :kbd:`salish` ones:

.. code-block:: bash

    #- On salish (UBC)
    LIBNETCDF=/usr
    mpif90 -c grid.f90 -I${LIBNETCDF}/include -L${LIBNETCDF}/lib -lnetcdf
    mpif90 -c map.F90 -I${LIBNETCDF}/include -L${LIBNETCDF}/lib -lnetcdf
    mpif90 -c get_weight_nemo.F90 -I${LIBNETCDF}/include -L${LIBNETCDF}/lib -lnetcdf
    mpif90 -o get_weight_nemo get_weight_nemo.o map.o grid.o -I${LIBNETCDF}/include -L${LIBNETCDF}/lib -lnetcdf -lnetcdff

Build :program:`get_weight_nemo`:

.. code-block:: bash

    $ ./make.sh

:program:`get_weight_nemo` creates a file of weighting factors that allow 
atmospheric forcing variable values on one grid to be interpolated on to the model grid
(as defined in the bathymetry dataset).
To do that it requires:

#. a bathymetry dataset,
   the name of which is hard-coded to :file:`bathy_meter.nc`
#. a namelist file,
   the name of which is hard-coded to :file:`namelist`,
   and an example of which is contained in the :file:`NEMO_Preparation/4_weights_ATMOS/` directory
#. one or more atmospheric forcing dataset file(s),
   the name of which is defined in the namelist

The output of :program:`get_weight_nemo` is a weights file,
the name of which is hard-coded to :file:`met_gem_weight.nc`.

We'll run :program:`get_weight_nemo` in a clone of the :ref:`grid-repo`,
so start by symlinking the sample namelist file to there as :file:`namelist`:

.. code-block:: bash

    $ cd MEOPAR/grid/
    $ ln -s namelist.get_weight_nemo.gem2.5-ops namelist

Symlink the bathymetry dataset as :file:`bathy_meter.nc`:

.. code-block:: bash

    $ ln -s bathymetry_201702.nc bathy_meter.nc

The only values that :program:`get_weight_nemo` actually uses from the atmospheric forcing dataset file is the grid point locations,
but the namelist file is more complicated.
We can reduce the complexity by using a single atmospheric forcing dataset file as a climatology,
so we symlink one as :file:`atmos.nc`:

.. code-block:: bash

    $ ln -s /results/forcing/atmospheric/GEM2.5/operational/ops_y2015m01d01.nc atmos.nc

The namelist file looks like:

.. code-block:: fortran

    !-----------------------------------------------------------------------
    &namsbc_core !   namsbc_core  CORE bulk formulea
    !-----------------------------------------------------------------------
    !            ! file name ! variable   ! clim  ! 'yearly'/
    !            !           !  name      ! (T/F) ! 'monthly'
       sn_wndi   = 'atmos',   'u_wind'   , .true., 'yearly'
       sn_wndj   = 'atmos',   'v_wind'   , .true., 'yearly'
       sn_qsr    = 'atmos',   'solar'    , .true., 'yearly'
       sn_qlw    = 'atmos',   'therm_rad', .true., 'yearly'
       sn_tair   = 'atmos',   'tair'     , .true., 'yearly'
       sn_humi   = 'atmos',   'qair'     , .true., 'yearly'
       sn_prec   = 'atmos',   'precip'   , .true., 'yearly'
       sn_snow   = 'atmos',   'snow'     , .true., 'yearly'
       cn_dir    = './'      !  root directory for the location of the bulk files
    /

The important things here are:

* The file name must match the name of the atmospheric forcing dataset file symlink,
  without the :file:`.nc` extension.
* The climatology field (:kbd:`clim (T/F)`) must be set to :kbd:`.true.` for all variables.
* The value of :kbd:`cn_dir` must be :kbd:`'./'`.

Finally,
run :program:`get_weight_nemo`:

.. code-block:: bash

    MEOPAR/NEMO-EastCoast/NEMO_Preparation/4_weights_ATMOS/get_weight_nemo

The output should be something like::

   sbc_blk_core : flux formulation for ocean surface boundary condition
   ~~~~~~~~~~~~
             namsbc_core Namelist
             list of files
                  root filename: ./atmos variable name: u_wind climatology:  T  data type: yearly
                  root filename: ./atmos variable name: v_wind climatology:  T  data type: yearly
                  root filename: ./atmos variable name: qair climatology:  T  data type: yearly
                  root filename: ./atmos variable name: solar climatology:  T  data type: yearly
                  root filename: ./atmos variable name: therm_rad climatology:  T  data type: yearly
                  root filename: ./atmos variable name: tair climatology:  T  data type: yearly
                  root filename: ./atmos variable name: precip climatology:  T  data type: yearly
                  root filename: ./atmos variable name: snow climatology:  T  data type: yearly
   reading : ./atmos.nc
   atmospheric forcing netcdf grid dimensions: nx=         256 , ny=         266
             get_atmo_grid ~~~ found X axis varid:           3
             get_atmo_grid ~~~ found Y axis varid:           2
   grid_type           2
  xmin/xmax/origin  0.230833E+03  0.240530E+03  0.230833E+03
   writing variable : src01
          8065        8065        8321        8321
   status put           0
   writing variable : wgt01
   writing variable : src02
          8321        8321        8065        8065
   status put           0
   writing variable : wgt02
   writing variable : src03
          8064        8064        8320        8320
   status put           0
   writing variable : wgt03
   writing variable : src04
          8320        8320        8064        8064
   status put           0
   writing variable : wgt04

and a :file:`met_gem_weight.nc` file should be created.

Use the `I_ForcingFiles/Atmos/ImproveWeightsFile.ipynb`_ notebook to transform 
:file:`met_gem_weight` into a netCDF4 file called 
:file:`weights-gem2.5-ops_201702.nc` with well-structured metadata
(see :ref:`netCDF4FilesCreationAndConventions`).

.. _I_ForcingFiles/Atmos/ImproveWeightsFile.ipynb: https://nbviewer.org/github/SalishSeaCast/tools/blob/master/I_ForcingFiles/Atmos/ImproveWeightsFile.ipynb


.. _CGRF-Dataset:

CGRF Dataset
============

The Canadian Meteorological Centre's
(CMC)
Global Deterministic Prediction System
(GDPS)
Reforecasts
(CGRF)
dataset is a relatively high-resolution forcing dataset for ocean models [Smith_etal2013]_.
The dataset is hosted on an :program:`rsync` server at :kbd:`goapp.ocean.dal.ca`.
User id and password credentials are required to access it.

At the command line you can explore the dataset with commands like:

.. code-block:: bash

    rsync <userid>@goapp.ocean.dal.ca::canadian_GDPS_reforecasts_v1/2002/2002091500
    Password:
    dr-xr-xr-x        4096 2012/06/14 06:59:22 2002091500

and

.. code-block:: bash

    rsync <userid>@goapp.ocean.dal.ca::canadian_GDPS_reforecasts_v1/2002/2002091500/
    Password:
    dr-xr-xr-x        4096 2012/06/14 06:59:22 .
    -r-xr-xr-x     8844469 2011/06/06 07:46:01 2002091500_precip.nc.gz
    -r-xr-xr-x    27045976 2011/01/14 21:37:09 2002091500_q2.nc.gz
    -r-xr-xr-x    20960161 2011/01/14 21:37:26 2002091500_qlw.nc.gz
    -r-xr-xr-x    10451631 2011/01/14 21:37:34 2002091500_qsw.nc.gz
    -r-xr-xr-x    11655341 2011/01/14 21:37:37 2002091500_slp.nc.gz
    -r-xr-xr-x    27080056 2011/01/14 21:37:15 2002091500_t2.nc.gz
    -r-xr-xr-x    37703920 2011/01/14 21:37:04 2002091500_u10.nc.gz
    -r-xr-xr-x    37641390 2011/01/14 21:37:05 2002091500_v10.nc.gz

Note that the trailing slash causes the contents of a directory to be accessed while its absence refers to the directory itself.

To make a local copy of files use the :kbd:`-rltv` options and provide a destination directory
(which will be created if it doesn't already exist):

.. code-block:: bash

    rsync -rltv <userid>@goapp.ocean.dal.ca::canadian_GDPS_reforecasts_v1/2002/2002091500/ 2002-09-15/
    Password:
    receiving incremental file list
    ./
    2002091500_precip.nc.gz
    2002091500_q2.nc.gz
    2002091500_qlw.nc.gz
    2002091500_qsw.nc.gz
    2002091500_slp.nc.gz
    2002091500_t2.nc.gz
    2002091500_u10.nc.gz
    2002091500_v10.nc.gz

    sent 234 bytes  received 181405678 bytes  6596578.62 bytes/sec
    total size is 181382944  speedup is 1.00

The local files are created with :kbd:`555` permissions.
Make them user and group writable so that they can be decompressed,
and non-executable with:

.. code-block:: bash

    chmod 664 2002-09-15/*

The :command:`salishsea get_cgrf` tool automates this process.


.. _Pressure-Correction:

Pressure Correction
====================

The CGRF atmospheric model uses a terrain following vertical coordinate system which means that the lowest grid cells are not at sea level in mountainous regions such as those surrounding the Salish Sea.
As such, we have developed an algorithm to adjust CGRF pressure files to sea level.
First, the altitude of each grid cell is computed since this is not given in the CGRF output.
Given the of an air parcel, we can approximate its height :math:`z_1` above sea level using the following formula [Holton1992]_:

.. math::
   p_s = p_1\left(\gamma\frac{z_1}{T_1} +1 \right)^\frac{g}{\gamma R}

where :math:`g` is the acceleration due to gravity, :math:`R` is the ideal gas constant, and :math:`\gamma` is the temperature lapse rate of the atmosphere (0.0098 degrees/m).

To arrive at this formula we have made a few assumptions:

1. The atmosphere is in hydrostatic equilibrium: :math:`\frac{d p}{d z} = -\rho g`
2. The atmosphere is an ideal gas: :math:`p = \rho R T`
3. The temperature of the atmosphere decreases with height at a constant rate: :math:`\frac{dT}{dz} = -\gamma`

The altitude of each grid cell is stored in a file :file:`altitude_CGRF.nc` in the :file:`tools/I_ForcingFiles/Atmos` repository.

Ths sea level pressure calculation is performed in :file:`nc_tools.generate_pressure_file`, which is used in :command:`salishsea get_cgrf` to correct pressure files on download.
Corrected pressure files are named :file:`slp_corr_y0000m00d00.nc`.
See the `tools docs`_ for details on :file:`nc_tools.generate_pressure_file` method.

.. _tools docs: https://salishsea-meopar-tools.readthedocs.io/en/latest/SalishSeaTools/api.html#salishsea_tools.nc_tools.generate_pressure_file

.. note::

   :command:`salishsea get_cgrf` requires a link to :file:`altitude_CGRF.nc` in :file:`/NEMO-atmos/`.


.. [Smith_etal2013] Smith, G. C., Roy, F., Mann, P., Dupont, F., Brasnett, B., Lemieux, J.-F., Laroche, S. and Bélair, S. (2013), A new atmospheric dataset for forcing ice–ocean models: Evaluation of reforecasts using the Canadian global deterministic prediction system. Q.J.R. Meteorol. Soc.. doi: 10.1002/qj.2194 https://dx.doi.org/10.1002/qj.2194

.. [Holton1992] Holton, J., An introduction to dynamic meteorology 3rd edition (Acadmeic Press: 1992)
