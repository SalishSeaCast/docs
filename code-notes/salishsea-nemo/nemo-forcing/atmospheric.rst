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

* Within that directory are 2 sub-directories:

  * :file:`rsync-mirror/` is a collection of :file:`yyyy-mm-dd/` directories that contain uncompressed CGRF files for the day identified by the directory name
  * :file:`NEMO-atmos/` is a collection of symbolic links to the CGRF files using the names required by the NEMO CORE bulk interface.
    The :file:`NEMO-atmos/` directory also contains symbolic links to the :ref:`AtmosphericForcingInterpolationWeights` file and the :ref:`NoSnowConstraint` file in the :ref:`NEMO-forcing-repo` repo.

* The files in the :file:`/ocean/dlatorne/MEOPAR/CGRF/` directory are managed by the :command:`salishsea get_cgrf` tool;
  see the :ref:`salishsea-get_cgrf` docs.

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
That constraint is provided by the :file:`atmospheric/no_snow.nc` file in the :ref:`NEMO-forcing-repo` repo.
That file ensures that the solid phase precipitation is zero at all times and locations.
It is used as annual climatology forcing in the :kbd:`namsbc_core` name list.


.. _AtmosphericForcingInterpolationWeights:

Interpolation Weights
=====================

JP Paquin created a weights file that allows NEMO's Interpolation On the Fly
(IOF)
feature to be used to take atmospheric forcing values from the :ref:`CGRF-Dataset`.
It is in the :file:`4_weights_ATMOS/` directory of the 2-Oct-2013 WC3_PREP tarball and has been added to the :ref:`grid-directory` of the :ref:`NEMO-forcing-repo` repo.
Those weight values were subsequently used to create a netCDF4 weights file with :kbd:`zlib` compression enabled and CF-1.6 conventions compliant attributes.


.. _CGRF-Dataset:

CGRF Dataset
============

The Canadian Meteorological Centre's
(CMC)
Global Deterministic Prediction System
(GDPS)
Reforecasts
(CGRF)
dataset is a relatively high-resolution forcing dataset for ocean models [Smith_etal2013].
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

See the :command:`salishsea` :ref:`salishsea-get_cgrf` docs for details of a tool that automates this process.

.. _Pressure-Correction:
 
Pressure Correction
====================

The CGRF atmospheric model uses a terrain following vertical coordinate system which means that the lowest grid cells are not at sea level in mountainous regions such as those surrounding the Salish Sea. 
As such, we have developed an algorithm to adjust CGRF pressure files to sea level.
Given the altitude :math:`z_1`,  temperature :math:`T_1` and pressure :math:`p_1` of an air parcel, we can estimate the sea level pressure as [Holton 1992]: 

First, the altitude of each grid cell is computed since this is not given in the CGRF output. 
Given the of an air parcel, we can approximate its height :math:`z_1` above sea level using the following formula [Holton, 1992]:

.. math:: 
   p_s = p_1\left(\gamma\frac{z_1}{T_1} +1 \right)^\frac{g}{\gamma R}

where :math:`g` is the acceleration due to gravity, :math:`R` is the ideal gas constant, and :math:`\gamma` is the temperature lapse rate of the atmosphere (0.0098 degrees/m).

To arrive at this formula we have made a few assumptions: 

1. The atmopshere is in hydrostatic equilibrium: :math:`\frac{d p}{d z} = -\rho g` 
2. The atmosphere is an ideal gas: :math:`p = \rho R T`
3. The temperature of the atmosphere decreases with height at a constant rate: :math:`\frac{dT}{dz} = -\gamma`

The altiude of each grid cell is stored in a file :file:`altitude_CGRF.nc` in the :file:`tools/I_ForcingFiles/Atmos` repository.

Ths sea level pressure calculation is performed in :file:`nc_tools.generate_pressure_file`, which is used in `get_cgrf`_ to correct pressure files on download. 
Corrected pressure files are named :file:`slp_corr_y0000m00d00.nc`. 
See the `tools docs`_ for details on :file:`nc_tools.generate_pressure_file` method.. 

.. _get_cgrf: http://salishsea-meopar-tools.readthedocs.org/en/latest/SalishSeaCmd/salishsea-cmd.html#salishsea-get-cgrf
 
.. _tools docs: http://salishsea-meopar-tools.readthedocs.org/en/latest/SalishSeaTools/salishsea-tools.html#module-nc_tools

.. note::
   
   `get_cgrf`_ requires a link to :file:`altitude_CGRF.nc` in :file:`/NEMO-atmos/`.


.. [Smith_etal2013] Smith, G. C., Roy, F., Mann, P., Dupont, F., Brasnett, B., Lemieux, J.-F., Laroche, S. and Bélair, S. (2013), A new atmospheric dataset for forcing ice–ocean models: Evaluation of reforecasts using the Canadian global deterministic prediction system. Q.J.R. Meteorol. Soc.. doi: 10.1002/qj.2194 http://dx.doi.org/10.1002/qj.2194

.. [Holton1992] Holton, J., An introduction to dynamic meteorology 3rd edition (Acadmeic Press: 1992)
