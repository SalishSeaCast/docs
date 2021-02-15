.. _SalishSeaModelResultsServer:

*******************************
Salish Sea Model Results Server
*******************************

Model Results & Forcing Files Storage
=====================================

The :file:`/results/` file system on :kbd:`skookum` houses the storage used for:

* the :ref:`NowcastProductionDeployment`
* the nowcast system run results
* the Salish Sea NEMO model spin-up run results
* results from the Environment Canada GEM 2.5km HRDPS operational model runs that are used to force the nowcast system runs,
  and results from the research deployment of that model that are periodically evaluated
* Fraser River run-off forcing files produced from data downloaded from the Environment Canada Wateroffice service that are used to force the nowcast system runs
* Neah Bay sea surface height forcing files produced from data downloaded from the NOAA water level observations and forecast service that are used to force the nowcast system runs


The :file:`/results/` file system is organized as follows::

  /results/
   |-- forcing/
   |   |-- atmospheric/
   |   |   `-- GEM2.5/
   |   |       |-- GRIB/
   |   |       |   |-- 20140911/
   |   |       |   |   |-- 00/
   |   |       |   |   |   |-- 001/
   |   |       |   |   |   |   |-- CMC_hrdps_west_APCP_SFC_0_ps2.5km_2015111200_P001-00.grib2
   |   |       |   |   |   |   |-- ...
   |   |       |   |   |   |   ~-- CMC_hrdps_west_APCP_SFC_0_ps2.5km_2015111200_P001-00.grib2
   |   |       |   |   |   |-- ...
   |   |       |   |   |   `-- 048/
   |   |       |   |   |       `-- ...
   |   |       |   |   |-- 06/
   |   |       |   |   |   `-- ...
   |   |       |   |   |-- 12/
   |   |       |   |   |   `-- ...
   |   |       |   |   `-- 18/
   |   |       |   |       `-- ...
   |   |       |   |-- ...
   |   |       |   |-- 20151113/
   |   |       |   |-- ...
   |   |       |-- operational/
   |   |       |   |-- fcst/
   |   |       |   |   |-- ops_y2014m11d18.nc
   |   |       |   |   |-- ...
   |   |       |   |   |-- ops_y2015m11d15.nc
   |   |       |   |   `-- ...
   |   |       |   |-- no_snow.nc@ -> /results/nowcast-sys/NEMO-forcing/atmospheric/no_snow.nc
   |   |       |   |-- ops_y2012m12d13.nc
   |   |       |   |-- ...
   |   |       |   |-- ops_y2015m11d13.nc
   |   |       |   `-- ...
   |   |       |   `-- weights-2.5kmGEM.nc@ -> /results/nowcast-sys/NEMO-forcing/grid/weights-2.5kmGEM.nc
   |   |       `-- research/
   |   |           |-- get_res.cron.sh
   |   |           |-- get_res.sh
   |   |           |-- no_snow.nc@ -> /results/nowcast-sys/NEMO-forcing/atmospheric/no_snow.nc
   |   |           |-- res_y2014m09d10.nc
   |   |           |-- ...
   |   |           |-- res_y2015m11d13.nc
   |   |           |-- ...
   |   |           `-- weights-2.5kmGEM.nc@ -> /results/nowcast-sys/NEMO-forcing/grid/weights-2.5kmGEM.nc
   |   |-- rivers/
   |   |   |-- R201702DFraCElse_y2014m09d01.nc
   |   |   |-- ..
   |   |   |-- R201702DFraCElse_y2018m08d18.nc
   |   |   `-- ..
   |   `-- sshNeahBay/
   |       |-- fcst/
   |       |   |-- ssh_y2014m10d29.nc
   |       |   |-- ...
   |       |   |-- ssh_y2015m11d16.nc
   |       |   `-- ...
   |       |-- obs/
   |       |   |-- ssh_y2014m10d28.nc
   |       |   |-- ...
   |       |   |-- ssh_y2015m11d12.nc
   |       |   `-- ...
   |       `-- txt/
   |           |-- sshNB_2014-10-29.txt
   |           |-- ...
   |           |-- sshNB_2014-12-06_17.txt
   |           |-- ...
   |           |-- sshNB_2015-11-13_20.txt
   |           `-- ...
   |-- lost+found/
   |-- nowcast-sys/
   |   |-- grid
   |   |   `-- ...
   |   |-- rivers_climatology
   |   |   `-- ...
   |   |-- tides
   |   |   `-- ...
   |   |-- tracers
   |   |   `-- ...
   |   |-- nowcast/
   |   |   |-- nowcast.yaml@ -> /results/nowcast-sys/tools/SalishSeaNowcast/nowcast.yaml
   |   |   `-- www/
   |   |       |-- templates@ -> /results/nowcast-sys/tools/SalishSeaNowcast/www/templates/
   |   |       `-- salishsea-site/
   |   |           `-- ...
   |   |-- nowcast-env/
   |   |   |-- bin/
   |   |   |-- conda-meta/
   |   |   |-- etc/
   |   |   |-- imports/
   |   |   |-- include/
   |   |   |-- lib/
   |   |   |-- mkspecs/
   |   |   |-- plugins/
   |   |   |-- sbin/
   |   |   |-- share/
   |   |   |-- ssl/
   |   |   `-- tests/
   |   |-- private-tools/
   |   |   `-- ...
   |   `-- tools/
   |       `-- ...
   `-- SalishSea/
       |-- forecast/
       |   |-- 15oct16/
       |   `-- ...
       |-- forecast2/
       |   |-- 15oct16/
       |   `-- ...
       |-- nowcast-blue/
       |   |-- 27oct14/
       |   |-- ...
       |-- nowcast-green/
       |   |-- 05dec15/
       |   `-- ...

The :file:`/results/lost+found/` directory is a `filesystem maintenance directory used by Linux`_.
Don't worry about it.

.. _filesystem maintenance directory used by Linux: https://unix.stackexchange.com/questions/18154/what-is-the-purpose-of-the-lostfound-folder-in-linux-and-unix


.. _SalishSeaModelResults:

Salish Sea Model Results
========================

The Salish Sea NEMO Model results from "production" runs of the model are stored in the :file:`/results/SalishSea/` directory.
The sub-directories there are:

* :file:`/results/SalishSea/forecast/`
    Results from the nowcast system daily forecast runs using NEMO-3.6 since 2016-10-15.
    Forecast for :kbd:`day + 1` based on restart file from nowcast run for :kbd:`day`,
    same atmospheric and river run-off forcing,
    and updated western boundary sea surface height forcing.

    Earliest daily results directory is :file:`/results/SalishSea/forecast/15oct16/`.
    Most,
    but not all dates since then are available.
    :file:`forecast/` runs are secondary priority
    (below :file:`nowcast/`)
    when the nowcast automation system has difficulties.

    See :ref:`NowcastResults` for details of the configuration and model parameter values changes over time.


* :file:`/results/SalishSea/forecast2/`
    Results from the nowcast system daily forecast2 runs using NEMO-3.6 since 2016-10-15.
    Forecast for :kbd:`day + 2` based on restart file from forecast run for :kbd:`day + 1`,
    updated atmospheric,
    river run-off forcing,
    and western boundary sea surface height forcing.

    Earliest daily results directory is :file:`/results/SalishSea/forecast2/15oct16/`.
    Most,
    but not all dates since then are available.
    :file:`forecast2/` runs are lowest priority
    (below :file:`forecast/`)
    when the nowcast automation system has difficulties.

    See :ref:`NowcastResults` for details of the configuration and model parameter values changes over time.



* :file:`/results/SalishSea/nowcast-blue/`
    Results from the nowcast system daily nowcast runs using NEMO-3.6 since 2016-10-15.

    Earliest daily results directory is :file:`/results/SalishSea/nowcast/15oct16/`.

    See :ref:`NowcastResults` for details of the configuration and model parameter values changes over time.

* :file:`/results/SalishSea/nowcast-green/`
    Results from the nowcast system daily nowcast green ocean runs.

    Earliest daily results directory is :file:`/results/SalishSea/nowcast-green/05dec15/`.

    See :ref:`NowcastResults` for details of the configuration and model parameter values changes over time.


Details of Configurations for Results
=====================================

.. toctree::
   :maxdepth: 2

   nowcast
