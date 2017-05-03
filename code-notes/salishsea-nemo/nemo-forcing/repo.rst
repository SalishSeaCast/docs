Getting the Repo
================

Team members using SSH key authentication on Bitbucket may clone the :ref:`NEMO-forcing-repo` repo with:

.. code-block:: bash

    hg clone ssh://hg@bitbucket.org/salishsea/nemo-forcing NEMO-forcing

For password authentication use:

.. code-block:: bash

    hg clone https://<you>@bitbucket.org/salishsea/nemo-forcing NEMO-forcing

where :kbd:`<you>` is your Bitbucket user id.


Repo Contents
=============

:file:`atmospheric/` Directory
------------------------------

The :file:`atmospheric/` directory contains forcing data that supports the :ref:`AtmosphericForcing` of the model.

:file:`no_snow.nc`: A :ref:`CGRF-Dataset`-like file that can be used as annual climatology to impose the :ref:`NoSnowConstraint`. Created by the :ref:`tools-repo` repo `I_ForcingFiles/Atmos/NoSnow.ipynb`_ notebook.

.. _I_ForcingFiles/Atmos/NoSnow.ipynb: https://nbviewer.jupyter.org/urls/bitbucket.org/salishsea/tools/raw/tip/I_ForcingFiles/Atmos/NoSnow.ipynb


.. _grid-directory:

:file:`grid/` Directory
-----------------------

The :file:`grid/` directory contains coordinates,
bathymetry,
and interpolation weights files.


Full Salish Sea Domain
~~~~~~~~~~~~~~~~~~~~~~

NEMO has the file names of the coordinates and bathymetry files hard-coded as :file:`coordinates.nc` and :file:`bathy_meter.nc` so the files used for a particular run-set need to be copied or symlinked to those names.

Coordinates and bathymetry (original and with smoothed Juan de Fuca mouth)

* :file:`coordinates_seagrid_SalishSea.nc`
* :file:`bathy_meter_SalishSea.nc`
* :file:`bathy_meter_SalishSea2.nc`

.. _SalishSeaBathy-image:

.. figure:: images/SalishSeaBathy.png

    Full Salish Sea domain bathymetry.

Interpolation weights:

* :file:`weights-CGRF.nc`: :ref:`AtmosphericForcing` weights for Interpolation On the Fly
  (IOF)
  from the :ref:`CGRF-Dataset`


Initial Tests Sub-Domain
~~~~~~~~~~~~~~~~~~~~~~~~

Coordinates and bathymetry for the initial sub-domain test case known as :kbd:`JPP` or :kbd:`WCSD_RUN_tide_M2_OW_ON_file_DAMP_ANALY`:

* :file:`SubDom_coordinates_seagrid_WestCoast.nc`
* :file:`SubDom_bathy_meter_NOBCchancomp.nc`

.. _SalishSeaSubdomainBathy-image:

.. figure:: images/SalishSeaSubdomainBathy.png

    Sub-domain bathymetry used for initial tests.

:file:`open_boundaries/west` Directory
---------------------------------------

The :file:`open_boundaries/` directory contains the open boundary forcing information for the full domain.  Currently we are only using the :file:`west/` subdirectory as we have the north boundary closed.

***Original TS Files***

Original file specifies set temperature and salinity (uniform across the boundary) from Thomson et al, 2007 winter picture.

* :file:`SalishSea_bdyT_tra.nc`

Two files keep the original TS until the model starts and then move the Thomson et al, 2007 winter picture.  Two files are for the two bathymetries.

* :file:`SalishSea_Soft_tra.nc`
* :file:`SalishSea2_Soft_tra.nc`

The preparation of the NetCDF files is done by the python notebook `I_ForcingFiles/OBC/PrepareSimpleOBC.ipynb`_
and the soft start combination is done in the python notebook `I_ForcingFiles/OBC/TS_OBC_Softstart.ipynb`_

.. _I_ForcingFiles/OBC/PrepareSimpleOBC.ipynb: https://nbviewer.jupyter.org/urls/bitbucket.org/salishsea/tools/raw/tip/I_ForcingFiles/OBC/PrepareSimpleOBC.ipynb
.. _I_ForcingFiles/OBC/TS_OBC_Softstart.ipynb: https://nbviewer.jupyter.org/urls/bitbucket.org/salishsea/tools/raw/tip/I_ForcingFiles/OBC/TS_OBC_Softstart.ipynb

***Seasonal TS Climatology***

Diane Masson through Wendy Callendar provided model results from Masson and Fine (2013) for all their model years at the mouth of Juan de Fuca.  These were interpolated onto our z-levels and then interpolated onto our horizontal grid.

* :file:`SalishSea_TS_Masson.nc`

The multiple years were averaged and re-sampled on a weekly basis to give a climatology

* :file:`SalishSea2_Masson_Clim.nc`

Then the partial cells had their TS corrected to the proper depth (depth-corrected, DC)

* :file:`SalishSea2_Masson_DC.nc`

The three steps are done in the python notebooks `Tools/I_ForcingFiles/OBC/MakeTSfromMasson.ipynb`_ and `Tools/I_ForcingFiles/OBC/MassonClimatology.ipynb`_ and `Tools/I_ForcingFiles/OBC/MassonClimDC.ipynb`_ respectively.

.. _Tools/I_ForcingFiles/OBC/MakeTSfromMasson.ipynb: https://nbviewer.jupyter.org/urls/bitbucket.org/salishsea/tools/raw/tip/I_ForcingFiles/OBC/MakeTSfromMasson.ipynb
.. _Tools/I_ForcingFiles/OBC/MassonClimatology.ipynb: https://nbviewer.jupyter.org/urls/bitbucket.org/salishsea/tools/raw/tip/I_ForcingFiles/OBC/MassonClimatology.ipynb
.. _Tools/I_ForcingFiles/OBC/MassonClimDC.ipynb: https://nbviewer.jupyter.org/urls/bitbucket.org/salishsea/tools/raw/tip/I_ForcingFiles/OBC/MassonClimDC.ipynb

*** ssh Climatology ***

The sea surface height climatology is taken from 2000-2010 hourly sea surface height at Tofino (`http://tides.gc.ca/eng/data`_), averaged over years and by the month to give a climatology, which is in

* :file:`SS2_SSH_climatology.nc`

Preparation of the file was done in Jupyter Notebook `I_ForcingFiles/OBC/SSH.ipynb`_

.. _http://tides.gc.ca/eng/data: http://tides.gc.ca/eng/data
.. _I_ForcingFiles/OBC/SSH.ipynb: https://nbviewer.jupyter.org/urls/bitbucket.org/salishsea/tools/raw/tip/I_ForcingFiles/OBC/SSH.ipynb

:file:`open_boundaries/west/tides/` Directory
----------------------------------------------

The :file:`open_boundaries/west/tides/` sub-directory contains the tidal forcing files for the western boundary of the full domain.

Currently there are two complete sets of files that specify eight tidal components.  These files specify the cosine and sine components for the sea surface height (T), the x-direction velocity (U) and y-direction velocity (V).  Files are of the form

* :file:`bathy_west_sea_tide_component_grid_TUV.nc`

where bathy is one of *SalishSea* or *SalishSea2*, the latter specifying the bathymetry with the smoothed mouth of JdF, component is one of *M2*, *K1*, *O1*, *Q1*, *K2*, *P1*, *S2*, *N2* specifying which of the eight tidal components and TUV is one of *T*, *U*, *V* specifying the parameter in the file.

The tidal components come from `Webtide`_ and are prepared in the Jupyter Notebook `I_ForcingFiles/Tides/Prepare Tide Files.ipynb`_

.. _Webtide: http://www.bio.gc.ca/science/research-recherche/ocean/webtide/index-eng.php
.. _I_ForcingFiles/Tides/Prepare Tide Files.ipynb: https://nbviewer.jupyter.org/urls/bitbucket.org/salishsea/tools/raw/tip/I_ForcingFiles/Tides/Prepare%20Tide%20Files.ipynb

:file:`initial_strat/` Directory
--------------------------------

The :file:`initial_strat/` directory contains initial data for the temperature and salinity fields.

***JPP Domain***

Currently there are two files for the JPP Sub-domain based on 500 m maximum water depth and 40 vertical levels.

* :file:`SoG0318_1y_temperature_nomask.nc`
* :file:`SoG0318_1y_salinity_nomask.nc`

The data is horizontally uniform, based on STRATOGEM profile at S4-1 in September 2003 from cruise 0318.  Original data file is

* :file:`sg0318006.cnv`

and the profile is plotted in

* :file:`sg0318006_profile.ps`
* :file:`sg0318006_profile.fig`

***Full Domain***

There are two files each for the Salish Sea domain (SS) and the Salish Sea domain with smoothed Juan de Fuca mouth (SS2).  The latter is depth-corrected for partial cells.

* :file:`SS_SoG0318_1y_temperature_nomask.nc`
* :file:`SS_SoG0318_1y_salinity_nomask.nc`
* :file:`SS2_SoG0318_1y_temperature_nomask.nc`
* :file:`SS2_SoG0318_1y_salinity_nomask.nc`

The preparation of the NetCDF files is done by the python notebook `Tools/I_ForcingFiles/Initial/PrepareTS.ipynb`_

.. _Tools/I_ForcingFiles/Initial/PrepareTS.ipynb: https://nbviewer.jupyter.org/urls/bitbucket.org/salishsea/tools/raw/tip/I_ForcingFiles/Initial/PrepareTS.ipynb

:file:`Rivers/`: Directory
--------------------------

The :file:`Rivers/` directory contains forcing data for the rivers.

Currently there are four files, for the full Salish Sea domain and two preparation files.  One that puts a constant 2000 m3/s out of the southern arm of the Fraser River.

* :file:`rivers_Fraser_only_cnst.nc`

and one that includes all three mouths of the Fraser, Burrard Inlet, Squamish River, Puntledge (Comox), Nanaimo and Cowichan.  All at constant values (yearly averages).

* :file:`rivers_Fraserplus_cnst.nc`

The preparation of these NetCDF files is done by the python notebook `Tools/I_ForcingFiles/Rivers/AddRivers.ipynb`_

.. _Tools/I_ForcingFiles/Rivers/AddRivers.ipynb: https://nbviewer.jupyter.org/urls/bitbucket.org/salishsea/tools/raw/tip/I_ForcingFiles/Rivers/AddRivers.ipynb

Morrison et al, 2011 prepared climatologies of a number of river watersheds in the Salish Sea region.  These are in the files

* :file:`Salish_allrivers_monthly.nc`
* :file:`Salish_allrivers_cnst.nc`

The construction of these NetCDF files is done by the python notebook `Tools/I_ForcingFiles/Rivers/Prep_Seasonal_Rivers.ipynb`_

.. _Tools/I_ForcingFiles/Rivers/Prep_Seasonal_Rivers.ipynb: https://nbviewer.jupyter.org/urls/bitbucket.org/salishsea/tools/raw/tip/I_ForcingFiles/Rivers/Prep_Seasonal_Rivers.ipynb

and then this information is downscaled onto 150 rivers to give the forcing files:

* :file:`rivers_month.nc`
* :file:`rivers_cnst.nc`

The preparation of these NetCDF files is done by the python notebook `Tools/I_ForcingFiles/Rivers/Add Rivers Month and Constant.ipynb`_

.. _Tools/I_ForcingFiles/Rivers/Add Rivers Month and Constant.ipynb: https://nbviewer.jupyter.org/urls/bitbucket.org/salishsea/tools/raw/tip/I_ForcingFiles/Rivers/Add\%20Rivers%20Month%20and%20Constant.ipynb


:file:`bdydta/` Directory
-------------------------

The :file:`bdydta/` directory contains forcing data for the open boundaries of the sub-domain used for initial tests.

Currently there are three files that specify the M2 tidal components for the Western and Northern open boundaries of the West Coast SubDomain.  These files specify the cosine and sine components for the sea surface height (T), the x-direction velocity (U) and y-direction velocity (V).

* :file:`JPP_bdytide_M2_grid_T.nc`
* :file:`JPP_bdytide_M2_grid_U.nc`
* :file:`JPP_bdytide_M2_grid_V.nc`
