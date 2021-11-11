.. _StormSurgeNowcast:

Nowcast Stations
================

The Salish Sea Nowcast system provides water level predictions and storm surge warnings for several locations in the Salish Sea.
Initially, we used Point Atkinson, Victoria and Campbell River but we found a need to provide predictions for other key locations in the domain.
This section explains how to add a new station to the nowcast production system.


Adding a new station
^^^^^^^^^^^^^^^^^^^^

In order to add a new station, we first need some key information beyond the trivial latitude/longitude of the desired station.
This information includes:

1. Historical extreme water level and mean sea level.
2. A year long time series of water levels or a set of tidal constituents for generating tidal predictions.
3. Latitude and longitude of desired station.

Points 1 and 2 require that a tide gauge was in operation at this location at some point in history.
The reason we need this information is to calculate appropriate thresholds for high water based on historical values.
We also calculate a correction to the model water level using tidal predictions.

To search for this information, you can visit the DFO_ and NOAA_ websites.

.. _DFO: http://www.isdm-gdsi.gc.ca/isdm-gdsi/twl-mne/index-eng.htm

.. _NOAA: https://tidesandcurrents.noaa.gov/stations.html?type=Water+Levels


Steps for adding a new station to nowcast
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Adding new tidal stations requires access to the Salish Sea tools_ repository.
Most of the work will be on editing figures.py in the SalishSeaNowcast package.
You will also need access to scripts in the analysis-storm-surges_ repository and the nowcast :file:`iodef.xml` file in SS-run-sets_.

.. _tools: https://github.com/SalishSeaCast/tools

.. _analysis-storm-surges: https://github.com/SalishSeaCast/analysis-storm-surges

.. _SS-run-sets: https://github.com/SalishSeaCast/SS-run-sets


1. Add extreme water level, mean sea level, latitude and longitude for this location in the PLACES dictionary object of :file:`places.py`.
2. Add location name to TIDAL_SITES list in :file:`figures.py`.
3. Use the MATLAB scripts in :file:`analysis-storm_surges/tide_analysis_scripts/generate_tidal_predictions.m` to generate Jan 1, 2015 to Jan 1, 2020 tidal predictions for your location.

   * The input file can either be a year-long water level time series from NOAA/DFO or a constituent file from DFO or NOAA.
   * If using a water level time series, be sure that the time zone is PST and the latitude is added to the second row, second column of the csv file.
   * Use :file:`exclude_long=1`, :file:`cut_off=0.3`.
   * Copy the tidal predictions output file to :file:`tools/SalishSeaNowcast/tidal_predictions/`. Add this file to the repository, commit and push.
   * See :ref:`StormTools` for a description of how to use the MATLAB scripts.
   * You will need ttide_ to run these scripts.

.. _ttide: https://www.eoas.ubc.ca/~rich/#T_Tide

.. warning::

  Some of the constituents published on the NOAA_ website are not recognized by ttide_. So, it is best to use a time-series to produce tidal predictions at a NOAA site.


4. Look up the grid point indices of your station and add a 15 minute ssh output to :file:`SS-run-sets/SalishSea/nowcast/iodef.xml`
5. Test ssh plotting calls with your new station in `OutTemplate.ipynb`_

   * Testing won't be possible until we have a 15 minute output file for this station. You can test the process by linking one of the existing 15 minute files into a new file with the appropriate name. The plots will be gibberish but you can test the procedure.

.. _OutTemplate.ipynb: https://nbviewer.org/github/SalishSeaCast/SalishSeaNowcast/blob/main/notebooks/Out_Template.ipynb

6. Add appropriate calls to plotting functions and saving/displaying figures in :file:`tools/SalishSeaNowcast/nowcast/make_plots.py` and :file:`tools/SalishSeaNowcast/nowcast/make_site_page.py`.

   * Testing the workers won't be possible until we have a 15 minute output file for this station. You can test the process by linking one of the existing 15 minute files into a new file with the appropriate name. The plots will be gibberish but you can test the procedure.


