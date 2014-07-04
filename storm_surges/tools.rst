.. _StormTools:

Analysis tools
======================================================================================================

An sample script for analyzing storm surge results can be found here: `analysisSS.ipynb <http://nbviewer.ipython.org/urls/bitbucket.org/salishsea/analysis/raw/tip/storm_surges/analysisSS.ipynb>`_.

There are also a variety of functions written in :file:`salishsea_tools/stormtools`.

The surge
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
We are often interested in the behaviour of the surge component of the water level, that is the anomaly after the tides have been removed. 
Further, we could like to compare the modelled surge to the observed surge. 
To determine the modelled surge, we perform two simulations: one with all of our forcing conditions, including the tides, atmopsheric conditions, rivers, and sea surface height at the open boundaries and another with only the tidal forcing and rivers. 
We define the modelled surge as the difference between the sea surface height of these two simulations.

To calculate the observed surge, we need water level observations and tidal predictions.
Water level observations are easily obtained from the `Fisheries and Oceans Canada`_ (DFO) website. 
Tidal predictions can be calculated using a package called :file:`t_tide` (Pawlowicz et al., 2002).

.. _Fisheries and Oceans Canada: http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/maps-cartes/inventory-inventaire-eng.asp
 

Tidal predictions
^^^^^^^^^^^^^^^^^
Using, the :file:`t_tide` package, we have a matlab script that will calculate and save tidal harmonics and predictions. 
It also calculates tidal predictions using only the 8 consituents with which we are forcing the model. 
We would like to know what error might appear by only using 8 constituents.

The MATLAB script is called :file:`get_ttide_8.m` and is found in the :file:`analysis/storm_surges/data/` repository. ::

  get_ttide_8(csvfilename,location, starts, ends)

This MATLAB function takes in the csv file with water level observations from the DFO website to calculate tidal harmonics and tidal predictions over a time period defined by date strings :file:`starts` and :file:`ends`.
It then saves harmonics in :file:`location_harmonics_date1_date2.csv` where location is one of the arguments of :file:`get_ttide_8`. 
:file:`date1` and :file:`date2` are string representations of the start and end date of the observation time series. 
For years heavy in storm surges, it is suggested to use a time series from the year prior to the surge event of interest since a large surge may affect the harmonic analysys.

This function also saves a file called :file:`location_t_tide_compare8_starts_ends.csv` where :file:`starts` and :file:`ends` are arguments of :file:`get_ttide_8`.
This file contains tidal predictions with all the consituents determined by :file:`t_tide` and the 8 used to force the model. 
These files are then used to determine the observed surge.      



References
^^^^^^^^^^
Pawlowicz, R., B. Beardsley, and S. Lentz (2002). Classical tidal harmonic analysis including error estimates in matlab using t tide. Computers & Geosciences 28 (8), 929-937.

