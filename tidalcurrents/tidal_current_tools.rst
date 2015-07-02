.. _TidalCurrentsTools:

Tidal Currents Tools
=========================


Tidal currents
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Tidal currents are tightly knit with the hydrodynamics. When the currents are averaged over a long period of time what we have left is the tidal effect on the currents. We can average out the winds and surface currents anomalies. It is deeply dependant on bottom friction and mixing. Evaluating tidal currents enables one to see these effects at a chosen locations.

To calculate and evaluate tidal currents we need long (many months) currents time series for a particular location.


Observations
^^^^^^^^^^^^^

* Current data can be obtained or seen at `Ocean Networks Canada`_ (ONC) website. 
* The ONC data is then processed by Dr. Rich Pawlowicz.
* Phase and amplitude information can be reported in paper, we use  values reported in Foreman et al. (2004).


.. _Ocean Networks Canada: http://venus.uvic.ca/data/data-plots/#strait-of-georgia-plots


Tidal ellipses
^^^^^^^^^^^^^^^^^^^^

We perform a hamonic analysis on the time series in order to extract the tidal harmonic constants. From the speed and direction of the tidal current at one location over time we can construct harmonic constituents ellipse which is used to decribe the motion of the water due to one tidal constituent (eg. M2).

Seperating the constituents
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The tidal currents are composent of many different harmonic constituents like tides. When calculating and comparing tidal ellipses it is of one constituents at one location, it is important to have a long enough time series to be able to fully seperate the constituents. 


Freshet and changing shorlines
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is important to avoid using data for a tidal analysis during the Fraser River freshet. The outflow from the river is very larger from the end of may to mid June and this skews the tidal ellipses.
The bathymetry and shoreline of a location destically effects the tidal currents. However, they can change over time. It is important to select data that would have a consistent shorline and bathymetry across datasets that are being compared.


MATLAB Scripts
~~~~~~~~~~~~~~~

Some python functions have been written to facilitate calculating tidal ellipses, depth averaging and 

* :file:`get_ttide_8_filter.m` - This script does most of the work. ::

   get_ttide_8_filter(csvfilename, location, starts, ends, type)

This function uses water level observations stored in csvfilename to calculate tidal harmonics and tidal predictions over a time period defined by date strings :file:`starts` and :file:`ends`.
Water level observations can either be from the DFO website or the NOAA website, as specified by the type argument.
The calculated haromincs are saved in :file:`location_harmonics_date1_date2_filter.csv` where location is one of the arguments of :file:`get_ttide_8`.
:file:`date1` and :file:`date2` are string representations of the start and end date of the observation time series.

This function also saves a file called :file:`location_t_tide_compare8_starts_ends_snr2_filter.csv` where :file:`starts` and :file:`ends` are arguments of :file:`get_ttide_8_filter`.
This file contains three types of tidal predictions:

    + pred_all - predictions with all constituents except shallow water and ones with low signal to noise
    + pred_8 - predictions with only eight constituents
    + pred_noshallow - like pred_all but with no shallow water constituents.
* :file:`calculate_harmonics.m` and :file:`calculate_harmonics_NOAA.m` - these files perform the harmonics analysis for DFO and NOAA data respectively.
* :file:`filter_tides.m` and :file:`filter_tides_NOAA.m` - these files do the filtering work.
* :file:`get_ttide_8.m` and :file:`calculate_harmonics.m` - these files only work for DFO data and do not apply the filtering or removal of shallow water/ long period constituents.

.. note::

  The NOAA observations csv files should have the station's latitude in the second row, second column of the file.

Other tidal predictions
~~~~~~~~~~~~~~~~~~~~~~~~

A few other files have been developed to generate tidal predictions based on Canadian Hydrographic Service constituents.
These methods do not remove the long period/shallow water constituents.
These files are in a private repository :file:`private-tools/tides`.

* :file:`tide_pred8.m` - generate tidal prediction with all CHS constituents and with only eight.
The function works in a similar manner to the other ones.  ::

   tide_pred8(tidefile,location,starts, ends)

Predictions are saved in a file :file:`location_atide_compare8_starts_ends.csv`.

* :file:`read_harmonics.m` - read the CHS tidal harmonics from a file.

Storm surge forcing files
^^^^^^^^^^^^^^^^^^^^^^^^^^

Several notebooks have been developed for generating the anomaly forcing files used in simulation hindcasts.

* `SSH_Tofino.ipynb`_
* `SSH_PortHardy.ipynb`_

.. _SSH_Tofino.ipynb: http://nbviewer.ipython.org/urls/bitbucket.org/salishsea/tools/raw/tip/I_ForcingFiles/OBC/SSH_Tofino.ipynb

.. _SSH_PortHardy.ipynb: http://nbviewer.ipython.org/urls/bitbucket.org/salishsea/tools/raw/tip/I_ForcingFiles/OBC/SSH_PortHardy.ipynb


Analysis
^^^^^^^^^

Some analysis functions are stored in a module `salishsea_tools/stormtools`_.

.. _salishsea_tools/stormtools: http://salishsea-meopar-tools.readthedocs.org/en/latest/SalishSeaTools/salishsea-tools.html#module-stormtools

Examples include functions that calculate the observed residual, modelled residual, error statistics, and so on.

.. note::

  A different module was used for analysis in the AO storm surge paper. It is in a private repository :file:`storm-surge/stormtools_revisions.py`.The functions are almost identical as :file:`stormtools.py` but with a few minor changes

References
^^^^^^^^^^
Pawlowicz, R., B. Beardsley, and S. Lentz (2002). Classical tidal harmonic analysis including error estimates in matlab using t tide. Computers & Geosciences 28 (8), 929-937.

Parker, B. B. (2007). Tidal Analysis and Prediction. NOAA Special Publication  NOS CO-OPS 3.
