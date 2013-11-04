Progress on tidal comparisons
===========================================

Time series comparisons
--------------------------------------

Water level
*********** 

* Measured water level observations at DFO stations can be compared to water level time series from NEMO

* ipython notebook: `comp_wlev_ts.ipynb`_

.. _comp_wlev_ts.ipynb: http://nbviewer.ipython.org/urls/bitbucket.org/salishsea/tools/raw/tip/compare_tides/comp_wlev_ts.ipynb

* TO DO: make this notebook into a function that plots time vs. water level at the station location for model and measured: ::

	comp_wlev_ts(model_run,stat_num)

Currents
**********
 
* Measured current time series - have not got measured time series of currents yet could be compared to current time series from NEMO

* no ipython notebook yet

* Would be nice to write a function that would plot tidal ellipses at the station location for model and measured: ::
	
	comp_current_ts(model_run,stat_num)

Harmonics comparisons
-------------------------------------------

Water level 
************

M2 and K1
+++++++++++++++++++

* Measured M2 and K1 harmonics from water level (calculated and shown by Foreman et al (1995))  can be compared to harmonics calculated inside NEMO model

* ipython notebook: `comp_wlev_harm.ipynb`_

.. _comp_wlev_harm.ipynb: http://nbviewer.ipython.org/urls/bitbucket.org/salishsea/tools/raw/tip/compare_tides/comp_wlev_harm.ipynb

* This notebook has a switch for choosing which model run to analyse. Then it evaluates the model's performance by calculating differences (as described by Foreman et al (1995) and Masson & Cummins (2004)) and saving these to a text file. Finally, the notebook plots the differences as circles of varying radii on a map of the bathymetry. So far, the notebook only does this for M2.

* This notebook will be made into a function that calculates differences (as described by Foreman et al (1995) and Masson & Cummins (2004)) for each constituent and save them to a text file at all stations in the model domain: ::
	
	comp_wlev_harm_M2(model_run)
	comp_wlev_harm_K1(model_run)

Other constituents
+++++++++++++++++++++

* Other constituents will have to be either requested from someone who has calculated them (e.g. Foreman) or calculated from time series using t_tide.

* no ipython notebook yet

* something like: ::
	
	comp_wlev_harm_minor(model_run)


Currents
************

M2 and K1
+++++++++++++++++++

* M2 and K1 harmonics calculated from current measurements (calculated and shown by Foreman et al (1995))  can be compared to harmonics calculated inside NEMO model. 

* Not sure how to compare harmonics from model to measured harmonics, because measured harmonics are rectilinear and just have a direction. Which is OK for converting the measured amplitude to U and V components, but I don't know what to do with the phase...

* Also, the bathymetry is not well resolved around many of the measured points (because they're in narrow inlets and passes) so there is no model point to compare to the measurements

* ipython notebook: `comp_current_harm.ipynb`_

.. _comp_current_harm.ipynb: http://nbviewer.ipython.org/urls/bitbucket.org/salishsea/tools/raw/tip/compare_tides/comp_current_harm.ipynb

* something like: ::

	comp_current_harm_M2(model_run)
	comp_current_harm_K1(model_run)

Other constituents
+++++++++++++++++++++

* TO DO: Other measured current constituents will have to be either requested from someone who has calculated them (e.g. Foreman) or calculated from time series using t_tide.

* no ipython notebook yet

* something like: ::

	comp_current_harm_minor(model_run)




