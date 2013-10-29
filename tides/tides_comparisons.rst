Progress on tidal comparisons
===========================================

Time series comparisons
--------------------------------------

Water level
*********** 

* Measured water level observations at DFO stations can be compared to water level time series from NEMO

* ipython notebook: tools/dfo_w_level/DFO_waterleveldownload.ipynb

* Eventually will make this notebook into a function that plots time vs. water level at the station location for model and measured: ::

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

* ipython notebook: tools/dfo_w_level/harm_comp_model_foreman95.ipynb

* Eventually will write this notebook into a function that will calculate differences (as described by Foreman et al (1995) and Masson & Cummins (2004)) for each constituent and save them to a text file at all stations in the model domain: ::
	
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

* TO DO: M2 and K1 harmonics calculated from current measurements (calculated and shown by Foreman et al (1995))  can be compared to harmonics calculated inside NEMO model

* no ipython notebook yet

* something like: ::

	comp_current_harm_M2(model_run)
	comp_current_harm_K1(model_run)

Other constituents
+++++++++++++++++++++

* TO DO: Other measured current constituents will have to be either requested from someone who has calculated them (e.g. Foreman) or calculated from time series using t_tide.

* no ipython notebook yet

* something like: ::

	comp_current_harm_minor(model_run)




