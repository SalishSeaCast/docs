Tide Tuning
===========

This document outlines our efforts to best tune the tides.  Emphasis was on tuning the tides in the Strait of Georgia.  Specifically, in the final tuning, the western tides were tuned to produce the best match with data at Point Atkinson, Gibsons Landing, Winchelsea and Halfmoon Bay and the northern tides were tuned to produce the best math with data at Kelsey Bay and Yorke Island.

The modelled tidal response is compared to observations from Foreman et al. (1995), Foreman et al. (2004), and Foreman et al. (2012).

Step One: New Base Run: M2 & K1 only
------------------------------------

Based on the tidal variations study, we started with a run that included a shift downward by 5 degrees at the west for both the M2 and K1 tides and an increase in amplitude of 15% for K1.  Run time was 5 days, bottom friction was 0.005, viscosity was 15 :math:`m^2 s^{-1}`.  (Called CBase)

Step Two: Vary the Velocity Phase for M2
----------------------------------------

A series of runs (Cd10, Cd20, Cd30, Cu10, Cu20, Cu30) were done which varied the phase of the tidal velocity field at the western boundary compared the sea level.  Increasing the phase by 30 degrees, decreased the phase difference between Point Atkinson and Port Renfrew by 2 degrees.

However, this run (Cu30) had low amplitude and late phase everywhere.  Three runs were done (RC_amp, RC_pha, RC_both), that corrected the amplitude, phase and both, respectively.  Correcting the amplitude and phase, returned the phase difference to its original value.

Thus, varying the velocity phase relative to the sea surface height phase was abandoned.  (Two more runs FTN and FTK1 were done, tuning the north boundary and the west K1).

Step Three: Correct North Flux
------------------------------

The cross-section of the northern boundary is larger than the cross-section where the north measurements were made by Thomson.  This required a correction (reduction) in the northern flux.  Double the correction was used for M2 to better match with observations.

* Jupyter Notebook: `Analysis8Components.ipynb`_

.. _Analysis8Components.ipynb: https://nbviewer.jupyter.org/github/SalishSeaCast/tools/blob/master/I_ForcingFiles/Tides/johnstone_strait_tides.ipynb

This run was called CBase2.

Step Four: Correct west K1 and north M2
---------------------------------------

The western K1 amplitude and phase were tuned and the northern M2 was tuned (run FTK1b)

Step Five: Western M2 Flux
--------------------------

From tide variations, we noticed that increasing the velocity flux for the M2 tide at the west, decreased the phase error.  In Flux_RC we also tuned the phase and amplitude forthe western M2 tide.

Step Six: Add O1
----------------

A series of runs including the next largest constituent (O1) were run.  After the first one listed below, all runs were done for 10 days because at 5 days the K1 and O1 could not be reliably separated.

* Flux_RC_wO1 : same as Flux_RC but including O1 tide
* Cbase_RC_wO1 : same as FTK1b (no M2 Flux correction) but with adjusted western M2 phase and amplitude and including O1 tide
* Flux_RC2_wO1 : like Flux_RC_wO1 but tuning M2 phase and amplitude, west and north
* Flux_RC3_wO1 : like Flux_RC2_wO1 but further tuning of west M2, west and north K1/O1

Note that for diurnal constituents O1, P1 and Q1 the ratio of their amplitude to K1 and difference of their phase to K1 was tuned.  Similarly for S2, N2 and K2 relative to M2.  This was on the suggestion of David Greenberg and worked very well.

Step Seven: Add S2
------------------

* Flux_RC3_wO1S2 : like Flux_RC3_wO1 but include S2
* RC4_wO1S2: further fine tuning of M2, K1, O1 and S2 phase and amplitude west and north

Step Eight: Add more
--------------------

* RC4_w01S2P1N2 : add another two
* RC4_wO1S2P1N2Q1K2 : all eight

10 days was not enough to separate constituents so further runs were done with 20 days.

From RC5-RC7, continued fine tuning.

Then moved to 30 days for RC8 and 40 days for R9 and further.

From RC9 on, tuning used Newton-Ralphson method assuming that the western amplitude only affected the amplitude in SoG, the western phase determines the phase in SoG and the northern amplitude and phase determined the Johnstone Strait amplitude and phase, respectively.  There is indeed little correlation from the west to the north.  However, phase does affect amplitude and vice versa.

Stopping
--------

For the longer runs, error estimates were made by sub-sampling the full time-series multiple times.  This allowed an estimate of the error in determining the harmonics.  Once the difference between the mean value and the observations was less than twice the standard deviation of the model results, the tuning was consider finished.

At RC13 it was determined that the western tides were tuned.

At corr15 it was determined that the northern tides were tuned.

Full run parameters are at:

* spreadsheet: `TideTuningRunParameters.ods`_

.. _TideTuningRunParameters.ods: https://bitbucket.org/salishsea/analysis/src/tip/compare_tides/TideTuningRunParameters.ods?at=default

Newton-Ralphson Process is at:

* spreadsheet: `diagnostics.ods`_

.. _diagnostics.ods: https://bitbucket.org/salishsea/analysis/src/tip/Susan/diagnostics.ods?at=default

And final results can be seen in:

* Jupyter Notebook: `Analysis8Components`_

.. _Analysis8Components: https://nbviewer.jupyter.org/urls/bitbucket.org/salishsea/analysis/raw/tip/compare_tides/Analysis8Components.ipynb

