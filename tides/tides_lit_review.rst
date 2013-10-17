Literature review (tides)
===================================

Evaluating the performance of the NEMO model of the Strait of Georgia will begin with an evaluation of its skill at reproducing the tides in the domain. So how have other authors done this?

Also, how have other authors forced their models with tidal data?

Foreman et al. (1995) 
-------------------------
1. 	* Barotropic model of the Strait of Georgia

	* Used tidal data at 38 sites, subsampled from a list of 90 tidal stations (taken from Parker 1977) to only include stations with more than 120 days of records. 

	* Calculated amplitudes and phases of the eight major consituents at each tidal observation site by harmonic analysis (Foreman 1977)

	* 'As discussed and illustrated by Foreman and Walters [1990], the harmonics calculated from the observations are only estimates of the true tides.' (Foreman et al 1995)

2. 	* Model forced with data

Foreman et al. (2000)
---------------------------

* Model of the north east Pacific Ocean, including Alaskan and BC shelf (no detail in Strait of Georgia)

* Compared model :math:`M_2` and :math:`K_1` harmonics against those calculated from harmonic analysis of 5.3 years of altimeter observations (Topex/Poseidon), at locations of crossover within the model domain

* Model accuracy was determined by calculating RMS differences between modelled and observed tidal harmonics at the crossover locations:

.. math:: 
	D_{rms} = (C^2_{rms}+S^2_{rms})^{1/2}

	C_{rms} = [\sum_1^N(A_t \cos G_T - A_m \cos G_m)^2/N]^{1/2}

	S_{rms} = [\sum_1^N(A_t \sin G_T - A_m \sin G_m)^2/N]^{1/2}

where N is the number of crossover sites, :math:`A_T`, :math:`G_T`, :math:`A_m` and :math:`G_m` are the altimeter and modelled amplitudes and phases respectively

* :math:`M_2` differences showed that modelled amplitude and phase lags were generally too small, possibly due to the existence of an amphidromic ridge near the south and west boundaries

* :math:`K_1` amplitude and phase lags looked better

* other semidiurnal and diurnal consituents had similar (but scaled down) inaccuracies

* assimilated 

Masson & Cummins (2004)
------------------------------------

* to do
  .. to do::

      Add this section

Sutherland & MacCready (2011)
-------------------------------------------
* Calibrated their model with tidal data from NOAA tide gauges (none in BC, all in OR and WA)

* Amplitude, phase and spring-neap variability of tidal signals

* Compared ratios of modeled to observed amplitudes of M2, S2 and K1 constituents using Pawlowicz et al (2002) - better skill at diurnal frequencies because at semi-diurnal frequencies, modeled amplitude was too low

* Calculated Skill Score (SS)

* Group's website: http://faculty.washington.edu/pmacc/MoSSea/index.html

References
-------------------------
* Foreman, M.G.G., R.A. Walters, R.F. Henry, C.P. Keller and A.G. Dolling, 1995. A tidal model for eastern Juan de Fuca Strait and the southern Strait of Georgia, Journal of Geophysical Research, 100, 721-740.

* Foreman, M.G.G., W.R. Crawford, J.Y. Cherniawsky, R.F. Henry and M.R. Tarbottom. 2000. A high-resolution assimilating tidal model for the northeast Pacific Ocean. Journal of Geophysical Research, 105, 28,629-28,652.

* Masson, D. and P.F. Cummins, 2004. Observations and modeling of seasonal variability in the Straits of Georgia and Juan de Fuca, Journal of Marine Research, 62, 491-516.

* Sutherland, D.A. and P. MacCready, 2011. A model study of the Salish Sea estuarine circulation, Journal of Physical Oceanography, 41, 1125-1143.
