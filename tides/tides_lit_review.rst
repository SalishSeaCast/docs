Literature review (tides)
===================================

Evaluating the performance of the NEMO model of the Strait of Georgia will begin with an evaluation of its skill at reproducing the tides in the domain. 

(a) how are models evaluated in terms of tidal data?

(b) how are tidal forcings applied at boundaries?

Foreman et al. (1995) 
-------------------------
Barotropic model of the Strait of Georgia, does not include stratification or estuarine flow

*(a) how is model evaluated?*

* Used tidal data at 38 sites, subsampled from a list of 90 tidal stations (taken from Parker 1977) to only include stations with more than 120 days of records 
* Calculated amplitudes and phases of the eight major consituents at each tidal observation site by harmonic analysis (Foreman 1977) 
* 'As discussed and illustrated by Foreman and Walters [1990], the harmonics calculated from the observations are only estimates of the true tides.' (Foreman et al 1995)

*(b) how are tidal forcings applied at boundaries?*

* :math:`e^{i \omega t}` time dependence assumed for each tidal constituent with frequency :math:`\omega`
* 8 tidal constituents used at boundaries (M2, S2, N2, K2, K1, O1, P1 and Q1) plus a residual tide and compound tides and overtides as a result of nonlinear interactions between these constituents
* Zero flow normal to the coast at boundaries
* Specified elevations on open sea boundaries mostly taken from observations

	- cotidal charts (Parker 1977, Crean et al. 1988) were sometimes used to deduce trends for interpolation and extrapolation along boundaries

* Specific boundaries (see Figure 2 of Foreman et al. (1995) for map):

	- Admiralty Inlet boundary: forced with Port Townsend and Admiralty Head tidal harmonics
	- Juan de Fuca boundary: forced with Sheringham Point and Seiku tidal harmonics
	- Northern boundaries: forced with Irvines Landing, Northwest Bay, Squitty Bay, False Bay and Skerry Bay tidal harmonics

Foreman et al. (2000)
---------------------------
Model of the north east Pacific Ocean, including Alaskan and BC shelf (no detail in Strait of Georgia)

*(a) how is model evaluated?*

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

*(b) how are tidal forcings applied at boundaries?*

Masson & Cummins (2004)
------------------------------------

(Description of model)

*(a) how is model evaluated?*

*(b) how are tidal forcings applied at boundaries?*

Sutherland & MacCready (2011)
-------------------------------------------

(Description of model)
Group's website: http://faculty.washington.edu/pmacc/MoSSea/index.html

*(a) how is model evaluated?*

* Calibrated their model with tidal data from NOAA tide gauges (none in BC, all in OR and WA)
* Amplitude, phase and spring-neap variability of tidal signals
* Compared ratios of modeled to observed amplitudes of M2, S2 and K1 constituents using Pawlowicz et al (2002) - better skill at diurnal frequencies because at semi-diurnal frequencies, modeled amplitude was too low
* Calculated Skill Score (SS)

*(b) how are tidal forcings applied at boundaries?*

References
-------------------------
* Foreman, M.G.G., R.A. Walters, R.F. Henry, C.P. Keller and A.G. Dolling, 1995. A tidal model for eastern Juan de Fuca Strait and the southern Strait of Georgia, Journal of Geophysical Research, 100, 721-740.

* Foreman, M.G.G., W.R. Crawford, J.Y. Cherniawsky, R.F. Henry and M.R. Tarbottom. 2000. A high-resolution assimilating tidal model for the northeast Pacific Ocean. Journal of Geophysical Research, 105, 28,629-28,652.

* Masson, D. and P.F. Cummins, 2004. Observations and modeling of seasonal variability in the Straits of Georgia and Juan de Fuca, Journal of Marine Research, 62, 491-516.

* Sutherland, D.A. and P. MacCready, 2011. A model study of the Salish Sea estuarine circulation, Journal of Physical Oceanography, 41, 1125-1143.
