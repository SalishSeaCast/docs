Literature review (tides)
===================================

Evaluating the performance of the NEMO model of the Strait of Georgia will begin with an evaluation of its skill at reproducing the tides in the domain. 

(a) how are models evaluated in terms of tidal data?

(b) how are tidal forcings applied at boundaries?

.. _foremanetal95:

Foreman et al. (1995) 
-------------------------
Three dimensional, barotropic model of eastern Juan de Fuca Strait and southern Strait of Georgia, does not include stratification, wetting/drying or estuarine flow

*(a) how is model evaluated?*

* For tidal heights, amplitudes and phases of the eight major consituents at each tidal observation site were calculated by harmonic analysis (Foreman 1977) 
* For tidal currents, ellipse parameters from a similar analysis were used for comparisons
* Used tidal data at 38 sites, subsampled from a list of 90 tidal stations (taken from Parker 1977) to only include stations with more than 120 days of records 
* Used current meter data from 10 sites
* Model values at each observation location were interpolated between model node values
* Differences calculated as distances in the complex plane:

	:math:`D = [(A_0 \cos g_0 - A_m \cos g_m)^2 + (A_0 \sin g_0 - A_m \sin g_m)^2]^{1/2}`

	where :math:`A_0`, :math:`A_m`, :math:`g_0` and :math:`g_m` are observed and modelled amplitudes and phases

* Also calculated root-mean-square differences between all measured and modelled amplitudes and phase differences

	- rms amplitude differences all within 2.0cm
	- rms phase differences all within 6.3 degrees

* Plotted out co-amplitude and co-phase lines for major constituents to compare to Crean et al (1988)
* Ellipses were compared (qualitatively?) between measured currents and modelled currents throughout the water column, but could not account for baroclinic effects such as internal tides which were probably affecting speed variations with depth 
* Observed M2 constituent was not constant throughout the year at Victoria, possibly due to the exclusion of estuarine flow

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

.. _foremanetal00:

Foreman et al. (2000)
---------------------------
Model of the north east Pacific Ocean, including Alaskan and BC shelf. Resolution ranges from 80km off shore to about 100m along the coast. The grid in eastern Juan de Fuca Strait and southern Strait of Georgia is identical to :ref:`foremanetal95`. Since there are only two crossings in the Strait of Georgia, only part of the Strait is included in the model.

*(a) how is model evaluated?*

* Compared model :math:`M_2` and :math:`K_1` harmonics against those calculated from harmonic analysis of 5.3 years of altimeter observations (Topex/Poseidon), at locations of crossover of the two satellite paths within the model domain
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

* along southern and western open boundaries, initial elevation amplitudes and phases for major constituents (M2, S2, N2, K2, K1, O1, P1 and Q1) were calculated from the TPXO.3 world tidal model
* Puget Sound boundary (Admiralty Inlet) forced with identical amplitudes and phases to Foreman et al. (1995)
* Strait of Georgia boundary (Northern boundaries) forced with identical amplitudes and phases to Foreman et al. (1995)
* Queen Charlotte Strait forced with identical amplitudes and phases to Foreman et al. (1993)
* All coastal boundaries were free slip

Masson & Cummins (2004)
------------------------------------

POM model of southern Strait of Georgia and Juan de Fuca Strait

*(a) how is model evaluated?*

* Compared qualitatively to Foreman et al. (1995) e.g. the model reproduces the degenerate M2 amphidrome
* Calculated root-mean-square differences between all measured and modelled amplitudes and phase differences
* Average relative and absolute rms differences (D) between observed and calculated amplitudes and phases at 44 tide gauge sites, calculated by:

	.. math:: 
	 D = [\frac{1}{2} (A_m^2 + A_0^2) - A_m A_o \cos (\phi_m - \phi_o)]^{1/2}

	where :math:`A_m` and :math:`A_o` are sea level amplitude of model and observations and :math:`\phi_m` and :math:`\phi_o` phases

* Absolute error of 1.7cm - 5.5cm, relative error (=D/Ao) of 2.2% - 13.7%

*(b) how are tidal forcings applied at boundaries?*

* Model is forced at two open boundaries with four tidal constituents (K1, O1, M2 and S2) through a 'forced gravity wave radiation condition on the normal component of the depth-integrated velocity (Flather 1987)' 
* These four constituents account for about 70% of tidal stream velocity

.. _sutherlandetal11:

Sutherland et al. (2011)
-------------------------------------------

ROMS model of Salish Sea and Puget Sound

Group's website: http://faculty.washington.edu/pmacc/MoSSea/index.html

*(a) how is model evaluated?*

* Calibrated their model with tidal data from NOAA tide gauges (none in BC, all in OR and WA)
* Amplitude, phase and spring-neap variability of tidal signals
* Calculated Skill Score (SS) and 
* Calculated correlation coeffienct (R2), which is the variance between two variables:
	.. math:: 
	 R = \frac{1}{\sigma_m} \frac{1}{\sigma_o} \frac{1}{N} \sum^N_{i=1} (m_i-\bar{m})(o_i-\bar{o})
	
	where :math:`m_i` is the model variable at time or location i, :math:`o_i` is the observed variable at time or location i, N is the number of observations, math:`\sigma_m` and :math:`\sigma_o` are the standard deviations of model and observed variables and overbar indicates an average

* Also compared ratios of modeled to observed amplitudes of M2, S2 and K1 constituents using t_tide (Pawlowicz et al 2002)
* The model had better skill at diurnal frequencies because at semi-diurnal frequencies, modeled amplitude was too low
* In regions where tidal observations were not available, comparisons were made to an empirical tidal model developed for Puget Sound (Lavelle et al. 1988)
* Weather induced pressure anomalies are not represented in the model, so large difference occured during one winter event

*(b) how are tidal forcings applied at boundaries?*

* Open boundaries forced with eight constituents (M2, S2, K1, O1, N2, P1, K2 and Q1) derived from the 1/4 degree TPXO7.1 inverse global tidal model (Egbert and Erofeeva 2002)

References
-------------------------
* Egbert, G.D. and S.Y. Erofeeva, 2002. Efficient inverse modeling of barotropic ocean tides, Joundal of Atmospheric and Oceanic Technology, 19, 183-204.

* Foreman, M.G.G., R.A. Walters, R.F. Henry, C.P. Keller and A.G. Dolling, 1995. A tidal model for eastern Juan de Fuca Strait and the southern Strait of Georgia, Journal of Geophysical Research, 100, 721-740.

* Foreman, M.G.G., W.R. Crawford, J.Y. Cherniawsky, R.F. Henry and M.R. Tarbottom, 2000. A high-resolution assimilating tidal model for the northeast Pacific Ocean. Journal of Geophysical Research, 105, 28,629-28,652.

* Masson, D. and P.F. Cummins, 2004. Observations and modeling of seasonal variability in the Straits of Georgia and Juan de Fuca, Journal of Marine Research, 62, 491-516.

* Sutherland, D.A., P. MacCready, 2011, N.S. Banas and L.F. Smedstad, 2011. A model study of the Salish Sea estuarine circulation, Journal of Physical Oceanography, 41, 1125-1143.
