Literature review (tidal currents)
======================================

(a) Why use tidal ellipses?

(b) What have researchers previously done in the Salish Sea and the Strait of Georgia?

Overview of tidal ellipses
-----------------------------

*(a) Why use tidal ellipses?*

Tidal current vary alot within a fjord-like estuarine system. The Salish Sea is not an execption of this. The hydrodynamics of the channel can drastically change the tidal currents depending on the gradient of these variations. We can observe larger current speeds in the many narrow straits of the Georgia Strait, as well as near the entrance, where it connects to the Pacific Ocean. Tidal currents also vary vertically, generally mainly due to bottom friction, but also baroclinic effects. (NOAA 2007)

* Tidal current constituent ellipses describe the flow as it rotates at a single location for a single contituent's cycle. 
* Tidal ellipses can communicate alot about a depth profile(or a depth averaged generalisation) of the flow or at a particular locations within a couple parameters. We mainly look at the M2 and K1 contituents' ellipses because they summarise most (percentage?) of the flow. The M2 componen is much stronger than the K1. (Thomson 1981)
* Tidal ellipses enable us to see the speed and direction of a flow due to a chosen tidal constituent at every hour of the day as a depth profile using only othogoonal velocities at that point over a long period of time. (NOAA 2007)
* Comparisons of tidal ellipses from the model output to observations facilitates the understanding of which physical processes are properly and poorly represented and may even provide information of techniques to use to improve the poorly represented processes. 

*(b) What have researchers previously done in the Salish Sea and the Strait of Georgia?*

.. _NOAA2007:

National Oceanic and Atmospheric Administration (2007)
----------------------------------------------------------

* Suggest using harmonic contituent ellipses to combine the orthogonal compenent time series (u and v current vectors)
* The orthogonal components can be chosen to be north/south or along direction of maximum flood and perpentidular to it.  
* In this book uses the derivation of Doodson and Warburg (1941, p.180-1):

	:math:`N(t) = W_N \cos(\eta t - \kappa_N)`
	:math:`E(t) = W_E \cos(\eta t - \kappa_E)`

	where :math:`N(t)`, and :math:`E(t)`, are the north and east components of the tidal constituents, :math:`W_N` and :math:`W_E` are the current vectors north and east components, :math:`\eta` is the frequency and :math:`\kappa_N` and :math:`\kappa_E` is the phase lag in it's respective direction.
	
* Then generalise for any two orthogonal components MJ(t) and MN(t), major and minor components:
	
	:math:`MJ(t) = N(t) \cos(\theta) + E(t) \sin(\theta)`
	:math:`MN(t) = E(t) \cos(\theta) - N(t) \sin(\theta)`

	where :math:`\theta`, is the major axis direction clockwise from the north.


.. _foremanetal04:

Foreman et al. (2004)
---------------------------

* Use a observations to do data assimilation.
* They only report amplitude and phase.
* We can transform these into tidal ellipses


.. _Xu:

Zigang Xu (2000)
-------------------

Short paper describing a technique for ellipse conversion explained in much detail.

* Uses complex tidal currents to convert between tidal current amplitude and phase lag paramters to tidal current ellipse parameters and vice versa.
	:math:`w = u +iv`
	:math:`u = a_u \cos(\omega t - \phi_u)`
	:math:`v = a_v \cos(\omega t - \phi_v)`
	
	where :math:`w` is the complex tidal current, :math:`\omega`, is the frequency of the chosen tidal constituent, :math:`\phi_u` and :math:`\phi_v` are the phase lag for the u- and v- components and :math:`a_u` and :math:`a_v` are the amplitudes for the u- and v- components.
	

* Tracing out :math:`w` on a complex plane gives an ellipse, from this ellipse we can calculate many parameters that provide information about the flow.

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

* Open boundaries were offshore i.e. straight boundary along 127 degrees W and the northern boundary in the Strait of Georgia was closed

References
-------------------------
* Egbert, G.D. and S.Y. Erofeeva, 2002. Efficient inverse modeling of barotropic ocean tides, Joundal of Atmospheric and Oceanic Technology, 19, 183-204.

* Foreman, M.G.G., R.A. Walters, R.F. Henry, C.P. Keller and A.G. Dolling, 1995. A tidal model for eastern Juan de Fuca Strait and the southern Strait of Georgia, Journal of Geophysical Research, 100, 721-740.

* Foreman, M.G.G., W.R. Crawford, J.Y. Cherniawsky, R.F. Henry and M.R. Tarbottom, 2000. A high-resolution assimilating tidal model for the northeast Pacific Ocean. Journal of Geophysical Research, 105, 28,629-28,652.

* Masson, D. and P.F. Cummins, 2004. Observations and modeling of seasonal variability in the Straits of Georgia and Juan de Fuca, Journal of Marine Research, 62, 491-516.

* Sutherland, D.A., P. MacCready, 2011, N.S. Banas and L.F. Smedstad, 2011. A model study of the Salish Sea estuarine circulation, Journal of Physical Oceanography, 41, 1125-1143.
