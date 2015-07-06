Literature review (tidal currents)
======================================

(a) Why use tidal ellipses?

(b) What have researchers previously done in the Salish Sea and the Strait of Georgia?

Overview of tidal ellipses
-----------------------------

*(a) Why use tidal ellipses?*

Tidal currents vary a lot within a fjord-like estuarine system. The Salish Sea is not an exception of this. The hydrodynamics of the channel can drastically change the tidal currents depending on the gradient of these variations. We can observe larger current speeds in the many narrow straits of the Georgia Strait, as well as near the entrance, where it connects to the Pacific Ocean. Tidal currents also vary vertically, generally due to bottom friction, but also baroclinic effects. (NOAA 2007)

* Tidal current constituent ellipses describe the flow as it rotates at a single location for a single constituent's cycle. 
* Tidal ellipses can communicate a lot about a depth profile of the flow at particular locations with only a couple of parameters. At present, we have focused our analysis on the M2 and K1 constituents because they are the largest components in our domain. The M2 component is much stronger than the K1. (Thomson 1981)
* Tidal ellipses enable us to see the speed and direction of a flow due to a chosen tidal constituent at every hour of the day as a depth profile using only orthogonal velocities at that point over a long period of time. (NOAA 2007)
* Comparisons of tidal ellipses from the model output with observations facilitates the understanding of which physical processes are properly and poorly represented and may even inform techniques to improve the poorly represented processes. 

*(b) What have researchers previously done in the Salish Sea and the Strait of Georgia?*

.. _NOAA2007:

National Oceanic and Atmospheric Administration (2007)
----------------------------------------------------------

* Suggest using harmonic constituent ellipses to combine the orthogonal component time series (u and v current vectors)
* The orthogonal components can be chosen to be north/south or along direction of maximum flood and perpendicular to it.  
* This book uses the derivation of Doodson and Warburg (1941, p.180-1):

	.. math::	
	  N(t) = W_N \cos(\eta t - \kappa_N)
	
	  E(t) = W_E \cos(\eta t - \kappa_E)

	where :math:`N(t)`, and :math:`E(t)`, are the north and east components of the tidal constituents, :math:`W_N` and :math:`W_E` are the amplitudes of the north and east components, :math:`\eta` is the frequency and :math:`\kappa_N` and :math:`\kappa_E` are the phase lags in the respective direction.
	
* This can be generalized for any two orthogonal components MJ(t) and MN(t), major and minor components:

	.. math::
	  MJ(t) = N(t) \cos(\theta) + E(t) \sin(\theta)
	
	  MN(t) = E(t) \cos(\theta) - N(t) \sin(\theta)

	where :math:`\theta`, is the major axis direction clockwise from the north.


.. _foremanetal04:

Foreman et al. (2004)
---------------------------

* Currents calculated with the TIDE3D finite element model. This model did not include vertical variations in currents by using a large vertical viscosity. This basically outputs constant currents through the water column.
* Presents along channel amplitude and phase.
* Although stratification is known to be important at the Haro Strait and Johnstone Strait Central sites, baroclinic affects are not captured by this model.
* They only report amplitude and phase.
* The model uses data assimilation of the elevation observations to improve predictions. These model values are an improvement to the original model presented.

	 - The semi-major axes are smaller at the more southern sites and larger at the northern sites
	 - Improvements at southern sites but mostly no changes at the northern sites
	 - Decreased current speeds by 10 cm/s in most of Juan de Fuca Strait, Gulf and San Juan Islands and the southern Strait of Georgia
	 - Increased currents in northern Georgia and in Johnstone Strait
* We can transform these into tidal ellipses using Xu (2000) technique described below to facilitate the comparison of our Salish Sea model output to the Foreman (2004) model.


.. _Xu:

Zhigang Xu (2000)
-------------------

Short paper describing a technique for ellipse conversion explained in much detail.

* Uses complex tidal currents to convert between tidal current amplitude and phase lag parameters and tidal current ellipse parameters and vice verse.

	.. math::
	  w = u +iv
	
	  u = a_u \cos(\omega t - \phi_u)
	
	  v = a_v \cos(\omega t - \phi_v)
	
	where :math:`w` is the complex tidal current, :math:`\omega`, is the frequency of the chosen tidal constituent, :math:`\phi_u` and :math:`\phi_v` are the phase lag for the u- and v- components and :math:`a_u` and :math:`a_v` are the amplitudes for the u- and v- components.
	

* Tracing out :math:`w` on a complex plane gives an ellipse, from this ellipse we can calculate many parameters that provide information about the flow.

	.. math::
	  w = W_p e^{i(\omega t + \theta_p)} + W_m e^{-i(\omega t - \theta_m)}
	
* From these equations we can extract all the ellipse parameters
* This is the method we used to calculate the tidal ellipses from the model outputs and from the observation that were provided by Dr. Rich Pawlowicz, Dr. Mark Halveson and Dr. Richard Dewey.
	

.. _Thomson_Huggett:

Thomson R.E. & Huggett W.S. (1980)
--------------------------------------

Johnstone Strait is part of the domain, it is important to understand the research that has previously been done regarding the area and the observations that are being compiled.

* Five current meters were deployed across channel in June 1973 in the Johnstone Strait eastward of Newcastle Sill
* More were deployed between 1976 and 1978. 
* Table of 10 stations in the western basin of Johnstone Strait containing the along channel amplitude (semi-major axis) and the phase (from 120 :math:`\deg` W.) and 2 to 10 depths for each station. The moorings were out for 7 to 92 days, depending on the device type, location and start time.
* The semi-minor axis amplitude was not reported because it is very small in this region.
* M2 components are only fully resolved for time series that are longer than 27.6 days.



References
-------------------------
* Parker, B. B., 2007. Tidal analysis and prediction. US Department of Commerce, National Oceanic and Atmospheric Administration, National Ocean Service, Centre for Operational Oceanographic Products and Services, 378 pages.

* Dodson, A.T. and H.D. War burg, 1941. Admiralty Manual of Tides. Hydro graphic Department, Admiralty, London, 270 pages.

* Foreman, M. G. G., Sutherland, G., & Cummings, P. F., 2004. M2 tidal dissipation around Vancouver Island: an inverse approach. Continental Shelf Research, 24(18), 2167-2185.

* Thomson, R.E., 1981. Oceanography of the British Columbia Coast. Canadian Special Publication of Fisheries and Aquatic Sciences 56, Department of Fisheries and Oceans, Ottawa, 291pp.

* Thomson, R.E. and W.S. Huggett, 1980. M2 Baroclinic Tides in Johnstone Strait, British Columbia. J. Phys. Oceanogr., 10, 1509–1539.

* Xu, Z., 2000. Ellipse parameters conversion and vertical velocity profiles for tidal currents. Bed ford Institute of Oceanography, Dartmouth, Nova Scotch, Canada, 20 pages.
