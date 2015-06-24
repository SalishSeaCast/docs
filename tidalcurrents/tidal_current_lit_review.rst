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

	.. math::	
	  N(t) = W_N \cos(\eta t - \kappa_N)
	
	  E(t) = W_E \cos(\eta t - \kappa_E)

	where :math:`N(t)`, and :math:`E(t)`, are the north and east components of the tidal constituents, :math:`W_N` and :math:`W_E` are the current vectors north and east components, :math:`\eta` is the frequency and :math:`\kappa_N` and :math:`\kappa_E` is the phase lag in it's respective direction.
	
* Then generalise for any two orthogonal components MJ(t) and MN(t), major and minor components:

	.. math::
	  MJ(t) = N(t) \cos(\theta) + E(t) \sin(\theta)
	
	  MN(t) = E(t) \cos(\theta) - N(t) \sin(\theta)

	where :math:`\theta`, is the major axis direction clockwise from the north.


.. _foremanetal04:

Foreman et al. (2004)
---------------------------

* Currents calculated with the TIDE3D finite element model. This model did not include vertical variations in currents by using a large vertical viscocity. This basically outputs constant currents through the water column.
* Present along channel amplitude and phase.
* Stratification is too significant for this model to capture it at the Haro Strait and Johnson Strait Central sites.
* They only report amplitude and phase.
* The model uses data assimilation of the elevation observations to impove predictions. These model values are an improvement to the original model presented.
	 - The major semi-axes are smaller at the more southern sites and larger at the northen sites
	 - Improvements are southern sites but mostly no changes at the northen sites
	 - Decreased current speeds by 10 cm/s in most of Juan de Fuca Strait, Gulf and San Juan Islands and the sourther Strait of Georgia
	 - Increased currents in norther Georgia and in Johnstone Strait
* We can transform these into tidal ellipses using Xu (2000) technique described below to facilitate the comparison of our Salish Sea model output to the Foreman (2004) model.


.. _Xu:

Zhigang Xu (2000)
-------------------

Short paper describing a technique for ellipse conversion explained in much detail.

* Uses complex tidal currents to convert between tidal current amplitude and phase lag paramters to tidal current ellipse parameters and vice versa.

	.. math::
	  w = u +iv
	
	  u = a_u \cos(\omega t - \phi_u)
	
	  v = a_v \cos(\omega t - \phi_v)
	
	where :math:`w` is the complex tidal current, :math:`\omega`, is the frequency of the chosen tidal constituent, :math:`\phi_u` and :math:`\phi_v` are the phase lag for the u- and v- components and :math:`a_u` and :math:`a_v` are the amplitudes for the u- and v- components.
	

* Tracing out :math:`w` on a complex plane gives an ellipse, from this ellipse we can calculate many parameters that provide information about the flow.

	.. math::
	  w = W_p e^{i(\omega t + \theta_p)} + W_m e^{-i(\omega t - \theta_m)}
	
* From these equations we can extract all the ellipse parameters
* This is the method we used to calculate out tidal ellipse from the model outputs and from the observation that were provided by Dr. Rich Pavlowich, Dr. Mark Halverson and Richard Dewey.
	


References
-------------------------
* Parker, B. B., 2007. Tidal analysis and prediction. US Department of Commerce, National Oceanic and Atmospheric Administration, National Ocean Service, Center for Operational Oceanographic Products and Services, 378 pages.

* Doodson, A.T. and H.D. Warburg, 1941. Admiralty Manual of Tides. Hydrographic Department, Admiralty, London, 270 pages.

* Foreman, M. G. G., Sutherland, G., & Cummins, P. F., 2004. M2 tidal dissipation around Vancouver Island: an inverse approach. Continental Shelf Research, 24(18), 2167-2185.

* Thomson, R.E., 1981. Oceanography of the British Columbia Coast. Canadian Special Publication of Fisheries and Aquatic Sciences 56, Department of Fisheries and Oceans, Ottawa, 291pp.

* Xu, Z., 2000. Ellipse parameters conversion and vertical velocity profiles for tidal currents. Bedford Institute of Oceanography, Dartmouth, Nova Scotia, Canada, 20 pages.