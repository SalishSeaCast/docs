.. _TidalCurrentsTools:

Tidal currents tools
=========================


Tidal currents
-----------------------

Tidal currents are tightly knit with the hydrodynamics. When the currents are averaged over a long period of time what we have left is the tidal effect on the currents. We can average out the winds and surface currents anomalies. It is deeply dependant on bottom friction and mixing (Parker, 2007). Evaluating tidal currents enables one to see these effects at a chosen locations.

To calculate and evaluate tidal currents we need long (many months) currents time series for a particular location.


Observations
---------------

* Current data can be obtained or seen at `Ocean Networks Canada`_ (ONC) website. 
* The ONC data is then processed by Dr. Rich Pawlowicz.
* Phase and amplitude information can be reported in papers, we use values reported in Foreman et al. (2004).


.. _Ocean Networks Canada: http://venus.uvic.ca/data/data-plots/#strait-of-georgia-plots


Tidal ellipses
----------------

We perform a harmonic analysis on the time series in order to extract the tidal harmonic constants. From the speed and direction of the tidal current at one location over time we can construct harmonic constituents ellipse which are used to describe the motion of the water due to one tidal constituent (eg. M2).

Separating the constituents
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Similarly to tides, tidal currents are composed of many different harmonic constituents. When calculating and comparing tidal ellipses it is of one constituents at one location, it is important to have a long enough time series to be able to fully separate the constituents. 


Freshet and changing shorelines
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is important to avoid using data for a tidal analysis during the Fraser River freshet. The outflow from the river is very larger from the end of may to mid June and this skews the tidal ellipses.
The bathymetry and shoreline drastically effects the tidal currents. However, they can change over time. It is important to select data that would have a consistent shoreline and bathymetry across datasets that are being compared.


Python Scripts
----------------

Tidal ellipse calculation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Some python functions have been written to facilitate calculating amplitude and phase from velocity vectors and to calculate the ellipse parameters from these values. These scripts are found in :file:`tools/SalishSeaTools/salishsea_tools/tidetools.py`

* :file:`fittit` - This script fits a velocity vector time series to a formula and extracts the amplitude and phase of each component in the formula.::

   fittit(uaus, time, imin=0, imax=0, jmin=0, jmax=0, dj=1)

This function finds tidal parameters from a tidal current component across a specified area of the grid at a single depth, at a single point through the water column or a single depth averaged grid point. Must perform twice, once for each tidal current vector in order to complete the analysis.
It assumes the current is only affected by M2 and K1, which is incorrect, but it is a good approximation because they are the two main components throughout our domain. The fit performed in fittit is based on Xu, Z. (2000).


    	.. math::	
	  u = mean + A_{M2}cos(\omega_{M2}t-\theta_{M2}) + A_{K1}cos(\omega_{K1}t-\theta_{K1})
	  
	  v = mean + A_{M2}cos(\omega_{M2}t-\theta_{M2}) + A_{K1}cos(\omega_{K1}t-\theta_{K1})
      
    
      
    
    
where :math:`\omega_{M2}` and :math:`\omega_{K1}`, :math:`\theta_{M2}` and :math:`\theta_{K1}` and :math:`A_{M2}` and :math:`A_{K1}` are the frequencies, phase lags and amplitudes for the M2 and K1 components.
    
    
* :file:`ellipse_params` - This script converts from the amplitude and phase lag parameters to the tidal current ellipse parameters.::    

    ellipse_params(uamp, upha, vamp, vpha)
    
This function calculates the tidal ellipse parameters based on the conversions shown in Xu, Z. (2000). It outputs the positively and negatively rotating amplitude and phase, as well as the major and minor axis and the axis tilt and phase.


References
^^^^^^^^^^^^

* Parker, B. B., 2007. Tidal analysis and prediction. US Department of Commerce, National Oceanic and Atmospheric Administration, National Ocean Service, Centre for Operational Oceanographic Products and Services, 378 pages.

* Xu, Z., 2000. Ellipse parameters conversion and vertical velocity profiles for tidal currents. Bed ford Institute of Oceanography, Dartmouth, Nova Scotch, Canada, 20 pages

