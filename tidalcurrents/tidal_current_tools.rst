.. _TidalCurrentsTools:

********************
Tidal Currents Tools
********************

Tidal currents
==============

Tidal currents are tightly knit with the hydrodynamics. When the currents are averaged over a long period of time what we have left is the tidal effect on the currents. We can average out the winds and surface currents anomalies. It is deeply dependant on bottom friction and mixing (Parker, 2007). Evaluating tidal currents enables one to see these effects at a chosen locations.

To calculate and evaluate tidal currents we need long (many months) currents time series for a particular location.


Observations
============

* Current data can be obtained from the `Ocean Networks Canada (ONC) website`_.
  An example is here:
  https://data.oceannetworks.ca/DataPreview?TREETYPE=1&LOCATION=233&DEVICECATEGORY=69&TIMECONFIG=0
* The ONC data is then processed by Dr. Rich Pawlowicz.
* Phase and amplitude information can be reported in papers, we use values reported in Foreman et al. (2004).

.. _Ocean Networks Canada (ONC) website: https://data.oceannetworks.ca/home


Tidal Ellipses
==============

We perform a harmonic analysis on the time series in order to extract the tidal harmonic constants. From the speed and direction of the tidal current at one location over time we can construct harmonic constituents ellipse which are used to describe the motion of the water due to one tidal constituent (eg. M2).


Separating the Constituents
---------------------------

Similarly to tides, tidal currents are composed of many different harmonic constituents. When calculating and comparing tidal ellipses it is of one constituents at one location, it is important to have a long enough time series to be able to fully separate the constituents.


Freshet and Changing Shorelines
-------------------------------

It is important to avoid using data for a tidal analysis during the Fraser River freshet. The outflow from the river is very larger from the end of may to mid June and this skews the tidal ellipses.
The bathymetry and shoreline drastically effects the tidal currents. However, they can change over time. It is important to select data that would have a consistent shoreline and bathymetry across datasets that are being compared.


Python Tools for Tidal Analysis
===============================

Tidal Analysis and Ellipse Calculation
--------------------------------------

Some python functions have been written to facilitate calculating amplitude and phase from velocity vectors and to calculate the ellipse parameters from these values. These scripts are found in :file:`tools/SalishSeaTools/salishsea_tools/ellipse.py` and :file:`tools/SalishSeaTools/salishsea_tools/tidetools.py`

* :file:`tidetools.fittit` - This script fits a time series to a tide curve and extracts the amplitude and phase of each tidal constituent in the tide curve.::

   tidetools.fittit(uaus, time, nconst)

This function finds tidal parameters from a tidal constituent across a specified area of the grid at a single depth, at a single point through the water column or a single depth averaged grid point. For currents, this function must perform twice, once for each tidal current vector in order to complete the analysis. This function should also work with water level analysis.

.. note::

   This function can be used to analyze a time series of sea surface height, u velocity, or v velocity. In fact, it can be used for any scalar variable. But it does not handle important things like inference or nodal corrections. In those cases, it is much better to use t_tide_ or apply inference and nodal corrections on your own.

.. _t_tide: https://www.eoas.ubc.ca/~rich/#T_Tide


The nconst input sets how many tidal constituents will be analysed. They come in pairs and in order of importance the domain. It returns a dictionary object containing the phase and amplitude for each component for the input array.

The fitting routine solves for a set of amplitude and phase parameters (one set for each tidal constituent) by finding the best match between the model's time series and a tidal curve that is predetermined by the tidal constituents that it will be solving for. Below is an example of the equation for the u and v tidal curves for the M2 and K1 constituents:

    	.. math::
	  u = mean + A_{M2}cos(\omega_{M2}t-\theta_{M2}) + A_{K1}cos(\omega_{K1}t-\theta_{K1})

	  v = mean + A_{M2}cos(\omega_{M2}t-\theta_{M2}) + A_{K1}cos(\omega_{K1}t-\theta_{K1})


where :math:`\omega_{M2}` and :math:`\omega_{K1}`, :math:`\theta_{M2}` and :math:`\theta_{K1}` and :math:`A_{M2}` and :math:`A_{K1}` are the frequencies, phase lags and amplitudes for the M2 and K1 components. "Mean" is an unknown values that the fitting routine will solve for however it is not used in the tidal ellipse calculations.

* :file:`ellipse.ellipse_params` - This script converts from the amplitude and phase lag parameters to the tidal current ellipse parameters.::

    ellipse.ellipse_params(uamp, upha, vamp, vpha)

This function calculates the tidal ellipse parameters based on the conversions shown in Xu, Z. (2000). It outputs the positively and negatively rotating amplitude and phase, as well as the major and minor axis and the axis tilt and phase.

* :file:`ellipse.get_params_nowcast` - This script gives the tidal ellipse parameters for a given date range and location based on the hourly model output values.::

    ellipse.get_params_nowcast(to, tf, i, j, path, nconst, depthrange='None', depav=False, tidecorr=CorrTides)

This function loads all the data between the start and the end date that contains hourly velocity netCDF4 files. Then it mask, unstaggers and rotates the velocities by component about the grid point described by the i and j. Lastly it fits the velcities and caculates the tidal ellipse parameters for that date range using the fittit and ellipse_param functions above.

After finding the amplitude and phase of the orthogonal vector by using fittit it does a nodal correction, determined by the start date of the nowcasts, Sept 10, 2014. These values values and other constituents tide corrections can be found in: /data/dlatorne/MEOPAR/SalishSea/nowcast/08jul15/ocean.output/.
This function outputs a dictionary object containing the ellipse parameters for each tidal harmonic constituent.

* In this notebook: `UsingEllipse.py.ipynb`_  there are simple examples of the functions above.

.. _UsingEllipse.py.ipynb: https://nbviewer.org/github/SalishSeaCast/analysis-muriel/blob/master/notebooks/TidalEllipses/UsingEllipse.py.ipynb


MATLAB Scripts for Tidal Analysis
===========================================

Some MATLAB tools have been written for analyzing barotropic and baroclinic tidal currents from NEMO u/v output.
The scripts load NEMO data and then apply t_tide_ to perform a harmonic analysis.
The advantage of using t_tide_ is that it can analyze many constituents and easily handles nodal corrections and inference.
Several scripts for baroclinic, barotropic and surface currents analysis have been written.
These scripts take care of masking, unstaggering, rotating and depth averaging as needed.

.. note::

    The NEMO u and v output are expected to be contained in a single netCDF file. Remember that u and v are stored on slightly different lat and lon grids. The longitude and latitude grid stored in the netCDF file should correspond to the T-grid.

.. note::

    Depending on the length of your time series and size of your subdomain, it may be very memory intensive to load your files.

These scripts and their dependencies are stored in :file:`analysis/Nancy/currents/t_tide_analysis`.

* :file:`area_surface_tides.m` - This script analyzes the full tidal current at the surface. It saves the ellipse parameters for each constituent in a file. ::

    area_surface_tides(filename, outfile, t0, ref_time, time_units)

    * filename is the name of the netCDF file where the u/v/grid/time information is stored.
    * outfile is the name of the file where the output is to be saved
    * t0 is the time index for beginning the tidal anlaysis. e.g t0=1 for analysis at the beginning of the time series or t0=241 to skip the first 240 records in the time series.
    * ref_time is a matlab date vector that specifies the reference time for the time variable. e.g ref_time=[2014 9 10] means that the time measurement in the netCDF file is measured relative to Sept 10, 2014.
    * time_units is the units that the time variable is measured in. e.g 'h' for hours or 's' for seconds.

* :file:`area_depav_tides.m` - This script analyzes the depth-averaged tidal currents. ::

    area_depav_tides(filename, outfile, depthfile, t0, ref_time, time_units, use_mask)

    * depthfile is the mesh_mask file, where actual NEMO depths are stored.
    * use_mask indicates if the depth averaging should be calculated with depths stored in the mesh_mask file (1) or with depths stored with u/v in the netCDF file
    * all other inputs are the same as those described in area_surface_tides.m

 * :file:`area_baroclinic_tides.m` - This script analyzes the baroclinic tidal currents. The baroclinic tidal currents are defined as the full current subtract the depth-averaged current. This definition may be inaccurate in regions where boundary layer effects are important. ::

    area_baroclinic_tides(filename, outfile, depthfile, t0, ref_time, time_units, use_mask)

    * all inputs are the same as those in area_depav_tides.m


Output
-------

This notebook_ gives an example of loading the output from these scripts in python. It makes use of functions in :file:`analysis/Nancy/currents/baroclinic.py`.

.. _notebook: https://nbviewer.org/github/SalishSeaCast/analysis/blob/master/Nancy/currents/Baroclinic%20Tides%20-%20CODAR%20region%20-%20phase%20and%20inclination.ipynb


References
----------

* Pawlowicz, R., B. Beardsley, and S. Lentz, 2002. Classical tidal harmonic analysis including error estimates in MATLAB using T_TIDE, Computers and Geosciences, 28, 929-937

* Parker, B. B., 2007. Tidal analysis and prediction. US Department of Commerce, National Oceanic and Atmospheric Administration, National Ocean Service, Centre for Operational Oceanographic Products and Services, 378 pages.

* Xu, Z., 2000. Ellipse parameters conversion and vertical velocity profiles for tidal currents. Bedford Institute of Oceanography, Dartmouth, Nova Scotch, Canada, 20 pages
