.. _TidalCurrentsTools:

Tidal currents tools
=========================


Tidal currents
-----------------------

Tidal currents are tightly knit with the hydrodynamics. When the currents are averaged over a long period of time what we have left is the tidal effect on the currents. We can average out the winds and surface currents anomalies. It is deeply dependent on bottom friction and mixing (Parker, 2007). Evaluating tidal currents enables one to see these effects at a chosen locations.

To calculate and evaluate tidal currents we need long (many months) currents time series for a particular location.


Observations
---------------

* Current data can be obtained or seen at `Ocean Networks Canada`_ (ONC) website. 
* The ONC data is then processed by Dr. Rich Pawlowicz.
* Phase and amplitude information can be reported in papers, we use values reported in Foreman et al. (2004).


.. _Ocean Networks Canada: http://venus.uvic.ca/data/data-plots/#strait-of-georgia-plots


Tidal ellipses
----------------

We perform a harmonic analysis on the model time series in order to extract the tidal harmonic constants. From the speed and direction of the tidal current at one location over time we can construct tidal ellipses which are used to describe the motion of the water due to one tidal constituent (eg. M2).

Separating the constituents
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Similar to tidal elevation, tidal currents are composed of many different harmonic constituents. When calculating and comparing tidal ellipses for one constituent at one location, it is important to have a long enough time series to be able to fully separate the constituents.


Freshet and changing shorelines
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is important to avoid using data for a tidal analysis during the Fraser River freshet. The outflow from the river is very large from the end of May to mid June and this skews the tidal ellipses.
The bathymetry and shoreline drastically affects the tidal currents. However, they can change over time. It is important to select data that would have a consistent shoreline and bathymetry across data sets that are being compared.


Python Scripts
----------------

Tidal ellipse calculation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Some python functions have been written to facilitate calculating amplitude and phase from velocity vectors and to calculate the ellipse parameters from these values. These scripts are found in :file:`tools/SalishSeaTools/salishsea_tools/ellipse.py`

* :file:`fittit` - This script fits a velocity vector time series to a formula and extracts the amplitude and phase of each component in the formula.::

   fittit(uaus, time, nconst)

This function finds tidal parameters from a tidal current component across a specified area of the grid at a single depth, at a single point through the water column, or a single depth averaged grid point. This function must be applied twice, once for each tidal current vector (u/v) in order to complete the analysis.

The nconst input sets how many tidal harmonic constituents will be analyzed. They come in pairs and in order of importance the domain. It returns a dictionary object containing the phase and amplitude for each component for the velocity given. The fit performed in fittit is based on Xu, Z. (2000).


    	.. math::	
	  u = mean + A_{M2}cos(\omega_{M2}t-\theta_{M2}) + A_{K1}cos(\omega_{K1}t-\theta_{K1})
	  
	  v = mean + A_{M2}cos(\omega_{M2}t-\theta_{M2}) + A_{K1}cos(\omega_{K1}t-\theta_{K1})
    
    
where :math:`\omega_{M2}` and :math:`\omega_{K1}`, :math:`\theta_{M2}` and :math:`\theta_{K1}` and :math:`A_{M2}` and :math:`A_{K1}` are the frequencies, phase lags and amplitudes for the M2 and K1 components.
    
    
* :file:`ellipse_params` - This script converts from the amplitude and phase lag parameters to the tidal current ellipse parameters.::    

    ellipse_params(uamp, upha, vamp, vpha)
    
This function calculates the tidal ellipse parameters based on the conversions shown in Xu, Z. (2000). It outputs the positively and negatively rotating amplitude and phase, as well as the major and minor axis and the axis tilt and phase.

* :file:`get_params_nowcast` - This script gives the tidal ellipse parameters for given date range and location based on the hourly model nowcast values.::
 
    get_params_nowcast(to, tf, i, j, path, nconst, depthrange='None', depav=False, tidecorr=CorrTides)
    
This function loads all the data between the start and the end date that contains hourly velocity netCDF4 files. Then it mask, unstaggers and rotates the velocities by component about the grid point described by the i and j. Lastly it fits the velocities and calculates the tidal ellipse parameters for that date range using the fittit and ellipse_param functions above.
After finding the amplitude and phase of the orthogonal vector by using fittit it does a tide correction  which is set to September 10th 2014 by the nowcast. The nodel correction factors can be found in: /data/dlatorne/MEOPAR/SalishSea/nowcast/08jul15/ocean.output/.
This function outputs a dictionary object containing the ellipse parameters for each tidal harmonic constituent.

* :file:`plot_ellipses_area`  &  :file:`plot_ellipses` - These scripts are used to plot the tidal ellipses on a map based on the parameters calculated by the functions above.::

    plot_ellipses_area(params, depth='None', imin=0, imax=398, jmin=0, jmax=898)
    
    plot_ellipses(params, x, y, depth='None', numellips=1, imin=0, imax=398, jmin=0, jmax=898)
    
* In this notebook: `TidalEllipseTools.ipynb`_  there are simple examples of the functions above.

.. _TidalEllipseTools.ipynb: http://nbviewer.ipython.org/urls/bitbucket.org/salishsea/analysis/raw/tip/Muriel/TidalEllipseTools.ipynb

MATLAB Scripts
----------------
Loading and processing of the observational data from the ONC VENUS Central, East and Delta nodes is done in MATLAB scripts written by Dr. Rich Pawlowicz. The processing is done in three parts and is tailored for each deployment at each node.

Processing scripts
~~~~~~~~~~~~~~~~~~~~~

* The first part is :file:`GET_DATA_fun.m` This script will get the data that is directly output from the ADCP. It does this for the two days before the day indicated. It will put this data in a directory at pth/raw/ and organize it by year and month. This function calls a script written by Marlene Jeffries at Ocean Network Canada :file:`getSoGAdcpDataMay15_mod`. This script contains many functions that are used to ultimately retrieve the raw data from the ONC website.
 
 * The next step is to run :file:`GET_DEPL_fun.m`. This function goes through all the data in the raw directory gathered by GETDATA_fun and bins it into 30 minutes bins. 
 
 * Lastly, the bulk of the processing is done in :file:`LTIM_fun.m`. This script filters out the tides, corrects the angles for the velocities to get major axis in the direction of the flood current.

Adjustments for running daily
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * When running these scripts for a single day of data at a time, a few modifications have to be done to keep the scripts running. First of all, GETDEPL_fun creates a new deployment file with the new updated raw data that was loaded by GETDATA_fun, however, LTIM_fun needs only one mat file per deployment in the directory where it looks. The :file:`compare_daily` function works to seamlessly join the new updated deployment file and the previous deployment file.
 
New deployment
~~~~~~~~~~~~~~~

* Every few months to a year the nodes need maintenance or for whatever reason a new deployment with new devices is installed. This requires a lot of effort because the numbers in :file:`LTIM_fun` have previously been found manually. These values are based on the tilt, depth, angle and other physical aspects of the node. The processing that is done may need weeks of data from a new deployment to accurately get the information to realign the ADCP output into usable data.

* All the raw data will have to be deleted so that only the present deployment gets reloaded every time.

* Contact Marlene Jeffries at Ocean Networks Canada for an updated :file:`getSoGAdcpDataMay15_mod.m` script that contains the correct device and sensor IDs of the new deployment.

Changing users
~~~~~~~~~~~~~~~
If you will be running the processing in a new directory for the first time there are a couple things to change in order to facilitate the transitions.

* 1. In :file:`compare_daily.m` change the path to be where you want everything to be saved. Many extra files will appear in this directory every time you run the scripts.

* 2. Make an account on http://www.oceannetworks.ca/information to get userId. In :file:`getSoGAdcpDataMay15_mod.m` insert your email and userId at lines 173 and 174 of the script. You will receive an email every time you load raw data from the website.  

* 3. In :file:`GET_DATA_fun` change the firstdate variable to be at least 3 days before the lastdate. This is because the filter length in :file:`LTIM_fun` needs at least that much data for the processing. 
 
 
References
^^^^^^^^^^^^

* Parker, B. B., 2007. Tidal analysis and prediction. US Department of Commerce, National Oceanic and Atmospheric Administration, National Ocean Service, Centre for Operational Oceanographic Products and Services, 378 pages.

* Xu, Z., 2000. Ellipse parameters conversion and vertical velocity profiles for tidal currents. Bedford Institute of Oceanography, Dartmouth, Nova Scotch, Canada, 20 pages

