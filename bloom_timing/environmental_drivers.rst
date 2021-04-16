.. _environmental_drivers:

=====================
Environmental Drivers 
=====================

Many environmental drivers were compared to the spring bloom date to investigate which 
factors had the most influence on interannual variability of the bloom timing. For each driver, the 
monthly averages for January, February and March were extracted. 

**The environmental drivers considered:**
	- Wind speed (as wind speed cubed)
	- Solar radiation at ocean surface
	- Surface temperature
	- Surface salinity
	- Fraser river flow
        - Mid-depth nitrate concentration (30-90m)
        - Deep nitrate concentration (below 250m)
        - Halocline depth
        - Turbocline depth
        - Average eddy diffusivity in the upper 15m and 30m
        - Density difference from surface to a series of depths (5m, 10m, 15m, 20m, 25m , 30m)
        - Surface zooplankton concentration
        - Surface mesozooplankton concentration
        - Surface microzooplankton concentration
        - Depth-integrated zooplankton concentration
        - Depth-integrated mesozooplankton concentration
        - Depth-integrated microzooplankton concentration

For efficient data usage, four pickle files for each year were created for different variables using 
this notebook_. As with bloom timing variables, these files are stored in 
:file:`/ocean/aisabell/MEOPAR/extracted_files`. The format is :file:`loc` being the location of 
interest (eg. S3), :file:`modver` being the model run (eg. 201905) and :file:`year` being the year. 

**There is a file for each year for:**													
	- Location-specific variables :file:`JanToMarch_TimeSeries_{year}_{loc}_{modver}.pkl`
	- Non-location-specific variables :file:`JanToMarch_TimeSeries_{year}_{modver}.pkl`
	- Location-specific mixing variables :file:`JanToMarch_Mixing_{year}_{loc}_{modver}.pkl`

Biological variables, such as nitrate, diatom, flagellate, ciliate, microzooplankton and  
mesozooplankton concentrations as well as time were all extracted from daily :file:`.ptrc_T` files.  
Temperature and salinity were extracted from daily :file:`.grid_T` files. Eddy diffusivity values 
were taken from hourly :file:`.grid_W` files. Here "T" and "W" specify the vertical model grid on 
which the variables in each file are defined. Wind and solar variables were extracted from model 
forcing files. Fraser flow data is from Environment Canada and comes as daily flow averages, 
measured at Hope. Deep nitrate concentrations (below 250m) were also calculated from 
:file:`.ptrc_T` files, but these are not location specific and rather represent the average across 
the entire Strait of Georgia region.

Some key factors that relate to mixing and stratification were calculated to compare to bloom 
timing and to other environmental drivers. This includes the monthly averages of halocline depth 
and turbocline depth, the density difference from the surface to a series of depths from 5m to 
30m, in increments of 5m, and the average eddy diffusivity over the upper 15m and 30m. 
Halocline depth was defined as the depth with the maximum gradient in salinity between two 
grid cells. Turbocline depth was defined as the depth above which the eddy diffusivity passes a 
threshold value of 1 x 10\ :sup:`-3`\ m\ :sup:`2`\s\ :sup:`-1`\. This threshold value was 
determined by examining depth profiles of eddy diffusivity, and is consistent with a turbocline 
definition by Luneva et. al (2015) [1]_.The code for these functions can be found here_.

**To recreate extracting these variables for a different location:**
	1. Open the makePickles201905 notebook_
	2. Follow the instructions in the second code cell of the notebook

.. _here: https://github.com/SalishSeaCast/tools/blob/master/SalishSeaTools/salishsea_tools/bloomdrivers.py
								
.. _notebook: bloom_notebooks/makePickles201905.ipynb

.. [1] Luneva, M. V., Y. Aksenov, J. D. Harle, and J. T. Holt (2015), The effects of tides on the water mass mixing and sea ice in the Arctic Ocean, J. Geophys. Res. Oceans, 120, 6669–6699, doi:10.1002/2014JC010310.


