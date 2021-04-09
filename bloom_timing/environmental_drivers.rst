.. _environmental_drivers:

=====================
Environmental Drivers 
=====================

Many environmental drivers were compared to the spring bloom date to investigate which 
factors had the most influence on interannual variability of the bloom timing. For each driver, the 
monthly averages for January, February and March were extracted. 

For efficient data usage, four pickle files for each year were created for different variables. As 
with bloom timing variables, these files are stored in :file:`/ocean/aisabell/MEOPAR/extracted_files`. 
The format is :file:`loc` being the location of interest (eg. S3), :file:`modver` being the model run (eg. 
201905) and :file:`year` being the year. 

**There is a file for each year for:**													
	- Location specific variables :file:`JanToMarch_TimeSeries_{year}_{loc}_{modver}.pkl`
	- Non-location specific variables :file:`JanToMarch_TimeSeries_{year}_{modver}.pkl`
	- Location specific mixing variables :file:`JanToMarch_Mixing_{year}_{loc}_{modver}.pkl`

Biological variables, such as nitrate, diatom, flagellate, ciliate, microzooplankton and 
mesozooplankton concentrations as well as time were all extracted from daily :file:`.ptrc` files.  
Temperature and salinity were extracted from daily :file:`.grid_T` files. Eddy diffusivity values were 
taken from hourly :file:`.grid_W` files. Depth was taken from both :file:`grid_T` and :file:`grid_W`. Wind and 
solar variables were extracted from :file:`ops` forcing files. Fraser flow data is from Environment Canada and comes 
as daily flow averages, measured at Hope. Deep nitrate concentrations (below 250m) were also 
calculated from :file:`.ptrc` files, but these are not location specific and rather represent the average 
across the entire Strait of Georgia region.

Some key factors that relate to mixing were calculated to compare to bloom timing and to other 
environmental drivers. This includes the monthly averages of halocline depth and turbocline 
depth, the density difference from the surface to a series of depths from 5m to 30m, in 
increments of 5m, and the average eddy diffusivity over the upper 15m and 30m. The code for 
these functions can be found here_.

**To recreate extracting these variables for a different location:**
	1. Open the makePickles201905_ notebook
	2. Follow the instructions in the second code cell of the notebook

.. _here: https://github.com/SalishSeaCast/tools/blob/master/SalishSeaTools/salishsea_tools/bloomdrivers.py
								
.. _makePickles201905: bloom_notebooks/makePickles201905.ipynb

