.. _bloom_definitions:

===================================================
Defining the Date of the Spring Phytoplankton Bloom
===================================================

Although the concept of the spring phytoplankton bloom is widely accepted, there is no 
unanimous definition to determine the date at which this occurs. For this analysis, 3 different 
methods were used to describe the bloom date. They were arbitrarily named Metric 1, 2 and 3 
for the purpose of this analysis.

These definitions and functions can be found in this .py file (add link to file with the code and 
definitions). 

Metric 1, defined by Allen and Wolfe (2013), states that the spring bloom is the peak 
phytoplankton concentration within four days of the nitrate concentration going below 0.5 
micromolar for two consecutive days. [1]_ This threshold value was changed to 2.0 micromolar, 
to compensate for the nitrate problems(?) in the 201905 model run. The phytoplankton and 
nitrate concentration values for this method are the average from the surface to three meters in 
depth.

Metric 2 is a definition from Olson et. al (2020) that describes the spring bloom as the first peak 
in which chlorophyll concentrations are above 5 micrograms/L for more than 2 days. [2]_
												
Metric 3 is a method used by Suchy et. al in prep. For a given year, bloom initiation is 
determined to be the week that first reaches the threshold value (by looking at weekly averages), 
as long as one of the two following weeks was >70% of the threshold value. The threshold value is 
the median + 5% of the annual chlorophyll concentration.
												
While Metric 2 and 3 both represent the beginning or peak of the bloom, Metric 1 is more of a 
representation of the end of the bloom. 
													
A 2007-2019 time series of annual bloom dates according to each metric can be found in this notebook. (bloomdate_timeseries) 
Metric 2 and 3 bloom dates were quite consistently close 
to one another. Metric 3 was not as consistent from year to year, and was not as heavily 
considered in the subsequent analyses. 

The variables for calculating bloom timing were taken from daily :file:`.ptrc` files from February 15th to 
June 14th, as spring blooms in the region have historically only ever occurred within this time 
frame. Pickle files holding these variables were created for each year from 2007-2020, and are 
stored in :file:`/ocean/aisabell/MEOPAR/extracted_files`. The file names are in the format 
:file:`springBloomTime_{year}_{loc}_{modver}.pkl`, with :file:`loc` being the location of interest (eg. S3), 
:file:`modver` being the model run (e.g. 201905) and :file:`year` being the year. In the same directory, 
there are also spring bloom time files containing variables from the 201812 model run from 
2015-2019.

**References**

.. [1] Allen, S. E. and M. A. Wolfe. (2013). Hindcast of the Timing of the Spring Phytoplankton 
	Bloom in the Strait of Georgia, 1968-2010. Progress in Oceanography, volume 115, pp 6-13. 
	https://dx.doi.org/10.1016/j.pocean.2013.05.026

.. [2] Olson, E. M., Allen, S. E., Do, V., Dunphy, M., & Ianson, D. (2020). Assessment of nutrient 
	supply by a tidal jet in the northern Strait of Georgia based on a biogeochemical model. Journal 
	of Geophysical Research: Oceans, 125, e2019JC015766. https://doi.org/10.1029/2019JC015766 







