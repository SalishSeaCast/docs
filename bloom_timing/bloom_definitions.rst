.. _bloom_definitions:

===================================================
Defining the Date of the Spring Phytoplankton Bloom
===================================================

Although the concept of the spring phytoplankton bloom is widely accepted, there is no single 
accepted definition to determine the date at which this occurs. For this analysis, 3 different 
methods were used. They were arbitrarily named Metric 1, 2 and 3 for the purpose of this 
analysis.

These definitions and functions can be found `here`_ . 

Metric 1, defined by Allen and Wolfe (2013), defines the spring bloom date as the peak surface  
phytoplankton concentration within four days of the surface nitrate concentration going below  
0.5 micromolar for two consecutive days. [1]_ This threshold value was changed to 2.0  
micromolar, for consistency with the nitrate half saturation constant used in the 201905 
SalishSeaCast model run. The surface phytoplankton and nitrate concentrations are assessed 
as the average from  the surface to three meters in depth.

Metric 2 is a definition from Olson et. al (2020) that describes the spring bloom time as the date 
of the first peak in which mean upper-3-m chlorophyll concentrations are above 5 micrograms/L 
for more than two consecutive days. [2]_ 

Metric 3 is a method used by Suchy et. al in prep. For a given year, bloom initiation is identified 
from the week the chlorophyll concentration first reaches a threshold value (by looking at weekly 
averages), as long as one of the two following weeks was >70% of that threshold value. The 
threshold value is the median + 5% of the annual chlorophyll concentration.

While Metric 2 and 3 both represent early bloom conditions, Metric 1 is associated with nitrate 
depletion, a trigger of bloom decline.

A 2007-2019 time series of annual bloom dates at Station S3 according to each metric can be 
found in this notebook_. Metric 2 and 3 bloom dates were consistently close to one another. Metric 3 bloom dates were not as consistent from year to year, as they ranged from occurring 
within a few days of Metric 1 and 2 to a difference of nearly 3 weeks. Metric 3 was not as 
heavily considered in the subsequent analyses. 

The variables for calculating bloom timing were taken from daily model output  (:file:`.ptrc` files) 
from February 15th to June 14th, a time frame chosen based on previous knowledge of spring 
bloom timing in the region [3]_. Pickle files holding these variables were created for each year 
from 2007-2020, and are stored in :file:`/ocean/aisabell/MEOPAR/extracted_files`. These files 
were created using this code_. The file names are in the format 
:file:`springBloomTime_{year}_{loc}_{modver}.pkl`, with :file:`loc` being the location of 
interest (eg. S3), :file:`modver` being the model run (e.g. 201905) and :file:`year` being the  
year. In the same directory, there are also spring bloom time files containing variables from the  
201812 model run from 2015-2019.


**References:**

.. [1] Allen, S. E. and M. A. Wolfe. (2013). Hindcast of the Timing of the Spring Phytoplankton Bloom in the Strait of Georgia, 1968-2010. Progress in Oceanography, volume 115, pp 6-13. https://dx.doi.org/10.1016/j.pocean.2013.05.026

.. [2] Olson, E. M., Allen, S. E., Do, V., Dunphy, M., & Ianson, D. (2020). Assessment of nutrient supply by a tidal jet in the northern Strait of Georgia based on a biogeochemical model. Journal of Geophysical Research: Oceans, 125, e2019JC015766. `https://doi.org/10.1029/2019JC015766`_

.. _https://doi.org/10.1029/2019JC015766: https://onlinelibrary.wiley.com/doi/10.1029/2019JC015766

.. [3] Collins, A. K., Allen, S. E., & Pawlowicz, R. (2009). The role of wind in determining the timing of the spring bloom in the Strait of Georgia. Canadian Journal of Fisheries and Aquatic Sciences, 66(9), 1597-1616. doi:10.1139/f09-071

.. _here: https://github.com/SalishSeaCast/tools/blob/main/SalishSeaTools/salishsea_tools/bloomdrivers.py

.. _notebook: bloom_notebooks/201905EnvironmentalDrivers_S3.ipynb#Bloom-Date-Time-Series

.. _code: bloom_notebooks/makePickles201905.ipynb







