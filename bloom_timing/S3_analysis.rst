.. _S3_analysis:

=========================
Station S3 Bloom Analysis
=========================

Station S3 is a location off the central/south east coast of Vancouver Island. It was chosen as 
the first location to examine in order to compare to the results of a previous environmental driver 
analysis of spring bloom timing from a one dimensional model at this location [1]_.

.. figure:: StationS3.png

A time series of phytoplankton concentrations, environmental forcings and bloom timing at 
Station S3 from 2007-2020 can be found in this notebook_. The environmental drivers were 
averaged over each month for January, February and March. It was found that the most 
significant relationships between bloom date and drivers occurred in March. This is apparent in 
the bloom date v.s. Environmental driver plots in the aforementioned time series notebook_. 
Although zooplankton concentrations were examined in this part of the analysis, it was decided 
to not explore that relationship further, as zooplankton concentrations were likely more 
dependent on bloom time than the other way around. 

Next, correlation, residuals analysis and multiple linear regression were done on environmental 
forcings and bloom timing. The notebook can be found here_. By looking at 3 correlation 
plots between bloom date and environmental driver averages for January, February and March, 
the most significant correlations were once again seen in the March averages. 
												
It was found that the most correlated driver with bloom date was wind speed cubed, with 
turbocline depth being a close second. While turbocline depth has strong correlations with 
bloom timing, it is also very highly correlated with wind speed variability. Due to the causal 
relationship between wind speeds and turbocline depth through wind driven mixing, it was 
therefore decided to only consider wind as a key environmental driver. There is a strong positive 
relationship between wind and bloom timing because stronger wind leads to increased mixing 
near the surface. When phytoplankton are mixed to greater depth, their average exposure to 
light is reduced, leading to slower growth and a delayed bloom. In contrast, early stratification 
allows phytoplankton to be exposed to greater light levels than when mixed to greater depths, 
and allows an earlier bloom to occur. The next strongest correlation with bloom timing was solar 
radiation. Higher solar radiation (reduced cloud cover) in March exposes phytoplankton to 
greater light levels, which promotes faster growth and, consequently, an earlier bloom. 
Additionally, solar radiation increases near-surface temperature. Temperature itself was the third 
environmental condition that was highly correlated with bloom timing at station S3. Temperature 
can influence bloom timing in multiple ways. Cooler temperatures reduce phytoplankton growth 
rates, so years with warmer temperatures in late winter/early spring will promote fast growth and 
early blooms. Phytoplankton mortality and grazing losses are also temperature dependent, with 
grazing pressures decreasing in warmer conditions [2]_. Furthermore, because warmer waters 
are less dense than cold water, temperature increases can increase stratification, and the same 
effect of wind driven stratification on bloom timing can occur. Another possible tertiary 
environmental driver for spring bloom timing at station S3 is the Fraser river input. Although the 
Fraser flow initially had a weak correlation with bloom timing, residual analysis showed that 
when wind was taken out of the equation, there was quite a strong negative relationship with the 
spring bloom. The same situation was observed in residual analysis with solar radiation and 
bloom timing. The Fraser river is the most significant single source of freshwater to the Salish 
Sea. When Fraser flow rates are high in March, it creates strong stratification between low 
salinity surface waters and high salinity deep waters. Similarly to the effects of wind, this 
stratification allows phytoplankton to be mixed at shallower depths, which exposes them to more 
light and results in fast growth and an early bloom. Although mid-depth nitrate concentrations 
(30-90m) had high correlations with bloom timing, it was determined from examining a time 
series that the variation of this environmental factor was likely due to the effects of 
phytoplankton growth, and is not a notable driver for bloom timing. 

In summary, this analysis identifies the four strongest environmental drivers of spring 
phytoplankton bloom timing at Station S3. The primary driver was found to be wind speeds, the 
secondary driver was solar radiation, and sea surface temperatures and Fraser flow rates were 
found to be equally strong tertiary drivers. This is partially consistent with the findings of Collins 
et. al (2009), which determined through analysis of a 1D model at Station S3 that the primary 
driver of the spring bloom was wind speed, with solar radiation being a secondary driver [1]_. 
They did not, however, find any effect from surface temperatures or Fraser input on 
phytoplankton bloom timing, whereas the present analysis did identify a strong impact from both 
of these factors. The effect of the Fraser River was parameterized in Collins et al. (2009) but is 
more fully represented in the present model, which could be a leading cause in this discrepancy. 

**References:**

.. [1] Collins, A. K., Allen, S. E., & Pawlowicz, R. (2009). The role of wind in determining the timing of the spring bloom in the Strait of Georgia. Canadian Journal of Fisheries and Aquatic Sciences, 66(9), 1597-1616. doi:10.1139/f09-071

.. [2] Suchy, K. D., Baron, N. L., Hilborn, A., Perry, R. I., & Costa, M. (2019). Influence of environmental drivers on spatio-temporal dynamics of satellite-derived chlorophyll a in the Strait of Georgia. Progress in Oceanography, 176, 102134. doi:10.1016/j.pocean.2019.102134

.. _notebook: bloom_notebooks/201905EnvironmentalDrivers_S3.ipynb

.. _here: bloom_notebooks/201905analysis_S3.ipynb





