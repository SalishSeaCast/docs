Storm surge
======================================================================================================

We need to test our model to see how well it can represent storm surge events in the Salish Sea. We have wind forcing available from 2002-2010, so storms need to be in this period.

High water levels can be due to a combination of high seasonal tide, strong winds, low atmospheric pressure and sea level height anomalies due to ENSO events.

What classifies as a storm?
--------------------------------------

How long is a storm? 
	Decision: more than 6 hours

What is the water level elevation during a storm? 
	Decision: sea surface anomaly greater than 40cm

Does the storm affect the whole domain or does it not matter?
	For now, look at the DFO water level sites that are in the domain and have water level data for the period of interest (2002 - 2010). These sites are:

	* Point Atkinson
		* lat/lon: 49.34,-123.25
		* grid co-ordinates: i=468, j=328
	* Victoria
		* lat/lon: 48.42,-123.37
		* grid co-ordinates: i=298, j=195
	* Patricia Bay
		* lat/lon: 48.65,-123.45
		* grid co-ordinates: i=351, j=213
	* Campbell River
		* lat/lon: 50.04,-125.25
		* grid co-ordinates: i=747, j=123

Finding storms in the record
------------------------------------------

Compare predicted tide with measured water level using t_tide (Pawlowicz et al, 2002) and the following MATLAB scripts: ::

	get_tidal_anomaly.m
	find_storm_events.m

Usage: ::

	[pred,wlev,anomaly,tim] = get_tidal_anomaly(csvfilename)
	[startind,endind,lengthstorm] = find_storm_events(anomaly,tim,anomthres,stormlength)

where:

* csvfilename - name of csv file of hourly measured water level at Point Atkinson
* pred - predicted tides from t_tide (m CD)
* wlev - measured water level at Point Atkinson (m CD)
* anomaly - difference between prediction and water level (m)
* tim - time vector for pred, wlev and anomaly (MATLAB date format)
* startind - indice in tim for start of each storm (-)
* endind - indice in tim for end of each storm (-)
* lengthstorm - length of each storm (hours)
* anomthres - water level elevation defined as a storm (m)
* stormlength - minimum length defined as a storm (hrs)

Outputs a text file called 'storms.txt' that contains a list of the start dates of storms and the length of each storm in hours.

Literature search for big storms 
-----------------------------------------

* Jan 02, 2003, Victoria
* Dec 24, 2003, Vancouver
* Nov 2006, Vancouver
* Dec 15, 2006, Vancouver 
* Dec 25, 2008, Vancouver
* Nov 24, 2011, Vancouver (not within range)
* Jan 05, 2012, Vancouver (not within range)
* Dec 17, 2012, Vancouver (not within range)
* Sep 30, 2013, Vancouver (not within range)

Prediction for 2013/2014 from Storm Surge Almanac:

`Flooding risk is greatest during the seasonal perigean spring tides, which correspond to times of extreme high tidal levels during the winter months. This year the highest tides for the Lower Mainland are expected in the first weeks of December, January and February; for Victoria the highest tides are expected in the first and last weeks of December, and the final week of January.`

Also, from Abeysirigunawardena et al (2011), extremes generally occur from October to March.

Existing storm surge models
---------------------------------------

The Government of British Columbia launched the `BC Storm Surge Forecast System <http://www.env.gov.bc.ca/cas/adaptation/storm_surges.html>`_ in 2011. This model is driven by the Pacific Ocean Model (which is driven by 7 day weather forecasts from NOAA) and a 6 day forecast from Environment Canada. Forecast bulletins are available for Point Atkinson, Victoria and Campbell River. There is no hindcasting avaiable in the model.


References
-------------------------------
Abeysirigunawardena, D.S., D.J. Smith and B. Taylor, 2011. Extreme Sea Surge Responses to
Climate Variability in Coastal British Columbia, Canada, Annals of the Association of American Geographers, 101:5, 992-1010,
DOI: 10.1080/00045608.2011.585929



