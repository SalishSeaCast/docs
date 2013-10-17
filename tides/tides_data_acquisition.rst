Data acquisition (tides)
===================================

To evaluate the performance of the NEMO model in reproducing the tides, we will compare the modelled water level output to measured water level data. 

DFO 
-------------------------

Measured water level data can be downloaded in .csv format from the DFO `website
<http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/maps-cartes/inventory-inventaire-eng.asp>`_.

* 10 years of hourly water level measured data can be downloaded at a time
* 1 month of highest resolution (e.g. 1 minute water level at Point Atkinson) can be downloaded at a time

Permanent data stations
````````````````````````````````````

Permanent DFO stations in Strait of Georgia and on Vancouver Island (station number, sampling interval, starting year):

* 	`Point Atkinson <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7795&user=isdm-gdsi&region=PAC>`_ (7795, 1min, 1914)
*	`Vancouver <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7735&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7735, 1min, 1909) 
*	`Patricia Bay <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7277&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7277, 1min, 1966)
* 	`Victoria Harbour <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7120&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7120, 1min, 1909)
*	`Bamfield <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=8545&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (8545, 1min, 1969)
*	`Tofino <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=8615&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (8615, 1min, 1909)
*	`Winter Harbour <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=8735&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (8735, 1min, 1989)
*	`Port Hardy <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=8408&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (8408, 1min, 1964)
*	`Campbell River <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=8074&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (8074, 1min, 1965) 
*	`New Westminster <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7654&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7654, 1min, 1969)

Temporary data stations
````````````````````````````````````
Temporary DFO stations in Strait of Georgia and on Vancouver Island (station number, sampling interval, starting year, end year, days of data, consecutive record?):

Vancouver:

*	`station name <>`_ (num, ??min, year1, year2, numdays)
*	`Point Grey <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7635&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7635, 60min, 1977, 1978, 552)
*	`North Arm <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7634&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7634, 60min, 1969, 1969, 175)
*	`Sand Heads (Stn Harry) <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/maps-cartes/inventory-inventaire-eng.asp#divGoogleMaps>`_ (7604, 60min, 1968, 1969, 283)
*	`Sand Heads <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7594&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7594, 60min, 1969, 1969, 188)
*	`Roberts Bank <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7592&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7592, 60min, 1981, 1982, 203)
*	`Tsawwassen <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7590&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7590, 60min, 1967, 1978, 4002)
*	`Steveston <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7607&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7607, 60min, 1969, 1997, 10440)
*	`Canoe Pass <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7603&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7603, 60min, 1991, 1993, 423, no)
*	`White Rock <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7577&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7577, 60min, 1972, 1972, 158, yes)

Washington:

*	`Ferndale, Wash. <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7564&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7564, 60min, 1967, 1970, 1087, yes)

Lower SoG Island:

*	`Patos Island <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7505&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7505, 60min, 1967, 1969, 422, no)

Gulf Islands:

*	`Montague Harbour <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7420&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7420, 60min, 1964, 1964, 29, yes)
*	`Ganges <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7407&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7407, 60min, 1915, 1915, 30, yes)
*	`Whaler Bay <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7532&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7532, 60min, 1964, 1975, 1665, no)
*	`Georgina Point <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7525&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7525, 60min, 1959, 1959, 37, yes)
*	`Village Bay <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7414&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7414, 60min, 1964, 1964, 29, yes)
*	`Samuel Island <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7370&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7370, 60min, 1961, 1961, 31, yes)
*	`Samuel Island (north shore) <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7515&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7515, 60min, 1961, 1961, 31, yes)
*	`Hope Bay <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7360&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7360, 60min, 1961, 1961, 32, yes)
*	`Bedwell Harbour <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7350&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7350, 1min, 2001, 2002, 119, no)
*	`Narvaez Bay <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7345&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7345, 60min, 1965, 1965, 40, yes)
*	`Tumbo Channel <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7510&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7510, 60min, 1967, 1976, 3267, yes)
*	`Fulford Harbour <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7330&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7330, 60min, 1952, 1992, 14493, yes)
*	`Maple Bay <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7315&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7315, 60min, 1969, 1970, 367, yes)
*	`Crofton <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7450&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7450, 60min, 1960, 1971, 485, no)
*	`Chemainus <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7455&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7455, 60min, 1961, 1961, 34, yes)
*	`Ladysmith <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7460&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7460, 60min, 1954, 1955, 402, yes)
*	`Preedy Harbour <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7471&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7471, 60min, 1961, 1961, 35, yes)
*	`North Galliano <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7435&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7435, 60min, 1972, 1972, 55, yes)
*	`Dionisio Point <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7535&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7535, 60min, 1963, 1968, 50, no)
*	`Valdes Island <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7542&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7542, 60min, 1963, 1963, 15, yes)
*	`Boat Harbour <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7480&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7480, 60min, 1972, 1972, 50, yes)
*	`Northumberland Channel <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7915&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7915, ??min, 1949, 1949, numdays)*
*	`Pylades Channel <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7442&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7442, 3min, 1999, 1999, 1, yes)
*	`Silva Bay <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7550&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7550, 60min, 1967, 2002, 873, no)
*	`Nanaimo Harbour <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7917&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7917, 3min, 1997, 2003, 2259, yes)
*	`Hammond Bay <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7924&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7924, ??min, 1959, 1959, numdays)*
*	`Winchelsea Is. <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7935&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7935, 60min, 1967, 1978, 4159, yes)
*	`Nanoose Harbour <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7930&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7930, 60min, 1986, 1993, 2764, yes)
*	`Northwest Bay <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7938&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7938, 60min, 1967, 1968, 411, yes)


*	`Cowichan Bay <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7310&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7310, 60min, 1961, 1961, 39, yes)
*	`Piers Harbour <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7272&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7272, 60min, 1980, 1980, 212, yes)
*	`Swartz Bay <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7270&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7270, 60min, 1963, 1963, 29, yes)
*	`Tsehum Harbour <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7262&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7262, 60min, 1983, 1983, 62, yes)
*	`station name <>`_ (num, ??min, year1, year2, numdays)
*	`station name <>`_ (num, ??min, year1, year2, numdays)
*	`station name <>`_ (num, ??min, year1, year2, numdays)
*	`station name <>`_ (num, ??min, year1, year2, numdays)
*	`station name <>`_ (num, ??min, year1, year2, numdays)
*	`station name <>`_ (num, ??min, year1, year2, numdays)
*	`station name <>`_ (num, ??min, year1, year2, numdays)
*	`station name <>`_ (num, ??min, year1, year2, numdays)
*	`station name <>`_ (num, ??min, year1, year2, numdays)
*	`station name <>`_ (num, ??min, year1, year2, numdays)
*	`station name <>`_ (num, ??min, year1, year2, numdays)
*	`station name <>`_ (num, ??min, year1, year2, numdays)
*	`station name <>`_ (num, ??min, year1, year2, numdays)
*	`station name <>`_ (num, ??min, year1, year2, numdays)
*	`station name <>`_ (num, ??min, year1, year2, numdays)
*	`station name <>`_ (num, ??min, year1, year2, numdays)
*	`station name <>`_ (num, ??min, year1, year2, numdays)
*	`station name <>`_ (num, ??min, year1, year2, numdays)
*	`station name <>`_ (num, ??min, year1, year2, numdays)
*	`station name <>`_ (num, ??min, year1, year2, numdays)
*	`station name <>`_ (num, ??min, year1, year2, numdays)
*	`station name <>`_ (num, ??min, year1, year2, numdays)
*	`station name <>`_ (num, ??min, year1, year2, numdays)
*	`station name <>`_ (num, ??min, year1, year2, numdays)


*=not digitised

Data format
````````````````````````````````````

Data can be downloaded online until the end of the previous month. 

Downloaded files have 8 header lines with station name, number, lat, long, datum, time zone, type of data and column headers. Data is then in the form::

	YYYY/MM/DD HH:MM,SLEV,

For example::

	Station_Name,New Westminster, BC
	Station_Number,7654
	Latitude_Decimal_Degrees,49.2
	Longitude_Decimal_Degrees,122.91
	Datum,CD
	Time_Zone,PST
	SLEV=Observed Water Level
	Obs_date,SLEV(metres)
	2013/09/01 00:00,2.21,
	2013/09/01 01:00,2.3,
	2013/09/01 02:00,2.37,
	2013/09/01 03:00,2.3,



