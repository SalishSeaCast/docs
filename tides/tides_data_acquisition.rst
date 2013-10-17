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
*	`Sandy Cove <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7786&user=isdm-gdsi&region=PAC>`_ (7786, ??min, year1, year2, numdays)
*	`Cascadia Terminals <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7743&user=isdm-gdsi&region=PAC>`_ (7743, ??min, year1, year2, numdays)
*	`Stanovan <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7747&user=isdm-gdsi&region=PAC>`_ (7747, ??min, year1, year2, numdays)
*	`Port Moody <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7755&user=isdm-gdsi&region=PAC>`_ (7755, ??min, year1, year2, numdays)
*	`Sea Island <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7625&user=isdm-gdsi&region=PAC>`_ (7625, ??min, year1, year2, numdays)
*	`N. Arm, Fraser <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7640&user=isdm-gdsi&region=PAC>`_ (7640, ??min, year1, year2, numdays)
*	`Port Mann <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7657&user=isdm-gdsi&region=PAC>`_ (7657, ??min, year1, year2, numdays)
*	`Pitt River <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7660&user=isdm-gdsi&region=PAC>`_ (7660, ??min, year1, year2, numdays)
*	`Pitt Lake <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7666&user=isdm-gdsi&region=PAC>`_ (7666, ??min, year1, year2, numdays)
*	`Whonnock <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7676&user=isdm-gdsi&region=PAC>`_ (7676, ??min, year1, year2, numdays)
*	`Mission City <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7680&user=isdm-gdsi&region=PAC>`_ (7680, ??min, year1, year2, numdays)

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

Nanaimo region:

*	`Nanaimo Harbour <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7917&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7917, 3min, 1997, 2003, 2259, yes)
*	`Hammond Bay <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7924&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7924, ??min, 1959, 1959, numdays)*
*	`Winchelsea Is. <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7935&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7935, 60min, 1967, 1978, 4159, yes)
*	`Nanoose Harbour <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7930&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7930, 60min, 1986, 1993, 2764, yes)
*	`Northwest Bay <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7938&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7938, 60min, 1967, 1968, 411, yes)
*	`Boat Harbour <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7480&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7480, 60min, 1972, 1972, 50, yes)
*	`Northumberland Channel <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7915&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7915, ??min, 1949, 1949, numdays)*
*	`Pylades Channel <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7442&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7442, 3min, 1999, 1999, 1, yes)
*	`Silva Bay <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7550&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7550, 60min, 1967, 2002, 873, no)

*	`Cowichan Bay <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7310&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7310, 60min, 1961, 1961, 39, yes)
*	`Piers Harbour <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7272&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7272, 60min, 1980, 1980, 212, yes)
*	`Swartz Bay <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7270&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7270, 60min, 1963, 1963, 29, yes)
*	`Tsehum Harbour <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7262&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7262, 60min, 1983, 1983, 62, yes)

Victoria region:

*	`Sidney <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7260&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7260, 60min, 1953, 2000, 936, no)
*	`Finlayson Arm <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7284&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7284, 60min, 1966, 1966, 102, yes)
*	`Finnerty Cove <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7140&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7140, 60min, 1967, 1975, 2829, yes)
*	`Oak Bay <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7130&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7130, 1min, 2001, 2004, 1017, no)
*	`Clover Point <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7115&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7115, 60min, 1967, 1967, 235, yes)
*	`Selkirk Water <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7121&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7121, 1min, 2011, 2011, 145, yes)
*	`Esquimalt Lagoon <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7107&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7107, 60min, 1972, 1984, 2569, no)
*	`Esquimalt Harbour <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7109&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7109, 60min, 1981, 2010, 1313, no)
*	`Esquimalt Government Hbr <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7110&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7110, 60min, 1972, 1978, 1604, no)
*	`Portage Inlet <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7125&user=isdm-gdsi&region=PAC>`_ (7125, ??min, year1, year2, numdays)
*	`Gorge at Craigflower<http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7124&user=isdm-gdsi&region=PAC>`_ (7124, ??min, year1, year2, numdays)
*	`Gorge at Tillicum <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7123&user=isdm-gdsi&region=PAC>`_ (7123, ??min, year1, year2, numdays)
*	`Gorge at Aaron Point <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7122&user=isdm-gdsi&region=PAC>`_ (7122, ??min, year1, year2, numdays)
*	`Selkirk Water, <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7121&user=isdm-gdsi&region=PAC>`_ (7121, ??min, year1, year2, numdays)

Southern Vancouver Island

*	`Pedder Bay <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7080&user=isdm-gdsi&region=PAC>`_ (7080, 60min, 1967, 1969, 601, yes)
*	`Becher Bay <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7030&user=isdm-gdsi&region=PAC>`_ (7030, 60min, 1976, 1976, 56, yes)
*	`Twin Cove <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7022&user=isdm-gdsi&region=PAC>`_ (7022, 1min, 2011, 2011, 50, yes)
*	`Sooke Basin <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7024&user=isdm-gdsi&region=PAC>`_ (7024, 60min, 1977, 1983, 1077, no)
*	`Sooke <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7020&user=isdm-gdsi&region=PAC>`_ (7020, 60min, 1972, 1985, 4416, yes)

Washington:

*	`Ferndale, Wash. <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7564&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7564, 60min, 1967, 1970, 1087, yes)
*	`Patos Island <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7505&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7505, 60min, 1967, 1969, 422, no)
*	`Reservation Bay <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7196&user=isdm-gdsi&region=PAC>`_ (7196, 60min, 1971, 1971, 302, yes)
*	`Cornet Bay <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7193&user=isdm-gdsi&region=PAC>`_ (7193, 60min, 1971, 1971, 121, yes)
*	`Port Townsend <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7160&user=isdm-gdsi&region=PAC>`_ (7160, 60min, 1971, 1971, 121, yes)
*	`Meadowdale <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7182&user=isdm-gdsi&region=PAC>`_ (7182, 60min, 1971, 1971, 121, yes)
*	`Seattle <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7180&user=isdm-gdsi&region=PAC>`_ (7180, 60min, 1970, 1970, 365, yes)
*	`Port Angeles <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7060&user=isdm-gdsi&region=PAC>`_ (7060, 60min, 1973, 1973, 151, yes)
*	`Crescent Bay <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7050&user=isdm-gdsi&region=PAC>`_ (7050, 60min, 1964, 1964, 30, yes)
*	`Sekiu (Clallam Bay) <http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7037&user=isdm-gdsi&region=PAC>`_ (7037, 60min, 1973, 1974, 370, yes)

West coast Vancouver Island (southern)

*	`Port Renfrew <>`_ (8525, ??min, year1, year2, numdays)
*	`Mutine Point <>`_ (8556, ??min, year1, year2, numdays)
*	`Pocahontas Pt <>`_ (8560, ??min, year1, year2, numdays)
*	`Chesnuknuw Cr <>`_ (8562, ??min, year1, year2, numdays)
*	`Sproat Narrows <>`_ (8564, ??min, year1, year2, numdays)
*	`Franklin River <>`_ (8565, ??min, year1, year2, numdays)
*	`Stamp Narrows <>`_ (8570, ??min, year1, year2, numdays)
*	`Port Alberni <>`_ (8575, ??min, year1, year2, numdays)
*	`Brooksby Point <>`_ (8558, ??min, year1, year2, numdays)
*	`Kildonan <>`_ (8557, ??min, year1, year2, numdays)
*	`Head of Uchucklesit <>`_ (8559, ??min, year1, year2, numdays)
*	`Effingham <>`_ (8585, ??min, year1, year2, numdays)
*	`Walsh Island <>`_ (8586, ??min, year1, year2, numdays)
*	`Ucluelet <>`_ (8595, ??min, year1, year2, numdays)
*	`Kennedy Cover <>`_ (8623, ??min, year1, year2, numdays)
*	`Warn Bay <>`_ (8626, ??min, year1, year2, numdays)
*	`Cypress Bay <>`_ (8630, ??min, year1, year2, numdays)
*	`Herbert Inlet <>`_ (8632, ??min, year1, year2, numdays)
*	`Sulphur Passage <>`_ (8634, ??min, year1, year2, numdays)
*	`Riley Cove <>`_ (8637, ??min, year1, year2, numdays)

West coast Vancouver Island (northern)

*	`Gold River <>`_ (8650, ??min, year1, year2, numdays)
*	`Saavedra Islands <>`_ (8645, ??min, year1, year2, numdays)
*	`Esperanza <>`_ (8665, ??min, year1, year2, numdays)
*	`Tahsis <>`_ (8658, ??min, year1, year2, numdays)
*	`Zeballos <>`_ (8670, ??min, year1, year2, numdays)
*	`Kyuquot <>`_ (8710, ??min, year1, year2, numdays)
*	`Port Alice <>`_ (8750, ??min, year1, year2, numdays)
*	`Bergh Cove <>`_ (8754, ??min, year1, year2, numdays)
*	`Kwokwesta Creek <>`_ (8755, ??min, year1, year2, numdays)
*	`Makwazniht I <>`_ (8756, ??min, year1, year2, numdays)
*	`Coal Harbour <>`_ (8765, ??min, year1, year2, numdays)
*	`Hunt Islet <>`_ (8736, ??min, year1, year2, numdays)
*	`Cape Scott <>`_ (8790, ??min, year1, year2, numdays)

Howe Sound

*	`Squamish Inner <>`_ (7811, ??min, year1, year2, numdays)
*	`Squamish <>`_ (7810, ??min, year1, year2, numdays)
*	`Latona Beach <>`_ (7805, ??min, year1, year2, numdays)
*	`Gibsons <>`_ (7820, ??min, year1, year2, numdays)

Sunshine Coast

*	`Roberts Creek <>`_ (7824, ??min, year1, year2, numdays)
*	`Porpoise Bay <>`_ (7852, ??min, year1, year2, numdays)
*	`Halfmoon Bay <>`_ (7830, ??min, year1, year2, numdays)
*	`Storm Bay <>`_ (7847, ??min, year1, year2, numdays)
*	`Irvines Landing <>`_ (7836, ??min, year1, year2, numdays)
*	`Egmont <>`_ (7842, ??min, year1, year2, numdays)
*	`Saltery Bay <>`_ (7868, ??min, year1, year2, numdays)
*	`Powell River <>`_ (7880, ??min, year1, year2, numdays)
*	`Okeover Inlet <>`_ (8006, ??min, year1, year2, numdays)
*	`Lund <>`_ (7885, ??min, year1, year2, numdays)
*	`Prideaux Haven <>`_ (8008, ??min, year1, year2, numdays)

Mid Strait of Georgia 

*	`Squitty Bay <>`_ (7980, ??min, year1, year2, numdays)
*	`Skerry Bay <>`_ (7985, ??min, year1, year2, numdays)
*	`False Bay <>`_ (7982, ??min, year1, year2, numdays)
*	`Welcome Bay <>`_ (7990, ??min, year1, year2, numdays)
*	`Blubber Bay <>`_ (7875, ??min, year1, year2, numdays)
*	`Hornby Island <>`_ (7953, ??min, year1, year2, numdays)
*	`Denman Island <>`_ (7955, ??min, year1, year2, numdays)
*	`Comox <>`_ (7965, ??min, year1, year2, numdays)
*	`Little River <>`_ (7993, ??min, year1, year2, numdays)

Northern Strait of Georgia

*	`Mitlenatch <>`_ (7895, ??min, year1, year2, numdays)
*	`Twin Island <>`_ (7892, ??min, year1, year2, numdays)
*	`Surge Narrows <>`_ (8045, ??min, year1, year2, numdays)
*	`Florence Cove <>`_ (8055, ??min, year1, year2, numdays)
*	`Octopus Island <>`_ (8050, ??min, year1, year2, numdays)
*	`Owen Bay <>`_ (8120, ??min, year1, year2, numdays)
*	`Okis Island <>`_ (8124, ??min, year1, year2, numdays)
*	`Brown Bay <>`_ (8110, ??min, year1, year2, numdays)
*	`Seymour Narrows <>`_ (8105, ??min, year1, year2, numdays)
*	`Chatham Pt <>`_ (8180, ??min, year1, year2, numdays)
*	`Hardinge Is <>`_ (8127, ??min, year1, year2, numdays)
*	`Big Bay, Stuart Island <>`_ (8060, ??min, year1, year2, numdays)
*	`Blind Channel <>`_ (8155, ??min, year1, year2, numdays)
*	`Kelsey Bay <>`_ (8215, ??min, year1, year2, numdays)
*	`Yorke Island <>`_ (8233, ??min, year1, year2, numdays)
*	`Warren Islands <>`_ (8254, ??min, year1, year2, numdays)
*	`Cedar Island <>`_ (8325, ??min, year1, year2, numdays)
*	`Alert Bay <>`_ (8280, ??min, year1, year2, numdays)
*	`Sullivan Bay <>`_ (8364, ??min, year1, year2, numdays)
*	`Stuart Narrows <>`_ (8379, ??min, year1, year2, numdays)
*	`Drury Inlet <>`_ (8381, ??min, year1, year2, numdays)
*	`Jennis Bay <>`_ (8384, ??min, year1, year2, numdays)
*	`Frederick Sd <>`_ (8458, ??min, year1, year2, numdays)
*	`Alison Sound <>`_ (8488, ??min, year1, year2, numdays)
*	`Nugent Sound <>`_ (8464, ??min, year1, year2, numdays)
*	`Charlotte Bay <>`_ (8443, ??min, year1, year2, numdays)
*	`Mereworth Sound <>`_ (8476, ??min, year1, year2, numdays)
*	`Egg Island <>`_ (8805, ??min, year1, year2, numdays)
*	`Wadhams <>`_ (8840, ??min, year1, year2, numdays)

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



