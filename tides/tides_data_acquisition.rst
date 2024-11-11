Data acquisition (predicted tides and measured water level)
======================================================================================================

Tidal forcing is required on the boundaries of the NEMO model. Also, to evaluate the performance of the NEMO model in reproducing the tides, modelled water level output must be compared to measured water level data.

DFO modelled tidal predictions
---------------------------------------

WebTide_ is an online tidal prediction tool developed by DFO. The predictions for the North East Pacific region are based on the model of :ref:`foremanetal00`. Tidal currents and elevations can be predicted from the model.

.. _WebTide: https://www.bio.gc.ca/science/research-recherche/ocean/webtide/index-en.php

WebTide was used for the Juan de Fuca boundary conditions in the preliminary runs of the NEMO model. WebTide doesn't quite include our Johnstone Strait boundary, which is around -126.5 deg, 50.5 deg. Webtide cuts out around -126.8 deg, 50.6 deg... so it's close!

Tidal constituents (amplitude and phase) can be output from Webtide for surface elevation and tidal currents.

For tidal currents, these constituents are relative to compass north, south etc. However, firstly our grid is rotated 29 degrees and secondly, NEMO takes Z1 and Z2, where

	.. math::
	 Z1 = A\cos\phi

	 Z2 = A\sin\phi

So to go from Webtide constituents to grid constituents, with a rotation of theta (= 29 degrees), for U

	.. math::
	 Z1 = u_a\cos\theta\cos\phi_u - v_a\sin\theta\sin\phi_v

	 Z2 = u_a\cos\theta\sin\phi_u + v_a\sin\theta\cos\phi_v

And for V

	.. math::
	 Z1 = -u_a\sin\theta\cos\phi_u - v_a\cos\theta\sin\phi_v

	 Z2 = -u_a\sin\theta\sin\phi_u + v_a\cos\theta\cos\phi_v

where

	.. math::
	 u_a = \text{Webtide u amplitude}

	 v_a = \text{Webtide v amplitude}

	 \phi_u = \text{Webtide u phase}

	 \phi_v = \text{Webtide v phase}

	 \theta = \text{Grid rotation} (29^\circ)


Thomson & Huggett (1980) - Johnstone Strait
------------------------------------------------------------------

Thomson & Huggett (1980) measured tidal elevations at 4 stations in Johnstone Strait 1976 and 1977. Tidal elevation harmonics for O1, K1, M2 and S2 are reported in Table 2 and stations are shown in Figure 4.

They also measured tidal currents at 11 stations (at various depths) in Johnstone Strait and report tidal current harmonics in Table 1.

Note that phase in these tables is reported as phase lag relative to 120 degrees W longitude. This means the measurements were probably in Pacific Standard Time (PST) and that's how the lags were calculated.

Now, 120 degrees W (Pacific Standard Time) is 8 hours different to GMT, so to convert M2 phase from PST to GMT:

GMTphase = PSTphase - (-8hours)*(1/12.42hours)*360degrees

(For details on this conversion,
see Manual for Tide Heights Analysis and Prediction by M.G.G. Foreman,
Pacific Marine Science Report 77-10, IOS, 1977 (Revised 2004),
Section 2.3.1 Astronomical argument and Greenwich phase lag,
available `from here <ftp://canuck.seos.uvic.ca/docs/MFTides/heights.pdf>`_
or `also here <https://www.dfo-mpo.gc.ca/science/data-donnees/tidal-marees/index-eng.html>`_)

This paper also have some info on temperature and salinity in Johnstone Strait that may be useful.

Thomson (1976) and Thomson (1977) - Johnstone Strait
------------------------------------------------------------------------------

Four months of current observations in Johnstone Strait


TPXO7.1 modelled tidal predictions
----------------------------------------------

`TPXO7.1 <https://www.esr.org/research/polar-tide-models/tmd-software/>`_ is an online tidal prediction tool funded by NASA. The predictions are based on the model by Egbert and Erofeeva (2002) at Oregon State University. The model is a 1/4 degree x 1/4 degree global inverse tide model. Predictions can be calculated using a Matlab based GUI called TMD (the Tide Model Driver).

TPXO7.1 was used for the boundary conditions of the model by :ref:`sutherlandetal11`... but resolution is coarse in Juan de Fuca Strait and the Strait of Georgia is an inland sea... Sutherland et al. (2011) used the tidal predictions for a straight boundary along 127 degrees W. The TPXO7.1 predictions are probably only useful when boundaries are offshore from the coast.


DFO measured data
-----------------

Measured water level data can be downloaded in .csv format from Canada's Department of Fisheries
and Oceans (DFO) `website
<https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/maps-cartes/inventory-inventaire-eng.asp?user=isdm-gdsi&region=MEDS&tst=1&perm=0>`_.

* 10 years of hourly water level measured data can be downloaded at a time
* 1 month of highest resolution (e.g. 1 minute water level at Point Atkinson) can be downloaded at a time


Permanent data stations
```````````````````````

Permanent DFO stations in Strait of Georgia and on Vancouver Island (station number, sampling interval, starting year):

* 	`Point Atkinson <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7795&user=isdm-gdsi&region=PAC>`_ (7795, 1min, 1914)
*	`Vancouver <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7735&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7735, 1min, 1909)
*	`Patricia Bay <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7277&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7277, 1min, 1966)
* 	`Victoria Harbour <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7120&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7120, 1min, 1909)
*	`Bamfield <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=8545&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (8545, 1min, 1969)
*	`Tofino <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=8615&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (8615, 1min, 1909)
*	`Winter Harbour <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=8735&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (8735, 1min, 1989)
*	`Port Hardy <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=8408&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (8408, 1min, 1964)
*	`Campbell River <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=8074&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (8074, 1min, 1965)
*	`New Westminster <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7654&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7654, 1min, 1969)

Temporary data stations
````````````````````````````````````
Temporary DFO stations in Strait of Georgia and on Vancouver Island (station number, sampling interval, starting year, end year, days of data, consecutive record?):

Vancouver:

*	station name  (num, ??min, year1, year2, numdays)
*	`Point Grey <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7635&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7635, 60min, 1977, 1978, 552)
*	`North Arm <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7634&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7634, 60min, 1969, 1969, 175)
*	`Sand Heads (Stn Harry) <https://www.isdm-gdsi.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7604&user=isdm-gdsi&region=MEDS>`_ (7604, 60min, 1968, 1969, 283)
*	`Sand Heads <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7594&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7594, 60min, 1969, 1969, 188)
*	`Roberts Bank <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7592&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7592, 60min, 1981, 1982, 203)
*	`Tsawwassen <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7590&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7590, 60min, 1967, 1978, 4002)
*	`Steveston <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7607&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7607, 60min, 1969, 1997, 10440)
*	`Canoe Pass <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7603&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7603, 60min, 1991, 1993, 423, no)
*	`White Rock <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7577&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7577, 60min, 1972, 1972, 158, yes)
*	`Sandy Cove <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7786&user=isdm-gdsi&region=PAC>`_ (7786, 1min, 2009, 2017, 2931, yes)
*	`Cascadia Terminals <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7743&user=isdm-gdsi&region=PAC>`_ (7743, 1min, 2001, 2002, 84, yes)
*	`Stanovan <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7747&user=isdm-gdsi&region=PAC>`_ (7747, 60 & 1min, 1963, 2002, 304, no)
*	`Port Moody <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7755&user=isdm-gdsi&region=PAC>`_ (7755, 60min, 1962, 1965, 672, no)
*	`Sea Island <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7625&user=isdm-gdsi&region=PAC>`_ (7625, 60min, 1969, 1969, 146, yes)
*	`N. Arm, Fraser <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7640&user=isdm-gdsi&region=PAC>`_ (7640, 60min, 1959, 1959, 274, yes)
*	`Port Mann <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7657&user=isdm-gdsi&region=PAC>`_ (7657, N/A, N/A, N/A, N/A)
*	`Pitt River <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7660&user=isdm-gdsi&region=PAC>`_ (7660, 15min, 1959, 1959, 273, yes)
*	`Pitt Lake <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7666&user=isdm-gdsi&region=PAC>`_ (7666, N/A, N/A, N/A, N/A)
*	`Whonnock <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7676&user=isdm-gdsi&region=PAC>`_ (7676, N/A, N/A, N/A, N/A)
*	`Mission City <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7680&user=isdm-gdsi&region=PAC>`_ (7680, N/A, N/A, N/A, N/A)

Gulf Islands:

*	`Montague Harbour <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7420&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7420, 60min, 1964, 1964, 29, yes)
*	`Ganges <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7407&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7407, 60min, 1915, 1915, 30, yes)
*	`Whaler Bay <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7532&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7532, 60min, 1964, 1975, 1665, no)
*	`Georgina Point <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7525&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7525, 60min, 1959, 1959, 37, yes)
*	`Village Bay <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7414&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7414, 60min, 1964, 1964, 29, yes)
*	`Samuel Island <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7370&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7370, 60min, 1961, 1961, 31, yes)
*	`Samuel Island (north shore) <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7515&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7515, 60min, 1961, 1961, 31, yes)
*	`Hope Bay <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7360&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7360, 60min, 1961, 1961, 32, yes)
*	`Bedwell Harbour <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7350&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7350, 1min, 2001, 2002, 119, no)
*	`Narvaez Bay <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7345&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7345, 60min, 1965, 1965, 40, yes)
*	`Tumbo Channel <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7510&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7510, 60min, 1967, 1976, 3267, yes)
*	`Fulford Harbour <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7330&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7330, 60min, 1952, 1992, 14493, yes)
*	`Maple Bay <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7315&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7315, 60min, 1969, 1970, 367, yes)
*	`Crofton <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7450&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7450, 60min, 1960, 1971, 485, no)
*	`Chemainus <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7455&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7455, 60min, 1961, 1961, 34, yes)
*	`Ladysmith <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7460&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7460, 60min, 1954, 1955, 402, yes)
*	`Preedy Harbour <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7471&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7471, 60min, 1961, 1961, 35, yes)
*	`North Galliano <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7435&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7435, 60min, 1972, 1972, 55, yes)
*	`Dionisio Point <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7535&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7535, 60min, 1963, 1968, 50, no)
*	`Valdes Island <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7542&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7542, 60min, 1963, 1963, 15, yes)

Nanaimo region:

*	`Nanaimo Harbour <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7917&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7917, 3min, 1997, 2003, 2259, yes)
*	`Hammond Bay <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7924&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7924, ??min, 1959, 1959, numdays)*
*	`Winchelsea Is. <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7935&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7935, 60min, 1967, 1978, 4159, yes)
*	`Nanoose Harbour <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7930&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7930, 60min, 1986, 1993, 2764, yes)
*	`Northwest Bay <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7938&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7938, 60min, 1967, 1968, 411, yes)
*	`Boat Harbour <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7480&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7480, 60min, 1972, 1972, 50, yes)
*	`Northumberland Channel <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7915&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7915, ??min, 1949, 1949, numdays)*
*	`Pylades Channel <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7442&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7442, 3min, 1999, 1999, 1, yes)
*	`Silva Bay <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7550&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7550, 60min, 1967, 2002, 873, no)

*	`Cowichan Bay <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7310&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7310, 60min, 1961, 1961, 39, yes)
*	`Piers Harbour <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7272&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7272, 60min, 1980, 1980, 212, yes)
*	`Swartz Bay <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7270&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7270, 60min, 1963, 1963, 29, yes)
*	`Tsehum Harbour <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7262&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7262, 60min, 1983, 1983, 62, yes)

Victoria region:

*	`Sidney <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7260&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7260, 60min, 1953, 2000, 936, no)
*	`Finlayson Arm <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7284&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7284, 60min, 1966, 1966, 102, yes)
*	`Finnerty Cove <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7140&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7140, 60min, 1967, 1975, 2829, yes)
*	`Oak Bay <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7130&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7130, 1min, 2001, 2004, 1017, no)
*	`Clover Point <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7115&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7115, 60min, 1967, 1967, 235, yes)
*	`Selkirk Water <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7121&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7121, 1min, 2011, 2011, 145, yes)
*	`Esquimalt Lagoon <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7107&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7107, 60min, 1972, 1984, 2569, no)
*	`Esquimalt Harbour <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7109&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7109, 60min, 1981, 2010, 1313, no)
*	`Esquimalt Government Hbr <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7110&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7110, 60min, 1972, 1978, 1604, no)
*	`Portage Inlet <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7125&user=isdm-gdsi&region=PAC>`_ (7125, ??min, year1, year2, numdays)
*	`Gorge at Craigflower <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7124&user=isdm-gdsi&region=PAC>`_ (7124, ??min, year1, year2, numdays)
*	`Gorge at Tillicum <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7123&user=isdm-gdsi&region=PAC>`_ (7123, ??min, year1, year2, numdays)
*	`Gorge at Aaron Point <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7122&user=isdm-gdsi&region=PAC>`_ (7122, ??min, year1, year2, numdays)
*	`Selkirk Water, <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7121&user=isdm-gdsi&region=PAC>`_ (7121, ??min, year1, year2, numdays)

Southern Vancouver Island

*	`Pedder Bay <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7080&user=isdm-gdsi&region=PAC>`_ (7080, 60min, 1967, 1969, 601, yes)
*	`Becher Bay <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7030&user=isdm-gdsi&region=PAC>`_ (7030, 60min, 1976, 1976, 56, yes)
*	`Twin Cove <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7022&user=isdm-gdsi&region=PAC>`_ (7022, 1min, 2011, 2011, 50, yes)
*	`Sooke Basin <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7024&user=isdm-gdsi&region=PAC>`_ (7024, 60min, 1977, 1983, 1077, no)
*	`Sooke <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7020&user=isdm-gdsi&region=PAC>`_ (7020, 60min, 1972, 1985, 4416, yes)

Washington:

*	`Ferndale, Wash. <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7564&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7564, 60min, 1967, 1970, 1087, yes)
*	`Patos Island <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7505&user=isdm-gdsi&region=PAC&ref=maps-cartes>`_ (7505, 60min, 1967, 1969, 422, no)
*	`Reservation Bay <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7196&user=isdm-gdsi&region=PAC>`_ (7196, 60min, 1971, 1971, 302, yes)
*	`Cornet Bay <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7193&user=isdm-gdsi&region=PAC>`_ (7193, 60min, 1971, 1971, 121, yes)
*	`Port Townsend <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7160&user=isdm-gdsi&region=PAC>`_ (7160, 60min, 1971, 1971, 121, yes)
*	`Meadowdale <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7182&user=isdm-gdsi&region=PAC>`_ (7182, 60min, 1971, 1971, 121, yes)
*	`Seattle <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7180&user=isdm-gdsi&region=PAC>`_ (7180, 60min, 1970, 1970, 365, yes)
*	`Port Angeles <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7060&user=isdm-gdsi&region=PAC>`_ (7060, 60min, 1973, 1973, 151, yes)
*	`Crescent Bay <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7050&user=isdm-gdsi&region=PAC>`_ (7050, 60min, 1964, 1964, 30, yes)
*	`Sekiu (Clallam Bay) <https://meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?no=7037&user=isdm-gdsi&region=PAC>`_ (7037, 60min, 1973, 1974, 370, yes)

West coast Vancouver Island (southern)

*	Port Renfrew  (8525, ??min, year1, year2, numdays)
*	Mutine Point  (8556, ??min, year1, year2, numdays)
*	Pocahontas Pt  (8560, ??min, year1, year2, numdays)
*	Chesnuknuw Cr  (8562, ??min, year1, year2, numdays)
*	Sproat Narrows  (8564, ??min, year1, year2, numdays)
*	Franklin River  (8565, ??min, year1, year2, numdays)
*	Stamp Narrows  (8570, ??min, year1, year2, numdays)
*	Port Alberni  (8575, ??min, year1, year2, numdays)
*	Brooksby Point  (8558, ??min, year1, year2, numdays)
*	Kildonan  (8557, ??min, year1, year2, numdays)
*	Head of Uchucklesit  (8559, ??min, year1, year2, numdays)
*	Effingham  (8585, ??min, year1, year2, numdays)
*	Walsh Island  (8586, ??min, year1, year2, numdays)
*	Ucluelet  (8595, ??min, year1, year2, numdays)
*	Kennedy Cover  (8623, ??min, year1, year2, numdays)
*	Warn Bay  (8626, ??min, year1, year2, numdays)
*	Cypress Bay  (8630, ??min, year1, year2, numdays)
*	Herbert Inlet  (8632, ??min, year1, year2, numdays)
*	Sulphur Passage  (8634, ??min, year1, year2, numdays)
*	Riley Cove  (8637, ??min, year1, year2, numdays)

West coast Vancouver Island (northern)

*	Gold River  (8650, ??min, year1, year2, numdays)
*	Saavedra Islands  (8645, ??min, year1, year2, numdays)
*	Esperanza  (8665, ??min, year1, year2, numdays)
*	Tahsis  (8658, ??min, year1, year2, numdays)
*	Zeballos  (8670, ??min, year1, year2, numdays)
*	Kyuquot  (8710, ??min, year1, year2, numdays)
*	Port Alice  (8750, ??min, year1, year2, numdays)
*	Bergh Cove  (8754, ??min, year1, year2, numdays)
*	Kwokwesta Creek  (8755, ??min, year1, year2, numdays)
*	Makwazniht I  (8756, ??min, year1, year2, numdays)
*	Coal Harbour  (8765, ??min, year1, year2, numdays)
*	Hunt Islet  (8736, ??min, year1, year2, numdays)
*	Cape Scott  (8790, ??min, year1, year2, numdays)

Howe Sound

*	Squamish Inner  (7811, ??min, 1961, 1962, 395)
*	Squamish  (7810, ??min, 1961, 1961, 30)
*	Latona Beach  (7805, ??min, 1973, 1973, 60)
*	Gibsons  (7820, ??min, 1967, 1973, numdays)

Sunshine Coast

*	Roberts Creek  (7824, ??min, 1968, 1969, numdays)
*	Porpoise Bay  (7852, ??min, 1951, 1952, numdays)
*	Halfmoon Bay  (7830, ??min, 1967, 1969, numdays)
*	Storm Bay  (7847, ??min, 1963, 1963, numdays)
*	Irvines Landing  (7836, ??min, 1967, 1974, numdays)
*	Egmont  (7842, ??min, year1, year2, numdays)
*	Saltery Bay  (7868, ??min, 1967, 1969, numdays)
*	Powell River  (7880, ??min, 1965, 1973, numdays)
*	Okeover Inlet  (8006, ??min, year1, year2, numdays)
*	Lund  (7885, ??min, year1, year2, numdays)
*	Prideaux Haven  (8008, ??min, year1, year2, numdays)

Mid Strait of Georgia

*	Squitty Bay  (7980, ??min, year1, year2, numdays)
*	Skerry Bay  (7985, ??min, year1, year2, numdays)
*	False Bay  (7982, ??min, year1, year2, numdays)
*	Welcome Bay  (7990, ??min, year1, year2, numdays)
*	Blubber Bay  (7875, ??min, year1, year2, numdays)
*	Hornby Island  (7953, ??min, year1, year2, numdays)
*	Denman Island  (7955, ??min, year1, year2, numdays)
*	Comox  (7965, ??min, year1, year2, numdays)
*	Little River  (7993, ??min, year1, year2, numdays)

Northern Strait of Georgia

*	Mitlenatch  (7895, ??min, year1, year2, numdays)
*	Twin Island  (7892, ??min, year1, year2, numdays)
*	Surge Narrows  (8045, ??min, year1, year2, numdays)
*	Florence Cove  (8055, ??min, year1, year2, numdays)
*	Octopus Island  (8050, ??min, year1, year2, numdays)
*	Owen Bay  (8120, ??min, year1, year2, numdays)
*	Okis Island  (8124, ??min, year1, year2, numdays)
*	Brown Bay  (8110, ??min, year1, year2, numdays)
*	Seymour Narrows  (8105, ??min, year1, year2, numdays)
*	Chatham Pt  (8180, ??min, year1, year2, numdays)
*	Hardinge Is  (8127, ??min, year1, year2, numdays)
*	Big Bay, Stuart Island  (8060, ??min, year1, year2, numdays)
*	Blind Channel  (8155, ??min, year1, year2, numdays)
*	Kelsey Bay (8215, 60min, 1988, 1993, 1825)
*	Yorke Island  (8233, 60min, 1943, 1944, 365)
*	Warren Islands  (8254, 60min, 1981, 1981, 35)
*	Cedar Island  (8325, ??min, year1, year2, numdays)
*	Alert Bay  (8280, ??min, year1, year2, numdays)
*	Sullivan Bay  (8364, ??min, year1, year2, numdays)
*	Stuart Narrows  (8379, ??min, year1, year2, numdays)
*	Drury Inlet  (8381, ??min, year1, year2, numdays)
*	Jennis Bay  (8384, ??min, year1, year2, numdays)
*	Frederick Sd  (8458, ??min, year1, year2, numdays)
*	Alison Sound  (8488, ??min, year1, year2, numdays)
*	Nugent Sound  (8464, ??min, year1, year2, numdays)
*	Charlotte Bay  (8443, ??min, year1, year2, numdays)
*	Mereworth Sound  (8476, ??min, year1, year2, numdays)
*	Egg Island  (8805, ??min, year1, year2, numdays)
*	Wadhams  (8840, ??min, year1, year2, numdays)

* = not digitised

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


NOAA measured data
-------------------------------

Harmonic constituents and up to 31 days of measured water level data at NOAA tide gauges in the US can be downloaded
`here <https://tidesandcurrents.noaa.gov/stations.html?type=Historic+Water+Levels>`_.

(remember that tidal heights are in feet!)

Tulare Beach, Port Susan 9448043
May 20 2013 - present

9447963 Tulalip Bay, WA
- present

Tulalip Bay 9447773
Apr 8 1935 - Nov 13 1974

Toke Point 9440910
Feb 20 1922 - present

Westport 9441102
Jun 7 1982 - Jul 23 1982

La Push 9442396
Dec 19 1924 - Dec 18 1969

Neah Bay 9443090
Jul 23 1934 - present

Port Angeles 9444090
Aug 30 1975 - present

Port Townsend 9444900
Dec 31 1971 - present

Tacoma 9446484
Jul 22 1996 - present

Seattle 9447130
Jan 1 1899 - present

Shilshole Bay Gps Buoy 9447214
Jan 1 2009 - present

Cherry Point 9449424
Nov 7 1971 - present

Friday Harbor 9449880
Jan 25 1932 - present

Cherry Point cp0101
(currents)
