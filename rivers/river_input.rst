.. _RiverInput:

River Input
===========

Sources
-------

River input provides a significant volume of freshwater to the Salish Sea and can influence stratification,
circulation and primary productivity.
We need to parametrise the rivers that flow into the Salish Sea throughout the domain.

Morrison et al. (2011) provides a method for estimating freshwater runoff in the Salish Sea region based on precipitation.
We acquired the exact data from Morrison, which includes the runoff volumes for each watershed for each year from 1970 to 2012,
as well as monthly averages. These data are saved in netcdf files in the :ref:`rivers-repo`.

Figure 1 of Morrison et al. (2011) shows the watershed boundaries but for more precise detail,
some of these boundaries coincide with watershed boundaries shown 
`on this ArcGIS map <https://www.arcgis.com/home/webmap/viewer.html?services=aeef4efc47e842a59ea11431fcffa2bd>`_.

Next,
we split the freshwater runoff from each watershed between the rivers in that watershed.
To do this, we needed accurate maps of the rivers in the region. The following sources were used:

* For BC, maps of rivers can be found on the Atlas of Canada - 
  `Toporama site <https://atlas.nrcan.gc.ca/toporama/en/index.html>`_.

* For BC, maps of watersheds used to be available at ``http://www.env.gov.bc.ca/fish/pdf/prov_wsgs.pdf``.

* In Washington, maps of watersheds are available `at this site <https://mywaterway.epa.gov/>`_.

* For the Fraser River, the split between the arms is given in Thomson, 1981,
  available `here <https://www.dfo-mpo.gc.ca/Library/487.pdf>`_

From the maps, the percentage of the watershed that each river drains was estimated.

Based on the values in Morrison et al. (2011), the approximate percentage of freshwater input from each watershed in our domain is given below:

* Fraser 44%
* Skagit 12%
* East Vancouver Is (North and South) 12%
* Howe 7%
* Bute 7%
* Puget 6%
* Juan de Fuca 5%
* Jervis 4%
* Toba 3%

Through the method described above, we have parameterised a total of 150 rivers in our domain.

Watersheds
----------

Fraser
^^^^^^

* Source used: BCCF map and WA map and Thomson, 1981 to split Fraser flow among its arms
* Includes three arms of the Fraser River (v. important to Salish Sea!) and some little US rivers south of the Fraser, shown in WRIA1
* Assume that the Fraser River itself occupies 98% of this watershed, and that WRIA1 occupies 2% of this watershed

* WRIA1

	* 20% of WRIA1 flows into the Fraser anyway
	* WRIA1 = Flux*(0.02*0.80)
	* Fraser = Flux - WRIA1
	* Dakota Creek occupies 6% of WRIA1, enters at 48.9684184, -122.7371679, i = 362, j = 357
	* Terrel Creek occupies 4% of WRAI1, enters at 48.9063480, -122.7649440, i = 351, j = 345
	* Nooksack River occupies 75% of WRIA1, enters at 48.7896486, -122.6667206, i = 321, j = 347
	* Squallum River occupies 5% of WRIA1, enters at 48.7602333, -122.5129287, i = 305, j = 365
	* Lake river thingo occupies 6% of WRIA1, i = 302, j = 367, enters at 48.7520645, -122.4882228
	* Chuckanut occupies 4% of WRIA1, enters at 48.7243762, -122.5068995, i = 298, j = 361

* Fraser River

	 * Main South Arm cells based on map are i=414,415,416 and j=334
	 * Assumed that 75% goes into the main South Arm (Thomson, 1981)
	 * Southern South Arm 5% (aka Canoe Pass, although that is not
	   open), cells i= 409,410 j=315 . This position was corrected
	   in November 2015.
	 * Main Arm 5%, cells i=434,435, j=318
	 * North Arm 15%, cells i=440, j=323,324

Skagit
^^^^^^

* Source used: WA map
* Includes sub-watersheds WRIA3, WRIA4, WRIA5 and WRIA7 from WA map
* WRIA4 represents 33% of Skagit Watershed

	* Baker River and lake drains 8% of WRIA4 but does not enter ocean (24.7)... but Morrison spreadsheet is for coastal runoff therefore assume Skagit River drains 100% of WRIA4 and then enters WRIA3
* WRIA3 represents 17% of Skagit Watershed

	* Skagit River drains 75% of WRIA3

		* assume 50% enters at 48.297470, -122.390614 (i = 207, j = 326)
		* assume 50% enters at 48.370163,-122.497387 (i = 229, j = 319)
	* Samish River drains 20% of WRIA3 (48.569395,-122.472496) (i = 265, j = 348)
	* Joe Leary Slough drains 5% of WRIA 3 (48.520811,-122.484426) (i = 257, j = 339)
* WRIA5 represents 17% of Skagit Watershed

	* Stillaguamish River drains 100% of WRIA5

		* assume 70% enters at 48.196188,-122.371902 (i = 186, j = 316)
		* assume 10% enters at 48.222042,-122.392502 (i = 192, j = 315)
		* assume 20% enters at 48.254513,-122.40263 (i = 200, j = 318)
* WRIA7 represents 33% of Skagit Watershed

	* Snohomish River drains 96% of WRIA7, Quilceda Creek drains 1% of WRIA7, Allen Creek drains 1% of WRIA7, all enter at 48.028853,-122.212429 (i = 143, j = 318)
	* Tulalip Creek drains 1% of WRIA7 (48.064343,-122.284926) (i = 154, j = 311)
	* Mission Creek drains 1% of WRIA7 (48.056656,-122.274742) (i = 152, j = 312)

EVI_N
^^^^^

* Source used: Toporama map at 1:100,000 (CA).  Areas (other than Oyster, Campbell, Sayward) were estimated from print outs of the graphs.  Large rivers areas are given in Environment Canada (EC) database. Note that not all of Morrison's EVI_N drains into our model.

Percent drained from each river was proportional to its drainage area versus the total drainage area given by Morrison.

Below, rivers marked with '+' means I made up the name.

 * Oyster River is at 705,122: area 363 km2 (according to EC) -
   corrected in November 2015 to move it off land

 * Qunisam River flows into Campbell River. #08HD003 is Campbell River at the town, drainage area 1470 km2 (EC) Latitude: 50.0353065 Longitude: -125.2629857, Lat/Lon of mouth -125.2601, 50.0510 for the mouth gives two points 123 749 and 750

  * Snowden Creek 139 km2 CA Latitude: 50.1125819 Longitude: -125.3723186 point 117 770

  * Menzies Creek 31 km2 CA  Latitude: 50.1383412 Longitude: -125.3908277 point  117 773

  * Creek 1+ 23 km2 CA  Latitude: 50.1952619 Longitude: -125.3825383 point 123 786

  * Creek 2+ 16 km2 CA  Latitude: 50.2333137 Longitude: -125.3975887 point 126 795

  * Creek 3 23 km2 CA  Latitude: 50.2435598 Longitude: -125.3991498 point 127 798
  * Elk Creek+ 23 km2 CA  Latitude: 50.2819399 Longitude: -125.4402655 point 127 807

  * Slab Creek+ 12 km2 CA  Latitude: 50.3063334 Longitude: -125.4381633 point 129 813

  * Pye Creek 109 km2 CA  Latitude: 50.336607 Longitude: -125.5188295 point 121 826

  * Bear Point Creek+ 12 km2 CA  Latitude: 50.3628639 Longitude: -125.6340551 point 107 839

  * Amor de Cosmos Creek 229 km2 CA Latitude: 50.3582412 Longitude: -125.6876354 point 96 843

  * Humpback+ 10 km2 CA Latitude: 50.3561671 Longitude: -125.7174369 point 93 844

  * Palmer+ 14 km2 CA Latitude: 50.3603414 Longitude: -125.7371761 point 92 845

  * Hkusam+ 14 km2 CA Latitude: 50.3624995 Longitude: -125.7693388  point 87 848

  * Camp Point South+ 14 km2 CA Latitude: 50.3815933 Longitude: -125.8429452 point 77, 858

  * Camp Point North+ 14 km2 CA Latitude: 50.3807285 Longitude: -125.8540664 point 78, 858 so done together with Camp Point South

  * Salmon River drainage area 1210 km2 (EC), #08HD006 is Salmon River near Sayward,   Latitude: 50.3930713 Longitude: -125.9514349 points 64 866-867

  * Sayward+ 14 km2 CA Latitude: 50.388379 Longitude: -125.9592292 point 64, 866

  * Kelsey+ 10 km2 CA Latitude: 50.4122688 Longitude: -125.9864134 point 62 872

  * double rivers Communication+ 7 km2 CA Latitude: 50.4253357 Longitude: -126.0181504 & Latitude: 50.4292907 Longitude: -126.0299097  points 59, 877 and 58, 879  put both in at 59, 878

  * unmarked+ 7 km2 CA Latitude: 50.4427467 Longitude: -126.0688793 point 54 884

  * Newcastle+ 34 km2 CA Latitude: 50.4526841 Longitude: -126.1194916 point 47 890

  * Windy+ 10 km2 CA Latitude: 50.4552649 Longitude: -126.1585149 point 42 893


Howe
^^^^

* Source used: BCCF information to determine amount coming from Burrard Inlet.  This is a region we could return to using the Toporama maps.
* Squamish River is 90% of watershed (i = 532 and j=385, j=386)
* Burrard Inlet is 10% of watershed (i=457-459 and j=343)

Bute
^^^^

* Source used: Numerous sources on major rivers in the region including wikipedia entries, tourist agencies etc.  This is a region we could return to using the Toporama maps.
* Assume Homathko is 58% of watershed (i=897,j=294)
* Assume Southgate is 35% of watershed (i=885,j=296-297)
* Assume Orford is 7% of watershed (i=831, j=249)

Puget
^^^^^

* Source used: WA map
* Includes sub-watersheds WRIA17, WRIA16, WRIA15, WRIA14, WRIA08, WRIA09, WRIA10, WRIA12 and WRIA11 from WA map
* WRIA17 10% of Puget Sound Watershed

	* Johnson 5% of WRIA17, 48.061231,-123.039665, i = 207, j = 202
	* Jimmycomelately 5% of WRIA17, 48.028911,-123.004131, i = 199, j = 202
	* Salmon and Snow 25% of WRIA17, 47.997331,-122.873926, i = 182, j = 219
	* Chimacum 20% of WRIA17, 48.048939, -122.769771, i = 185, j = 240
	* Thorndike 5% of WRIA17, 47.808831,-122.739944, i = 137, j = 215
	* Torboo 5% of WRIA17, 47.843407,-122.812986, i = 149, j = 208
	* Little Quilcene/Big Quilcene 35% of WRIA17, 47.813846,-122.854614, i = 146, j = 199

* WRIA16 10% of Puget Sound Watershed

	* Dosewalips 20% of WRIA16, 47.681628,-122.893496, i = 124, j = 177
	* Duckabush 14% of WRIA16, 47.645094,-122.92973, i = 119, j = 167
	* Fulton 2% of WRIA16, 47.616376,-122.973876, i = 116, j = 156
	* Waketick 2% of WRIA16, 47.557241,-123.023751, i = 108, j = 141
	* Hamma Hamma 14% of WRIA16, 47.548001,-123.038936, i = 107, j = 139
	* Jorsted 2% of WRIA16, 47.527069,-123.049386, i = 104, j = 135
	* Eagle 2% of WRIA16, 47.484004,-123.076165, i = 98, j = 127
	* Lilliwaup 2% of WRIA16, 47.462407,-123.113351, i = 95, j = 118
	* Finch 2% of WRIA16, 47.406308,-123.138102, i = 87, j = 108
	* Skokomish 40% of WRIA16, 47.345802,-123.121719, i = 75, j = 103

* WRIA15 15% of Puget Sound Watershed

	* Rendsland 2.5% of WRIA15, 47.385624,-123.114982, i = 81, j = 107
	* Tahuya 20% of WRIA15, 47.36842,-123.052325, i = 72, j = 114
	* Mission 5% of WRIA15, 47.428697,-122.873712, i = 73, j = 149
	* Union 10% of WRIA15, 47.437899, -122.854443, i = 74, j = 153
	* Coulter 5% of WRIA15, 47.400179,-122.821827, i = 64, j = 153
	* Minter 5% of WRIA15, 47.358072,-122.690935, i = 46, j = 168
	* Butley 5% of WRIA15, 47.380568,-122.633307, i = 47, j = 178
	* Olalla 5% of WRIA15, 47.42125,-122.54071, i = 48, j = 197
	* Blackjack 5% of WRIA15, 47.545278,-122.627292
	* Clear 5% of WRIA15, 47.64735,-122.686901
	* Barker 2.5% of WRIA15, 47.636998,-122.674971
	* Big Valley 10% of WRIA15, 47.736812,-122.653127
	* Assume 50% of Blackjack+Clear+Barker+BigValley enters Puget Sound at i = 68, j = 210
	* Assume 50% of Blackjack+Clear+Barker+BigValley enters Puget Sound at 47.724083,-122.551725 i = 108, j = 232
	* Big Bear 5% of WRIA15, 47.657482,-122.785542, i = 112, j = 189
	* Swaback 2.5% of WRIA15, 47.638589,-122.835217, i = 112, j = 182
	* Stavis 2.5% of WRIA15, 47.632595,-122.868519, i = 113, j = 174
	* Anderson 5% of WRIA15, 47.567261,-122.97143, i = 107, j = 150
	* Dewatta 5% of WRIA15, 47.452208,-123.058977, i = 94, j = 122

* WRIA14 5% of Puget Sound Watershed

	* Sherwood 15% of WRIA14, 47.378098,-122.828994, i = 60, j = 149
	* Deer 10% of WRIA14, 47.250193,-123.026683
	* Johns 10% of WRIA14, 47.243843,-123.043656
	* Goldborough 15% of WRIA14, 47.210765,-123.089018
	* Mill 15% of WRIA14, 47.19779,-122.99336
	* Skookum 10% of WRIA14, 47.136374,-123.075929
	* Kennedy 10% of WRIA14, 47.057873,-123.006234
	* Schneider 5% of WRIA14, 47.057932,-122.998338
	* Perry 10% of WRIA14,
	* 50% of Deer+Johns+Goldborough+Mill+Skookum+Kennedy+Schneider enter Puget Sound at 47.289476,-122.894711,i = 47, j = 130
	* 50% of Deer+Johns+Goldborough+Mill+Skookum+Kennedy+Schneider +100% of Perry enter Puget Sound at 47.166609,-122.861266, i = 20, j = 120

* WRIA13 3% of Puget Sound Watershed

	* McClane 10% of WRIA13
	* Deschutes 70% of WRIA13
	* Woodward 10% of WRIA13
	* Woodland 10% of WRIA13
	* Assume McClane+Deschutes+Woodward+Woodland enter Puget Sound at 47.182713,-122.83659, i = 22, j = 121

* WRIA12 2% of Puget Sound Watershed

	* Chambers 100% of WRIA12 47.187438,-122.584419, i = 6, j = 162

* WRIA11 15% of Puget Sound Watershed

	* Nisqually 99.5% of WRIA11 47.099227,-122.701149
	* McAllister 0.5% of WRIA11 47.098233,-122.723994
	* Assume Nisqually+McAllister enter Puget Sound at i = 0, j = 137

* WRIA10 20% of Puget Sound Watershed

	* Puyallup 99.5% of WRIA10 47.269678,-122.428036, i = 10, j = 195
	* Hylebas 0.5% of WRIA10 47.284935,-122.410011, i = 13, j = 199

* WRIA9 10% of Puget Sound Watershed

	* Duwamish 100% of WRIA9, 50% 47.586831,-122.361259, (i = 68, j = 243) 50% 47.592099,-122.344866 (i = 68, j = 246)

* WRIA8 10% of Puget Sound Watershed

	* Cedar/Sammamish 100% of WRIA8, 47.672894,-122.409207, i = 88, j = 246

JdF
^^^

* Source used: BCCF map and Toporama map
* The Juan de Fuca watershed in Morrison et al (2011) includes the north side of Juan de Fuca Strait from Victoria to Port Renfrew (inclusive) and the south side of Juan de Fuca Strait from Cape Flattery to Port Townsend.
* Assume that 50% of the area of the JdF watershed defined by Morrison et al (2011) is on north side of JdF (Canada side):

	* From BCCF map, assume  33% of Canada side is part of San Juan River/Harris Creek watershed

		* San Juan River (in the steelhead map) (includes Harris Creek from the steelhead map) 48.560449,-124.404595 (i = 402, j = 56)
	* Assume that 14% of Canada side is in the Gordon River Watershed 48.575897,-124.415281 (i = 403, j = 56)
	* Assume that 20% of Canada side is in Muir/Loss/Tugwell/Jordan

		* Loss Creek  (5% of Canada side) 48.480062,-124.27331 (i = 375, j = 71)
		* River Jordan (5% of Canada side) 48.421255,-124.056244 (i = 348, j = 96)
		* Muir Creek (5% of Canada side) 48.378744,-123.867352 (i = 326, j = 119)
		* Tugwell Creek (5% of Canada side) 48.375024, -123.853737 (i = 325, j = 120)
	* Assume that 33% of Canada side is in Sooke River Watershed 48.383846,-123.700011 (i = 308, j = 137)

* Assume that 50% of the area of the watershed defined by Morrison et al (2011) is on south side of JdF (US side)

	* Assume that 60% of US side of JdF is occupied by watershed WRIA 18, two main rivers Elwha and Dungeness

		* Elwha River 48.14616,-123.567095 (50% of watershed WRIA 18) 48.148193,-123.565807 (i = 261, j = 134)
		* Tumwater Creek (1% of watershed WRIA 18) 48.124708,-123.445626 (i = 248, j = 151)
		* Valley Creek (1% of watershed WRIA 18) 48.122445,-123.437018 (i = 247, j = 152)
		* Ennis Creek (2% of watershed WRIA 18) 48.117202,-123.405132 (i = 244, j = 156)
		* Morse Creek (7% of watershed WRIA 18) 48.117861,-123.354084 (i = 240, j = 164)
		* Bagley Creek (2% of watershed WRIA 18) 48.114344,-123.340791 (i = 239, j = 165)
		* Siebert Creek (2% of watershed WRIA 18) 48.120669,-123.289497 (i = 235, j = 174)
		* McDonald Creek (3% of watershed WRIA 18) 48.12561,-123.220167 (i = 233, j = 183)
		* Matriotti Creek (2% of watershed WRIA 18) Dungeness River (30% of watershed) are at same grid point 48.154520, -123.130217 (i = 231, j = 201)

	* Assume that 40% of US side of JdF is occupied by watershed WRIA 19 Lyre-Hoko

		* Coville Creek (5% of watershed WRIA 19) 48.138342,-123.611684 (i = 263, j = 128)
		* Salt Creek (5% of watershed WRIA 19) 48.16328,-123.70481 (i = 275, j = 116)
		* Field Creek (5% of watershed WRIA 19) 48.154406,-123.810554 (i = 281, j = 100)
		* Lyre River (20% of watershed WRIA 19) at 48.160675, -123.828499 (i = 283, j = 98)
		* East Twin River/West Twin River (5% of watershed WRIA 19) 48.165957,-123.949835 (i = 293, j = 81)
		* Deep Creek (5% of watershed WRIA 19) 48.175316,-124.026289 (i = 299, j = 72)
		* Pysht River (10% of watershed WRIA 19) 48.204541,-124.095984 (i = 310, j = 65)
		* Clallom River (10% of watershed WRIA 19) 48.254713,-124.267824 (i = 333, j = 45)
		* Hoko River (20% of watershed WRIA 19) 48.287419, -124.362191 (i = 345, j = 35)
		* Sekiu River (10% of watershed WRIA 19) 48.288676,-124.394159 (i = 348, j = 31)
		* Sail River (5% of watershed WRIA 19) 48.360327,-124.556508 (i = 373, j = 17)

EVI_S
^^^^^

* Source used: BCCF map and fluxes.  Could return here using the Toporama maps.
* Total flux (according to Morrison is 329.5 m3/s.  Adding fluxes and areas to estimate fluxes as given below gave 292.6 so values were multiplied up to make the difference

* Koksilah flows into Cowichan (9.77 m3/s)
* Cowichan (55 m3/s + Koksilah) gives 22% of watershed (i=383, j=201,202)
* Chemanius 19.2 m3/s and by area, north and south of Chemanius another 1/2.  Mouths are split:
      o Chemanius1 6.5% of watershed (i=414, j=211)
      o Chemanius2, 6.5% of watershedof watershed (i=417, j=212)
* Nanaimo 39.7 m3/s.  Mouths are split:
      o Nanaimo1, 9.4% of watershedof watershed (i=478, j=208, 209)
      o Nanaimo2, 4.6% of watershedof watershed (i=477, j=210)
* NorNanaimo, North of Naniamo and area of 1/2 Little Qualicum gives 2% of watershed (i=491-493, j=213)
* Goldstream, 2.2 m3/s gives 8% of watershed (i=334, j=185)
* Nanoose, area of 1/2 Little Qualicum gives 2% of watershed (i=518, j=185)
* Englishman, 14 m3/s gives 5% of watershed (i=541, j=175)
* FrenchCreek, area of 1/2 of Qualicum gives 1% of watershed (i=551, j=168)
* LittleQualicum, 11.8 m3/s plus 1/2 of Qualicum in nearby area gives 5% of watershed (i=563, j=150)
* Qualicum, 7.3 m3/s gives 2% of watershed (i=578, j=137)
* SouthDenman, about the size of Tsable + Qualicum gives 5% of watershed (i=602, j=120)
* Tsable, 7.99 m3/s but double for surrounding region 3% of watershed (i=616-617, j=120)
* Trent, 3 m3/s gives 1% of watershed (i=648, j=121)
* Puntledge, 42 m3/s gives 14% of watershed (i=656, j=119-120)
* BlackCreek, 1.8 m3/s plus area of Qualicum gives 3% of watershed (i=701, j=123)

Jervis
^^^^^^

* Source used: Toporama map,
* See `this site <https://www.pac.dfo-mpo.gc.ca/science/oceans/bc-inlets-mer-de-bras-cb/index-eng.html>`_
* As there were no gauged rivers in the Jervis Inlet watershed, Trites (1955) estimated the freshwater discharge using  the area of the watershed (~1400 km2) and local precipitation data.   The estimated mean annual discharge of 180 m3 s-1 is considerably smaller than the discharge in most of the longer BC inlets.  Unlike many of the BC inlets where the main river enters at the head, there are many small rivers and streams distributed along the shores of Jervis Inlet.  The runoff cycle for Jervis Inlet more closely follows the local precipitation cycle as the area of snow fields which store winter precipitation is relatively small (Macdonald and Murray 1973).
* Pickard (1961) (`https://doi.org/10.1139/f61-062_`): The chief difference between these inlets is that Jervis has less than 40% as much river runoff as Bute, and only one-quarter of this comes in at the head whereas in Bute three-quarters of the total enters at the head. The flushing effect of the large runoff into the head of Bute is expected to be greater on the whole of the inlet length than that of the smaller runoff distributed along the length of Jervis.

.. _https://doi.org/10.1139/f61-062: https://nrc-prod.literatumonline.com/doi/10.1139/f61-062

* Flow out of Powell Lake taken from Sanderson et al. (1986)
* Jervis Inlet only area = 1400km2 (Trites 1955) ==> 25% of Jervis watershed (5785km2)
* Assume Skwawka/Hunaechin/Lausmann/Slane/Smanit/ account for 30% of Jervis only watershed
* Assume Loquilts accounts for 4% of Jervis only, enters at 50.204868,-123.77326 (ish) i = 650, j = 318
* Assume Potato Creek accounts for 4% of Jervis only, enters at 50.154741,-123.837075
* Assume Deserted River accounts for 10% of Jervis only, enters at 50.0922,-123.745022
* Assume Stakawus Creek accounts for 4% of Jervis only, enters at 50.074273,-123.776457
* Crabapple Creek accounts for 4% of Jervis only, enters at 50.1207422, -123.8436382
* Osgood Creek accounts for 4% of Jervis only, enters at 50.0371886, -123.8964722
* Skwawka/Hunaechin/Lausmann/Slane/Smanit/Loquilts/Potato/Deserted/Crabapple/Stakawus/Osgood all enter
* domain at the same point 50.0894746,-123.7828011, i = 650, j = 309 -
  moved off land in November 2015
* Glacial Creek accounts for 5% of Jervis only, enters at 50.0062107,
  -123.9070838, i = 649, j = 310 - moved off land in November 2015
* Seshal Creek accounts for 5% of Jervis only, enters at 50.0246890,
  -123.9260495, i = 651, j = 307 - moved off land in November 2015
* Brittain River/Treth Creek accounts for 10% of Jervis only, enters at 49.9958119. -124.0119219, i = 650, j = 301
* Assume Vancouver River/High Creek accounts for 10% of Jervis only and  enter at 49.9219882, -123.8696986, i = 626, j = 311
* Assume Perketts Creek accounts for 5% of Jervis only and enters at 49.8799903, -123.8681308, i = 619, j = 307
* Assume Treat Creek accounts for 5% of Jervis only and enters at 49.8423159, -123.8742022, i = 612, j = 301
* Sechelt is about 66% of Jervis Inlet, based on values in Table II of Pickard (1961) (110m3/s / 180m3/s) ==> 17% of Jervis watershed
* Sechelt Inlet isn't in the domain, assume the input enters at 49.770844,-123.955708, i = 604, j = 280
* Outflow from Powell Lake is 3e9m3/year (Sanderson et al 1986) ==> 32% of Jervis watershed, enters at 49.874421,-124.565288, i = 666, j = 202
* From Section 4 of this report ``http://www.powellriverrd.bc.ca/wp-content/uploads/2011/09/Community-Profile.pdf`` which is no longer available on the web.
* Lois Lake drains 45,000ha = 450km2 ==> 8% of Jervis watershed...make it 10% to account for little rivers nearby, enters at  49.771481, -124.332197, i = 629, j = 224
* From Section 4 of this report ``http://www.powellriverrd.bc.ca/wp-content/uploads/2011/09/Community-Profile.pdf`` which is no longer available on the web.
* Haslam Lake drains 13,140ha = 131km2 ==> 2% of Jervis watershed, enters at 49.77356,-124.367173, i = 632, j = 219
* estimate Chapman Creek drains about 2% of the catchment, enters at 49.4381655, -123.7229658, i = 522, j = 273
* estimate Lapan Creek drains about 2% of the catchment, enters at 49.8368204, -123.9942065, i = 619, j = 282
* estimate Nelson Island represents 2% of the catchment and this drains from West Lake, into the domain at 49.7350557, -124.0575565, i = 599, j = 257
* estimate Wakefield Creek represents 2% of the catchment, into the domain at 49.4673394, -123.8048516, i = 533, j = 263
* estimate Halfmoon Creek represents 2% of the catchment, into the domain at 49.5103863, -123.9119698, i = 549, j = 253
* estimate Myers/Kleindale/Anderson represent 4% of catchment, into the domain at 49.6340820, -123.9952235, i = 571, j = 248

Toba
^^^^

* Source used: Toporama Maps
* Toba River 50% at 50.492 124.365
* Theodosia River 12% at 50.080 124.66
* Quatam River 9% at 50.380 124.942
* Brem River 9% at 50.435 124.663
* Tahumming River 8% at 50.493 124.387
* Racine Creek (with neighbours) 4% at 50.399 124.555
* Homfray Creek (wn) 3% at 50.293 124.635
* Forbes Creek wn 3% at 50.242 124.591
* Chusan Creek wn 2% at 50.473 124.381


Temperature
--------------

Temperature records are available for a number of the Rivers but at
this point we are only using the record from Hope (originally from
Water Office data, as compiled by Allen and Wolfe, 2013).


References
----------

* Allen and Wolfe, 2013. Hindcast of the timing of the spring phytoplankton bloom in the Strait of Georgia, 1968-2010. Progress in Oceanography, 115, 6-13.
* Environment and Climate Change Canada Water Office. https://wateroffice.ec.gc.ca
* J. Morrison , M. G. G. Foreman and D. Masson, 2012. A method for estimating monthly freshwater discharge affecting British Columbia coastal waters, Atmosphere-Ocean, 50:1, 1-8, DOI: `10.1080/07055900.2011.637667`_
* Sanderson et al (1986)
* Thomson, 1982: Physical Oceanography of the BC Coast.

.. _10.1080/07055900.2011.637667: https://dx.doi.org/10.1080/07055900.2011.637667

