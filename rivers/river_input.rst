River input 
======================================================================================================

Sources
---------------------------------

River input provides a significant volume of freshwater to the Salish Sea and can influence stratification, circulation and primary productivity. We need to parametrise the rivers that flow into the Salish Sea throughout the domain. 

Morrison et al. (2011) provides a method for estimating freshwater runoff in the Salish Sea region based on precipitation. We acquired the exact data from Morrison, which includes the runoff volumes for each watershed for each year from 1970 to 2012, as well as monthly averages. These data are saved in netcdf files in the nemo-forcing/rivers directory.

Figure 1 of Morrison et al. (2011) shows the watershed boundaries but for more precise detail, some of these boundaries coincide with watershed boundaries shown `on this ArcGIS map <http://www.arcgis.com/home/webmap/viewer.html?services=aeef4efc47e842a59ea11431fcffa2bd>`_.

Next, we split the freshwater runoff from each watershed between the rivers in that watershed. To do this, we needed accurate maps of the rivers in the region. The following sources were used: 

* For BC, maps of rivers can be found on the Atlas of Canada - `Toporama site <http://atlas.nrcan.gc.ca/site/english/toporama/index.html>`_.

* For some areas of BC, maps of watersheds that include rivers where wild steelhead live are available from the `BCCF <http://www.bccf.com/steelhead/watersheds.htm>`_ .

* In Washington, maps of watersheds are available `here <http://www.ecy.wa.gov/apps/watersheds/wriapages/>`_.

From these maps, the percentage of the watershed that each river drains was estimated.

Watersheds
--------------------------------------

Fraser

* Source used: BCCF map and WA map
* Includes three arms of the Fraser River (v. important to Salish Sea!) and some little US rivers south of the Fraser, shown in WRIA1

Skagit

* Source used: WA map
* Includes subwatersheds WRIA3, WRIA4, WRIA5 and WRIA7 from WA map

EVI_N

* Source used: Toporama map

Howe

* Source used: 

Bute

* Source used: 

Puget

* Source used: WA map
* Includes subwatersheds WRIA17, WRIA16, WRIA15, WRIA14, WRIA08, WRIA09, WRIA10, WRIA12 and WRIA11 from WA map

JdF

* Source used: BCCF map and Toporama map
* The Juan de Fuca watershed in Morrison et al (2011) includes the north side of Juan de Fuca Strait from Victoria to Port Renfrew (inclusive) and the south side of Juan de Fuca Strait from Cape Flattery to Port Townsend.

EVI_S

* Source used: 

Jervis

* Source used: Toporama map, 
* See `this site <http://www.pac.dfo-mpo.gc.ca/science/oceans/BCinlets/jervis-eng.htm>`_
* As there were no gauged rivers in the Jervis Inlet watershed, Trites (1955) estimated the freshwater discharge using  the area of the watershed (~1400 km2) and local precipitation data.   The estimated mean annual discharge of 180 m3 s-1 is considerably smaller than the discharge in most of the longer BC inlets.  Unlike many of the BC inlets where the main river enters at the head, there are many small rivers and streams distributed along the shores of Jervis Inlet.  The runoff cycle for Jervis Inlet more closely follows the local precipitation cycle as the area of snow fields which store winter precipitation is relatively small (Macdonald and Murray 1973). 
* Pickard (1961) (http://www.nrcresearchpress.com/doi/pdf/10.1139/f61-062): The chief difference between these inlets is that Jervis has less than 40% as much river runoff as Bute, and only one-quarter of this comes in at the head whereas in Bute three-quarters of the total enters at the head. The flushing effect of the large runoff into the head of Bute is expected to be greater on the whole of the inlet length thzrn that of the smaller runoff distributed along the length of Jervis.
* Flow out of Powell Lake taken from Sanderson et al. (1986)

Toba

* Source used: 

Creating input files for NEMO
-------------------------------------------------

The grid point of the location of each river mouth was found. The ipython notebook 'AddRivers' creates a NetCDF files containing the river flow at the respective grid cell for each river throughout the domain. Where the river mouth was not included in the domain, the river was added to the closest grid point to the river mouth. 

In some cases (e.g. the end of Jervis inlet, Puget Sound) numerous rivers were not included in the domain, so the sum of all the omitted rivers' flow was added to the closest grid point.

References
-------------------------------

* J. Morrison , M. G. G. Foreman and D. Masson, 2012. A method for estimating monthly freshwater discharge affecting British Columbia coastal waters, Atmosphere-Ocean, 50:1, 1-8, DOI: 10.1080/07055900.2011.637667
* Sanderson et al (1986)

