.. _Tracers with Ariane:

***********************************************
Tracers with Ariane
***********************************************
In addition to a particle's trajectory (longitude and latitude) and depth, Ariane can also help us analyze tracers along the particle's trajectory.

* Temperature
* Salinity
* Density

We will be making changes in :kbd:`namelist`.

Namelist: Sections
===================
Ariane requires both salinity and temperature data as input. It also requires density data or an indication that density should instead be calculated using salinity and temperature.

Our model produces files with filenames that follow this format: *SalishSea_t_yyyymmdd_yyyymmdd_grid_T.nc*. These files contain salinity and temperature. Therefore, Ariane will be calculating density.

Temperature
------------

Salinity
^^^^^^^^^


Namelist: Parameters
===================
