.. _Sequential Mode:

***********************************************
Sequential Mode
***********************************************

Until now, we have entered only one input file into Ariane. Use Ariane's sequential mode to enter multiple files.

Input Files
===========
The NetCDF files used as input must have the following format:
*prefix - number - suffix*

If the file names do not follow this format, create symbolic links that do. Create this link by using the command :kbd:`ln -s [target file directory] [symbolic link name]`

For example, you may consider:

* *prefix* = **SalishSea_**
* *number* = **01**, **02**, **etc**
* *suffix* = **_grid_T.nc**, **_grid_U.nc**, **_grid_V.nc**

Note: *number* must contain a constant digit number and its value must increase by one in chronological order. For example, file **SalishSea_01_grid_T.nc** contains tracers for November 1st and **SalishSea_02_grid_T.nc** contains tracers for November 2nd.
