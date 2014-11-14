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



Namelist: Modify Sections
=========================

First, let's take a closer look at the parameteres in the namelist sections. The parameter names are :kbd:`c_dir_X`, :kbd:`c_prefix_X`, :kbd:`ind0_X`, :kbd:`indn_X`, :kbd:`maxsize_X`, and :kbd:`c_suffix_X` where :kbd:`X` is :kbd:`zo`, :kbd:`me`, :kbd:`te`, :kbd:`sa` for the **ZONALCRT**, **MERIDCRT**, **TEMPERAT**, and **SALINITY** sections, respectively.

Input File Directory
--------------------


New Input Filename
------------------


Sequential Parameter
--------------------



Namelist: Add Section
======================

Sequential
----------


Notebooks
=========
* `Ariane_Sequential.ipynb`_

.. _Ariane_Sequential.ipynb: http://nbviewer.ipython.org/urls/bitbucket.org/salishsea/analysis/raw/tip/Idalia/Ariane_Sequential.ipynb

