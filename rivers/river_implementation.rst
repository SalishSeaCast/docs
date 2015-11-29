.. _RiverImplementation:

River Implementation
=========================

.. figure:: ../_static/rivers.png

Creating input files for NEMO
-----------------------------

The grid point of the location of each river mouth was found. A large dictionary of the grid points and flow mapping is the `rivertools.py`_ script. The ipython notebook `Add Rivers Month and Constant.ipynb`_ creates a NetCDF files containing the river flow at the respective grid cell for each river throughout the domain. Where the river mouth was not included in the domain, the river was added to the closest grid point to the river mouth. 

.. _rivertools.py: https://bitbucket.org/salishsea/tools/src/tip/SalishSeaTools/salishsea_tools/rivertools.py
.. _Add Rivers Month and Constant.ipynb: https://bitbucket.org/salishsea/tools/src/tip/I_ForcingFiles/Rivers/Add%20Rivers%20Month%20and%20Constant.ipynb

In some cases (e.g. the end of Jervis inlet, Puget Sound) numerous rivers were not included in the domain, so the sum of all the omitted rivers' flow was added to the closest grid point.

Everywhere the depth of freshwater is set to 3 m so that the freshwater is distributed over the top 3 m at the specified grid cell.

Everywhere the temperature of the freshwater is set to the temperature of the Fraser at Hope.

Two files are created `rivers_month.nc`_ and `rivers_const.nc`_.  The first contains the monthly climatology, the second the yearly average.

.. _rivers_month.nc: https://bitbucket.org/salishsea/nemo-forcing/src/tip/rivers/rivers_month.nc
.. _rivers_const.nc: https://bitbucket.org/salishsea/nemo-forcing/src/tip/rivers/rivers_const.nc

Nowcast Files
----------------

For the nowcast runs, the Fraser River is split into the amount that enters at Hope and amount downstream of that.  The amount at Hope is taken from the published measurements (Environment Canada, WaterOffice via `data mart`_) and distributed appropriately to the Fraser River mouths.  All other freshwater is set to climatology. 

.. _data mart: http://dd.meteo.gc.ca/about_dd_apropos.txt
