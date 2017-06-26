**************************************
netCDF4 Files Creation and Conventions
**************************************

The Salish Sea MEOPAR project uses netCDF4_ files as input for the NEMO model and for other purposes,
where appropriate.
The :ref:`netCDF4FilesCreationAndConventions` section in the :ref:`project tools documentation <SalishSeaToolsDocs>` documents the recommended way of creating netCDF4 files with compression of varibles,
limitation of variables to appropriate precision,
and appropriate metadata attributes for the variables and the dataset as a whole.
The recommendations are based on the `NetCDF Climate and Forecast (CF) Metadata Conventions, Version 1.6, 5 December, 2011`_.
Use of the `netCDF4-python`_ library
(included in :ref:`AnacondaPythonDistro`)
is assumed.

.. _netCDF4: https://www.unidata.ucar.edu/software/netcdf/
.. _NetCDF Climate and Forecast (CF) Metadata Conventions, Version 1.6, 5 December, 2011: http://cfconventions.org/cf-conventions/v1.6.0/cf-conventions.pdf
.. _netCDF4-python: https://unidata.github.io/netcdf4-python/

The :ref:`salishsea_tools.nc_tools` in the :ref:`SalishSeaToolsPackage` is a library of Python functions for exploring and managing the attributes of netCDF files.
