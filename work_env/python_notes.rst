.. _Python_netCDF_Visualization:

***********************************************
Python, netCDF, and Model Results Visualization
***********************************************

Intro to Python
===============

Please see the :ref:`moaddocs:MOAD-Python` section of the :ref:`UBC-EOAS-MOAD-docs`.


Jupyter Notebook, netCDF, and Model Results
===========================================

We have an ongoing project to develop a collection of
`Jupyter Notebooks <https://jupyter.org/>`_ that provide discussion,
examples,
and best practices for plotting various kinds of model results from `netCDF`_ files.
There are code examples in the notebooks and also examples of the use of functions from the :ref:`SalishSeaToolsPackage`.

.. _netCDF: https://unidata.github.io/netcdf4-python/

If you are new to the Salish Sea project,
or to `Jupyter Notebook <https://jupyter.org/>`,
netCDF_,
and matplotlib_ you should read the notebooks in the following order:


* `Exploring netCDF Files.ipynb`_
* `Plotting Bathymetry Colour Meshes.ipynb`_
* `Plotting Tracers on Horizontal Planes.ipynb`_
* `Plotting Velocity Fields on Horizontal Planes.ipynb`_
* `Plotting Velocities and Tracers on Vertical Planes.ipynb`_

.. _matplotlib: https://matplotlib.org/

.. _Exploring netCDF Files.ipynb: https://nbviewer.org/github/SalishSeaCast/tools/blob/main/analysis_tools/Exploring%20netCDF%20Files.ipynb

.. _Plotting Bathymetry Colour Meshes.ipynb: https://nbviewer.org/github/SalishSeaCast/tools/blob/main/analysis_tools/Plotting%20Bathymetry%20Colour%20Meshes.ipynb

.. _Plotting Tracers on Horizontal Planes.ipynb: https://nbviewer.org/github/SalishSeaCast/tools/blob/main/analysis_tools/Plotting%20Tracers%20on%20Horizontal%20Planes.ipynb

.. _Plotting Velocity Fields on Horizontal Planes.ipynb: https://nbviewer.org/github/SalishSeaCast/tools/blob/main/analysis_tools/Plotting%20Velocity%20Fields%20on%20Horizontal%20Planes.ipynb

.. _Plotting Velocities and Tracers on Vertical Planes.ipynb: https://nbviewer.org/github/SalishSeaCast/tools/blob/main/analysis_tools/Plotting%20Velocities%20and%20Tracers%20on%20Vertical%20Planes.ipynb

The links here are to static renderings of the notebooks via
`nbviewer.org`_ .
The notebook source files are in the `analysis_tools`_ directory of the :ref:`tools-repo` repo.

.. _nbviewer.org: https://nbviewer.org/
.. _analysis_tools: https://github.com/SalishSeaCast/tools/tree/main/analysis_tools


ERDDAP and :kbd:`xarray`
------------------------

From late-2013 until early-2016 we used the `netCDF4-python`_ library to open locally stored files.
The notebooks above describe that way of working.
In early-2016 we set up an `ERDDAP server`_  to provide public access to our model results.
The `netCDF4-python`_ library can open datasets from ERDDAP URLs just as easily as it can open them from local files.
So,
here is a reworking of the `Exploring netCDF Files.ipynb`_ notebook using ERDDAP:

* `Exploring netCDF Datasets from ERDDAP.ipynb`_

.. _netCDF4-python: https://unidata.github.io/netcdf4-python/
.. _ERDDAP server: https://salishsea.eos.ubc.ca/erddap/index.html
.. _Exploring netCDF Datasets from ERDDAP.ipynb: https://nbviewer.org/github/SalishSeaCast/tools/blob/main/analysis_tools/Exploring%20netCDF%20Datasets%20from%20ERDDAP.ipynb

One reason that you might want to use ERDDAP to access our model results is if you don't have access to our results files stored on the UBC EOAS Ocean cluster.
Our ERDDAP server is public.

Another reason to use ERDDAP is that it provides access to the daily model results as continuous data streams,
hiding the fact that they are stored in per-day files.
ERDDAP makes it much easier to work with a dataset that spans multiple days because it removes the task of opening each day's file(s) and splicing the variable values into arrays.
You can just ask for a slice of the dataset in time and space and ERDDAP takes care of the slicing and splicing
(provided that the resulting dataset is less than 2Gb in size).

Another new development is the `xarray`_ package.
Quoting from the introduction to its documentation:

  **xarray** ... is an open source project and Python package that aims to bring the labeled data power of `pandas`_ to the physical sciences,
  by providing N-dimensional variants of the core pandas data structures.

  Our goal is to provide a pandas-like and pandas-compatible toolkit for analytics on multi-dimensional arrays,
  rather than the tabular data for which pandas excels.
  Our approach adopts the `Common Data Model`_ for self-describing scientific data in widespread use in the Earth sciences:
  :py:class:`xarray.Dataset` is an in-memory representation of a netCDF file.

.. _xarray: https://docs.xarray.dev/en/stable/
.. _pandas: https://pandas.pydata.org/
.. _Common Data Model: https://docs.unidata.ucar.edu/netcdf-java/4.6/userguide/index.html

Here is a reworking of the `Exploring netCDF Files.ipynb`_ notebook using :kbd:`xarray`:

* `Exploring netCDF Datasets Using xarray.ipynb`_

.. _Exploring netCDF Datasets Using xarray.ipynb: https://nbviewer.org/github/SalishSeaCast/tools/blob/main/analysis_tools/Exploring%20netCDF%20Datasets%20Using%20xarray.ipynb

`xarray`_ uses the `netCDF4-python`_ library so it is capable of accessing netCDF datasets from either local files or from ERDDAP servers.
The :py:class:`xarray.Dataset` object hides many of the low level details of the :py:class:`netcdf4.Dataset` objects to provide a more Pythonic interface to the dataset that is heavily inspired by `pandas`_. Like :kbd:`panada` variables,
:kbd:`xarray` variables have a :py:meth:`plot` method that makes quick visualization of datasets very easy.

:kbd:`xarray` provides sophisticated handling of the time coordinate of datasets.
In combination with ERDDAP that feature makes accessing arbitrary length time slices from the daily Salish Sea Nowcast system results collection very easy.

In summary,
you can think of ERDDAP as a higher level abstraction for storage of our model results,
and :kbd:`xarray` as a higher level abstraction for working with the results as Python objects.
The ERDDAP abstraction hides some of the discrete daily runs storage details,
and the :kbd:`xarray` abstraction hides some of the netCDF4 file structure details.

Here is a notebook that demonstrates some of the features of `xarray`_ combined with accessing model results from our `ERDDAP server`_:

* `Exploring a Nowcast Time Series from ERDDAP.ipynb`_

.. _Exploring a Nowcast Time Series from ERDDAP.ipynb: https://nbviewer.org/github/SalishSeaCast/tools/blob/main/analysis_tools/Exploring%20a%20Nowcast%20Time%20Series%20from%20ERDDAP.ipynb
