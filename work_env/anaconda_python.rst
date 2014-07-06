.. _AnacondaPythonDistro:

Anaconda Python Distribution
============================

The the Anaconda_ Python distribution is the easiest way to install Python and a collection of scientific libraries and other tools
(`Sphinx`_,
`IPython Notebook`_,
`NumPy`_,
`matplotlib`_,
to name a few)
that we use in the Salish Sea MEOPAR project.

.. _Anaconda: https://store.continuum.io/cshop/anaconda/
.. _Sphinx: http://sphinx-doc.org/
.. _IPython Notebook: http://ipython.org/ipython-doc/dev/index.html
.. _NumPy: http://docs.scipy.org/doc/numpy/reference/index.html
.. _matplotlib: http://matplotlib.org/contents.html

Follow the `installation instructions`_ for your operating system and accept the option at the end of the installation to make Anaconda your default Python.

.. _installation instructions: http://www.continuum.io/downloads

Thanks to the shared storage and user configurations across all of the Waterhole machines and :kbd:`salish` you only need to do the installation once on a Waterhole machine for the libraries to be available on all of those machines.
There is no need to install Anaconda on the Westgrid machines.

Start a new shell session and confirm that :file:`$HOME/anaconda/bin/` is on your path.

There is one library that we use extensively that is not included in the base Anaconda_ installation: `netcdf4-python`_.
To install it,
do:

.. code-block:: bash

    conda install netcdf4

and follow the prompts.

.. _netcdf4-python: http://netcdf4-python.googlecode.com/svn/trunk/docs/netCDF4-module.html
