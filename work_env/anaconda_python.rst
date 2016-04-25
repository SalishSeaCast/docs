.. _AnacondaPythonDistro:

Anaconda Python Distribution
============================

The the Anaconda Python distribution is the easiest way to install Python and a collection of scientific packages and other tools
(`Sphinx`_,
`Jupyter Notebook`_,
`NumPy`_,
`matplotlib`_,
to name a few)
that we use in the Salish Sea MEOPAR project.

.. _Sphinx: http://sphinx-doc.org/
.. _Jupyter Notebook: https://jupyter.org/
.. _NumPy: http://docs.scipy.org/doc/numpy/reference/index.html
.. _matplotlib: http://matplotlib.org/contents.html

Choose the *Python 3* download from the `installation instructions`_ for your operating system,
follow the rest of those instructions,
and accept the option at the end of the installation to make Anaconda your default Python.

.. _installation instructions: http://www.continuum.io/downloads

If you are installing Anaconda Python on a Waterhole machine,
choose the *Python 3 Linux 64-bit* download.
Thanks to the shared storage and user configurations across all of the Waterhole machines and :kbd:`salish` each user only needs to do the installation once on a Waterhole machine for the packages to be available on all of those machines.
There is no need to install Anaconda on the Westgrid machines.

Start a new shell session and confirm that :file:`$HOME/anaconda3/bin/` is on your path.

There is are a few packages that we use extensively that is not included in the base Anaconda installation:

* `netcdf4-python`_
* `xarray`_
* `bottleneck`_

.. _netcdf4-python: http://unidata.github.io/netcdf4-python/
.. _bottleneck: https://pypi.python.org/pypi/Bottleneck
.. _xarray: http://xarray.pydata.org/en/stable/index.html

To install them,
do:

.. code-block:: bash

    conda install netcdf4 xarray bottleneck

and follow the prompts.
