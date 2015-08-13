.. _Python_netCDF_Visualization:

***********************************************
Python, netCDF, and Model Results Visualization
***********************************************

Intro to Python
===============

Most of the analysis and tools in the Salish Sea MEOPAR project are written in `Python`_,
though Matlab makes occasional guest appearances.

.. _Python: http://python.org/

`This slide deck`_ from a `physics course`_ at Cornell University provides a good,
fairly detailed,
introduction to Python for people who already know at least one programming language.
Of course,
no two groups make exactly the same choices within a language and the few differences to our choices are detailed below.
Also,
don't get too bogged down in the details of object-oriented and functional programming
(especially slides 18 through 22) as we don't use those aspects much.

.. _This slide deck: http://pages.physics.cornell.edu/~myers/teaching/ComputationalMethods/LectureNotes/Intro_to_Python.pdf
.. _physics course: http://pages.physics.cornell.edu/~myers/teaching/ComputationalMethods/

A few differences you will see compared to our Python code:

* The Cornell course uses an older syntax for string interpolation:

  .. code-block:: python

      print ‘value of %s = %s’ % (name, val)

  In our notebooks and code you are more likely to see that spelled like:

  .. code-block:: python

      print(‘value of {n} = {v}’.format(n=name, v=val))

  or perhaps:

  .. code-block:: python

      print(‘value of {0} = {1}’.format(name, val))

* The :py:obj:`scipy.array` syntax discussed on slides 25 through 28 is a synonym for :py:obj:`numpy.ndarray` and you will see it used in our code as:

  .. code-block:: python

      import numpy as np

      a = np.array([[1,2,3], [4,5,6], [7,8,9]])
      ...
      p = np.arange(0.,1.,0.1)
      etc.

* The :py:obj:`pylab` namespace mentioned on slide 31 is a Matlab-like interface to the Matplotlib_ library.
  In our code we try to use the :py:obj:`pyplot` object-oriented interface,
  so you will see things like:

  .. code-block:: python

      import matplotlib.pyplot as plt
      import numpy as np

      xvals = np.linspace(-10., 10., 100)
      yvals = xvals**3
      fig, (ax1, ax2, ax3) = plt.subplots(1, 3)
      ax1.plot(xvals, yvals)
      ax2.plot(xvals, yvals, ‘r.’)
      ax3.hist(yvals)

.. _Matplotlib: http://matplotlib.org/


IPython Notebook, netCDF, and Model Results
===========================================

We have an ongoing project to develop a collection of `IPython notebooks`__ that provide discussion,
examples,
and best practices for plotting various kinds of model results from `netCDF`_ files.
There are code examples in the notebooks and also examples of the use of functions from the :ref:`salishsea_tools package<SalishSeaTools>`.

__ ipynb_
.. _ipynb: http://ipython.org/notebook.html
.. _netCDF: http://www.unidata.ucar.edu/software/netcdf/

If you are new to the Salish Sea project,
or to `IPython Notebook`__,
netCDF_,
and Matplotlib_ you should read the notebooks in the following order:

__ ipynb_


* `Exploring netCDF Files.ipynb`_
* `Plotting Bathymetry Colour Meshes.ipynb`_
* `Plotting Tracers on Horizontal Planes.ipynb`_
* `Plotting Velocity Fields on Horizontal Planes.ipynb`_
* `Plotting Velocities and Tracers on Vertical Planes.ipynb`_

.. _Exploring netCDF Files.ipynb: http://nbviewer.ipython.org/urls/bitbucket.org/salishsea/tools/raw/tip/analysis_tools/Exploring%20netCDF%20Files.ipynb

.. _Plotting Bathymetry Colour Meshes.ipynb: http://nbviewer.ipython.org/urls/bitbucket.org/salishsea/tools/raw/tip/analysis_tools/Plotting%20Bathymetry%20Colour%20Meshes.ipynb

.. _Plotting Tracers on Horizontal Planes.ipynb: http://nbviewer.ipython.org/urls/bitbucket.org/salishsea/tools/raw/tip/analysis_tools/Plotting%20Tracers%20on%20Horizontal%20Planes.ipynb

.. _Plotting Velocity Fields on Horizontal Planes.ipynb: http://nbviewer.ipython.org/urls/bitbucket.org/salishsea/tools/raw/tip/analysis_tools/Plotting%20Velocity%20Fields%20on%20Horizontal%20Planes.ipynb

.. _Plotting Velocities and Tracers on Vertical Planes.ipynb: http://nbviewer.ipython.org/urls/bitbucket.org/salishsea/tools/raw/tip/analysis_tools/Plotting%20Velocities%20and%20Tracers%20on%20Vertical%20Planes.ipynb

The links above are to static renderings of the notebooks via
`nbviewer.ipython.org`_ .
The notebook source files are in the `analysis_tools`_ directory of the :ref:`tools-repo` repo.

.. _nbviewer.ipython.org: http://nbviewer.ipython.org/
.. _analysis_tools: https://bitbucket.org/salishsea/tools/src/tip/analysis_tools/
