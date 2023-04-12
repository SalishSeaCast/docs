.. _Python3Enviro:

Building a Python 3 Conda Environment
=====================================

.. note::
    If you followed the :ref:`AnacondaPythonDistro` instructions to install a Python 3 environment,
    you can skip this section.
    It is about creating a Python 3 environment when your default environment is Python 2.

In the first stages of our move to Python 3, some important tools we use will
still be in Python 2.  To facilitate the change, you may find it useful to
have Python 3 in a separate environment. If you are doing EOSC 511 in 2015 or later
you will need a Python 3 environment.  This section explains how to set one up
assuming you already have conda and anaconda installed.

At the command line create your new environment, here called "mypython3" but you can call it what you wish:

.. code-block:: bash

   conda create -n mypython3 python=3

To activate the environment:

.. code-block:: bash

   source activate mypython3

To get all the goodies (e.g. `Jupyter Notebook`_, `NumPy`_, `matplotlib`_)
you can install `Anaconda`_, which will auto-magically use Python 3.

.. _Anaconda: https://www.anaconda.com/products/distribution
.. _Jupyter Notebook: https://jupyter.org/
.. _NumPy: https://numpy.org/doc/stable/reference/index.html
.. _matplotlib: https://matplotlib.org/contents.html

.. code-block:: bash

   conda install anaconda

and then our additional install, `netcdf4-python`_

.. code-block:: bash

   conda install netcdf4

.. _netcdf4-python: https://unidata.github.io/netcdf4-python/

Now you have set-up your Python 3 environment.  To start it in a new terminal

.. code-block:: bash

   source activate mypython3

When you are using that environment your prompt will change and will include (mypython3).  To return to using your "base" environment you can de-activate the conda environment with:

.. code-block:: bash

   source deactivate
