***********************
Working on :kbd:`nefos`
***********************

.. warning::

    This section contains work in progress notes.
    `nefos`_ is not yet ready to run the Salish Sea NEMO model.

These are working notes about the process of getting the Salish Sea NEMO model set up to run on  the `Ocean Newworks Canada`_ private cloud computing facility `nefos`_ that run on `OpenStack`_.

.. _Ocean Newworks Canada: http://www.oceannetworks.ca/
.. _nefos: https://www.westgrid.ca/support/systems/Nefos
.. _OpenStack: http://www.openstack.org/

The `OpenStack dashboard`_ provides a web interface to provision and report on cloud resources.
The :kbd:`nefos` dashboard is at https://nefos.westgrid.ca/dashboard/.

.. _OpenStack dashboard: http://docs.openstack.org/user-guide/content/ch_dashboard.html

Authentication and authorization for :kbd:`nefos` is managed by `Westgrid`_,
so use your Westgrid user id and password to log in to the dashboard.

.. _Westgrid: https://www.westgrid.ca/

To automate creation and management of cloud resources OpenStack provides a collection of `command-line clients`_.
There is also a `Python Software Development Kit (SKD)`_.
The SDK implements Python bindings to the OpenStack API,
which enables you to perform automation tasks in Python by making calls on Python objects rather than making REST calls directly.
All OpenStack command-line tools are implemented using the Python SDK.

.. _command-line clients: http://docs.openstack.org/user-guide/content/ch_cli.html
.. _Python Software Development Kit (SKD): http://docs.openstack.org/user-guide/content/ch_sdk.html


Local System Setup
==================

To isolate the installation of the OpenStack command-line clients and the Python SDK on our local machine
(Waterhole workstation,
laptop,
etc.)
we'll use a `Conda environment`_.
This presumes that you have the :ref:`AnacondaPythonDistro` installed on your local machine.

.. _Conda environment: http://conda.pydata.org/docs/intro.html

At the time of writing OpenStack only supports Python 2.7,
so create and activate an environment with Python 2.7 and `pip`_ installed:

.. code-block:: none

    $ conda create -n nefos python=2.7 pip
    ...
    $ source activate nefos

.. _pip: https://pip.readthedocs.org/en/latest/

Install a collection of packages that the command-line clients and SDK depend on and that are included in the Anaconda distribution.
Doing this avoids compilation and linking issues.

.. code-block:: none

    (nefos)$ conda install requests pyopenssl six pytz crytography cffi pycparser
