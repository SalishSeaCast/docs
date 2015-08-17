.. _TidalCurrentsTools:

Tidal currents tools
=========================


Tidal currents
-----------------------

Tidal currents are tightly knit with the hydrodynamics. When the currents are averaged over a long period of time what we have left is the tidal effect on the currents. We can average out the winds and surface currents anomalies. It is deeply dependant on bottom friction and mixing (Parker, 2007). Evaluating tidal currents enables one to see these effects at a chosen locations.

To calculate and evaluate tidal currents we need long (many months) currents time series for a particular location.


Observations
---------------

* Current data can be obtained or seen at `Ocean Networks Canada`_ (ONC) website.
* The ONC data is then processed by Dr. Rich Pawlowicz.
* Phase and amplitude information can be reported in papers, we use values reported in Foreman et al. (2004).


.. _Ocean Networks Canada: http://venus.uvic.ca/data/data-plots/#strait-of-georgia-plots


Tidal ellipses
----------------

We perform a harmonic analysis on the time series in order to extract the tidal harmonic constants. From the speed and direction of the tidal current at one location over time we can construct harmonic constituents ellipse which are used to describe the motion of the water due to one tidal constituent (eg. M2).

Separating the constituents
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Similarly to tides, tidal currents are composed of many different harmonic constituents. When calculating and comparing tidal ellipses it is of one constituents at one location, it is important to have a long enough time series to be able to fully separate the constituents.


Freshet and changing shorelines
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is important to avoid using data for a tidal analysis during the Fraser River freshet. The outflow from the river is very larger from the end of may to mid June and this skews the tidal ellipses.
The bathymetry and shoreline drastically effects the tidal currents. However, they can change over time. It is important to select data that would have a consistent shoreline and bathymetry across datasets that are being compared.


Python Scripts
----------------

Tidal ellipse calculation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Some python functions have been written to facilitate calculating amplitude and phase from velocity vectors and to calculate the ellipse parameters from these values. These scripts are found in :file:`tools/SalishSeaTools/salishsea_tools/ellipse.py`

* :file:`fittit` - This script fits a velocity vector time series to a formula and extracts the amplitude and phase of each component in the formula.::

   fittit(uaus, time, nconst)

This function finds tidal parameters from a tidal current component across a specified area of the grid at a single depth, at a single point through the water column or a single depth averaged grid point. Must perform twice, once for each tidal current vector in order to complete the analysis.
The nconst input sets how many tidal harmonic constituents will be analysed. They come in pairs and in order of importance the domain. It returns a dictionary object containing the phase and amplitude for each component for the velocity given. The fit performed in fittit is based on Xu, Z. (2000).


    	.. math::
	  u = mean + A_{M2}cos(\omega_{M2}t-\theta_{M2}) + A_{K1}cos(\omega_{K1}t-\theta_{K1})

	  v = mean + A_{M2}cos(\omega_{M2}t-\theta_{M2}) + A_{K1}cos(\omega_{K1}t-\theta_{K1})


where :math:`\omega_{M2}` and :math:`\omega_{K1}`, :math:`\theta_{M2}` and :math:`\theta_{K1}` and :math:`A_{M2}` and :math:`A_{K1}` are the frequencies, phase lags and amplitudes for the M2 and K1 components.


* :file:`ellipse_params` - This script converts from the amplitude and phase lag parameters to the tidal current ellipse parameters.::

    ellipse_params(uamp, upha, vamp, vpha)

This function calculates the tidal ellipse parameters based on the conversions shown in Xu, Z. (2000). It outputs the positively and negatively rotating amplitude and phase, as well as the major and minor axis and the axis tilt and phase.

* :file:`get_params_nowcast` - This script gives the tidal ellipse parameters for a given date range and location based on the hourly model output values.::

    get_params_nowcast(to, tf, i, j, path, nconst, depthrange='None', depav=False, tidecorr=CorrTides)

This function loads all the data between the start and the end date that contains hourly velocity netCDF4 files. Then it mask, unstaggers and rotates the velocities by component about the grid point described by the i and j. Lastly it fits the velcities and caculates the tidal ellipse parameters for that date range using the fittit and ellipse_param functions above.
After finding the amplitude and phase of the orthogonal vector by using fittit it does a tide correction  which is set to September 10th 2014 by the nowcast. These values values and other constituents tide corrections can be found in: /data/dlatorne/MEOPAR/SalishSea/nowcast/08jul15/ocean.output/.
This function outputs a dictionary object containing the ellipse parameters for each tidal harmonic constituent.

* :file:`plot_ellipses_area`  &  :file:`plot_ellipses` - These scripts are used to plot the tidal ellipses on a map based on the parameters calculated by the functions above.::

    plot_ellipses_area(params, depth='None', imin=0, imax=398, jmin=0, jmax=898)

    plot_ellipses(params, x, y, depth='None', numellips=1, imin=0, imax=398, jmin=0, jmax=898)

* In this notebook: `UsingEllipse.py.ipynb`_  there are simple examples of the functions above.

.. _UsingEllipse.py.ipynb: http://nbviewer.ipython.org/urls/bitbucket.org/salishsea/analysis/raw/tip/Muriel/UsingEllipse.py.ipynb

MATLAB Scripts
----------------
Loading and processing of the observational data from the ONC VENUS Central, East and Delta nodes is done in MATLAB scripts written by Dr. Rich Pawlowicz. The processing is done in three parts and is tailored for each deployment at each node.

Processing scripts
~~~~~~~~~~~~~~~~~~~~~

* The first part is :file:`GET_DATA_fun.m` This script will get the data that is directly output from the ADCP. It
 does this for the two days before the day indicated. It will put this data in a directory at pth/raw/ and organize it by year and month. This function calls a script written by Marlene Jeffries at Ocean Network Canada :file:`getSoGAdcpDataMay15_mod`. This script contains many functions that are used to ultimately retrieve the raw data from the ONC website.

 * The next step is to run :file:`GET_DEPL_fun.m` goes through all the data in the raw directory gathered by GETDATA_fun and bins it into 30 minutes bins. .

 * Lastly, the bulk of the processing is done in :file:`LTIM_fun.m`. This script filters out the tides, corrects the angles for the velocities to get major axis in the direction of the flood current.

Adjustments for running daily
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * When running these scripts for a single day of data at a time to have daily comparisons a few modifications have to be done to keep the scripts running. First of all, GETDEPL_fun creates a new deployment file with the new updated raw data that was loaded by GETDATA_fun however LTIM_fun needs only one mat file per deployment in the directory where it looks. The :file:`compare_daily` functions works helps seamlessy join the new update deployment file and the previous deployment file.

New deployment
~~~~~~~~~~~~~~~

* Every few months to a year the nodes need maintenance or for whatever reason a new deployment with new devices get installed. This requires a lot of effort because the numbers in :file:`LTIM_fun` have previously been found manually. These values are based on the tilt, depth, angle and other physical aspects of the node. The processing that is done may need weeks of data from a new deployement to accurately get the information to realign the ADCP output into usable data.

* All the raw data will have to be deleted so that only the present deployment gets reloaded every time.

* Contact Marlene Jeffries at Ocean Networks Canada for an updated :file:`getSoGAdcpDataMay15_mod.m` script that contains the correct device and sensor IDs of the new deployment.

Changing users
~~~~~~~~~~~~~~~
If you will be running the processing in a new directory for the first time there are a couple things to change in order to facilitate the transitions.

* 1. In :file:`compare_daily.m` change the path to be where you want everything to be saved. Many extra files will appear in this directory every time you run the scripts.

* 2. Make an account on http://www.oceannetworks.ca/information to get userId. In :file:`getSoGAdcpDataMay15_mod.m` insert your email and userId at lines 173 and 174 of the script. You will receive an email everytime you load raw data from the website.

* 3. In :file:`GET_DATA_fun` change the firstdate variable to be at least 3 days before the lastdate. This is because the filter length in :file:`LTIM_fun` needs at least that much data for the processing.


Setup of the :file:`/ocean/` ONC ADCP Data Filespace
----------------------------------------------------

This section describes the setup of the storage filespace on :file:`/ocean/` containing the accumulated raw and processed ONC ADCP data.
Those data are from the Strait of Georgia Central,
East,
and Delta Dynamics Laboratory (DDL) nodes.
Also described in this section is the software automation that updates those data daily with the observations from the previous day.

The data and processing scripts are stored in :file:`/ocean/dlatorne/MEOPAR/ONC_ADCP/`.
The accumulated,
processed data for the 3 nodes are in the files:

* :file:`/ocean/dlatorne/MEOPAR/ONC_ADCP/ADCPcentral`
* :file:`/ocean/dlatorne/MEOPAR/ONC_ADCP/ADCPddl`
* :file:`/ocean/dlatorne/MEOPAR/ONC_ADCP/ADCPeast`

The raw data downloaded from ONC are in directory trees organized by year and month number;
e.g. :file:`2015/07/` in the directories:

* :file:`/ocean/dlatorne/MEOPAR/ONC_ADCP/central/raw/`
* :file:`/ocean/dlatorne/MEOPAR/ONC_ADCP/ddl/raw/`
* :file:`/ocean/dlatorne/MEOPAR/ONC_ADCP/east/raw/`

The other files in the :file:`/ocean/dlatorne/MEOPAR/ONC_ADCP/` tree are the processing scripts,
sensor deployment data files,
etc.
Many of those files are symlinked from version controlled files in the :ref:`private-tools-repo`.


Preparing the :file:`/ocean/dlatorne/MEOPAR/ONC_ADCP/` Filespace
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* Create the directory and make it group writable for the :kbd:`sallen` group:

  .. code-block:: bash

      $ mkdir /ocean/dlatorne/MEOPAR/ONC_ADCP
      $ cd /ocean/dlatorne/MEOPAR/ONC_ADCP
      $ chgrp sallen
      $ chmod g+w .

* Copy the accumulated-to-date processed data files into the filespace:

  .. code-block:: bash

      $ cp --preserve=timestamps /ocean/mdunn/MEOPAR/analysis/Muriel/TidalEllipseData/Nodes/ADCPcentral.mat ./
      $ cp --preserve=timestamps /ocean/mdunn/MEOPAR/analysis/Muriel/TidalEllipseData/Nodes/ADCPddl.mat ./
      $ cp --preserve=timestamps /ocean/mdunn/MEOPAR/analysis/Muriel/TidalEllipseData/Nodes/ADCPeast.mat ./

* Create directories for the raw data and per-node processing files,
  and make them group writable for the :kbd:`sallen` group:

  .. code-block:: bash

      $ mkdir central ddl east
      $ chgrp sallen central ddl east
      $ chmod g+w cental ddl east

* Symlink the historic sensor deployment data files for each node into their respective directories:

  .. code-block:: bash

      $ cd central
      $ for n in {01..10}; do
      > ln -s /data/dlatorne/MEOPAR/private-tools/ONC_ADCP/central/DEPL${n}*.mat
      > done

      $ cd ../ddl
      $ for n in {1..2}; do
      > ln -s /data/dlatorne/MEOPAR/private-tools/ONC_ADCP/ddl/DEPL0${n}*.mat
      > done

      $ cd ../east
      $ for n in {01..12}; do
      > ln -s /data/dlatorne/MEOPAR/private-tools/ONC_ADCP/east/DEPL${n}*.mat
      > done
      $ cd ..

* Copy the current sensor deployment data files for each node into their respective directories:

  .. code-block:: bash

      $ cp --preserve=timestamps /ocean/mdunn/MEOPAR/analysis/Muriel/TidalEllipseData/Nodes/central/DEPL11VIP-12-11.mat central/
      $ cp --preserve=timestamps /ocean/mdunn/MEOPAR/analysis/Muriel/TidalEllipseData/Nodes/ddl/DEPL03BBL-SG-03-03.mat ddl/
      $ cp --preserve=timestamps /ocean/mdunn/MEOPAR/analysis/Muriel/TidalEllipseData/Nodes/east/DEPL13VIP-02-13.mat east/

* Copy the accumulated-to-date raw data file trees for each node into their respective directories and make the directories group writable for the :kbd:`sallen` group:

  .. code-block:: bash

      $ mkdir central/raw
      $ chgrp sallen central/raw
      $ cp -r --preserve=timestamps /ocean/mdunn/MEOPAR/analysis/Muriel/TidalEllipseData/Nodes/central/raw/2015 central/raw/
      $ find central/raw -type d | xargs chmod g+w

      $ mkdir ddl/raw
      $ chgrp sallen ddl/raw
      $ cp -r --preserve=timestamps /ocean/mdunn/MEOPAR/analysis/Muriel/TidalEllipseData/Nodes/ddl/raw/2015 ddl/raw/
      $ find ddl/raw -type d | xargs chmod g+w

      $ mkdir east/raw
      $ chgrp sallen east/raw
      $ cp -r --preserve=timestamps /ocean/mdunn/MEOPAR/analysis/Muriel/TidalEllipseData/Nodes/east/raw/2015 east/raw/
      $ find east/raw -type d | xargs chmod g+w


* Create symlinks to the version-controlled processing scripts:

  .. code-block:: bash

      $ ln -s /data/dlatorne/MEOPAR/private-tools/ONC_ADCP/compare_daily.m
      $ ln -s /data/dlatorne/MEOPAR/private-tools/ONC_ADCP/GETDATA_fun.m
      $ ln -s /data/dlatorne/MEOPAR/private-tools/ONC_ADCP/GETDEPL_fun.m
      $ ln -s /data/dlatorne/MEOPAR/private-tools/ONC_ADCP/LTIM_fun.m
      $ ln -s /data/dlatorne/MEOPAR/private-tools/ONC_ADCP/get_VENUS_ADCP.m
      $ ln -s /data/dlatorne/MEOPAR/private-tools/ONC_ADCP/get_VENUS_ADCP.cron.sh

* Copy the ONC-provided data download script into the filespace:

  .. code-block:: bash

      $ cp --preserve=timestamps /ocean/mdunn/MEOPAR/analysis/Muriel/TidalEllipseData/Nodes/getSogAdcpDataMay15_mod.m ./

  **TODO:** That script should be symlinked from a version controlled copy

* Create a :command:`matlab` function in :file:`get_VENUS_ADCP.m` to run :file:`compare_daily.m` for each node:

  .. code-block:: matlab

      function get_VENUS_ADCP
        % Run the compare_daily.m script for each ONC VENUS node of interest
        % to download and process the ADCP data for the previous day
        % using Rich Pawlowicz's scripts.

        compare_daily(date, 'central', 2)
        compare_daily(date, 'ddl', 2)
        compare_daily(date, 'east', 2)
      end

* Create a :command:`bash` script called :file:`get_VENUS_ADCP.cron.sh` for :command:`cron` to execute to run :file:`get_VENUS_ADCP.m`:

  .. code-block:: bash

      # Download and process VENUS nodes ADCP data for the previous day
      # using matlab scripts written by Muriel Dunn and Rich Pawlowicz.
      #
      # usage:   0 14 * * *  /ocean/dlatorne/MEOPAR/ONC_ADCP/get_VENUS_ADCP.cron.sh

      cd /ocean/dlatorne/MEOPAR/ONC_ADCP
      matlab -nodesktop -nodisplay -r get_VENUS_ADCP

* Make :file:`get_VENUS_ADCP.cron.sh` owner and group executable:

  .. code-block:: bash

      $ chmod ug+x get_VENUS_ADCP.cron.sh

* Add a line to the :file:`crontab` on :kbd:`salish` to execute :file:`get_VENUS_ADCP.cron.sh` daily:

  .. code-block:: bash

      OCEAN_MEOPAR=/ocean/dlatorne/MEOPAR
        0 10 * * *  ${OCEAN_MEOPAR}/ONC_ADCP/get_VENUS_ADCP.cron.sh


References
^^^^^^^^^^^^

* Parker, B. B., 2007. Tidal analysis and prediction. US Department of Commerce, National Oceanic and Atmospheric Administration, National Ocean Service, Centre for Operational Oceanographic Products and Services, 378 pages.

* Xu, Z., 2000. Ellipse parameters conversion and vertical velocity profiles for tidal currents. Bedford Institute of Oceanography, Dartmouth, Nova Scotch, Canada, 20 pages

