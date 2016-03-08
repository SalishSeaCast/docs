.. _TidalCurrentsTools:

********************
Tidal Currents Tools
********************

Tidal currents
==============

Tidal currents are tightly knit with the hydrodynamics. When the currents are averaged over a long period of time what we have left is the tidal effect on the currents. We can average out the winds and surface currents anomalies. It is deeply dependant on bottom friction and mixing (Parker, 2007). Evaluating tidal currents enables one to see these effects at a chosen locations.

To calculate and evaluate tidal currents we need long (many months) currents time series for a particular location.


Observations
============

* Current data can be obtained or seen at `Ocean Networks Canada`_ (ONC) website.
* The ONC data is then processed by Dr. Rich Pawlowicz.
* Phase and amplitude information can be reported in papers, we use values reported in Foreman et al. (2004).


.. _Ocean Networks Canada: http://venus.uvic.ca/data/data-plots/#strait-of-georgia-plots


Tidal Ellipses
==============

We perform a harmonic analysis on the time series in order to extract the tidal harmonic constants. From the speed and direction of the tidal current at one location over time we can construct harmonic constituents ellipse which are used to describe the motion of the water due to one tidal constituent (eg. M2).


Separating the Constituents
---------------------------

Similarly to tides, tidal currents are composed of many different harmonic constituents. When calculating and comparing tidal ellipses it is of one constituents at one location, it is important to have a long enough time series to be able to fully separate the constituents.


Freshet and Changing Shorelines
-------------------------------

It is important to avoid using data for a tidal analysis during the Fraser River freshet. The outflow from the river is very larger from the end of may to mid June and this skews the tidal ellipses.
The bathymetry and shoreline drastically effects the tidal currents. However, they can change over time. It is important to select data that would have a consistent shoreline and bathymetry across datasets that are being compared.


Python Scripts
==============

Tidal Ellipse Calculation
-------------------------

Some python functions have been written to facilitate calculating amplitude and phase from velocity vectors and to calculate the ellipse parameters from these values. These scripts are found in :file:`tools/SalishSeaTools/salishsea_tools/ellipse.py` and :file:`tools/SalishSeaTools/salishsea_tools/tidetools.py`

* :file:`tidetools.fittit` - This script fits a time series to a tide curve and extracts the amplitude and phase of each tidal constituent in the tide curve.::

   tidetools.fittit(uaus, time, nconst)

This function finds tidal parameters from a tidal constituent across a specified area of the grid at a single depth, at a single point through the water column or a single depth averaged grid point. For currents, this function must perform twice, once for each tidal current vector in order to complete the analysis. This function should also work with water level analysis.

The nconst input sets how many tidal constituents will be analysed. They come in pairs and in order of importance the domain. It returns a dictionary object containing the phase and amplitude for each component for the input array.

The fitting routine solves for a set of amplitude and phase parameters (one set for each tidal constituent) by finding the best match between the model's time series and a tidal curve that is predetermined by the tidal constituents that it will be solving for. Below is an example of the equation for the u and v tidal curves for the M2 and K1 constituents:

    	.. math::
	  u = mean + A_{M2}cos(\omega_{M2}t-\theta_{M2}) + A_{K1}cos(\omega_{K1}t-\theta_{K1})

	  v = mean + A_{M2}cos(\omega_{M2}t-\theta_{M2}) + A_{K1}cos(\omega_{K1}t-\theta_{K1})


where :math:`\omega_{M2}` and :math:`\omega_{K1}`, :math:`\theta_{M2}` and :math:`\theta_{K1}` and :math:`A_{M2}` and :math:`A_{K1}` are the frequencies, phase lags and amplitudes for the M2 and K1 components. "Mean" is an unknown values that the fitting routine will solve for however it is not used in the tidal ellipse calculations.

* :file:`ellipse.ellipse_params` - This script converts from the amplitude and phase lag parameters to the tidal current ellipse parameters.::

    ellipse.ellipse_params(uamp, upha, vamp, vpha)

This function calculates the tidal ellipse parameters based on the conversions shown in Xu, Z. (2000). It outputs the positively and negatively rotating amplitude and phase, as well as the major and minor axis and the axis tilt and phase.

* :file:`ellipse.get_params_nowcast` - This script gives the tidal ellipse parameters for a given date range and location based on the hourly model output values.::

    ellipse.get_params_nowcast(to, tf, i, j, path, nconst, depthrange='None', depav=False, tidecorr=CorrTides)

This function loads all the data between the start and the end date that contains hourly velocity netCDF4 files. Then it mask, unstaggers and rotates the velocities by component about the grid point described by the i and j. Lastly it fits the velcities and caculates the tidal ellipse parameters for that date range using the fittit and ellipse_param functions above.

After finding the amplitude and phase of the orthogonal vector by using fittit it does a nodal correction, determined by the start date of the nowcasts, Sept 10, 2014. These values values and other constituents tide corrections can be found in: /data/dlatorne/MEOPAR/SalishSea/nowcast/08jul15/ocean.output/.
This function outputs a dictionary object containing the ellipse parameters for each tidal harmonic constituent.

* In this notebook: `UsingEllipse.py.ipynb`_  there are simple examples of the functions above.

.. _UsingEllipse.py.ipynb: https://nbviewer.jupyter.org/urls/bitbucket.org/salishsea/analysis/raw/tip/Muriel/UsingEllipse.py.ipynb


MATLAB Scripts
==============

Loading and processing of the observational data from the ONC VENUS Central, East and Delta nodes is done in MATLAB scripts written by Dr. Rich Pawlowicz. The processing is done in three parts and is tailored for each deployment at each node.


Processing Scripts
------------------

* The first part is :file:`GET_DATA_fun.m` This script will get the data that is directly output from the ADCP. It does this for the two days before the day indicated. It will put this data in a directory at pth/raw/ and organize it by year and month. This function calls a script written by Marlene Jeffries at Ocean Network Canada :file:`getSoGAdcpDataMay15_mod`. This script contains many functions that are used to ultimately retrieve the raw data from the ONC website.
* The next step is to run :file:`GET_DEPL_fun.m` goes through all the data in the raw directory gathered by GETDATA_fun and bins it into 30 minutes bins. .
* Lastly, the bulk of the processing is done in :file:`LTIM_fun.m`. This script filters out the tides, corrects the angles for the velocities to get major axis in the direction of the flood current.


Adjustments for Running Daily
-----------------------------

* When running these scripts for a single day of data at a time to have daily comparisons a few modifications have to be done to keep the scripts running. First of all, GETDEPL_fun creates a new deployment file with the new updated raw data that was loaded by GETDATA_fun however LTIM_fun needs only one mat file per deployment in the directory where it looks. The :file:`compare_daily` functions works helps seamlessy join the new update deployment file and the previous deployment file.


New deployment
--------------

Once or twice a year ONC performs maintenance on the nodes that involves bringing the ADCP package to the surface and redeploying the same or a different sensor to the bottom a few hours later.
Each deployment has its own unique set of data and metadata including:

* date/time range (resolved to the hour)
* sensor serial number (also known as id)
* number of data bins
* data bin size
* data bin 1 distance
* vertical offset
* rotation angle

Values for those data and metadata are required in order for the automated daily download and processing of the ADCP data to be restarted following a new deployment.
Those values have to be added to arrays in the :file:`GETDATA_fun.m`,
:file:`GETDEPL_fun.m`,
and :file:`LTIM_fun.m`
Matlab scripts
(details below).
Some of the values must be determined by graphical analysis of data from the new sensor deployment,
and there must be enough data available for that analysis that the tidal signal can be averaged out.
So,
at least a week's data must be collected after a new deployment before the graphical analysis can be done.
Once the analysis has been completed the data since the beginning of the deployment must be re-processed with the newly obtained sensor data and metadata values.

As of October 2015,
downloading that data from ONC is accomplished via a Matlab script
(:file:`getSogAdcpData*.m`)
provided after each deployment by Marlene Jefferies of ONC.

The notes that follow were written during the process of restarting the daily download automation following the new deployments at the end of August 2015.


Obtain and Modify the :file:`getSogAdcpData.m` Script from ONC
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Obtain the new version of the :file:`getSogAdcpData.m` Matlab script for the deployment from Marlene Jefferies at ONC,
and store it in :file:`/ocean/dlatorne/MEOPAR/ONC_ADCP/` with the deployment date added to the file name;
e.g. :file:`getSogAdcpData_2Sep2015.m`.

Change the :kbd:`email` and :kbd:`userId` lines near line 180 to the email address and 5-digit ONC user id of the user that owns the automation cron job,
for example:

.. code-block:: matlab

    p.addParamValue('email','dlatornell@eos.ubc.ca', @ischar);
    p.addParamValue('userId', ddddd, @isnumeric);

The :kbd:`LocationName` comment blocks starting at about line 50 in the :file:`getSogAdcpData.m` script provide the site name
(e.g. :kbd:`VIP-13`)
and sensor serial number
(e.g. :kbd:`8497`)
for all of the deployments to date.
Those values for the most recent deployments are required to update the :file:`GETDATA_fun.m` and :file:`GETDEPL_fun.m` scripts.
Also required are the end date/time for the previous deployment,
and the start date/time for the present deployment from the :kbd:`switch` statement at about line 430.


Update the :file:`GETDATA_fun.m` Script
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Add the new deployment numbers,
start date/times,
and sensor serial numbers to the :kbd:`history` arrays in the :kbd:`switch` statement that starts at about line 35 in the copy of :file:`private-tools/ONC-ADCP/GETDATA_fun.m` that is symlinked into :file:`/ocean/dlatorne/MEOPAR/ONC-ADCP/`.
The :kbd:`history` arrays also require deployment end date/times;
choose a value in the future beyond the expected duration of the deployment.

The updates made for the late-August 2015 deployments are highlighted below:

.. code-block:: matlab
    :emphasize-lines: 8,18,28,30

    switch nodloc,
      case 'east',
        node='SOG-East-Node';
        history=[...
        01    2007 10 19 00     2008 09 25 00 8497;
        ...
        2     2015 03 31 22     2015 08 27 16 8497;
        13    2015 08 27 22     2016 12 31 00 8497];  % or present

      ...

      case 'central',
        node='SOG-Central-Node';
        history=[...
        01    2008 09 24 00     2009 09 27 00 8580;
        ...
        12    2014 09 20 02     2015 08 30 15 8580;
        13    2015 08 31 02     2016 12 31 00 8580];  % or present

      ...

      case 'ddl',
       node='SOG-Delta-Node';
       history=[...
       08    2013 10 23 17     2014 03 06 17 2940;   % DDL 148m
       02    2014 03 08 21     2014 09 20 18 17955;  % BBL-SG-02 at 142m
       03    2014 09 22 00     2015 08 28 15 17955;  % BBL-SG-03 at 149m
       04    2015 08 30 19     2016 12 31 00 17955]; % BBL-SG-04 at ???m

       depname={'DDL-','BBL-SG-','BBL-SG-','BBL-SG-'};


Download Raw Data from Deployment Date to Present
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Run the :file:`GETDATA_fun.m` script to download the raw data for each node for the days from the deployment to present.
That can be accomplished by running Matlab in command-line mode on :kbd:`salish`:

.. code-block:: bash

    $ cd /ocean/dlatorne/MEOPAR/ONC_ADCP/
    $ matlab -nodesktop -nodisplay
    ...
    >> GETDATA_fun('08-Sep-2015', 'central', '/ocean/dlatorne/MEOPAR/ONC_ADCP/', 10)

The data download takes about 10 minutes per day requested,
so it is advisable to start Matlab in 3 separate terminal sessions and run commands like the above for each of the nodes:
:kbd:`central`,
:kbd:`east`,
and :kbd:`ddl`.
The :file:`GETDATA_fun.m` script handles breaking the requested number of days into 7 day chunks
(the maximum that the ONC hardware can handle).
The :file:`getSogAdcpData*.m` script downloads the data into the :file:`{path}/{mode}/raw/` directory;
i.e. :file:`/ocean/dlatorne/MEOPAR/ONC_ADCP/central/raw/` in the example above.

The :file:`get_VENUS_ADCP_raw.cron.sh` and :file:`get_VENUS_ADCP_raw.m` scripts can be used to automate daily downloading of the raw data during the period required to obtain enough data to complete the analysis required to get the variable values to facilitiate fully automated processing.


Update the :file:`GETDEPL_fun.m` Script
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The :file:`GETDEPL_fun.m` script must be updated with the same deployment numbers,
start date/times,
and sensor serial numbers that were added to the :file:`GETDATA_fun.m` script.
Also required are the number of bins,
the bin size,
and the bin 1 distances values from one of the sensor metadata files that is generated for each download request.
The easiest way to obtain those values is to follow the link to the metadata HTML file that is included in the download completion email message generated by the ONC system.
Add those values to the :kbd:`history` arrays in the :kbd:`switch` statement that starts at about line 22 in the copy of :file:`private-tools/ONC-ADCP/GETDEPL_fun.m` that is symlinked into :file:`/ocean/dlatorne/MEOPAR/ONC-ADCP/`.
The :kbd:`history` arrays also require vertical offset values for the deployment;
use the same value as for the previous deployment to start with,
it will be tuned later.

The updates made for the late-August 2015 deployments are highlighted below:

.. code-block:: matlab
    :emphasize-lines: 8,18,29,31

    switch nodloc,
      case 'east',
        node='SOG-East-Node';
        history=[...                         %  ID  #bins binsize bin1distance (one that 'best' works) offset
        01    2007 10 19 00     2008 09 25 00 8497  85 2 6.14 0;
        ...
        2     2015 03 31 22     2015 12 31 00 8497  85 2 6.39 +3;
        13    2015 08 27 22     2016 12 31 00 8497  85 2 6.40 +3];

      ...

         case 'central',
          node='SOG-Central-Node';
          history=[...
          01    2008 09 24 00     2009 09 27 00 8580 38 8 12.47 0;
          ...
          12    2014 10 01 00     2015 12 31 00 8580 60 5 9.24  0;   % <- change in parameters here (same VIP)
          13    2015 08 31 02     2016 12 31 00 8580 60 5 9.25  0];

        ...

        case 'ddl',

          node='SOG-Delta-Node';
          history=[...
          08    2013 10 23 16     2014 03 06 17 2940  75  2    4.3  0;   % DDL 148m
          02    2014 03 08 21     2014 09 20 18 17955 115 1.33 3.96 0;  % BBL-SG-02 at 142m
          03    2014 09 22 00     2015 12 31 00 17955 115 1.33 3.96 0; % BBL-SG-03 at 149m
          04    2015 08 30 19     2016 12 31 00 17955 115 1.33 3.96 0]; % BBL-SG-04 at 147m

          depname={'DDL-','BBL-SG-','BBL-SG-','BBL-SG-'};


Update the :file:`LTIM_fun.m` Script
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The :file:`LTIM_fun.m` script must be updated with the rotation angles of the ADCPs for the new deployments.
Initially the angles are set to the same values as for the previous deployments so that the :file:`LTIM_fun.m` script can be run.

The updates for the late-August 2015 deployments are highlighted below:

.. code-block:: matlab
    :emphasize-lines: 7,18,27

    switch nodloc,
      case 'central',
        rotang=[0;  % VIP 01
                  22;  %  02
                  ...
                  58+180; % 12- v2
                  58+180; % 13
                ]-61;

      ...

      case 'east',
        rotang=[57;
                  0; % VIP 02  - can not start with 1!!
                  50;  %  03
                  ...
                  72;  % east2
                  72;  % 13
                ]-106;

      ...

      case 'ddl',
        rotang=[-60;     % DDL-08
                   35;   % BBL-SG-02
                   -20;  % BBL-SG-03
                   -20;  % BBL-SG-04
                ];

After at least 7 to 10 days of data have been downloaded and processed by the :file:`GETDEPL_fun.m` script,
the :file:`LTIM_fun.m` script will be run interactively to to produce plots that will allow the instrument rotation angles for the deployment to be determined.
Once that has been done the values highlighted above will be updated.


Changing users
--------------

If you will be running the processing in a new directory for the first time there are a couple things to change in order to facilitate the transitions.

* 1. In :file:`compare_daily.m` change the path to be where you want everything to be saved. Many extra files will appear in this directory every time you run the scripts.

* 2. Make an account on http://www.oceannetworks.ca/information to get userId. In :file:`getSoGAdcpDataMay15_mod.m` insert your email and userId at lines 173 and 174 of the script. You will receive an email every time you load raw data from the website.

* 3. In :file:`GET_DATA_fun` change the firstdate variable to be at least 3 days before the lastdate. This is because the filter length in :file:`LTIM_fun` needs at least that much data for the processing.


Setup of the :file:`/ocean/` ONC ADCP Data Filespace
====================================================

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
----------------------------------------------------------------

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
----------

* Parker, B. B., 2007. Tidal analysis and prediction. US Department of Commerce, National Oceanic and Atmospheric Administration, National Ocean Service, Centre for Operational Oceanographic Products and Services, 378 pages.

* Xu, Z., 2000. Ellipse parameters conversion and vertical velocity profiles for tidal currents. Bedford Institute of Oceanography, Dartmouth, Nova Scotch, Canada, 20 pages

