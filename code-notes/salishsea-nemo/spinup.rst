.. _spin-up:


************
Spin-up Runs
************

This section documents the workflows and results of a series of runs that were done to spin up the model to produce restart files that are representative of state of the Salish Sea over a 12 month period.
The spin-up runs span the period from 12-Sep-2002 to 31-Dec-2003,
with the last 12 months of that period providing the spun-up restart files.
Those restart files can be used as initial conditions for research runs in other years.

The spin-up run results are stored in :file:`/ocean/dlatorne/MEOPAR/SalishSea/results/spin-up/`.
Directory names there indicate the date range of the run results that they contain;
e.g. :file:`1jan30jan/` contains the results of the 30 day 1-Jan-2003 to 30-Jan-2003 run.
The restart files contained in the run directories have the name pattern :file:`SalishSea_dddddddd_restart.nc`,
where :file:`dddddddd` is the number of time steps after 00:00 on 16-Sep-2002
(left padded with zeros).
The model time step used for spin-up was 50 seconds.
So,
:file:`SalishSea_00069120_restart.nc` would contain a snapshot of the model state at 00:00 on 26-Oct-2002
(69120 time-steps / (86400 s/day / 50 s/time-step) = 40 days).

Spin-up Sections
================

16-Sep-2002 to 21-Sep-2002
--------------------------

The first 6 days of spin-up were run on :kbd:`salish` 1 day at a time.
The lateral turbulent viscosity
(NEMO :kbd:`namdyn_ldf` namelist variable :kbd:`rn_ahm_0_lap`)
was intially set to 80 :math:`m^2/s` and reduced by 5 :math:`m^2/s` each day to 55 :math:`m^2/s` on day 6.
That was done to stabilize the model as the initial boundary condition values
(especially deep salinity)
propogated through the moderate,
uniform initial stratification that was set for the entire domain.

.. note::

  These runs have :kbd:`nn_date0` set to the day for which the calculations are being done,
  not the day on which :kbd:`nn_it000` is 1
  (see :ref:`nn_date0-quirk`).
  So,
  the tidal forcing is inconsistent.
  This does not invalidate these runs as part of spin-up because they are early in the spin-up,
  and the objective of spin-up is temperature,
  salinity,
  and velocity fields,
  not tides.

22-Sep-2002
-----------

After the :kbd:`nn_date0` issue was discovered 22-Sep was rerun with the following changes:

1. Viscosity was set to 55 :math:`m^2/s`
2. :kbd:`nn_date0` was set to :kbd:`20020916` so that tides were consistent with time in the spin-up.
   That means that there is a jump in the tidal forcing at the beginning of this run.


23-Sep-2002 to 25-Oct-2002
--------------------------

The next days of spin-up were run with the lateral turbulent viscosity
(NEMO :kbd:`namdyn_ldf` namelist variable :kbd:`rn_ahm_0_lap`)
set to as low as possible but high enough that the code did not blow up.
The maximum value used was 55 :math:`m^2/s`.
This was done to  to try to avoid too much Juan de Fuca deep water from reaching the bottom of the Strait of Georgia as happened in a 10d spin-up run started on 23-Sep.  The fresh water in San Juan needs to be flushed to allow enough pre-mixing there.
Harmonics for the M2 and K1 tides were calculated over the entire duration of this run with a resolution of 9 time steps
(450 seconds).
The results during this period also include sea surface height at selected locations for analysis of storm surges,
and sea surface height and profiles of temperature,
salinity,
and u and v velocity components at 6 points along the thalweg and at a location in the Fraser River plume.


26-Oct-2002 --
--------------

Lack of stability in the previous runs and the continuous need to increase viscosity lead to an investigation of stability.  It was determined that the viscosity was too high (currents in the SoG were smaller than observed), and the time step was too large for a vertical CFL condition.  Reducing the time step (and reducing the viscosity) led o stable run of 26-Oct and onwards.

31-Oct-2002 --
--------------

Northern boundary was opened using tides (all 8 constituents) and seasonal T&S (based on Thomson and Huggett paper).

20-Dec-2002 --
--------------

Error in phase of northern boundary constituents was found and corrected (all 8 constituents).

10-Feb-2003 --
--------------

Error in the barotropic velocities at the western boundary was found and corrected.  The length of the arrays needs to be carefully matched or ssh contaminates the velocities.

28-Sep-2003 --
--------------

Our northern cross-section is bigger than the cross-section where Thomson & Huggett measured their currents.  Thus our flux in is too large.  We corrected the K1 tides by the estimated amount 25% and the M2 tides by twice this to better agree with observations.


**The Runs**

========== ===== ============= =============  ================== =========== ==================
 Dates      dt   Viscosity     EVD            Status              Platform   Note
            s    :math:`m^2/s` :math:`m^2/s`
========== ===== ============= =============  ================== =========== ==================
Sep 23      50   50                    100    blew up off Pender    Salish
Sep 23      50   55                    100    *complete*            Salish
Sep 24      50   50                    100    *complete*            Salish
Sep 25      50   45                    100    *complete*            Salish
Sep 26      50   40                    100    *complete*            Salish
Sep 27      50   35                    100    *complete*            Salish
Sep 28      50   30                    100    *complete*            Salish
Sep 29      50   25                    100    *complete*            Salish
Sep 30      50   20                    100    blew up off Stuart    Salish
Sep 30      50   25                    100    blew up off Pender    Salish
Sep 30      50   30                    100    *complete*            Salish
Oct 1-2     50   25                    100    blew up BP Mouth      Salish
Oct 1       50   30                    100    *complete*            Salish
Oct 2-3     50   30                    100    *complete*            Salish
Oct 4-5     50   30                    100    blew up off Stuart    Salish
Oct 4-5     50   35                    100    *complete*            Salish
Oct 6-9     50   35                    100    blew up BP Mouth      Jasper
Oct 6-7     50   40                    100    *complete*            Jasper   first 2 days of 4
Oct 8-9     50   40                    100    blew up NaN           Jasper   last 2 days of 4
Oct 8-9     50   45                    100    *complete*            Jasper   first 2 days of 4
Oct 10-11   50   45                    100    blew up off Pender    Jasper   last 2 days of 4
Oct 10-13   50   50                    100    blew up off San J.    Jasper
Oct 10-13   50   50                    150    *complete*            Jasper
Oct 14-17   50   50                    150    *complete*            Jasper   first 4 days of 10
Oct 18-23   50   50                    150    blew up off Pender    Jasper   last 6 days of 10
Oct 18-25   50   50                    200    *complete*            Jasper   first 8 days of 10
Oct 26-27   50   50                    200    blew up off Pender    Jasper   last 2 days of 10
Oct26-Nov4  50   55                    200    blew up off Pender    Jasper   blew before prev.
Oct 26      10   20                     20    *complete*            Salish
Oct 27-30   10   20                     20    *complete*            Jasper
Oct 31      10   20                     20    *complete*            Jasper   opened North
Nov 1-4     10   20                     20    *complete*            Jasper
Nov 5-14    10   20                     20    *complete*            Jasper
Nov 15-19   10   20                     20    *complete*            Jasper
Nov 20-24   10   20                     20    timed-out             Jasper
Nov 20-29   10   20                     20    *complete*            Jasper
Nov30-Dec9  10   20                     20    *complete*            Jasper
Dec 10-19   10   20                     20    *complete*            Jasper
Dec 20-31   10   20                     20    timed-out             Jasper
Dec 20-25   10   20                     20    *complete*            Jasper   fixed North tides
Dec 26-31   10   20                     20    *complete*            Jasper
Jan 1-5     10   20                     20    *complete*            Jasper
Jan 6-10    10   20                     20    *complete*            Jasper
Jan 11-20   10   20                     20    *complete*            Jasper   5d restart files
Jan 21-30   10   20                     20    *complete*            Jasper   5d restart files
Jan31-Feb9  10   20                     20    *complete*            Jasper
Feb 10-19   10   20                     20    *complete*            Jasper   fixed West bt vels
Feb20-Mar1  10   20                     20    *complete*            Jasper
Mar 2-11    10   20                     20    *complete*            Jasper
Mar 12-21   10   20                     20    *complete*            Jasper
Mar 22-31   10   20                     20    *complete*            Jasper
Apr 1-10    10   20                     20    *complete*            Jasper
Apr 11-20   10   20                     20    *complete*            Jasper
Apr 21-30   10   20                     20    *complete*            Jasper
May 1-10    10   20                     20    *complete*            Jasper
May 11-20   10   20                     20    *complete*            Jasper
May 21-30   10   20                     20    *complete*            Jasper
May31-Jun9  10   20                     20    *complete*            Jasper
Jun 10-19   10   20                     20    *complete*            Jasper
Jun 20-29   10   20                     20    *complete*            Jasper
Jun30-Jul9  10   20                     20    *complete*            Jasper
Jul 10-19   10   20                     20    *complete*            Jasper
Jul 20-29   10   20                     20    *complete*            Jasper
Jul30-Aug8  10   20                     20    *complete*            Jasper
9-18 Aug    10   20                     20    *complete*            Jasper
19-28 Aug   10   20                     20    *complete*            Jasper
Aug29-Sep7  10   20                     20    *complete*            Jasper
8-17 Sep    10   20                     20    *complete*            Jasper
18-27 Sep   10   20                     20    *complete*            Jasper
28Sep-7Oct  10   20                     20    *complete*            Jasper   flux corr N tides
8-17 Oct    10   20                     20    *complete*            Jasper
18-27 Oct   10   20                     20    *complete*            Jasper
Oct28-Nov6  10   20                     20    *complete*            Jasper
7-16 Nov    10   20                     20    *complete*            Jasper   5d restart files
17-26 Nov   10   20                     20    *complete*            Jasper   5d restart files
Nov27-Dec6  10   20                     20    *complete*            Jasper   5d restart files
7-16 Dec    10   20                     20    *complete*            Jasper   5d restart files
17-26 Dec   10   20                     20    finished              Jasper   5d restart files
27-31 Dec   10   20                     20    queued                Jasper   5d restart file
========== ===== ============= =============  ================== =========== ==================


Spin-up Run Workflows
=====================

Run Preparation and Queuing
---------------------------

These are the steps to prepare and queue a spin-up run on :kbd:`jasper.westgrid.ca`:

#. If the CGRF atmospheric forcing files for the period of the run are not already in place on :kbd:`jasper`,
   prepare them.
   Files for the day before the run starts and the day after it finishes are required so that interpolation of forcing values in NEMO works,
   for example:

   .. code-block:: bash

       ssh jasper
       cd MEOPAR/CGRF/
       salishsea get_cgrf 2002-10-03 -d 10

   You will be prompted for a userid and password for the :kbd:`goapp.ocean.dal.ca` rsync server.
   Those credentials can also be supplied in the command via the :kbd:`--user` and :kbd:`--password` options.

#. Create a YAML run description file for the run in the :file:`SS-run-sets/SalishSea/spin-up/` directory.
   That can be done by copying and renaming a previous run file.
   The name pattern for run description files is :file:`SalishSea.ddmmmddmmm.yaml`,
   where :file:`ddmmmddmmm` is the day and month of the first and last days of the run;
   e.g. :file:`SalishSea.23sep2oct.yaml`.
   The 2nd :file:`ddmmm` is omitted for 1 day long runs.

   The values that *must* be set correctly in every new spin-up run description file are:

   * :kbd:`initial conditions` in the :kbd:`forcing` stanza,
     which must be set to the path and file name of the restart file to use as initial conditions for the run,
     typically the last restart file from the previous spin-up run

   * the :file:`namelist.time` file name in the :kbd:`namelists` stanza
     (see below)

   Other namelist file names may also be used to set special conditions for the run.
   In general,
   the namelists from :file:`SS-run-sets/SalishSea/` are used unless there are changes for a particular spin-up run.
   Special condition namelists are created and commited to version control in the :file:`spin-up/` directory.

   A typical spin-up run description file looks like:

   .. code-block:: yaml

       # salishsea command processor run description for Salish Sea case
       #
       # Spin-up run
       #
       # Salish Sea full domain with:
       #   Smoothed JdF mouth bathymetry
       #   S4-1 uniform initial T and S, depth corrected
       #   Open, unstructured western boundary across Strait of Juan de Fuca
       #     Tidal forcing
       #     Masson model, depth corrected, T, S, U & V
       #   Monthly climatology river run-off forcing, all rivers
       #   Atmospheric forcing from CGRF dataset
       #     Atmospheric pressure as inverse sea surface height effect enabled

       config_name: SalishSea

       paths:
         NEMO-code: ../../../NEMO-code/
         forcing: ../../../NEMO-forcing/
         runs directory: ../../../SalishSea/

       grid:
         # If relative, paths are taken from forcing path above
         coordinates: coordinates_seagrid_SalishSea.nc
         bathymetry: bathy_meter_SalishSea2.nc

       forcing:
         # If relative, paths are taken from forcing path above
         atmospheric: ../CGRF/NEMO-atmos/
         initial conditions: ../../../SalishSea/results/spin-up/22sep/SalishSea_00012096_restart.nc
         open boundaries: open_boundaries/
         rivers: rivers/

       namelists:
         - namelist.time.23sep24sep
         - ../namelist.domain
         - ../namelist.surface
         - ../namelist.lateral
         - ../namelist.bottom
         - ../namelist.tracers
         - namelist.dynamics.nu55evd100  # 23sep24sep run only
         - ../namelist.compute.6x14

#. Create a :file:`namelist.time` file for the run in the :file:`SS-run-sets/SalishSea/spin-up/` directory.
   That can be done by copying and renaming a previous run file.
   The name pattern for run description files is :file:`namelist.time.ddmmmddmmm`,
   where :file:`ddmmmddmmm` is the day and month of the first and last days of the run;
   e.g. :file:`namelist.time.23sep2oct`.
   The 2nd :file:`ddmmm` is omitted for 1 day long runs.

   The values that *must* be set correctly in every new spin-up run :file:`namelist.time` file are:

   * :kbd:`nn_it000`: the first time step for the run,
     typically 1 greater than the final time step of the previous run that is included in the name of the restart in the run description file

   * :kbd:`nn_itend`: the final time step for the run,
     :kbd:`nn_it000 + days * 8640 - 1`,
     where days is the run duration in days

   * :kbd:`nn_date0`: the date when :kbd:`nn_it000` was 1;
     i.e. :kbd:`20021026` *Note that this convention changes in NEMO 3.6*

   * :kbd:`nit000_han`: the first time step for tidal harmonic analysis,
     typically the same value as :kbd:`nn_it000`

   * :kbd:`nitend_han`: the final time step for tidal harmonic analysis,
     typically the same value as :kbd:`nn_itend`

   Also ensure that :kbd:`ln_rstart` is set to :kbd:`.true.`

   A typical :file:`namelist.time` file looks like:

   .. code-block:: fortran

        !! Run timing control
        !!
        !! *Note*: The time step is set in the &namdom namelist in the namelist.domain
        !!         file.
        !!
        &namrun        !   Parameters of the run
        !-----------------------------------------------------------------------
           cn_exp      = "SalishSea"  ! experience name
           nn_it000    =      302401  ! first time step
           nn_itend    =      388800  ! last time step (std 1 day = 8640 re: rn_rdt in &namdom)
           nn_date0    =    20021026  ! date at nit_0000 = 1 (format yyyymmdd)
                                      ! used to adjust tides to run date (regardless of restart control)
           nn_leapy    =       1      ! Leap year calendar (1) or not (0)
           ln_rstart   =  .true.      ! start from rest (F) or from a restart file (T)
           nn_rstctl   =       2      ! restart control => activated only if ln_rstart = T
                                      !   = 0 nn_date0 read in namelist
                                      !       nn_it000 read in namelist
                                      !   = 1 nn_date0 read in namelist
                                      !       nn_it000 check consistency between namelist and restart
                                      !   = 2 nn_date0 read in restart
                                      !       nn_it000 check consistency between namelist and restart
           nn_istate   =       0      ! output the initial state (1) or not (0)
           nn_stock    =    43200      ! frequency of creation of a restart file (modulo referenced to 1)
           ln_clobber  =  .true.      ! clobber (overwrite) an existing file
        &end

        &nam_diaharm   !   Harmonic analysis of tidal constituents ('key_diaharm')
        !-----------------------------------------------------------------------
            nit000_han = 302401  ! First time step used for harmonic analysis
            nitend_han = 388800  ! Last time step used for harmonic analysis
            nstep_han  =     90  ! Time step frequency for harmonic analysis
            !! Names of tidal constituents
            tname(1)   = 'K1'
            tname(2)   = 'M2'
        &end

#. Create any special condition namelist files and ensure that they are correctly included in the :kbd:`nameslists` stanza of the run description file.

#. Choose or create an :file:`iodef.xml` file for the run.
   The name pattern for :file:`iodef.xml` files is :file:`iodef.nnt.xml`,
   where :file:`nn` is the frequency of output of the :file:`*_grid_[TUV].nc` files,
   and :file:`t` is the output interval;
   e.g. :file:`iodef.1d.xml`.

#. Create or update a TORQUE batch job file for the run.
   The name pattern for batch job files is :file:`SalishSea.nnd.pbs`,
   where :file:`nn` is the duration of the run in days;
   e.g. :file:`SalishSea.10d.pbs`.

   The values that *must* be set correctly for every job are:

   * The :file:`ddmmmddmmm` part in the following lines:

     * :kbd:`#PBS -N`: the job name
     * :kbd:`#PBS -o`: the path and name for stdout from the job
     * :kbd:`#PBS -e`: the path and name for stderr from the job
     * :kbd:`RESULTS_DIR=`: the path and name of the results directory where the run results are to be gathered

   * The :kbd:`walltime` limit;
     e.g.

     .. code-block:: bash

         #PBS -l walltime=16:00:00

     Runs typically required about 80 minutes of compute time per model-day if they allocated to the fast (X5675) nodes on jasper.
     However,
     if a run is allocated to the slow (L5420) nodes on jasper it can take nearly 180 minutes of compute time per model-day.
     It appears to be more advantageous to request that only fast nodes be used rather than requesting sufficient run walltime to allow runs to complete on the slow nodes.
     The directive to request only fast nodes is:

     .. .. code-block:: bash

         #PBS -l feature=X5675

     Wall time values that have been found to be adequate are 3h for a 2d run,
     8h for a 5d run, and 16h for a 10d run.

     You should also set your email address in the :kbd:`#PBS -M` line so that job start,
     end,
     and abort messages are emailed to you.

   A typical TORQUE batch job file looks like:

   .. code-block:: bash

       #!/bin/bash

       #PBS -N SpinUp26oct4nov
       #PBS -S /bin/bash
       #PBS -l procs=84
       # memory per processor
       #PBS -l pmem=2gb
       #PBS -l walltime=16:00:00
       #PBS -l feature=X5675
       # email when the job [b]egins and [e]nds, or is [a]borted
       #PBS -m bea
       #PBS -M sallen@eos.ubc.ca
       #PBS -o ../SpinUp26oct4nov/stdout
       #PBS -e ../SpinUp26oct4nov/stderr


       RESULTS_DIR=../SpinUp26oct4nov

       cd $PBS_O_WORKDIR
       echo working dir: $(pwd)

       module load application/python/2.7.3
       module load library/netcdf/4.1.3
       module load library/szip/2.1

       echo "Starting run at $(date)"
       mkdir -p $RESULTS_DIR
       mpirun ./nemo.exe
       echo "Ended run at $(date)"

       echo "Results gathering started at $(date)"
       $PBS_O_HOME/.local/bin/salishsea gather --no-compress SalishSea*.yaml $RESULTS_DIR
       chmod go+rx $RESULTS_DIR
       chmod go+r  $RESULTS_DIR/*
       echo "Results gathering ended at $(date)"

       echo "Scheduling cleanup of run directory"
       echo rmdir $PBS_O_WORKDIR > /tmp/SpinUp26oct4nov_cleanup
       at now + 1 minutes -f /tmp/SpinUp26oct4nov_cleanup 2>&1

#. Commit and push the run set file changes for each run prior to queuing the run so that there is a clear record of runs in the :ref:`SS-run-sets-repo` repo.
   Don't forget to add any files created for a run to the repo.

#. Prepare the run,
   copy the TORQUE batch job file to the run directory,
   go to the run directory,
   and submit the job to the scheduler:

   .. code-block:: bash

       salishsea prepare SalishSea.23sep24sep.yaml iodef.1d.xml
       cp SalishSea.2d.pbs ../../../SalishSea/bb1357d6-8c6e-11e3-bdd0-0025902b0cdc
       pushd ../../../SalishSea/bb1357d6-8c6e-11e3-bdd0-0025902b0cdc
       qsub SalishSea.2d.pbs
