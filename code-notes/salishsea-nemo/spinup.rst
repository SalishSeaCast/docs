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


23-Sep-2002 to 2-Oct-2002
-------------------------

The next 10 days of spin-up were run with the bottom turbulent kinetic energy background
(NEMO :kbd:`nambfr` namelist variable :kbd:`rn_bfeb2`)
set to :math:`1 \times 10^{-4} m^2/s^2`.
This was done to investigate the effect of reducing turbulent bottom friction on amplitudes of the M2 tidal harmonics.
Harmonics for the M2 and K1 tides were calculated over the entire duration of this run with a resolution of 9 time steps
(450 seconds).
The results during this period also include sea surface height at selected locations for analysis of storm surges,
and sea surface height and profiles of temperature,
salinity,
and u anv v velocity components at 6 points along the thalweg and at a location in the Fraser River plume.
