Notes on NEMO 3.4 Bugs and Quirks We Have Found
===========================================

The issues described here are clearly bugs in some cases,
and in other cases perhaps just things about NEMO that take time to understand.
In either case,
we hope that this page this will save someone some time!


Monthly (and Yearly) Averaging
------------------------------

Subroutine fldread.F90, NEMO v3_4_STABLE: Registered as trac issue #1201 and corrected fldread.F90
available there.

For monthly averaging, ztmp is initialized as a fractional part of a month, but inside the IF statement,
it is added to in seconds.

I believe a similar error occurs in the yearly averaging.


Values go NaN
-------------

If you have unstable  conditions and the values go NaN before your zonal velocities get greater than
20 m/s, the code will not stop.  It will continue on calculating.  In ocean.output Umax will be
written out as 0 and minimum salinity will be written out as 100.

I have added NaN checking to our version of stpctl.F90 because if the code has gone unstable we don't
want to waste the time waiting for completion.  It probably slows the code down, so I don't suspect
everyone will want to add it.

Email sallen@eos.ubc.ca if you want this code.


Isolated Ocean Grid Points Suppresion Report
--------------------------------------------

The list of isolated ocean grid points that is suppressed by :file:`OPA_SRC/DOM/domzgr.F90:zgr_bat_ctl()`
(i.e. single grid point bumps or holes in the bathymetry that are changed to match the level of adjacent grid points)
is only output to :file:`ocean.output` when the code is run on a single processor.
Otherwise,
it appear that only the isolated points (if any) on the MPI sub-domain on processor 0 are shown in :file:`ocean.output`;
for the Salish Sea domain that is none.


Values for Barotropic Boundary Conditions must be Interpolated
--------------------------------------------------------------

If you are using Tides + External Information on boundary values (nn_dyn2d_dta   =  3), you must turn interpolation on for the barotropic files read.  If you do not, the code will add the tides onto the previous value (of tides + external).  If you do use interpolation, the code will recalculate the external value and then add the tides.  So all is good.


.. _nn_date0-quirk:

Value and Use of :kbd:`nn_date0`
--------------------------------

The comment for the :kbd:`nn_date0` varaible in the :kbd:`namrun` in the AMM12 namelist
(and probably other configurations too)
from the SVN repo is very inaccurate::

  ! date at nit_0000 (format yyyymmdd) used if ln_rstart=F or (ln_rstart=T and nn_rstctl=0 or 1)

In models where tidal forcing is used via :kbd:`key_bdy` and the :kbd:`nambdy_tide` namelist,
the value of :kbd:`nn_date0` is used to adjust the tidal forcing to the timeframe of the run
(see :kbd:`Correcting tide for date:` in the :file:`ocean.output` file).
That is the case *regardless* of the values of :kbd:`ln_rstart` and :kbd:`nn_rstctl`.
So,
the value of :kbd:`nn_date0` must be set to the day on which :kbd:`nn_it000 = 1` for the run *even if the initial conditions are being supplied from a restart file*.

We have revised our namelists to say::

  ! date at nit_0000 = 1 (format yyyymmdd)
  ! used to adjust tides to run date (regardless of restart control)

Also note that NEMO will accept and use some nonsensical values for :kbd:`nn_date0`.
For example,
:kbd:`nn_date0 = 200209`
(note that the day digits have been truncated)
will result in tidal forcing being adjusted to a
(biblical?)
date of 9-Feb-20!


Salinity Extrema
----------------

Similar to others that have found extreme low temperatures in ORCA-LIM (see Trac Ticket 1180) we are getting extreme salinity values at depth, strongest near the Victoria Sill.

Reminder that we are running dev_v3_4_STABLE_2012
with vvl, partial z cells, tvd advection.

The advection scheme is not taking into account the partial cells, as
shown in Fig 5.5 in the NEMO 3.4 book, but the Laplacian diffusion is.  If we turn off the
partial cell correction in Laplacian diffusion we improve things by about
a factor of 10, but do not eliminate the problem.

For the deep Strait of Georgia it will be important to have the partial cell
correction in the diffusion.  Our plan is to keep that turned on and just to keep an eye on those
extreme salinities as we go through a year of spin-up.


Possible Memory Leak in :kbd:`timing.F90`
-----------------------------------------

Several multi-day duration spin-up runs were terminated by the resource manager on :kbd:`jasper` after 2000+ time steps because they substantially exceeded the per-processor memory requested for the jobs.
Increasing the per-processor memory allowed the job to run longer but always resulted in termination for the same reason
By process of elimination it was found that the issue arose when the value of the :kbd:`nn_timing` variable in the :kbd:`&namctl` namelist was set to :kbd:`1` but that runs were successful,
with normal memory usage,
when the value was set to :kbd:`0`.

A quick inspection of the :kbd:`timing.F90` code reveals that there is memory allocation and pointer arithmetic in some of its subroutines,
so a bug that is leaking memory is plausible.
No deeper investigation was undertaken.

It is recommended that profiling be disabled by setting :kbd:`nn_timing = 0` for all but short development runs lasting no longer than a few hundred time steps.

The problem has not been observed on :kbd:`salish`,
probably due to its less strict resource management.
However,
long runs with profiling enabled can be expected to fail when they exhaust machine memory on :kbd:`salish`.
