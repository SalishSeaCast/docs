Notes on a few Quirks we have found with NEMO
=============================================

These issues could perhaps be called bugs, but more than anything they take time to understand.  So hopefully this will save someone some time!


Monthly (and Yearly) Averaging
------------------------------

Subroutine fldread.F90, NEMO v3_4_STABLE: Registered as trac issue #1201 and corrected fldread.F90
available there.

For monthly averaging, ztmp is initialized as a fractional part of a month, but inside the IF statement,
it is added to in seconds.

I believe a similar error occurs in the monthly averaging.


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
