.. _OBC:

************************
Open Boundary Conditions
************************

NEMO separates open boundary conditions into four parts.

* Tides: see tide description elsewhere

* T & S : Temperature and Salinity fields.  For these we are using the built-in FRS system.  This is a multi-grid point forcing toward the specified boundary conditions with the forcing equal to one at the boundary and decreasing as a tanh from there.

* Baroclinic velocities: We are using the time-splitting scheme so barotropic and baroclinic velocities are set separately.

* Barotropic velocities and SSH

Currently open boundaries are only used on the western boundary.  It would be good to include T&S boundary conditions on the northern boundary.  Many updates for 2 boundaries occured between stable and current on 3-4.  We should update before implementing.

Temperature and Salinity
========================

We are using the FRS scheme with a 10-grid point rim.  Initial values come from a quick fit to Thomson et al.  These use identical values on the 10 grid points and uniform cross-strait values.  Are new set of values are from Masson and Fine (2013), with different values on the 10 grid points, and with a seasonal cycle, changing weekly.

Baroclinic Velocities
=====================

We have implemented zero-gradient boundary conditions.  These appeared to work well provided the temperature and salinity changes were started weakly.  However, when attempting a 40-day run, these conditions blew at about day 9.5 with an error at i=392, k=12.  We smoothed the near boundary topography identical in j (outward from the boundary for 6 grid points) and smoothed it.  Now we are running well beyond 50 days.

Velocities set at #2 grid points because #1 points are not used by BDY (set by OBC)

Barotropic Velocities & SSH
===========================

Built in Flather boundary conditions take the tides (ssh and velocity) and add them to barotropic forcing (ssh and velocity).  These conditions are implemented only on the barotropic velocities directly but affect the ssh indirectly.  We are setting ssh based on Tofino measurements.  We are no forcing barotropic velocities but allowing any "waves" set inside the Strait to propagate out (which is what Flather conditions do).

Velocities set at #2 grid points.
