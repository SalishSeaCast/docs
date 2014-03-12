Stability: Introduction
=======================

As of January 2014, the code was running, but during the spin-up runs and other tests it became obvious that the code was only marginally stable.  We increased the vertical mixing coefficient under static-instability, which helped greatly, but we still needed to run with larger horizontal viscosity than we felt was appropriate.  Code is stable with horizontal viscosity = 100 m2/s but is not stable with 50 m2/s.  We felt 20 m2/s, like they are using on the East Coast, would be better.

So the two choices appeared to be:
a) increase the viscosity or
b) smooth the bathymetry

As the OSM a talk by Lemarie suggested that small scale codes are often stability limited by the vertical advection. 
So I wished to evaluate:

1) our velocities, are they as large as we expect and
2) is the code subject to a vertical CFL condition and if so, are we meeting it.
