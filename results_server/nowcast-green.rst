.. _NowcastGreenResults:

***************************
Nowcast Green Model Results
***************************

:file:`/results/SalishSea/nowcast-green/` holds the results from the daily nowcast green ocean runs.

Those runs started 5-Dec-2015 using physical initial conditions from spin-up run at 1-Dec-2002 and biological initial conditions from observations on Oct ?, ????.

Initial run used:

* NEMO-3.6
* Bathymetry #6 (extended Fraser River)
* TS4 tides
* SOG biological model
* practical salinity
* Hollingsworth + energy and enstrophy conserving

Updates needed include:

* should increase rn_shlat to 0.5
* switch salinity to absolute salinity (TEOS-10)


Model Parameter Changes Over Time
=================================

.. |br| raw:: html

    <br>

===========  ===================================================  ==============  ==================
Date                       Change                                 New Value       Changeset
===========  ===================================================  ==============  ==================
5-Dec-2015   1st run results                                      N/A
8-Dec-2015   Nutrients went NaN                                   N/A
12-Dec-2015  Added western sea surface height boundary |br|       N/A
             conditions
12-Dec-2015  Added reversed barometer to sea surface height       N/A
12-Dec-2015  Added relative winds/currents (:kbd:`rn_vfac`)       1
12-Dec-2015  Added daily and tidal output files                   N/A
12-Dec-2015  Biology restart 07Dec2015 file (biology starting)    07-Dec-2015
13-Dec-2015  Repeatedly using initial file (biology starting)     original
15-Dec-2015  One day only: no weather available, used |br|        N/A             N/A
             14-Dec-2015 forecast
17-Dec-2015  Decrease baroclinic time step (:kbd:`rdt`)           30 s
18-Dec-2015  Restore baroclinic time step (:kbd:`rdt`)            40 s
18-Dec-2015  Hack sub-stepping passive tracers |br|               N/A
             (vertical advection tracers)
19-Dec-2015  Use restart file from prev. day (biology restart)    N/A
22-Dec-2015  Elise's proper sub-stepping |br|                     N/A
             (vertical advection tracers)
23-Dec-2015  use biology light in physics model |br|              True
             (:kbd:`ln_qsr_bio`)
24-Jan-2016  Add vertical eddy viscosity & diffusion |br|         see changesets  e927e26ebe34_ |br|
             coefficients to :file:`*grid_W.nc` output |br|
             files. |br|
             Remove snowfall rate from :file:`*_grid_T.nc` |br|                   71946bd297a4_
             output files.

07-Feb-2016  Restart biology and physics |br|                     |br|
             Use deepened grid. |br|                              |br|
             Use N36_D tides |br|                                 |br|
             Reduce minimum vertical diffusivity |br|             1e-6 |br|
             Reduce lateral viscosity |br|                        2 |br|
             Reduce lateral diffusivity |br|                      2 |br|
             Increase rn_shlat |br|                               0.5 |br|
             Switch to TEOS10, Relative Salinity |br|             |br|
             Bottom Friction, drag coefficient up |br|            1.25e-2 |br|
             Bottom roughness up |br|                             0.28 |br|
             First day time step |br|                             20 s |br|
             Corrected biology code |br|                          |br|
             Note we reran from here to correct biology etc |br|  |br|
             running in hindcast mode

08-Feb-2016  Second day time step                                 30 s

09-Feb-2016  Back to full time step                               40 s

21-Mar-2016  Linked biology solar radiation to physical heating

26-Apr-2016  In restart file of 25apr made Oxygen <= 180

03-May-2016  Reset Oxygen <=180 and Nitrate <= 30.45

12-May-2016  Reset Oxygen and Nitrate

31-May-2016  changed trb->trn in p4zsbc for rivers |br| 
             when value not specified 

03-Jun-2016  ran as nowcast on orcinus

04-Jun-2016  started running as nowcast on Salish
===========  ===================================================  ==============  ==================

.. _e927e26ebe34: https://bitbucket.org/salishsea/ss-run-sets/commits/e927e26ebe34
.. _71946bd297a4: https://bitbucket.org/salishsea/ss-run-sets/commits/71946bd297a4
