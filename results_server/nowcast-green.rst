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

21-Mar-2016  - Linked biology solar radiation to physical |br|
               heating
             - Changed trb->trn in p4zsbc for rivers |br|
               when value not specified
             - corrected p4zopt - commented out line |br|
               adjusting k_PAR based on uninitialized river |br|
               flow parameter
             - changed p4zopt nksrp to 35 (z=307m)
             - removed zz_rate_*_Si_ratio from p4zmort |br|
               (and from p4zmort namelist) and instead use |br|
               the value set in p4zprod
             - added MESZ (mesozooplankton) as output |br|
               variable
             - Updated NEMO code to NEMO-3.6r5912+ |br|
               changeset 421738d4896d_
             - Corrected red/blue extinction |br|
               coefficients calculation |br|
               changeset 7f8414960de2_
             - changed to Orlanski boundary conditions for |br|
               baroclinic velocity
             - changed to downbyone2 bathymetry with a |br|
               smoothed mouth at both north and west |br|
               boundaries
             - First day time step, 20 s |br|

22-Mar-2016  Second day time step                                 30 s

23-Mar-2016  Back to full time step                               40 s

06-May-2016  Blew up at 40 s, 30 s, ran at 20s                    20 s

07-May-2016  Second day time step (in case)                       30 s

08-May-2106  Back to full time step                               40 s

06-Jun-2016  Blew at 40 s, ran at 30 s                            30 s

07-Jun-2016  Back to full time step                               40 s

19-Jun-2016  Blew at 40 s, ran at 30 s                            30 s

20-Jun-2016  Back to full time step                               40 s

23-Jun-2016  Blew at 40 s, ran at 30 s                            30 s

24-Jun-2016  Back to full time step                               40 s

20-Jul-2016  Ran as nowcast on orcinus

22-Jul-2016  Salish automation started

20-Aug-2016  Updated NEMO code to NEMO-3.6r6036+                                  a9d5c04f7fea_

07-Sep-2016  Changed west & north temperature & salinity |br|
             boundary conditions to TEOS-10 conservative |br|
             temperature and reference salinity

11-Sep-2016  - Changed to new tides (N36_J) |br|                   |br|
             - Corrected bottom formulation so friction |br|       |br|
               in very shallow water stays high |br|               |br|
               maximum bottom friction |br|                        2 |br|
               minimum bottom friction |br|                        1.2e-2 |br|
               bottom roughness  |br|                              0.25 m |br|

03-Oct-2016  Changed river input to conservative |br|              |br|
             temperature, set ln_useCT on |br|                     .true. |br|

14-Oct-2016  Reverse barometer is in our Neah Bay ssh |br|         |br|
             set ln_apr_obs off |br|                               .false. |br|

26-Oct-2016  Updated NEMO code to NEMO-3.6r6204+                                  d39437beec6c_

20-Nov-2016  Updated NEMO code to NEMO-3.6r6459+ |br|                             a79d64786baa_
             Change to rebuild_nemo that writes |br|
             deflated netcdf4/hdf5 files directly.                                f9a8d03de741_

10-Jan-2017  Updated NEMO code to NEMO-3.6r6770+                                  3b0accb0e693_

25-Apr-2017  Increase lateral viscosity and diffusivity            2.2 m2/s       2c892a5bae80_

29-Apr-2017  Restored missing light attenuation with depth term                   fd69c1dc91be_

25-Aug-2017  Changed to v201702 bathymetry and parameters
             (see :ref:`HindcastResults` for details).
             Restart files from :file:`hindcast/24aug17`.
             *nowcast-green is now the daily "run of record".*

07-Oct-2017  Updated bathymetry to correct smoothing in Puget
             Sound, etc.

30-Dec-2017  New LiveOcean boundary condition processor with
             longer western boundary and NO3 from LiveOcean

18-Apr-2018  Reduce bSi and PON sinking rates                     5.6e-5 m/2      e9a5bc834f46_

25-Apr-2018  Change rivers climatology PON and DON to be based
             on model-derived profiles
===========  ===================================================  ==============  ==================

.. _e927e26ebe34: https://bitbucket.org/salishsea/ss-run-sets/commits/e927e26ebe34
.. _71946bd297a4: https://bitbucket.org/salishsea/ss-run-sets/commits/71946bd297a4
.. _421738d4896d: https://bitbucket.org/salishsea/NEMO-3.6-code/commits/421738d4896d
.. _7f8414960de2: https://bitbucket.org/salishsea/NEMO-3.6-code/commits/7f8414960de2
.. _a9d5c04f7fea: https://bitbucket.org/salishsea/NEMO-3.6-code/commits/a9d5c04f7fea
.. _d39437beec6c: https://bitbucket.org/salishsea/nemo-3.6-code/commits/d39437beec6c
.. _a79d64786baa: https://bitbucket.org/salishsea/nemo-3.6-code/commits/a79d64786baa
.. _f9a8d03de741: https://bitbucket.org/salishsea/nemo-3.6-code/commits/f9a8d03de741
.. _3b0accb0e693: https://bitbucket.org/salishsea/nemo-3.6-code/commits/3b0accb0e693
.. _2c892a5bae80: https://bitbucket.org/salishsea/ss-run-sets/commits/2c892a5bae80
.. _fd69c1dc91be: https://bitbucket.org/salishsea/nemo-3.6-code/commits/fd69c1dc91be
.. _e9a5bc834f46: https://bitbucket.org/salishsea/ss-run-sets/commits/e9a5bc834f46
