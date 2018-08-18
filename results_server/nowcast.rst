.. _NowcastResults:

***************************
Nowcast Model Results
***************************

:file:`/results/SalishSea/nowcast-green/` holds the results from the daily nowcast green ocean runs.

From 12-Sep-2014 until 24-Aug-2017 these results were produced from a hindcast (201702).  From the 25-Aug-2017 on, these results come from the daily nowcast, started on the 24-Aug-2017 hindcast result.

Note that hindcast runs were typically done 10 days at a time.  The first day directory includes the configuration etc files.  The last day includes restarts from day 5 and day 10.

These runs started 12-Sep-2014 using physical and biological initial conditions from the previous nowcast-green run at 11-Sep-2016.


* NEMO-3.6
* coordinates v201702 (expanded resolution for Fraser River)
* bathymetry v201702 (based on DFO, Cascadia and ABC, included datum and extended Fraser River)
* jetty with enhanced bottom friction over it
* R201702 rivers (daily Fraser, climatology else)
* river biology and temperature (all the same, based on Fraser)
* N36_AF tides
* TEOS-10 and reworked Juan de Fuca TEOS-10 boundary conditions
* SMELT biological model
* passive turbidity
* Hollingsworth + energy and enstrophy conserving
* 5-step substepping vertical advection
* Orlanski with sponge open boundaries on baroclinic T&S
* decreased horizontal diffusivity (1 m2/s) and viscosity (1.1 m2/s)
* horizontal diffusivity (so can increase values for sponge near boundaries)
* decreased bottom friction (7 cm bottom roughness)

Model Parameter Changes Over Time
=================================

.. |br| raw:: html

    <br>

===========  ===================================================  ==============  ==================
Date                       Change                                 New Value       Changeset
===========  ===================================================  ==============  ==================
12-Sep-2014   1st run results                                      N/A
28-Dec-2014   Updated Fraser turbidity tracer code and reset       N/A
31-Aug-2015   accidently switched to rivers for downbyone bathy    N/A
05-Sep-2015   Increased vertical time stepping nn_traadv_tvd_zts   20
05-Feb-2017   Switched back to correct rivers                      N/A
05-Feb-2017   Switched to LiveOcean boundary conditions at JdF     N/A

25-Aug-2017   Started running as nowcast

07-Oct-2017   Updated bathymetry to correct smoothing in |br|
              Puget Sound, etc.

30-Dec-2017   New LiveOcean boundary condition processor |br|
              with longer western boundary and NO3 from |br|
              LiveOcean

18-Apr-2018   Reduced bSi and PON sinking rates                    5.6e-5 m/2      e9a5bc834f46_

25-Apr-2018   Change boundary PON and DON climatologies |br|
              to be based on model-derived profiles

28-Apr-2018   Changed phytoplankton growth temperature |br|
              dependence parameter values                                          f253dfb8277b_

19-Jun-2018  Apply Neah Bay ssh to north boundary as well |br|                     6b89d1af1aec_
             as west boundary

16-Aug-2018  Reflect part of the PON/DON from the bottom |br|                      d1017236fff0_
             boundary

16-Aug-2018  Change Orlanksi boundary conditions to |br|                           7c6a30e8bbf0_
             caculate along boundary wave on the boundary |br|
             not one grid cell in.
===========  ===================================================  ==============  ==================


.. _e9a5bc834f46: https://bitbucket.org/salishsea/ss-run-sets/commits/e9a5bc834f46
.. _f253dfb8277b: https://bitbucket.org/salishsea/ss-run-sets/commits/f253dfb8277b
.. _6b89d1af1aec: https://bitbucket.org/salishsea/ss-run-sets/commits/6b89d1af1aec
.. _d1017236fff0: https://bitbucket.org/salishsea/nemo-3.6-code/commits/d1017236fff0
.. _7c6a30e8bbf0: https://bitbucket.org/salishsea/nemo-3.6-code/commits/7c6a30e8bbf0

