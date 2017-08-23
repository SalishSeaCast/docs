.. _HindcastResults:

*****************************
Hindcast 201702 Model Results
*****************************

:file:`/results/SalishSea/hindcast/` holds the results from the daily hindcast ocean runs.  Note that runs were typically done 10 days at a time.  The first day directory includes the configuration etc files.  The last day includes restarts from day 5 and day 10.

These runs started 12-Sep-2014 using physical and biological initial conditions from the nowcast-green run at 11-Sep-2016.

Initial run used:

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
===========  ===================================================  ==============  ==================

