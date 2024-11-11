.. _Lateral:

Lateral Dynamics
================
The lateral dynamics are controlled in the :file:`namelist.dynamics`. We have decided to experiment with the lateral eddy viscosity :file:`rn_ahm_0_lap`, the biharmonic operator :file:`ln_dynldf_bilap`, and the lateral boundary conditions for momentum set in the :file:`namelist.lateral`.

Lateral Eddy Viscosity
--------------------------------------
The lateral eddy viscosity controls how dissipative the simulation is. Since we are forcing tides at the Juan de Fuca boundary we would like the visocisty to be as small as possible in order to prevent tidal phase lags within the Strait of Georgia. We have had some success at reducing the lateral viscosity as outlined in the table below.

+------------------+---------------------+--------------------+------------------+-----------------------------+
| Simulation       |:file:`rn_ahm_0_lap` | :file:`ln_apr_dyn` | :file:`rn_avevd` |Notes                        |
+==================+=====================+====================+==================+=============================+
|:math:`\nu=100`   | 100                 | true or false      | 10               |stable                       |
+------------------+---------------------+--------------------+------------------+-----------------------------+
|:math:`\nu=60`    | 60                  | false              | 10               |stable                       |
+------------------+---------------------+--------------------+------------------+-----------------------------+
|:math:`\nu=60`    | 60                  | true               | 10               |unstable near islands        |
+------------------+---------------------+--------------------+------------------+-----------------------------+
|:math:`\nu=60`    | 60                  | true               | 100              |stable                       |
+------------------+---------------------+--------------------+------------------+-----------------------------+
|:math:`\nu=50`    | 50                  | false              | 10               |unstable                     |
+------------------+---------------------+--------------------+------------------+-----------------------------+
|:math:`\nu=40`    | 40                  | true               | 60               |stable                       |
+------------------+---------------------+--------------------+------------------+-----------------------------+
|:math:`\nu=40`    | 40                  | true               | 50               |unstable near islands        |
+------------------+---------------------+--------------------+------------------+-----------------------------+

These simulations were run on :file:`salish` with model time seven days. The boolean :file:`ln_apr_dyn` in :file:`namelist.surface` controls forcing at the free surface due to atmospheric pressure. We are having difficulty with stability when this is set to true and the viscosity is lower than 100. Unfortunately, this type of forcing is likely important when modelling storm surges.

The parameter :file:`rn_avevd` in :file:`namelist.dynamics` controls the vertical eddy viscosity only in areas where/when the stratification is statically unstable. When the model is presented with an unstable stratification it locally increases the amount of vertical diffusion which effectively mixes the unstable region (see :ref:`Vertical`). Through our analysis, we have observed that the stability of the model at lower viscosities depends on how it treats vertical mixing. Thus, this parameter is likely important for achieving stability at low lateral viscosity. This must be investigated further.

We are still having trouble with stability during our spin up runs. See :ref:`spin-up`.


Biharmonic Operator
------------------------------------------
The biharmonic operator dissipates energy selectively at smaller scales. It is a fourth order diffusive operator in the momentum equations and is chosen by modifiying :file:`ln_dynldf_bilap` in :file:`namelist.dynamics`. It can be used in conjuction with the second order laplacian operator. The AMM configuration employs the bilaplacian with :file:`rn_ahm_0_blp=-1e10` and :math:`\nu=60` for the laplacian operator. Note that AMM also uses free slip lateral boundary conditions and s-coordinates.

The biharmonic operator can be used in conjunction with the second order laplacian operator. Under the current resolution, typical values for the operator coefficient should be around :file:`rn_ahm_0_blp=-2000`. Decreasing the magnitude of this parameter has some stablizing effect with little change in the maximum currents. However, the simulations with this operator in use still display overturning and poor behaviour in the vertical salinity profiles. A summary of simulations is given below.

+------------------+---------------------+---------------------+------------------+--------------------------------+
| Simulation       |:file:`rn_ahm_0_lap` |:file:`rn_ahm_0_blp` | :file:`rn_avevd` |Notes                           |
+==================+=====================+=====================+==================+================================+
|apr60_nu50        | 50                  | none                | 60               |unstable near islands           |
+------------------+---------------------+---------------------+------------------+--------------------------------+
|apr60_nu50_bi2000 | 50                  | -2000               | 60               |unstable near islands           |
+------------------+---------------------+---------------------+------------------+--------------------------------+
|apr60_nu50_bi4000 | 50                  | -4000               | 60               |unstable near islands           |
+------------------+---------------------+---------------------+------------------+--------------------------------+
|apr60_nu50_bi1000 | 50                  | -1000               | 60               |stable, poorly behaved salinity |
+------------------+---------------------+---------------------+------------------+--------------------------------+


Lateral Boundary Conditions
-----------------------------------------
Currently we are using partial slip boundary conditions with :file:`rn_shlat =0.5` in :file:`namelist.lateral`. No slip conditions are applied when :file:`rn_shlat =2` and free slip when :file:`rn_shlat =0`.

At :math:`\nu=50`, we have seen some stablizing features as we take the lateral boundary torwards no slip. It seems that the no slip conditions change the location of the maximum velocities in the island regions, which can have an affect on the mixing. Our concern with no slip stems from resolving the boundary layer. We fear that using no slip BCs will leave the boundary layer unresolved, especially at lower viscosity.


+-----------------------+---------------------+---------------------+------------------+----------------------------+
| Simulation            |:file:`rn_ahm_0_lap` |:file:`rn_shlat`     | :file:`rn_avevd` |Notes                       |
+=======================+=====================+=====================+==================+============================+
|partial25/apr100_nu50  | 50                  | 0.25                | 100              |unstable at Stuart Island   |
+-----------------------+---------------------+---------------------+------------------+----------------------------+
|apr60_nu50             | 50                  | 0.5                 | 60               |unstable near islands       |
+-----------------------+---------------------+---------------------+------------------+----------------------------+
|partial1/apr60_nu50    | 50                  | 1                   | 60               |stable                      |
+-----------------------+---------------------+---------------------+------------------+----------------------------+
|noslip/apr60_nu50      | 50                  | 2                   | 60               |stable                      |
+-----------------------+---------------------+---------------------+------------------+----------------------------+
