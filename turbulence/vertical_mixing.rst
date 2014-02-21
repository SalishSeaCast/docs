.. _Vertical:

Vertical Mixing
================
The vertical mixing and diffusion are controlled in the :file:`namelist.dynamics` and in cpp keys for vertical turbulence selection. We would like to determine the most appropriate combintation of turbluence parameterization and lower boundary condition for modelling the Salish Sea. NEMO has several choices for turbulence closure which will be discussed here.  

Vertical Turbulence
--------------------------------------
NEMO parameterizes vertical turbulence using eddy coefficients. There are several different methods for calculating the eddy coefficients:

1. Constant coefficients
2. Richardson number based coefficients
3. Turbulent kinetic energy closure
4. Generalized length scale

The most relevent options for our purposes are the 3 and 4. Currently we are using the generelized length scale with a :math:`k-\epsilon` closure. 


Turbulent kinetic energy
^^^^^^^^^^^^^^^^^^^^^^^^
The turbulent kinetic energy closure schemes involve a prognostic equation for the turbulent kinetic energy. The eddy coefficients are proportional to the turblent kinetic energy and a turbulent length scale. The details are explained in the NEMO documentation.

This method includes options for dealing with strongly stratified regions and unstable stratifications which are attractive features for this project due to the strong mixing in the island regions. At this point, we have not attempted any simulations with this method of turbulence closure.

To do: 

* Examine vertical mixing with this turbulence closure scheme.

Generalized Length Scale
^^^^^^^^^^^^^^^^^^^^^^^^^
The generalized length scale (GLS) turbulence option also involves a prognostic equation for the turbulent kinetic energy. Additionally, it uses a prognostic equation to determine the turbulent length scale. This method allows for easy comparison between several well-known turbulence closure schemes such as :math:`k-\epsilon` and Mellor-Yamada. 

The behaviour of each of the closure methods in GLS has been examined by Warren et al (2005). Their results indicate that the :math:`k-\epsilon` option is appropriate for our scenario. This option performed well in a simulation of a wind-driven mixed layer and also in a simulation of estuarine circulation. This is a good first place to start in determining an appropriate mixing parameterization for our setup. 
 
Convection
------------------------------------------

The NEMO model is hydrostatic and thus cannot directly handle unstable density stratifications that arise due to convection or overturning. There are several ways to parameterize this process which include enhanced vertical diffusion and a non-penetrative convective algorithm. Tests with the non-penetretative convective algorithm have not been successful.

Enhanced Vertical Diffusion
^^^^^^^^^^^^^^^^^^^^^^^^^^^

The enhanced vertical diffusion option increases the amount of vertical mixing locally whenever an unstable density stratification is encountered. It accomplishes this by changing the vertical eddy coefficient to a namelist set value :file:`rn_avevd` in :file:`namelist.dynamics`.  

Note that the tubulent kinetic energy closure schemes inherently produce higher eddy coefficients whenever the squared buoyancy frequency is negative. As such, we have observed a warning in :file:`ocean.output` when using enhanced vertical diffusion in conjunction with a turbulent kinetic energy closure. However, several simulations with no enhanced vertical diffusion have been unsuccessful so we need a reliable way of dealing with unstable stratifications and will continue to use the enhanced vertical diffusion.

As noted in the discussion on lateral viscosity (:ref:`Lateral`), the model often experiences instablilty near the islands where vertical mixing is expected. Several experiments with a higher enhanced vertical diffusion parameter :file:`rn_avevd` have remained stable, even at low lateral viscosity. There will be a balance between an appropriate amount of vertical diffusion and a low enough lateral viscosity.  

The simulations performed to date are outlined in :ref:`Lateral`. 


Bottom Friction
-----------------------------------------

Bottom friction is parameterized through a bottom boundary layer with either linear or nonlinear flux terms. We are using the nonlinear setting. The NEMO documentation recommends using an implicit calculation of the bottom boundary condition when using the split-explicit time stepping.

There is some flexibility in setting the coefficients on the flux term, :file:`rn_bfri2` for the nonlinear setting. This is controlled in :file:`namelist.bottom`. Also, the NEMO documentation suggests using a low or zero value for :file:`rn_bfeb2` when tides are treated explicitly.

Reducing the parameter :file:`rn_bfri2` from :math:`5\times 10^{-3}` to :math:`4\times 10^{-3}` caused instability in the Puget Sounds region. Setting :file:`rn_bfeb2=0` made no difference. 

References
----------

Warner, J.C., Sherwood, C.R., Arango, H.G., and Signell, R.P.: Performance of four turbulence closure models implemented using a generic length scale method, Ocean Modelling, 8, 81-115, 2005.
