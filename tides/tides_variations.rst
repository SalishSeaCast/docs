Tidal Sensitivity
===================================

This document outlines the tidal sensitivity to various parameters such as bottom friction and lateral viscosity. The following details are presented: set up of simulations, analysis techniques, and tidal response. The :math:`M_2` and :math:`K_1` tidal constituents are the main focus. 

The modelled tidal response is compared to observations from Foreman et al. (1995), Foreman et al. (2004), and Foreman et al. (2012). 


Procedure
-------------------------
Several 5-day long simulations forced with :math:`M_2` and :math:`K_1` tidal constituents have been completed. A sinusoidal curve fitting technique is applied to the time series of the modelled sea surface high to estimate the :math:`M_2` and :math:`K_1` amplitude and phase. The fitting curve is defined as follows:

.. math::
   f(t) = A_{M_2}\cos(\omega_{M_2}t - \phi_{M_2}) + A_{K_1}\cos(\omega_{K_1}t - \phi_{K_1})

where :math:`A_{M_2}`, :math:`\phi_{M_2}`, :math:`A_{K_1}`, and :math:`\phi_{K_1}` are the :math:`M_2` and :math:`K_1` amplitudes and phases determined by the fitting procedure. The :math:`M_2` and :math:`K_1` frequencies :math:`\omega_{M_2}`, :math:`\omega_{K_1}` are known. 

Results
--------------
The modelled amplitude and phase will be compared to observations at several points within the domain: Port Renfrew, Point Atkinson, and Yorke Island. This way we can evaluate model performance over different regions. Other locations are available but will not be summarized here. 

We have calculated the phase difference :math:`\Delta \phi =\phi_{mod}-\phi_{obs}` and the amplitude ratio :math:`R = \frac{A_{mod}}{A_{obs}}`. The results are summarized in the tables below. (to follow)

:math:`M_2` Results
^^^^^^^^^^^^^^^^^^^
+----------------+----------------------------+----------------+-----------+--------------+---------------------+----------------+---------------------+----------------+---------------------+
| Simulation     |Description                 |                |           | Port Renfrew                       | Point Atkinson                       | Yorke Island                         |
+================+============================+================+===========+==============+=====================+================+=====================+================+=====================+
|                |                            |Bottom Friction | Viscosity | :math:`R`    | :math:`\Delta \phi` | :math:`R`      | :math:`\Delta \phi` | :math:`R`      | :math:`\Delta \phi` |   
+----------------+----------------------------+----------------+-----------+--------------+---------------------+----------------+---------------------+----------------+---------------------+
|tide_flux_M2K1  | original tides at west,    | :math:`0.005`  | 20        | 1.01         | 1.74                | 0.911          | 10.4                | 1.16           | -3.91               | 
|                | flux corrected at north    |                |           |              |                     |                |                     |                |                     |
+----------------+----------------------------+----------------+-----------+--------------+---------------------+----------------+---------------------+----------------+---------------------+
|tide_flux_west  | :math:`M_2` and            | :math:`0.005`  | 20        | 1.16         | 6.06                | 1.01           | 13.4                | 1.15           | -4.03               | 
|                | :math:`K_1` flux           |                |           |              |                     |                |                     |                |                     |
|                | increased by 25% at        |                |           |              |                     |                |                     |                |                     |
|                | west                       |                |           |              |                     |                |                     |                |                     |
+----------------+----------------------------+----------------+-----------+--------------+---------------------+----------------+---------------------+----------------+---------------------+
|tide_bottom     | decreased bottom friction  | :math:`0.003`  | 20        | 0.991        | -0.944              | 0.943          | 9.94                | 1.13           | -5.44               |   
+----------------+----------------------------+----------------+-----------+--------------+---------------------+----------------+---------------------+----------------+---------------------+
|tide_nu15       | decreased lateral          | :math:`0.005`  | 15        | 1.00         | 1.54                | 0.914          | 10.2                | 1.16           | -4.08               |
|                | viscosity                  |                |           |              |                     |                |                     |                |                     |
+----------------+----------------------------+----------------+-----------+--------------+---------------------+----------------+---------------------+----------------+---------------------+
|tide_bottom1e-3 | decreased bottom friction  | :math:`0.001`  | 20        | 0.965        | -5.14               | 0.993          | 9.13                | 1.09           | -8.24               |
+----------------+----------------------------+----------------+-----------+--------------+---------------------+----------------+---------------------+----------------+---------------------+
|tide_K1phase2   | :math:`K_1` phase          | :math:`0.005`  | 20        | 1.01         | 1.70                | 0.911          | 10.5                | 1.16           | -3.95               |
|                | decreased 5 deg            |                |           |              |                     |                |                     |                |                     |
+----------------+----------------------------+----------------+-----------+--------------+---------------------+----------------+---------------------+----------------+---------------------+
|tide_K1amp      | above plus :math:`K_1`     | :math:`0.001`  | 20        | 0.965        | -5.55               | 0.998          | 9.13                | 1.08           | -8.33               |
|                | amplitude                  |                |           |              |                     |                |                     |                |                     |
|                | decreased 15% and          |                |           |              |                     |                |                     |                |                     |
|                | b.f.                       |                |           |              |                     |                |                     |                |                     |
|                | :math:`1\times 10^{-3}`    |                |           |              |                     |                |                     |                |                     |
+----------------+----------------------------+----------------+-----------+--------------+---------------------+----------------+---------------------+----------------+---------------------+
|tide_M2phase    | above plus :math:`M_2`     | :math:`0.001`  | 20        |  0.969       | -15.2               | 1.01           | 0.662               | 1.08           | -6.66               |
|                | phase                      |                |           |              |                     |                |                     |                |                     |
|                | decreased 9 deg            |                |           |              |                     |                |                     |                |                     |
+----------------+----------------------------+----------------+-----------+--------------+---------------------+----------------+---------------------+----------------+---------------------+

**Summary**

* Decreasing bottom friction increases :math:`M_2` amplitude at Point Atkinson but decreases it at Port Renfrew. There is a good match at Point Atkinson when bottom friction is :math:`1\times10^{-3}`.

* Decreasing bottom friction decreases the :math:`M_2` phase difference at all three locations, with the largest response at Port Renfrew and Yorke Island.

* Decreasing the viscosity has little effect at all three stations. 

* Decreasing the :math:`K_1` phase has little effect on the :math:`M_2` amplitude and phase.

* Point Atkinson :math:`M_2` phases are very consistent over all of the iterations, except the last where the :math:`M_2` phase has changed in forcing. The phases at Port Renfrew and Yorke Island are more sensitive to changes in bottom friction and viscosity.


:math:`K_1` Results
^^^^^^^^^^^^^^^^^^^

.. tabularcolumns:: |l|p{1cm}|l|l|l|l|l|l|

+----------------+----------------------------+-----------------+-----------+--------------+---------------------+----------------+---------------------+----------------+---------------------+
| Simulation     |Description                 |                 |           | Port Renfrew                       | Point Atkinson                       | Yorke Island                         |
+================+============================+=================+===========+==============+=====================+================+=====================+================+=====================+
|                |                            | Bottom Friction | Viscosity | :math:`R`    | :math:`\Delta \phi` | :math:`R`      | :math:`\Delta \phi` | :math:`R`      | :math:`\Delta \phi` |  
+----------------+----------------------------+-----------------+-----------+--------------+---------------------+----------------+---------------------+----------------+---------------------+
|tide_flux_M2K1  | original tides at west,    | :math:`0.005`   | 20        | 1.07         | 3.51                | 1.09           | 8.46                | 1.14           | -5.78               | 
|                | flux corrected at north    |                 |           |              |                     |                |                     |                |                     |
+----------------+----------------------------+-----------------+-----------+--------------+---------------------+----------------+---------------------+----------------+---------------------+
|tide_flux_west  | :math:`M_2` and            | :math:`0.005`   | 20        | 1.19         | 0.151               | 1.18           | 7.11                | 1.16           | -5.09               | 
|                | :math:`K_1` flux           |                 |           |              |                     |                |                     |                |                     |
|                | increased 25% at           |                 |           |              |                     |                |                     |                |                     |
|                | west                       |                 |           |              |                     |                |                     |                |                     |
+----------------+----------------------------+-----------------+-----------+--------------+---------------------+----------------+---------------------+----------------+---------------------+
|tide_bottom     | decreased bottom friction  | :math:`0.003`   | 20        | 1.08         | 6.31                | 1.11           | 7.10                | 1.14           | -4.42               |
+----------------+----------------------------+-----------------+-----------+--------------+---------------------+----------------+---------------------+----------------+---------------------+
|tide_nu15       | decreased lateral          | :math:`0.005`   | 15        | 1.07         | 3.48                | 1.08           | 8.24                | 1.14           | -5.69               |
|                | viscosity                  |                 |           |              |                     |                |                     |                |                     |
+----------------+----------------------------+-----------------+-----------+--------------+---------------------+----------------+---------------------+----------------+---------------------+
|tide_bottom1e-3 | deceased bottom friction   | :math:`0.001`   | 20        | 1.10         | 10.2                | 1.15           | 5.27                | 1.14           | -2.22               |
+----------------+----------------------------+-----------------+-----------+--------------+---------------------+----------------+---------------------+----------------+---------------------+
|tide_K1phase2   | :math:`K_1` phase          | :math:`0.005`   | 20        | 1.07         | -1.36               | 1.09           | 3.52                | 1.16           | -5.56               |
|                | decreased 5 deg            |                 |           |              |                     |                |                     |                |                     |
+----------------+----------------------------+-----------------+-----------+--------------+---------------------+----------------+---------------------+----------------+---------------------+
|tide_K1amp      | above plus :math:`K_1`     | :math:`0.001`   | 20        | 0.934        | 5.81                | 0.984          | 0.541               | 1.13           | -5.78               |
|                | amplitude                  |                 |           |              |                     |                |                     |                |                     |
|                | decreased 15% and          |                 |           |              |                     |                |                     |                |                     |
|                | b.f.                       |                 |           |              |                     |                |                     |                |                     |
|                | :math:`1\times 10^{-3}`    |                 |           |              |                     |                |                     |                |                     |
+----------------+----------------------------+-----------------+-----------+--------------+---------------------+----------------+---------------------+----------------+---------------------+
|tide_M2phase    | above plus :math:`M_2`     | :math:`0.005`   | 20        | 0.955        |  5.68               | 1.01           | 0.594               | 1.14           | -5.24               |
|                | phase                      |                 |           |              |                     |                |                     |                |                     |
|                | decreased 9 deg            |                 |           |              |                     |                |                     |                |                     |
+----------------+----------------------------+-----------------+-----------+--------------+---------------------+----------------+---------------------+----------------+---------------------+

**Summary** 

* Decreasing the bottom friction increases the :math:`K_1` amplitude at Port Renfrew and Point Atkinson. There is no effect at Yorke Island.

* Decreasing the bottom friction increases the :math:`K_1` phase difference at Port Renfrew and Yorke Island, with the largest response at Port Renfrew. The Point Atkinson phase difference has decreased.

* Decreasing the viscosity has little effect on the :math:`K_1` amplitude and phase at these three locations. 


References
-------------------------
* Foreman, M.G.G., R.A. Walters, R.F. Henry, C.P. Keller and A.G. Dolling, 1995. A tidal model for eastern Juan de Fuca Strait and the southern Strait of Georgia, Journal of Geophysical Research, 100, 721-740.

* Foreman, M.G.G., G. Sutherland, and P.F. Cummins, 2004. M2 tidal dissipation around Vancouver Island: an inverse approach. Continental Shelf Research, 24, 2167-2185.

* Foreman, M.G.G., D.J. Stucchi, K.A. Garver, D. Tuele, J. Isaac, T. Grime, M. Guo, and J. Morrison, A Circulation Model for the Discovery Islands, British Columbia, 2012, Atmosphere-Ocean, 50:3, 301-316.


