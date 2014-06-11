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
+----------------+----------------------------+-------------+---------------------+----------------+---------------------+----------------+---------------------+
| Simulation     |Description                 |Port Renfrew                       | Point Atkinson                       | Yorke Island                         |
+================+============================+=============+=====================+================+=====================+================+=====================+
|                |                            | :math:`R`   | :math:`\Delta \phi` | :math:`R`      | :math:`\Delta \phi` | :math:`R`      | :math:`\Delta \phi` |   
+----------------+----------------------------+-------------+---------------------+----------------+---------------------+----------------+---------------------+
|tide_flux_M2K1  | original tides at west,    |             |                     |                |                     |                |                     | 
|                | flux corrected at north    |             |                     |                |                     |                |                     |
+----------------+----------------------------+-------------+---------------------+----------------+---------------------+----------------+---------------------+
|tide_flux_west  | :math:`M_2` and            |             |                     |                |                     |                |                     | 
|                | :math:`K_1` flux           |             |                     |                |                     |                |                     |
|                | increased by 25% at        |             |                     |                |                     |                |                     |
|                | west                       |             |                     |                |                     |                |                     |
+----------------+----------------------------+-------------+---------------------+----------------+---------------------+----------------+---------------------+
|tide_bottom     | bottom friction            |             |                     |                |                     |                |                     |
|                | decreased to               |             |                     |                |                     |                |                     |
|                | :math:`3\times 10^{-3}`    |             |                     |                |                     |                |                     |
+----------------+----------------------------+-------------+---------------------+----------------+---------------------+----------------+---------------------+
|tide_nu15       | lateral viscosity          |             |                     |                |                     |                |                     |
|                | decreased to 15            |             |                     |                |                     |                |                     |
+----------------+----------------------------+-------------+---------------------+----------------+---------------------+----------------+---------------------+
|tide_bottom1e-3 | bottom friction            |             |                     |                |                     |                |                     |
|                | decreased to               |             |                     |                |                     |                |                     |
|                | :math:`1\times 10^{-3}`    |             |                     |                |                     |                |                     |
+----------------+----------------------------+-------------+---------------------+----------------+---------------------+----------------+---------------------+
|tide_K1phase2   | :math:`K_1` phase          |             |                     |                |                     |                |                     |
|                | decreased 5 deg            |             |                     |                |                     |                |                     |
+----------------+----------------------------+-------------+---------------------+----------------+---------------------+----------------+---------------------+
|tide_K1amp      | above plus :math:`K_1`     |             |                     |                |                     |                |                     |
|                | amplitude                  |             |                     |                |                     |                |                     |
|                | decreased 15% and          |             |                     |                |                     |                |                     |
|                | b.f.                       |             |                     |                |                     |                |                     |
|                | :math:`1\times 10^{-3}`    |             |                     |                |                     |                |                     |
+----------------+----------------------------+-------------+---------------------+----------------+---------------------+----------------+---------------------+
|tide_M2phase    | above plus :math:`M_2`     |  .969       | -15.2               | 1.01           | 0.662               | 1.08           | -6.66               |
|                | phase                      |             |                     |                |                     |                |                     |
|                | decreased 9 deg            |             |                     |                |                     |                |                     |
+----------------+----------------------------+-------------+---------------------+----------------+---------------------+----------------+---------------------+


:math:`K_1` Results
^^^^^^^^^^^^^^^^^^^

.. tabularcolumns:: |l|p{1cm}|l|l|l|l|l|l|

+----------------+----------------------------+-------------+---------------------+----------------+---------------------+----------------+---------------------+
| Simulation     |Description                 |Port Renfrew                       | Point Atkinson                       | Yorke Island                         |
+================+============================+=============+=====================+================+=====================+================+=====================+
|                |                            | :math:`R`   | :math:`\Delta \phi` | :math:`R`      | :math:`\Delta \phi` | :math:`R`      | :math:`\Delta \phi` |   
+----------------+----------------------------+-------------+---------------------+----------------+---------------------+----------------+---------------------+
|tide_flux_M2K1  | original tides at west,    |             |                     |                |                     |                |                     | 
|                | flux corrected at north    |             |                     |                |                     |                |                     |
+----------------+----------------------------+-------------+---------------------+----------------+---------------------+----------------+---------------------+
|tide_flux_west  | :math:`M_2` and            |             |                     |                |                     |                |                     | 
|                | :math:`K_1` flux           |             |                     |                |                     |                |                     |
|                | increased 25% at           |             |                     |                |                     |                |                     |
|                | west                       |             |                     |                |                     |                |                     |
+----------------+----------------------------+-------------+---------------------+----------------+---------------------+----------------+---------------------+
|tide_bottom     | bottom friction            |             |                     |                |                     |                |                     |
|                | decreased to               |             |                     |                |                     |                |                     |
|                | :math:`3\times 10^{-3}`    |             |                     |                |                     |                |                     |
+----------------+----------------------------+-------------+---------------------+----------------+---------------------+----------------+---------------------+
|tide_nu15       | lateral viscosity          |             |                     |                |                     |                |                     |
|                | decreased to 15            |             |                     |                |                     |                |                     |
+----------------+----------------------------+-------------+---------------------+----------------+---------------------+----------------+---------------------+
|tide_bottom1e-3 | bottom friction            |             |                     |                |                     |                |                     |
|                | decreased to               |             |                     |                |                     |                |                     |
|                | :math:`1\times 10^{-3}`    |             |                     |                |                     |                |                     |
+----------------+----------------------------+-------------+---------------------+----------------+---------------------+----------------+---------------------+
|tide_K1phase2   | :math:`K_1` phase          |             |                     |                |                     |                |                     |
|                | decreased 5 deg            |             |                     |                |                     |                |                     |
+----------------+----------------------------+-------------+---------------------+----------------+---------------------+----------------+---------------------+
|tide_K1amp      | above plus :math:`K_1`     |             |                     |                |                     |                |                     |
|                | amplitude                  |             |                     |                |                     |                |                     |
|                | decreased 15% and          |             |                     |                |                     |                |                     |
|                | b.f.                       |             |                     |                |                     |                |                     |
|                | :math:`1\times 10^{-3}`    |             |                     |                |                     |                |                     |
+----------------+----------------------------+-------------+---------------------+----------------+---------------------+----------------+---------------------+
|tide_M2phase    | above plus :math:`M_2`     | 0.955       |  5.68               | 1.01           | 0.594               | 1.14           | -5.24               |
|                | phase                      |             |                     |                |                     |                |                     |
|                | decreased 9 deg            |             |                     |                |                     |                |                     |
+----------------+----------------------------+-------------+---------------------+----------------+---------------------+----------------+---------------------+

References
-------------------------
* Foreman, M.G.G., R.A. Walters, R.F. Henry, C.P. Keller and A.G. Dolling, 1995. A tidal model for eastern Juan de Fuca Strait and the southern Strait of Georgia, Journal of Geophysical Research, 100, 721-740.

* Foreman, M.G.G., G. Sutherland, and P.F. Cummins, 2004. M2 tidal dissipation around Vancouver Island: an inverse approach. Continental Shelf Research, 24, 2167-2185.

* Foreman, M.G.G., D.J. Stucchi, K.A. Garver, D. Tuele, J. Isaac, T. Grime, M. Guo, and J. Morrison, A Circulation Model for the Discovery Islands, British Columbia, 2012, Atmosphere-Ocean, 50:3, 301-316.


