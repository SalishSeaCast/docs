.. _Literature:

Literature Review
=================
This section documents modelling choices from other studies in this region or similar regions. It inlcudes an outline of choices in grid type, resolution, bottom friction, and turbulence closure.


+--------------------------+-----------------+-----------+------------------+------------------------+-------------------------+---------------------------------+--------------------+
| Study                    |Region           |  Model    | Horizontal Grid  | Vertical Grid          |Horizontal Mixing        | Vertical Mixing                 | Bottom Friction    |
+==========================+=================+===========+==================+========================+=========================+=================================+====================+
|Sutherland et al (2011)   | Salish Sea      | ROMS      | - min 280 m res  |- 20 terrain-following  | - constant diff 20 m2/s |- k-eps with Canuto A functions  |- quadratic  3e-3   |
|                          | and Puget Sound |           |                  |- smoothing dh/h<0.4    | - no explicit viscosity |- background diff/visc 5e-6 m2/s |                    |
+--------------------------+-----------------+-----------+------------------+------------------------+-------------------------+---------------------------------+--------------------+
|Masson and Cummins (2004) | Salish Sea      | POM       | - 2km res        |- 31 sigma levels       | - Smagorinsky           | - Mellor Yamada 2.5             |                    |
|                          |                 |           |                  |                        | - sensitivity to HORCON |                                 |                    |
|                          |                 |           |                  |                        | - HORCON = 0.03 or 0.06 |                                 |                    |
+--------------------------+-----------------+-----------+------------------+------------------------+-------------------------+---------------------------------+--------------------+
| Foreman et al (2012)     | Discovery       | FVCOM     | - triangular     | - 21 sigma levels      | - Smagorinsky           | - q-eps (Tian and Chen, 2006)   |- log layer         |
|                          | Islands         |           |                  | - smoothing dh/h<0.6   | - coefficient 0.2       | - background diff/visc 1e-6 m2/s|- roughness 0.001   |
|                          |                 |           |                  |                        |                         |                                 |- min 0.0025        |
+--------------------------+-----------------+-----------+------------------+------------------------+-------------------------+---------------------------------+--------------------+
| Stacey et al (1995)      | Knight Inlet    | 2D        |                  |                        | - constant diff/visc    | - Mellor Yamada 2.5             | - quadratic        |
|                          |                 |           |                  |                        | - choosen for stability | - adjustment for internal waves |                    |
+--------------------------+-----------------+-----------+------------------+------------------------+-------------------------+---------------------------------+--------------------+
| Foreman et al (2006)     | Broughton       | ELCIRC    |                  | - z-coordinate         |                         | - GLS                           | - quadratic 0.003  |
|                          | Archipelago     |           |                  | - smoothing dh/h<1     |                         |                                 |                    |
+--------------------------+-----------------+-----------+------------------+------------------------+-------------------------+---------------------------------+--------------------+
| Foreman et al (2009)     | Broughton       | FVCOM     | - triangular     | - 21 sigma levels      | - Smagorinksy           | - Mellor Yamada 2.5             | - log layer        |
|                          | Archipelago     |           |                  | - higher res at surface| - coefficient 0.2       | - background diff 1e-6 m2/s     | - cd0 = 0.003      |
|                          |                 |           |                  | - higher res at bottom |                         | - surface wave mixing param     | - roughness=0.005m |
+--------------------------+-----------------+-----------+------------------+------------------------+-------------------------+---------------------------------+--------------------+
| MacCready et al (2009)   |  Columbia River |  ROMS     | -400m res plume  |  -s-coordinate         |                         |  -GLS                           |    -log layer      |  
|                          |  estuary        |           |   & estuary      |  -smoothing dh/h<0.8   |                         |  -K-eps with Canuto A functions |    -quadratic 3e-3 |
|                          |                 |           |                  |  -20 sigma levels      |                         |  -background diff/visc 5e-6 m2/s|                    |       
+--------------------------+-----------------+-----------+------------------+------------------------+-------------------------+---------------------------------+--------------------+
References
==========

- Foreman et al (2006). Estuarine and Tidal Currents in the Broughton Archipelago. Atmosphere-Ocean.
- Foreman et al (2009). A finite volume model simulation for the Broughton Archipelago, Canada. Ocean Modelling. 
- Foreman et al (2012). A Circulation Model for the Discovery Islands, British Columbia. Atmosphere-Ocean.
- Masson and Cummins (2004). Observations and modeling of seasonal variability in the Straits of Georgia and Juan de Fuca. Journal of Marine Research.
- Sutherland  et al (2011). A Model Study of the Salish Sea Estuarine Circulation. Journal of Physical Oceanography.
- Stacey et al (1995). A Numerical Model of the Circulation in Knight Inlet, British Columbia, Canada. Journal of Physical Oceanography.
- MacCready e al (2009). A model study of tide- and wind-induced mixing in the COlumbia River Estuary and plume. Continental Shelf Research.
