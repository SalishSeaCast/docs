.. _Frequency Sensitivity Studies:

***********************************************
Frequency Sensitivity Studies
***********************************************

The model produces datasets containing information about the velocity field for a region. Ariane uses this information to produce particle trajectories. We wanted to know at what frequency would the model output need to be to produce the most reliable particle trajectories.

For the frequency sensitivity studies, we used model outputs with 30 minute, 1 hour, and 4 hour frequencies. This data was used in Ariane to generate particle trajectories with points at 30 minute intervals. We did this for particles starting their trajectories at the Fraser River, at various points along the thalweg, and in the areas between Haro Strait and the Strait of Juan de Fuca where there is strong vertical mixing.


On the Surface
===================

.. figure:: images/Sensitivity2D.png

At the Fraser River, we found that the particle trajectory generated using data at a 4 hour frequency does not capture subtleties in particle motion as do the trajectories deriving from data at 30 minute and 1 hour frequencies.

The trajectory that used 1 hour frequency data very closely resembles the trajectory that used 30 minute data.

:command:`Conclusion: We can use 1 hour or 30 minute NEMO output data when particle trajectories start at the Fraser River.`

At Depth
===================
.. figure:: images/Sensitivity3D.png

