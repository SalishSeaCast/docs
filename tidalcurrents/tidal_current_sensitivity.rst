Tidal current sensitivity
===========================================


Data Series
-------------------------------------------

Frequency
************

* The Salish Sea MEOPAR model outputs daily averages and hourly averages of tracers and velocities at very grid point. It also outputs quarter-hourly results but just at the ONC VENUS Central and East nodes.

* We wondered if it is beneficial to compile the quater-hourly data for the velocities, whether or not it would produce a more accurate tidal ellipse.

.. _FrequencySensitivity-image:

.. figure:: freqsens.png

* The figure above shows that at the central node there is 



:math:`M_2` Results
^^^^^^^^^^^^^^^^^^^
+-------------+------------+-----------+--------------+-------------+-----------+--------------+------------+-----------+--------------+
| Central     |  Quarter-Hourly                       | Hourly                                 | Hourly (6months)                      |
|             |                                       |                                        |                                       |
+=============+============+===========+==============+=============+===========+==============+============+===========+==============+
| Depth (m)   | Major-Axis | Minor-Axis|  Inclination |  Major-Axis | Minor-Axis|  Inclination | Major-Axis | Minor-Axis|  Inclination |  
|             |  (m/s)     |   (m/s)   | (deg. ccw E) |  (m/s)      |   (m/s)   | (deg. ccw E) |  (m/s)     |   (m/s)   | (deg. ccw E) |
+-------------+------------+-----------+--------------+-------------+-----------+--------------+------------+-----------+--------------+
| 0.5         | 0.26       | -0.12     | 130          | 0.26        | -0.12     | 130          | 0.20       | -0.07     | 137          |
+-------------+------------+-----------+--------------+-------------+-----------+--------------+------------+-----------+--------------+
| 2.5         | 0.23       | -0.11     | 131          | 0.23        | -0.10     | 131          | 0.19       | -0.07     | 137          |
+-------------+------------+-----------+--------------+-------------+-----------+--------------+------------+-----------+--------------+
| 4.5         | 0.19       | -0.08     | 130          | 0.19        | -0.08     | 130          | 0.18       | -0.06     | 137          |
+-------------+------------+-----------+--------------+-------------+-----------+--------------+------------+-----------+--------------+
| 6.5         | 0.18       | -0.07     | 131          | 0.18        | -0.07     | 131          | 0.17       | -0.05     | 137          |
+-------------+------------+-----------+--------------+-------------+-----------+--------------+------------+-----------+--------------+
| 8.5         | 0.18       | -0.07     | 132          | 0.18        | -0.07     | 132          | 0.17       | -0.05     | 137          |
+-------------+------------+-----------+--------------+-------------+-----------+--------------+------------+-----------+--------------+
| 10.5        | 0.18       | -0.07     | 133          | 0.18        | -0.07     | 133          | 0.17       | -0.05     | 138          |
+-------------+------------+-----------+--------------+-------------+-----------+--------------+------------+-----------+--------------+
| 12.5        | 0.18       | -0.07     | 134          | 0.18        | -0.07     | 134          | 0.17       | -0.04     | 138          |
+-------------+------------+-----------+--------------+-------------+-----------+--------------+------------+-----------+--------------+
| 14.6        | 0.18       | -0.06     | 134          | 0.17        | -0.06     | 134          | 0.17       | -0.04     | 138          |
+-------------+------------+-----------+--------------+-------------+-----------+--------------+------------+-----------+--------------+
| 16.8        | 0.17       | -0.05     | 133          | 0.17        | -0.05     | 133          | 0.17       | -0.04     | 137          |
 19.5      0.16        -0.04       132      0.16       -0.04       132      0.16       -0.03       136
 24.1      0.15        -0.02       129      0.15       -0.02       129      0.16       -0.03       133
 34.7      0.14        -0.00       123      0.13       -0.00       123      0.16       -0.02       127
 58.5      0.13        0.02       116      0.13       0.02       116      0.15       -0.01       124
 98.1      0.14        0.03       131      0.13       0.03       131      0.15       0.01       128
147.1      0.18        0.00       143      0.18       0.00       143      0.17       0.01       139
199.6      0.18        0.01       135      0.18       0.01       135      0.18       0.01       133
253.1      0.21        0.00       123      0.21       0.00       123      0.19       0.01       123
306.8      0.15        0.05       112      0.15       0.05       112      0.14       0.06       111




* ipython notebook: `Analysis8Components.ipynb`_

.. _Analysis8Components.ipynb: http://nbviewer.ipython.org/urls/bitbucket.org/salishsea/analysis/raw/tip/compare_tides/Analysis8Components.ipynb

* This notebook focuses on a set of 31 stations that form a line from Juan de Fuca up through the Strait of Georgia into Discovery Passage and the North End of the Model.

.. _Station_map-image:

.. figure:: Station_map.png

* Tidal harmonics are calculated using a least squares fit to the last 37.5 days of a 40-day model run without wind.  Scatter plots comparing the measured and modelled harmonics are made, example:

.. _K1scatter-image:

.. figure:: K1scatter.png

* The notebook calculates differences (as described by Foreman et al (1995)) for M2 and K1 and saves them to a text file and plots them.

.. _fit-image:

.. figure:: fit.png

* This image clearly shows that other than at Seymour Narrows (station 26) the model K1 tide is within 5 cm of the measured tide.  However, the M2 tide is poor not only in the Discovery Passage area (stations 24-28) but also in the Strait of Juan de Fuca.  The latter is a choice.  The model was tuned to match in Strait of Georgia (SoG).  Due to over-prediction of the amphidrome near Victoria the model cannot reproduce both the M2 tide in Juan de Fuca and in SoG at the same time.  This error is the only significant error with the tides.  We think it is due to over mixing in the Gulf/San Juan Island region and are working on that.

.. _alltidecomparison:

.. figure:: alltidecomparison.png 
	
In the above figure, green circles represent errors less than 5 cm, yellow circles represent errors between 5 and 10 cm and red circles mark errors greater than 10 cm.	


Currents
************

M2 and K1
+++++++++++++++++++

* M2 and K1 tidal ellipses calculated from current measurements at the ONC nodes have been compared to the model output.  The agreement is generally good, but the model currents are somewhat lower.

* M2 and K1 tidal ellipses calculated from CODAR current measurements have also been compared to the model output.  The agreement is generally good, but the model currents are somewhat higher!

* Drifter measurements to be made in Sep 2014 will also be used to compare to the modelled tidal ellipses.




