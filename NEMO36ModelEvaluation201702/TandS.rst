.. _Temperature and Salinity:

*************************
Temperature and Salinity
*************************

Citizen Science
======================

In addition to an observation - model plot that includes both 2015 and 2016, separate comparisons were also made for each region. 
The Victoria stations are shown below. 

.. figure:: images/csphysicsall.png
.. figure:: images/citsciphysics.png

`CitSci - full notebook`_

`CitSci - single days notebook`_

`CitSci - single depth profiles notebook`_

.. _CitSci - full notebook: https://nbviewer.jupyter.org/urls/bitbucket.org/salishsea/analysis-vicky/raw/tip/notebooks/ModelEvaluations/updated-nowcast-comparisons.ipynb
.. _CitSci - single days notebook: https://nbviewer.jupyter.org/urls/bitbucket.org/salishsea/analysis-vicky/raw/tip/notebooks/ModelEvaluations/updated-nowcast-comparisons-singledays.ipynb
.. _CitSci - single depth profiles notebook: https://nbviewer.jupyter.org/urls/bitbucket.org/salishsea/analysis-vicky/raw/tip/notebooks/ModelEvaluations/CitSci-single-depth-profiles.ipynb

Ferry
=================
Only samples with valid times, longitude, latitude, and salinity values were used in the comparison. 

+-------------------------+-----------------+
|    Statistic            | Value           |
+=========================+=================+
| bias                    | 1.66856979031   |
+-------------------------+-----------------+
| RMSE                    | 4.92462599804   |
+-------------------------+-----------------+
| Willmott Skill Score    | 0.782013448965  |
+-------------------------+-----------------+

.. figure:: images/ferrysalinity.png

`Ferry salinity notebook`_


.. _Ferry salinity notebook: https://nbviewer.jupyter.org/urls/bitbucket.org/salishsea/analysis-vicky/raw/tip/notebooks/ModelEvaluations/ferrysalinityvsnowcastgreen-by-longitude.ipynb 

Fraser Plume ctd
========================

Statistics, available in the notebook below, were calculated for each depth. 
An example depth profile from May 31, 2017 is shown below.

.. figure:: images/ctd.png

`CTD casts notebook`_

.. _CTD casts notebook: https://nbviewer.jupyter.org/urls/bitbucket.org/salishsea/analysis-vicky/raw/tip/notebooks/ModelEvaluations/CTDvsNowcastgreen.ipynb

IOS cruises
======================

+-------------------------+----------------------+----------------------+
|    Statistic            | Temperature          | Salinity             |
+=========================+======================+======================+
| bias                    | 0.05512010187655747  | -0.041040355046526145|
+-------------------------+----------------------+----------------------+
| RMSE                    | 0.7015201515771843   | 1.066426061995313    |
+-------------------------+----------------------+----------------------+
| Willmott Skill Score    | 0.9684729188294054   | 0.954343789715143    |
+-------------------------+----------------------+----------------------+

`comparison notebook`_

.. _comparison notebook: https://nbviewer.jupyter.org/urls/bitbucket.org/salishsea/analysis-vicky/raw/tip/notebooks/ModelEvaluations/DFO-comparison-blue.ipynb


Sentry Shoal
======================

+-------------------------+----------------------+----------------------+
|    Statistic            | Temperature          | Salinity             |
+=========================+======================+======================+
| bias                    | 0.4049012974465036   | -0.28715771259387424 |
+-------------------------+----------------------+----------------------+
| RMSE                    | 1.4453098826255502   | 1.1850808771660581   |
+-------------------------+----------------------+----------------------+
| Willmott Skill Score    | 0.9424725482806958   | 0.8290720829792324   |
+-------------------------+----------------------+----------------------+

`Time series notebook`_

.. _Time series notebook: https://nbviewer.jupyter.org/urls/bitbucket.org/salishsea/analysis-vicky/raw/tip/notebooks/ModelEvaluations/SentryShoalTS.ipynb


VENUS nodes
======================

Comparison to the observed salinity were made to the model's (then called hindcast) salinity. 

.. figure:: images/ComparisonHindcastVENUS.png

Water quality buoy
=========================

Comparison to the model temperature was made at the model's surface and depth = 1.5m, 
as well as daily and hourly averaged values. 
Below, statistics and plots are for model surface, and the observation - model plot is using the daily average. 

+-----------------------+-----------------+-------------------+
|    Statistic          |  Hourly Averaged| Daily Averaged    |
+=======================+=================+===================+
| bias                  | -0.834625751208 | -0.832611264165   |
+-----------------------+-----------------+-------------------+
| RMSE                  | 1.48700936448   | 1.47607008425     |
+-----------------------+-----------------+-------------------+
| Willmott Skill Score  | 0.983537985575  | 0.983654461941    |
+-----------------------+-----------------+-------------------+

.. figure:: images/waterqualitybuoyts.png
.. figure:: images/waterqualitybuoyom.png

`Water quality buoy notebook (daily averages)`_

`Water quality buoy notebook (hourly interpolated averages)`_

.. _Water quality buoy notebook (daily averages): https://nbviewer.jupyter.org/urls/bitbucket.org/salishsea/analysis-vicky/raw/tip/notebooks/ModelEvaluations/waterqualitybuoy-daily.ipynb
.. _Water quality buoy notebook (hourly interpolated averages): https://nbviewer.jupyter.org/urls/bitbucket.org/salishsea/analysis-vicky/raw/tip/notebooks/ModelEvaluations/waterqualitybuoy-hourly.ipynb

