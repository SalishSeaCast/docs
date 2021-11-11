SMELT Sensitivity Analysis: Applications
========================================

The results of the sensitivity analysis can be used to tune the full model to better match observational data.

An example use case is a discrepancy found between measured and predicted nitrate concentrations at 20m depth at several locations in the Salish Sea.
This was found by comparing Nowcast Green results from 2016 with nitrate measurements at various locations from 2003, 2004, and 2005.
The notebook used to compare these datasets can be found
`here <https://nbviewer.org/github/SalishSeaCast/analysis-james/blob/master/notebooks/nowcast_nitrate_comparison.ipynb>`_.
These are notebooks comparing
`temperature and salinity <https://nbviewer.org/github/SalishSeaCast/analysis-james/blob/master/notebooks/nowcast_ts_comparison.ipynb>`_,
`chlorophyl <https://nbviewer.org/github/SalishSeaCast/analysis-james/blob/master/notebooks/nowcast_chloro_comparison.ipynb>`_,
and
`diatom <https://nbviewer.org/github/SalishSeaCast/analysis-james/blob/master/notebooks/nowcast_diatom_comparison.ipynb>`_
concentrations at the same locations.
The temperature and salinity notebook indicates that the model is capturing mixing effects properly at these locations because the difference between surface and 20m concentrations is similar.

To determine what parameters are most useful to tune a 'Nitrate at 20m' metric is created.
This metric is defined as the integral of Nitrate between 15 and 25 metres depth for all times of the model run.
The value of this metric is then calculated for every one of the existing 5x5 model results in this
`notebook <https://nbviewer.org/github/SalishSeaCast/analysis-james/blob/master/notebooks/nitrate_at_20m_param_gradient.ipynb>`_.
The notebook shows that there are only a few parameters that have a substantial impact on the nitrate concentration at a depth of 20m, and only when the simulation is started in the winter.
The reason the parameters don't impact nitrate in later months is that by that time it has already been depleted by the spring bloom.
