SMELT Sensitivity Analysis: Applications
========================================

The results of the sensitivity analysis can be used to tune the full model to better match observational data.

An example use case is a discrepancy found between measured and predicted nitrate concentrations at 20m depth at several locations in the Salish Sea. This was found by comparing Nowcast Green results from 2015 with nitrate measurements at various locations from 2003, 2004, and 2005. The notebook used to compare these datasets can be found _`here <https://bitbucket.org/salishsea/analysis-james/src/01c6b65a9aaaf9b9c809ab006a87136a2a22bb64/notebooks/nowcast_nitrate_comparison.ipynb?at=default&fileviewer=file-view-default/>`.

There already exists a large dataset of 5x5 model results. To determine what parameters are most useful to tune a 'Nitrate at 20m' metric is created.
This metric is defined as the integral of Nitrate between 15 and 25 metres depth for all times of the model run.
