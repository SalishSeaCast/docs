SMELT Sensitivity Analysis: Metrics
===================================

Metrics are used to quantitatively compare all of the parameters. This provides a useful overview of how the parameters affect certain parts of the simulation.

An example of a metric would be peak primary producer biomass. This is defined as the maximum value of a three day running average of total primary producer biomass in the model domain. For every model result this metric can be calculated.

Then we can see how the value of this metric changes for different parameter modifications. This metric can be thought of as a function with inputs being the parameter values and a single valued output. Under this paradigm we can define partial derivatives.

This `notebook <http://nbviewer.jupyter.org/urls/bitbucket.org/salishsea/analysis-james/raw/tip/notebooks/metric_gradient_multiple_seasons.ipynb/>`_ is used to compare the normalised partial derivatives of every parameter for 3 different starting months. The partial derivatives are normalised so that the impact of parameters is related to their percent change, not their magnitude change. This allows for better comparison of parameters that are several orders of magnitude apart.

The notebook linked above uses a list of result directories and a list of metric functions, it applies each metric function to every result within those directories. Then a dataset is created with columns for metric name, modified parameter name, percent change in parameter, start month, and metric value. This dataset is used to create two types of plots.

    - The first type of plot is a facet plot with an individual axes for every unique metric and season combination. It shows the metric value on the y axis and the percent change in parameter on the x axis. The 15 parameters with the most impact on these metrics are drawn as differently coloured lines on these plots. This type of plot is useful to examine the trend of the metric value as these parameters change.

    - The next type of plot is a horizontal bar plot. As before there is an individual axes for every metric/season combination. These bar plots show the scaled partial derivative of that metric with respect to every parameter (with the specified start month). The bars are coloured by parameter section to more easily group together similar parameters. The bar plots give a quick overview of what parameters change each metric the most. They are a useful place to start when trying to tune the model.

All of the metric functions used in the linked notebook are found in this `python file <https://bitbucket.org/salishsea/tools/src/cab0513c8041d6dd6423fcf6cd0bffcf9c260273/SalishSeaTools/salishsea_tools/metric_tools_5x5.py?at=default&fileviewer=file-view-default/>`_. They all take 'grid_t' as the only input, where grid_t is an xarray object containing the tracer data found in :file:`*ptrc_t.nc` files.

Using a new metric is fairly easy: define a function that takes grid_t as input and add this function to metric_func_list. Next run the entire notebook (this may take a while because the notebook has to load 1800 xarray datasets and calculate each metric for every dataset).



