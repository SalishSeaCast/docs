.. _param_notebooks:

===============================================
SMELT Sensitivity Analysis: Parameter Notebooks
===============================================

Parameter notebooks are used to take an in depth look at how that parameter affects model results. They compare how tracer values evolve over time differently based on the value of the parameter. 

Three main types of plots are used in these notebooks:

    Heatmap of tracer concentration against depth and time for a particular parameter value

    Heatmap of the difference between tracer concentrations for two different runs against depth and time

    Facetplot showing tracer concentration vs. time at certain depths, with a coloured line for each of the six parameter values

`This <http://nbviewer.jupyter.org/urls/bitbucket.org/salishsea/analysis-james/raw/tip/notebooks/nampisrem_old_IC_june_17_analysis/nampisrem_zz_remin_d_pon.ipynb/>`_ is an example notebook.

A notebook was automatically created for every parameter using this `python script <https://bitbucket.org/salishsea/analysis-james/src/tip/generate_analysis_notebooks.py?at=default&fileviewer=file-view-default/>`_. Some of these notebooks have been uploaded to Bitbucket, and all of them can be found on the local filesystem at /ocean/jpetrie/MEOPAR/analysis-james/notebooks/.
