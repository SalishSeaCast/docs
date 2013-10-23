**************************
AMM12 Boundary Coordinates
**************************

The `AMM12-BDY-analysis.ipynb`_ IPython Notebook is an analysis of the differences in results obtained from runs of the AMM12 configuration with the unstructured boundaries
(BDY) set via the 2 available mechanisms in NEMO-3.4:

* using a :file:`coordinates.bdy.nc` file (stock configuration)
* using a :kbd:`nambdy_index` namelist

That analysis lead to our decision to use boundary coordinate calculations driven by the :kbd:`nambdy_index` namelist for :ref:`SalishSeaNEMO`.

.. _AMM12-BDY-analysis.ipynb: http://nbviewer.ipython.org/urls/bitbucket.org/salishsea/docs/raw/ea83d0f4/code-notes/dev-notes/AMM12-BDY-analysis.ipynb
