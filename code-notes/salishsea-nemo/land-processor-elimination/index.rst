.. _LandProcessorElimination:

**************************
Land Processor Elimination
**************************

NEMO-3.6 includes land processor elimination and it lets you reduce your computational cost when some of your processors are assigned tiles that are entirely land.
The amount of land
(and computational cost)
eliminated depends on the MPI decomposition and the model domain.
The feature is activated by specifying a MPI decomposition
(eg, jnpi, jpnj = 8, 18)
and then specifying the number of water processors
(jpnij = 88).
When NEMO notices that jpnij is less than the product of jpni and jpnj
(here 144)
it switches on the land elimination feature.
However,
you need to know the number of water processors before running NEMO so you can request that number of processors on the compute system
(eg, westgrid).


Forward problem
===============

Determining the number of water processors,
given a domain and a decomposition,
is the forward problem.
NEMO includes a tool under TOOLS/MPP_PREP that computes the number of water processors for all possible decompositions
(up to a maximum number of processors).
The calculation is unique to each bathymetry file, and we store the calculation results in a :file:`.csv` files in the :ref:`grid-repo`.
The :command:`salishsea run` and :command:`salishsea prepare` commands from the :ref:`SalishSeaCmdProcessor` can use the :file:`.csv` file as a lookup table in order to request the correct number of water processors for the domain decomposition that you set.


Reverse problem
===============

Determining the decomposition to use,
given a domain and target number of processors,
is the reverse problem.
It turns out that there are often multiple decompositions that yield the same number of water processors.

A good decomposition has a high fraction of land eliminated and a tile aspect ratio near unity.
We select the preferred decompositions by filtering for small aspect ratio
(ar <= 1.15)
and then for most land eliminated
(smallest r = ratio of water processors to total).
The tables linked below list the preferred decompositions for bathymetries "downbyone2" and "201702".
The scripts here_ produce the tables.

.. _here: https://bitbucket.org/salishsea/analysis-michael/src/tip/land-processor-elimination/


.. _Preferred-MPI-LPE-Decompositions:

Preferred MPI LPE Decompositions
================================

.. toctree::

   LPE-SalishSea-downonegrid2
   LPE-SalishSea-201702

