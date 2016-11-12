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
The results of this calculation are embedded in the salishsea command
(in salish.csv_)
such that specifying a decomposition
(4x9, 8x18, etc)
is enough information for the salishsea command to set jpnij and to request the correct number of water processors.

.. _salish.csv: https://bitbucket.org/salishsea/tools/raw/198ffbc7b9126add7dd518620f24e383d609ec3e/SalishSeaCmd/salishsea_cmd/salish.csv


Reverse problem
===============

Determining the decomposition to use,
given a domain and target number of processors,
is the reverse problem.
It turns out that there are often multiple decompositions that yield the same number of water processors.

A good decomposition will have a high fraction of land eliminated and a tile aspect ratio near unity.
Two decomposition tables are included below,
sorted by number of water processors.
The first table contains the preferred decompositions,
selected by filtering for small aspect ratio
(ar <= 1.15)
and then for most land eliminated
(smallest r = ratio of water processors to total).
The second table is not filtered and included for completeness.
The scripts here_ produce the tables.

.. _here: https://bitbucket.org/salishsea/analysis-michael/src/5b1b7ea73ba0af2bae824a32b21b2150935e60f6/land-processor-elimination/


.. toctree::

   LPE-SalishSea-preferred
   LPE-SalishSea-complete

