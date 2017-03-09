.. _transition:

Transition from SOG/PISCES to SMELT
======================================
The biological model dynamics are adapted from the 1-d SOG model with some modifications 
for the 3-d domain, as described in the following secionts.


Differences in Mesozooplankton Closure implementation
-----------------------------------------------------
It was necessary to adapt the implementation of the mesozooplankton closure term to three dimensions. 
In SMELT, the seasonally-varying mesozooplankton population is distributed in three dimensions so that 
its abundance is proportional to its food source.


Parameter definitions
----------------------
Parameter names have been adapted as described in the tables linked below. 

.. toctree::
   :maxdepth: 1

   paramTable_nampisprod


