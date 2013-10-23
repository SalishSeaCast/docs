****************************************************************************
Moving an AMM12 Configuration to JPP Configuration : Part Two: Add the Tides
****************************************************************************

At the end of part one, we had working code with nothing but noise.  Now we
need to add the tides.

Tide Forcing Files
------------------

CONCEPTS 110 uses tide files that give the tides over the whole domain as amplitude and phase.

NEMO 3.4 uses tide files that give the tides only on the open boundary as cosine and sine components.

In `Prepare Forcing Files`_ I calculate the latter from the former.

.. _Prepare Forcing Files: https://bitbucket.org/salishsea/tools/src/6849b9c32a2d66c1c7e608d30801d883e9e1aed9/I_ForcingFiles/Prepare%20Tide%20Files.ipynb?at=default

This produces three files : JPP_bdytide_M2_grid_X.nc where X = T, U or V.  These files are available in the `nemo-forcing repository`_ in the bdydta folder.

.. _nemo-forcing repository: https://bitbucket.org/salishsea/nemo-forcing
