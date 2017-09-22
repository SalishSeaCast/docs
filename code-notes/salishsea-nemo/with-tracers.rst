.. _With-Tracers:

****************************************
Adding Passive Tracers to SalishSea
****************************************

This page provides instructions on how a configuration including
tracers was built for the Salish Sea model and how to use it and
modify it. More information on adding passive tracers can also 
be found `here`_. 

.. _here: http://ccar-modeling-documentation.readthedocs.io/en/latest/code-notes/TRC/Tracer_define.html


SalishSea_TRC Configuration
----------------------------

The configuration including the tracers is in SalishSea_TRC.  Provided
you have an upto date clone of the NEMO 3.4 code (available on `Bitbucket`_), you can
build it as

In :kbd:`NEMO-code/NEMOGCM/CONFIG`

.. code-block:: bash

    ./makenemo -n SalishSea_TRC

.. _Bitbucket: https://bitbucket.org/salishsea/nemo-code

New MY_SRC Files
-----------------

There are three extra modified fortran files to be found in MY_SRC (all
the other files are symlinked to the main SalishSea configuration).

The file :kbd:`par_my_trc.F90` specifies that five passive tracers are
to be used and will have the last index of jpmyt1, jpmyt2, jpmyt3,
jpmyt4 and jpmyt5, respectively.

The file :kbd:`trcini_my_trc.F90` has been barely changed from the
original (in `NEMO/TOP_SRC/MY_TRC`).  It now writes out the number 
of tracers.  If you wished to initialize tracers that would go here.

The file :kbd:`trcsms_my_trc.F90` defines the chemistry/biology to be
done to the tracers.  Here we just specify the value of each tracer to
be 1 in the grid cell we think contains the sewage outfall for each of
Clover, Macaulay, Iona, Nanaimo and Campbell River outfalls.  Note
that cells are defined by lats and lons.  Specifying grid locations
plays havoc when running mpi.

New Namelist Files
------------------

Two new namelist files are included: :kbd:`namlist_top` and
:kbd:`namelist_my_trc`.  The main :kbd:`namelist` should be copied or symlinked
from an appropriate SalishSea namelist.

:kbd:`namelist_my_trc` is blank.  If one wanted to define the depths and
locations of outflows in a namelist, this would be the one to do it.

:kbd:`namelist_top` defines the output names of the five tracers.  You
could also add tracer damping using this namelist.  Also change this
file to use a restart file for the tracers.

New IODEF.xml
---------------

Writing out of the tracers goes into its own netcdf4 file with key prtc.  The
example given writes out 1h data.

Grid/Input Files
----------------

Use appropriate files from the SalishSea configuration.



