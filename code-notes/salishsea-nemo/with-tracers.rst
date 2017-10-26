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
you have an upto date clone of the NEMO 3.6 code (available on `Bitbucket`_), you can
build it on Salish in :kbd:`NEMO-3.6-code/NEMOGCM/CONFIG as

.. code-block:: bash

    ./makenemo -n SalishSea_TRC -m GCC_SALISH -j8

.. _Bitbucket: https://bitbucket.org/salishsea/nemo-3.6-code

New MY_SRC Files
-----------------

There are three extra modified fortran files to be found in MY_SRC (most of
the other files are symlinked to the SalishSea and SMELT configuration).

The file :kbd:`par_my_trc.F90` specifies that one passive tracer is
to be used and will have the last index of jpmyt1.

The file :kbd:`trcini_my_trc.F90` has been barely changed from the
original (in :kbd:`NEMO-3.6-code/NEMOGCM/NEMO/TOP_SRC/MY_TRC`).  It now 
writes out the number of tracers and sets the tracer to 1 below 
depth level 20.  If you wished to initialize tracers differently, 
that would go here.

The file :kbd:`trcsms_my_trc.F90` defines the chemistry/biology to be
done to the tracers. It has been left mostly unchanged from the original
in :kbd:`NEMO-3.6-code/NEMOGCM/NEMO/TOP_SRC/MY_TRC`. 

An old version of this file is available at `nemo-code`_ in 
:kbd:`NEMOGCM/CONFIG/SalishSea_TRC/MY_SRC`. Here we just specify 
the value of each tracer to be 1 in the grid cell we think 
contains the sewage outfall for each of Clover, Macaulay, Iona, 
Nanaimo and Campbell River outfalls.  Note that cells are 
defined by lats and lons.  Specifying grid locations plays 
havoc when running mpi.

.. _nemo-code: https://bitbucket.org/salishsea/nemo-code

New Namelist Files
------------------

Two new namelist files are included: :kbd:`namlist_top_ref` and
:kbd:`namelist_top_cfg`.  The main :kbd:`namelist_ref` and :kbd:`namelist_cfg`
should be copied or symlinked from an appropriate SalishSea namelist.

:kbd:`namelist_top_red` defines the output names of the tracer.  You
could also add tracer damping using this namelist.  Also change this
file to use a restart file for the tracers.

New IODEF.xml
---------------

Writing out of the tracers goes into its own netcdf4 file with key prtc.  The
example given writes out 1h data.

Grid/Input Files
----------------

Use appropriate files from the SalishSea configuration.



