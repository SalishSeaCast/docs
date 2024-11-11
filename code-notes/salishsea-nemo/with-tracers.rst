.. _With-Tracers:

****************************************
Adding Passive Tracers to SalishSea
****************************************

This page provides instructions on how a configuration including
tracers was built for the Salish Sea model and how to use it and
modify it. More information on adding passive tracers can also
be found `here`_.

.. _here: https://ccar-modeling-documentation.readthedocs.io/en/latest/code-notes/TRC/Tracer_define.html


Compile SalishSea_TRC
----------------------------

The configuration including the tracers is SalishSea_TRC.
The NEMO ARCH files use the :envvar:`XIOS_HOME` environment variable to find the XIOS-2 library you built above.
:envvar:`XIOS_HOME` *must* be an absolute path to your XIOS-2 clone directory.
You can set :envvar:`XIOS_HOME` on the command-line before the :command:`makenemo` and :command:`maketools` commands as shown below,
or you can set and export the value of :envvar:`XIOS_HOME` in your :file:`$HOME/.bashrc` file.

.. code-block:: bash

    cd NEMO-3.6-code/NEMOGCM/CONFIG
    XIOS_HOME=/data/$USER/MEOPAR/XIOS-2/ ./makenemo -n SalishSea_TRC -m GCC_SALISH -j8


New MY_SRC Files
-----------------

There are three extra modified fortran files to be found in MY_SRC (most of
the other files are symlinked to the SalishSea and SMELT configuration).

The file :kbd:`par_my_trc.F90` specifies that six passive tracers are
to be used and will have the last index of jpmyt1, jpmyt2, jpmty3, jpmty4, jpmty5, jpmty6 respectively.

The file :kbd:`trcini_my_trc.F90` has been barely changed from the
original (in :kbd:`NEMO-3.6-code/NEMOGCM/NEMO/TOP_SRC/MY_TRC`).  It now
writes out the number of tracers and sets the tracer concentration to 1 below
depth level 10, 15, 19, 22, 24, and 25 corresponding to depths 10, 15, 20, 30, 45, and 60 m respectively.
If you wished to initialize the tracers differently, that would go here.

The file :kbd:`trcsms_my_trc.F90` defines the chemistry/biology to be
done to the tracers. It now dictates that the tracers decay when they are
above depths 10, 15, 20, 30, 45, and 60 m respectively.
For example, if a tracer was at concentration 1 in the 'decay' zone,
after one model day, their concentration would be ~0.3679 (1/e).
The decay time for each tracer could be modified by changing the factor used to multiply
the trb array on lines 73, 78, 83, 88, 93, and 98 in the file.

An old version of this file is available at `nemo-code`_ in
:kbd:`NEMOGCM/CONFIG/SalishSea_TRC/MY_SRC`. Here we just specify
the value of each tracer to be 1 in the grid cell we think
contains the sewage outfall for each of Clover, Macaulay, Iona,
Nanaimo and Campbell River outfalls.  Note that cells are
defined by lats and lons.  Specifying grid locations plays
havoc when running mpi.

.. _nemo-code: https://github.com/SalishSeaCast/NEMO-3.4-Code

New Namelist Files
------------------

Two new namelist files are included: :kbd:`namlist_top_ref` and
:kbd:`namelist_top_cfg`.  The main :kbd:`namelist_ref` and :kbd:`namelist_cfg`
should be copied or symlinked from an appropriate SalishSea namelist.

:kbd:`namelist_top_ref` defines the output names of the tracer.  You
could also add tracer damping using this namelist.  Also change this
file to use a restart file for the tracers.

New IODEF.xml
---------------

Writing out of the tracers goes into its own netcdf4 file with key prtc.  The
example given writes out 1h data.

Grid/Input Files
----------------

Use appropriate files from the SalishSea configuration.
