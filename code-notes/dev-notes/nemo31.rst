Notes on NEMO v3.1
==================

These are notes on building and running NEMO v3.1 from https://forge.ipsl.jussieu.fr/nemo/browser/tags/nemo_v3_1.

The `Installing NEMO using modipsl`_ instructions and the `modipsl documentation`_ provided some guidance for the creation of the notes below.

.. _Installing NEMO using modipsl: http://www.nemo-ocean.eu/Using-NEMO/User-Guides/Basics/NEMO-Quick-Start-Guide
.. _modipsl documentation: http://forge.ipsl.jussieu.fr/igcmg/wiki/platform/en/documentation/C_installation


Getting the Code
----------------

* Create a new working directory and do an :command:`svn` trunk checkout of the :kbd:`modipsl` framework:

.. code-block:: bash

    mkdir nemo31
    cd nemo31
    svn checkout http://forge.ipsl.jussieu.fr/igcmg/svn/modipsl/trunk modipsl

* Edit :file:`modipsl/util/mod.def` to add a :kbd:`NEMO_31` section containing::

    #-H- NEMO_31  NEMO
    #-H- NEMO_31  OPA
    #-H- NEMO_31  LIM
    #-H- NEMO_31  TOP
    #-H- NEMO_31  IOIPSL/src - svn - tag v2_1_4
    #-H- NEMO_31  NEMO sources and configurations - svn - tag nemo_v3_1
    #-M- NEMO_31  nemo_st@locean-ipsl.upmc.fr
    #-C- NEMO_31  IOIPSL/tags/v2_1_9/src                        HEAD  8  IOIPSL/src             modeles
    #-C- NEMO_31  XMLF90                                        HEAD  12 XMLF90                 modeles
    #-C- NEMO_31  XMLIO_SERVER/trunk                              51  12 XMLIO_SERVER           modeles
    #-C- NEMO_31  tags/nemo_v3_1/EXTERNAL/XMLF90                HEAD  7  XMLF90/external        modeles
    #-C- NEMO_31  tags/nemo_v3_1/EXTERNAL/XMLIO_SERVER          HEAD  7  XMLIO_SERVER/external  modeles
    #-C- NEMO_31  tags/libIGCM_v1_4                             HEAD  10 libIGCM                .
    #-C- NEMO_31  tags/nemo_v3_1/AGRIF                          HEAD  7  .                      modeles
    #-C- NEMO_31  tags/nemo_v3_1/NEMO                           HEAD  7  .                      modeles
    #-C- NEMO_31  tags/nemo_v3_1/UTIL                           HEAD  7  .                      modeles
    #-C- NEMO_31  tags/nemo_v3_1/CONFIG/GYRE                    HEAD  7  GYRE                   config
    #-C- NEMO_31  tags/nemo_v3_1/CONFIG/GYRE_LOBSTER            HEAD  7  GYRE_LOBSTER           config
    #-C- NEMO_31  tags/nemo_v3_1/CONFIG/ORCA2_LIM               HEAD  7  ORCA2_LIM              config
    #-C- NEMO_31  tags/nemo_v3_1/CONFIG/ORCA2_LIM_PISCES        HEAD  7  ORCA2_LIM_PISCES       config
    #-C- NEMO_31  tags/nemo_v3_1/CONFIG/ORCA2_OFF_PISCES        HEAD  7  ORCA2_OFF_PISCES       config
    #-C- NEMO_31  tags/nemo_v3_1/CONFIG/POMME                   HEAD  7  POMME                  config
    #-C- NEMO_31  tags/nemo_v3_1/CONFIG/ORCA2_LIM/EXP00         HEAD  7  PARAM                  config/ORCA2_LIM/IGCM00
    #-C- NEMO_31  tags/nemo_v3_1/CONFIG/ORCA2_LIM_PISCES/EXP00  HEAD  7  PARAM                  config/ORCA2_LIM_PISCES/IGCM00
    #-C- NEMO_31  tags/nemo_v3_1/CONFIG/ORCA2_OFF_PISCES/EXP00  HEAD  7  PARAM                  config/ORCA2_OFF_PISCES/IGCM00

* Install NEMO v3.1 and its dependencies:

  .. code-block:: bash

      cd modipsl/util
      ./model NEMO_31

  Your :kbd:`nemos-ocean.eu` credentials are required for most of the checkouts that :command:`model` does and you will be asked for your password for each one unless you have :command:`svn` password caching configured.


Build Notes
-----------

The build process described here was done on :kbd:`jasper.westgrid.ca`.

* Load the following modules to configure the :kbd:`jasper` environment:

  .. code-block:: bash

      module load compiler/intel/12.1
      module load library/intelmpi/4.0.3.008
      module load library/netcdf/4.1.3
      module load library/szip/2.1
      module load application/ncview/2.1.1

* For the initial test the :kbd:`GYRE` configuration was used.
  Still working in the :file:`modipsl/util/` directory:

  .. code-block:: bash

      ../modeles/UTIL/fait_config GYRE

* Edit :file:`AA_make.gdef` to add a :kbd:`jasper` section containing::

    #-Q- jasper     #- Global definitions for jasper.westgrid.ca using Linux Compiler Intel v8
    #-Q- jasper     LIB_MPI = MPI2
    #-Q- jasper     LIB_MPI_BIS =
    #-Q- jasper     M_K = make
    #-Q- jasper     P_C = cpp
    #-Q- jasper     P_O = -P -C -traditional $(P_P)
    #-Q- jasper     F_C = mpiifort -c -fpp
    #-Q- jasper     #-D- MD    F_D = -g
    #-Q- jasper     #-D- MN    F_D =
    #-Q- jasper     #-P- I4R4  F_P = -i4
    #-Q- jasper     #-P- I4R8  F_P = -i4 -r8
    #-Q- jasper     #-P- I8R8  F_P = -i8 -r8
    #-Q- jasper     #-P- ??    F_P = -i4 -r8
    #-Q- jasper     F_O = -O3 $(F_P)  -I$(MODDIR) -I$(MODDIR)/oce -module $(MODDIR) -assume byterecl -convert big_endian -I $(NCDF_INC)
    #-Q- jasper     F_F = $(F_O) -extend_source
    #-Q- jasper     F_L = mpiifort
    #-Q- jasper     L_O =
    #-Q- jasper     A_C = ar -r
    #-Q- jasper     A_G = ar -x
    #-Q- jasper     C_C = gcc -c
    #-Q- jasper     C_O =
    #-Q- jasper     C_L = gcc
    #-Q- jasper     #-
    #-Q- jasper     NCDF_INC = -I/lustre/jasper/software/netcdf/netcdf-4.1.3/include -lhdf5_hl -lhdf5 -lz -lsz
    #-Q- jasper     NCDF_LIB = -L/lustre/jasper/software/netcdf/netcdf-4.1.3/lib -lnetcdf -lnetcdff -lhdf5_hl -lhdf5 -lz -lsz


* Edit :file:`../config/GYRE/BB_make.ldef` to add a :kbd:`jasper` prefix for preprocessing::

    #-Q- jasper  prefix = -D

* Run:

  .. code-block:: bash

      cd ../modeles/NEMO
      ../UTIL/fait_AA_make

  to calculate compilation rules,
  options,
  and build dependencies so as to create :file:`NEMO/WORK/AA_make`
  (which is symlinked to :file:`modipsl/config/GYRE/scripts/BB_make`)

  .. note::

      :file:`fait_AA_make` *must* be run from the :file:`modeles/NEMO/` directory.

* Remove any existing :file:`Makefiles` and create new ones:

  .. code-block:: bash

      cd ../../util
      ./clr_make
      ./ins_make -t jasper

* Compile and link the code:

  .. code-block:: bash

      cd ../config/GYRE
      make clean
      make

The results of a successful build are:

* a :file:`../../bin/opa` executable
* a :file:`../../lib/libioipsl.a` library
* a :file:`../../lib/oce/libopa.a` library
