Notes on the Oct-2013 :file:`CODE.tar` Version
==============================================

These are notes on building and running the version of NEMO configured for the Salish Sea as received in the :file:`CODE.tar` archive prepared by J-P Paquin and downloaded at UBC on 2-Oct-2013.

The "Notes on NEMO/OPA Usage" document in the :file:`DOCUMENTATION.tar` archive provided guidance for the creation of the notes below.

.. TODO::

   Add links to "Notes on NEMO/OPA Usage" .doc and PDF


What's in :file:`CODE.tar`
--------------------------

The :file:`CODE.tar` tarball unpacks to a directory called :file:`CONCEPTS110_WCSD_OW_NOBC_tide` which apparently indicates:

* :kbd:`CONCEPTS110`: Canadian Operational Network of Coupled Environmental PredicTion Systems model at the version 110 level
* :kbd:`WCSD`: MEOPAR West Coast Sub-Domain configuration
* :kbd:`OW`: Open West boundary
* :kbd:`NOBC`: ?? maybe refers to reading OBC file data instead of initial conditions ??
* :kbd:`tide`: focus of the model runs is to calculate tides in the domain

:file:`CONCEPTS110_WCSD_OW_NOBC_tide` contains :file:`jcompile.sh`,
as :command:`bash` script that runs the build tool chain described below,
and the :file:`modipsl/` directory.

:kbd:`modipsl`
(``http://forge.ipsl.fr/igcmg/wiki/platform/en/documentation/J_outils#modipsl``)
is the framework that was used to obtain and build the NEMO code prior to NEMO up to v3.2.
:kbd:`modipsl` is available via anonymous :command:`svn` checkout from
``http://forge.ipsl.fr/igcmg/svn/modipsl/``.
However,
it is unclear what revision/tag of :kbd:`modipsl` the tarball contains.

The documented
(in "Notes on NEMO/OPA Usage" and in the "NEMO Quick Start Guide":
``https://www.nemo-ocean.eu/Using-NEMO/User-Guides/Basics/NEMO-Quick-Start-Guide``)
means of obtaining the NEMO code is to run:

.. code-block:: bash

    cd modipsl/util
    model NEMO

which automates the process of doing checkouts of code from a collection of :command:`cvs` and :command:`svn` repositories.

Examination of the :file:`modipsl/util/log` file that the :file:`modipsl/util/model` script creates and the the :file:`modipsl/util/mod.def` definitions file for the script indicate that :file:`model` was run on 18-Mar-2010 and that the :kbd:`nemo_v3_1` tag was used to obtain the NEMO code.


Build Notes
-----------

.. note::

    The build process described here was attempted on several platforms with the following results:

    * MacBook running OS/X 10.8.5 with Xcode installed: :file:`fait_AA_make` reported :kbd:`sed: RE error: illegal byte sequence` numerous times,
    * :kbd:`salish`: :file:`fait_AA_make` failed because :command:`ksh` was not installed
    * :kbd:`jasper`: build was successful

The build NEMO 3.1 for a new configuration the following steps are required:

* :file:`modeles/NEMO/OPA_SRC/par_oce.F90` must be edited to set the number of processors to be used,
  and include configuration parameters via a :file:`.h90` file.
  The edits are done in C-preprocessor (CPP) blocks.
  J-P Paquin did this,
  using the CPP key :kbd:`key_wc3` to specify an 8x8 processor layout and parameters in :file:`par_WC3.h90`.
  The edits are flagged with comments that start with :kbd:`!JPP`.

* Based on :file:`modeles/NEMO/OPA_SRC/par_ORCA_R2.h90`,
  create the configuration parameters header file included via the new CPP key in :file:`modeles/NEMO/OPA_SRC/par_oce.F90`.
  The file created by J-P Paquin is :file:`par_WC3.h90`.

* Edit :file:`modeles/UTIL/fait_config` to define the NEMO source files that are required for the build.
  The configuration name,
  :kbd:`WC3` in the present case,
  must be added to the :kbd:`LIST` shell variable,
  a newline-separated list of configuration names.
  A :kbd:`set -A` statement must also be added to the collection that follows the :kbd:`LIST`.
  The configuration is referenced in the :kbd:`set -A` statement as :kbd:`DIR_configname`;
  i.e. :kbd:`DIR_WC3` for the present case.
  The :kbd:`set -A` statement lists the source code directories that are to be included in the build.
  Those edits are present in :file:`fait_config` in the :file:`CODE.tar` tarball.

* Run:

  .. code-block:: bash

      cd modeles/UTIL
      ./fait_config WC3

  That results in the creation of the :file:`modeles/NEMO/WORK/` directory in which the source files from the directories given in the :kbd:`set -A DRI_WC3 ...` statement in :file:`fait_config` are symlinked so as to appear to be all in the same directory.
  The :file:`modipsl/config/WC3/` directory is also created.
  Its :file:`scripts/` directory contains the :file:`BB_make` and :file:`BB_make.ldef` files.

  .. note:

     The :file:`AA_make` and :file:`AA_make.ldef` files in :file:`modeles/NEMO/WORK/` are symlinked to :file:`BB_make` and :file:`BB_make.ldef` in :file:`modipsl/config/WC3/scripts/`.

* Edit :file:`BB_make.ldef` to activate/deactivate CPP keys for the configuration.
  The keys are listed in a single,
  space-separated line as the value of the :kbd:`P_P` variable.
  :file:`modipsl/config/WC3/scripts/BB_make.ldef` in :file:`CODE.tar` contains three :kbd:`P_P` lists identified with :kbd:`#-- JPP` comments.
  The uncommented one
  (and so presumably most recently used) is:

  .. code-block:: sh

      #-- JPP 20130717 Run compilation keys for TEST1
      P_P = key_wc3 key_dtatem key_dtasal key_flx_core key_vvl key_zrefsurf key_zdftke key_traldf_c2d key_dynldf_c3d key_mpp_mpi key_ldfslp key_dynspg_ts2 key_dtatem_month key_dtasal_month key_obc_mer key_tide key_diaharm

  Also ensure that there is a prefix for preprocessing line for the build/run target,
  for example:

  .. code-block:: sh

      #-Q- jasper  prefix = -D

* Ensure that there is an appropriate set of definitions in :file:`modipsl/util/AA_make.gdef` for the build/run target,
  for example:

  .. code-block:: sh

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

* Run:

  .. code-block:: bash

      cd modeles/NEMO
      ../UTIL/fait_AA_make

  to calculate compilation rules,
  options,
  and build dependencies so as to create :file:`NEMO/WORK/AA_make`
  (which is symlinked to :file:`modipsl/config/WC3/scripts/BB_make`)

  .. note::

      :file:`fait_AA_make` *must* be run from the :file:`modeles/NEMO/` directory.

* Run:

  .. code-block:: bash

      cd modipsl/util
      ./clr_make
      ./ins_make -t target

  to remove existing :file:`Makefiles` and create new ones.
  The target argument to :file:`ins_make` specifies a compiler or host name defined in :file:`modipsl/util/AA_make.gdef` and :file:`modipsl/config/WC3/scripts/BB_make.ldef`.

* Run:

  .. code-block:: bash

      cd modipsl/config/WC3
      make clean
      make

  to compile and link the code.

The results of a successful build are:

* a :file:`../../bin/opa` executable
* a :file:`../../lib/libioipsl.a` library
* a :file:`../../lib/oce/libopa.a` library



Problems
~~~~~~~~

With the CPP keys above in :file:`BB_make.ldef` the :command:`make` command on :kbd:`jasper` completes with these messages::

  dynadv_ppm.F90(76): warning #6843: A dummy argument with an explicit INTENT(OUT) declaration is not given an explicit value.   [PHTRA_ADV]
     SUBROUTINE adv_ppm_hor ( kt, pun, pvn, tra, traa, phtra_adv, z2, sort )
  -----------------------------------------------------^
  ./ldfdyn_c3d.h90(148): remark #8291: Recommended relationship between field width 'W' and the number of fractional digits 'D' in this edit descriptor is 'W>=D+7'.
                 IF(lwp) WRITE(numout,'(34x,E7.2,8x,i3)') zcoef(jk) * ahm0, jk
  -------------------------------------------^
  dynzdf_imp.F90(20): remark #6536: All symbols from this module are already visible due to another USE; the ONLY clause will have no effect. Rename clauses, if any, will be honored.   [OCE]
     USE oce             ! ocean dynamics and tracers
  -------^

  The library is up-to-date

  mpiifort  -o ../../../bin/opa model.o ../../../lib/oce/libopa.a  ../../../lib/libioipsl.a -L/lustre/jasper/software/netcdf/netcdf-4.1.3/lib -lnetcdf -lnetcdff -lhdf5_hl -lhdf5 -lz -lsz
  /lustre/jasper/software/intel/l_ics_2012.0.032/composer_xe_2011_sp1.10.319/compiler/lib/intel64/libimf.so: warning: warning: feupdateenv is not implemented and will always fail
  OPA model is OK


Run Notes
---------

The :file:`WCSD_RUN_tide_M2_OW_ON_file_DAMP_ANALY.tar` tarball contain the :file:`namelist` and scripts to setup and run on the BIO HPC cluster:

* :file:`linkfile.sh` links the intial conditions,
  forcing,
  etc.
  files into the run directory with the file names that NEMO expects

* :file:`namelist` is the NEMO namelist for the run

* :file:`submit_64.sh` is the file containing PBS directives and shell commands that is submitted to the TORQUE resource manager via :command:`qsub`

The meaning of :kbd:`WCSD_RUN_tide_M2_OW_ON_file_DAMP_ANALY`
(from J-P's :file:`README.txt`) is::

   WCSD   : West Coast Sub Domain (398x345)
   M2     : Run with only M2 tides from WebTide
   OW_ON  : Open West & OpenNorth boundaries
   file   : reading OBC file (not initial conditions)
   DAMP   : increased horizontal eddy viscosity
   ANALY  : Analytical forcing (namsbc_ana) - no atm-ocean fluxes
            or atmospheric forcing

The :file:`WCSD_PREP.tar` tarball contains the intial conditions,
forcing,
etc.
files for the :kbd:`WCSD_RUN_tide_M2_OW_ON_file_DAMP_ANALY` case.

With those two tarballs unpacked beside each other one the :kbd:`dirPREP` variable in :file:`linkfile.sh` need to be set to:

.. code-block:: bash

    dirPREP=../WCSD_PREP

and :file:`linkfile.sh` run in :file:`WCSD_RUN_tide_M2_OW_ON_file_DAMP_ANALY` to prepare for the run.

:file:`submit_64.sh` is tailored to the BIO HPC cluster.
To run on :kbd:`jasper`,
the following script was used:

.. code-block:: bash

    #!/bin/bash

    #PBS -N WCSD_RUN_tide_M2_OW_ON_file_DAMP_ANALY
    #PBS -S /bin/bash
    #PBS -l procs=64
    # memory per processor
    #PBS -l pmem=2gb
    #PBS -l walltime=1:00:00
    # email  when the job [b]egins and [e]nds, or is [a]borted
    #PBS -m bea
    #PBS -M dlatornell@eos.ubc.ca
    #PBS -o OPA.output
    #PBS -e OPA.output.error


    cd $PBS_O_WORKDIR
    echo working dir: $(pwd)

    module load compiler/intel/12.1
    module load library/intelmpi/4.0.3.008
    module load library/netcdf/4.1.3
    module load library/szip/2.1

    mpiexec ./opa

If that script is stored as :file:`jasper.pbs`,
a run is submitted with the command:

.. code-block:: bash

    qsub jasper.pbs

As an initial test,
the run duration was set to 720 time steps via the :kbd:`&namrun.nitend` namelist item.
The run completed in just over 2 minutes.
A subsequent 4320 time step run took about 17 minutes.


Post-Processing
---------------

The results of the runs described above are groups of 64 netCDF files
(one for each processor)
for each of the calculated quantities:

* U, V, W, and T
* :file:`output.init`: initial time step output ??
* restart and open boundary condition restart
* 2D slice timeseries results
* tidal harmonics disagnostic results


NOCSCOMBINE
~~~~~~~~~~~

Google lead to the :kbd:`NOCSCOMBINE` tool at ftp://ftp.soc.soton.ac.uk/omfftp/NEMO/NOCSCOMBINE.tar.
Building it on :kbd:`jasper` required creation of a new :file:`makefile` with :kbd:`NCHOME` and :kbd:`LIBS` variable set to:

.. code-block:: make

    NCHOME = /lustre/jasper/software/netcdf/netcdf-4.1.3
    LIBS = -L$(NCHOME)/lib -I$(NCHOME)/include -lnetcdf -lnetcdff -lhdf5_hl -lhdf5 -lz -lsz

Commands like:

.. code-block:: bash

    cd WCSD_RUN_tide_M2_OW_ON_file_DAMP_ANALY/
    ../../NOCSCOMBINE/nocscombine -f WC3_CU60_20020102_20020104_grid_U_0000.nc

result in the 64 pre-processor files of u velocity results being combined into a single :file:`WC3_CU60_20020102_20020104_grid_U.nc` file.
The process takes over 10 minutes per quantity for
U, V, and T
for the 72 hour run,
and nearly 30 minutes for W.
