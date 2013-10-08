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

modipsl_ is the framework that was used to obtain and build the NEMO code prior to NEMO up to v3.2.
:kbd:`modipsl` is available via anonymous :command:`svn` checkout from http://forge.ipsl.jussieu.fr/igcmg/svn/modipsl/.
However,
it is unclear what revision/tag of :kbd:`modipsl` the tarball contains.

.. _modipsl: http://forge.ipsl.jussieu.fr/igcmg/wiki/platform/en/documentation/J_outils#modipsl

The documented
(in "Notes on NEMO/OPA Usage" and in the `NEMO Quick Start Guide`_)
means of obtaining the NEMO code is to run:

.. code-block:: bash

    cd modipsl/util
    model NEMO

which automates the process of doing checkouts of code from a collection of :command:`cvs` and :command:`svn` repositories.

.. _NEMO Quick Start Guide: http://www.nemo-ocean.eu/Using-NEMO/User-Guides/Basics/NEMO-Quick-Start-Guide

Examination of the :file:`modipsl/util/log` file that the :file:`modipsl/util/model` script creates and the the :file:`modipsl/util/mod.def` definitions file for the script indicate that :file:`model` was run on 18-Mar-2010 and that the :kbd:`nemo_v3_1` tag was used to obtain the NEMO code.


Build Notes
-----------

.. note::

    The build process described here was attempted on several platforms with the following results:

    * MacBook running OS/X 10.8.5 with Xcode installed: :file:`fait_AA_make` reported :kbd:`sed: RE error: illegal byte sequence` numerous times,
    * :kbd:`salish`: :file:`fait_AA_make` failed because :command:`ksh` was not installed
    * :kbd:`jasper`:

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

* Run:

  .. code-block:: bash

      cd modeles/NEMO
      ../UTIL/fait_AA_make

  :file:`fait_AA_make` must be run from the :file:`modeles/NEMO/` directory.
  It calculates compilation rules,
  options,
  and build dependencies so as to create :file:`NEM/WORK/AA_make`
  (which is symlinked to :file:`modipsl/config/WC3/scripts/BB_make`)

* Run:

  .. code-block:: bash

      cd modipsl/util
      ./clr_make
      ./ins_make -t target

  to remove existing :file:`Makefiles` and create new ones.
  The target argument to :file:`ins_make` specifies a compiler or host name defined in :file:`AA_make.gdef`.

* Run:

  .. code-block:: bash

      cd modipsl/config/WC3
      gmake clean
      gmake all

  to compile and link the code.


Problems
~~~~~~~~

With the CPP keys above in :file:`BB_make.ldef` the :command:`gmake all` command on :kbd:`jasper` fails with these messages::

  CHANGE of CPP KEYS yes/no ?
  CPP options changed
  CHECKING KEY
  KEY USED :
  key_trabbl_dif
  key_vectopt_loop
  key_vectopt_memory
  key_orca_r2
  key_lim2
  key_dynspg_flt
  key_diaeiv
  key_ldfslp
  key_traldf_c2d
  key_traldf_eiv
  key_dynldf_c3d
  key_dtatem
  key_dtasal
  key_tradmp
  key_trabbc
  key_zdftke
  key_zdfddm
  CHECKING THE NUMBER AND NAMES OF SOURCE FILES
     use OPA_SRC files
     use LIM_SRC_2 files
     use C1D_SRC files
     use NST_SRC files
  ifort: error #10236: File not found:  'key_trabbl_dif'
  ifort: error #10236: File not found:  'key_vectopt_loop'
  ifort: error #10236: File not found:  'key_vectopt_memory'
  ifort: error #10236: File not found:  'key_orca_r2'
  ifort: error #10236: File not found:  'key_lim2'
  ifort: error #10236: File not found:  'key_dynspg_flt'
  ifort: error #10236: File not found:  'key_diaeiv'
  ifort: error #10236: File not found:  'key_ldfslp'
  ifort: error #10236: File not found:  'key_traldf_c2d'
  ifort: error #10236: File not found:  'key_traldf_eiv'
  ifort: error #10236: File not found:  'key_dynldf_c3d'
  ifort: error #10236: File not found:  'key_dtatem'
  ifort: error #10236: File not found:  'key_dtasal'
  ifort: error #10236: File not found:  'key_tradmp'
  ifort: error #10236: File not found:  'key_trabbc'
  ifort: error #10236: File not found:  'key_zdftke'
  ifort: error #10236: File not found:  'key_zdfddm'
  make[1]: *** [../../../lib/oce/libopa.a(par_kind.o)] Error 1

Other variations on the list of CPP keys yielded similar results.

Leaving :kbd:`P_P` unset in :file:`BB_make.ldef` resulted in::

  CHANGE of CPP KEYS yes/no ?
  CHECKING KEY
  KEY USED :
  CHECKING THE NUMBER AND NAMES OF SOURCE FILES
     use OPA_SRC files
     use LIM_SRC_2 files
     use C1D_SRC files
     use NST_SRC files
  ar: creating ../../../lib/oce/libopa.a
  dynadv_ppm.F90(76): warning #6843: A dummy argument with an explicit INTENT(OUT) declaration is not given an explicit value.   [PHTRA_ADV]
     SUBROUTINE adv_ppm_hor ( kt, pun, pvn, tra, traa, phtra_adv, z2, sort )
  -----------------------------------------------------^
  step.F90(684): error #6404: This name does not have a type, and must have an explicit type.   [LK_OBC_MER]
        IF( lk_obc_mer         )   CALL obc_rst_mer_wri( kstp )           ! write open boundary restart file
  ----------^
  step.F90(684): error #6341: A logical data type is required in this context.   [LK_OBC_MER]
        IF( lk_obc_mer         )   CALL obc_rst_mer_wri( kstp )           ! write open boundary restart file
  ----------^
  compilation aborted for step.F90 (code 1)
  make[1]: *** [../../../lib/oce/libopa.a(step.o)] Error 1
