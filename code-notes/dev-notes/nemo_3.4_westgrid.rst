Notes on Downloading/Running NEMO 3.4 on Jasper
===============================================

Set-up for runs on one processor or multiprocessors included.

Getting your Jasper Shell Ready
-------------------------------

* make sure your shell is bash (echo $SHELL), if its not, write to Westgrid support and get it changed.
* follow the instructions in :ref:`LoadingModulesOnHPCClusters` to manually load the necessary software component modules or edit your :kbd:`jasper` :file:`$HOME/.bashrc` to make them load automatically when you :program:`ssh` into :kbd:`jasper`.


Getting the Code
----------------

see :ref:`GettingTheCodeNM34` in Notes on Downloading/Running NEMO 3.4 on Ocean Cluster including Salish

Making a Project
----------------

* NEMO uses an ARCH (architecture) file to determine compiler, maker, netcdf library location.

  .. code-block:: bash

      cd dev_v3_4_STABLE_2012/NEMOGCM/ARCH

* NEMO ships with a number of different arch files.  After changes to Jasper's operating system and updated arch file is needed:

  .. code-block:: sh

      # makefile definitions for mpif90 on jasper.westgrid.ca; based on ifort file from P. Myers group
      #
      # NCDF_INC    netcdf include file
      # NCDF_LIB    netcdf library
      # FC          Fortran compiler command
      # FCFLAGS     Fortran compiler flags
      # LD          linker
      # LDFLAGS     linker flags, e.g. -L<lib dir> if you have libraries in a
      # FPPFLAGS    pre-processing flags
      # AR          assembler
      # ARFLAGS     assembler flags
      # MK          make
      # USER_INC    additional include files for the compiler,  e.g. -I<include dir>
      # USER_LIB    additional libraries to pass to the linker, e.g. -l<library>

      %NCDF_HOME    /global/software/netcdf/netcdf-4.1.3
      %HDF5_HOME    /global/software/hdf5/hdf5-1.8.9
      %NCDF_INC     -I%NCDF_HOME/include
      %NCDF_LIB     -L%NCDF_HOME/lib -lnetcdff -lnetcdf -L%HDF5_HOME/lib -lhdf5_hl -lhdf5 -lhdf5
      %CPP          cpp
      %FC           mpif90
      %FCFLAGS      -c -fpp -r8 -O3 -assume byterecl -convert big_endian -heap-arrays
      %LD           mpif90
      %FFLAGS       %FCFLAGS
      %LDFLAGS      -lstdc++
      %FPPFLAGS     -P -C -traditional
      %AR           ar
      %ARFLAGS      -r
      %MK           make
      %USER_INC     %NCDF_INC
      %USER_LIB     %NCDF_LIB


*   then change directory and make a project, e.g.
    then for a new GYRE configuration using your new arch file ocean
*   Add the netcdf4 key to use netcdf4 capabilities
*   Exactly the same (except GYRE -> AMM12) for AMM12

    .. code-block:: bash

        cd ../CONFIG
        ./makenemo -m mpif90_jasper -r GYRE -n MY_GYRE add_key "key_netcdf4"

Running the Code: GYRE
----------------------

* Go to your version (where you want the results to end up)

  .. code-block:: bash

     cd MY_GYRE/EXP00

* Created a .pbs run file. For a simple run of GYRE that could be

* PBS file: :file:`GYRE.pbs` containing::

   # Script for running simple GYRE configuration

   #PBS -l procs=1
   #PBS -l pmem=500mb
   #PBS -l walltime=00:05:00

   module load compiler/intel/12.1
   module load library/intelmpi/4.0.3.008
   module load library/netcdf/4.1.3
   module load library/szip/2.1

   module list
   echo "Current working directory is `pwd`"
   cd dev_v3_4_STABLE_2012/NEMOGCM/CONFIG/MY_GYRE/EXP00
   echo "Current working directory is `pwd`"

   echo "Starting run at: `date`"
   ./opa
   echo "Program opa finished with exit code $? at: `date`"


* and run

  .. code-block:: bash

     qsub GYRE.pbs

Running the CODE: AMM12: 32 Processors
--------------------------------------

* Need to get the AMM12 forcing and initialization files, untar and unzip

  .. code-block:: bash

     curl -LO http://dodsp.idris.fr/reee512/NEMO/amm12_inputs_v3_4.tar
     cd dev_v3_4_STABLE_2012/NEMOGCM/CONFIG/MY_AMM12/EXP00/
     tar xvf ~/amm12_inputs_v3_4.tar
     gunzip *.gz
     rm ~/amm12_input_v3_4.tar

* To make AMM12 run on multiple processors, edit the namelist file, changing the following lines

  .. code-block:: fortran

    jpni = 8
    jpnj = 4
    jnpij = 32

* Need a .pbs file for multiple core run
  PBS file: :file:`AMM_multi.pbs` containing::

    # Script for running multiple processor AMM12  configuration

    #PBS -l procs=32
    #PBS -l pmem=500mb
    #PBS -l walltime=00:15:00

    module load compiler/intel/12.1
    module load library/intelmpi/4.0.3.008
    module load library/netcdf/4.1.3
    module load library/szip/2.1

    module list
    echo "Current working directory is `pwd`"
    cd dev_v3_4_STABLE_2012/NEMOGCM/CONFIG/MY_AMM12/EXP00
    echo "Current working directory is `pwd`"

    echo "Starting run at: `date`"
    mpiexec ./opa

* and run

  .. code-block:: bash

     qsub AMM12_multi.pbs
