Notes on Downloading/Running NEMO 3.4 on Jasper
===============================================

Set-up for runs on one processor or multiprocessors included.

Getting your Jasper Shell Ready
-------------------------------

* make sure your shell is bash (echo $SHELL), if its not, write to Westgrid support and get it changed.
* load the following modules (just putting these in my .bashrc file didn't work for me.)

.. code-block:: bash

     module load compiler/intel/12.1
     module load library/intelmpi/4.0.3.008
     module load library/netcdf/4.1.3
     module load library/szip/2.1


Getting the Code
----------------

see :ref:`GettingTheCodeNM34` in Notes on Downloading/Running NEMO 3.4 on Ocean Cluster including Salish

Making a Project
----------------

* Nemo uses an ARCH (architecture) file to determine compiler, maker, netcdf library location.

.. code-block:: bash

        cd dev_v3_4_STABLE_2012/NEMOGCM/ARCH

* NEMO ships with number of different arch files.  So far, however, what works is the jasper file from P. Myers GEOTRACES code:

* ARCH file: :file:`arch-ifort_jasper.fcm` containing::

    # ifort compiler options for jasper on westgrid: from P. Myers group
    # NCDF_INC    netcdf include file
    # NCDF_LIB    netcdf library
    # FC          Fortran compiler command
    # FCFLAGS     Fortran compiler flags
    # FFLAGS      Fortran 77 compiler flags
    # LD          linker
    # LDFLAGS     linker flags, e.g. -L<lib dir> if you have libraries in a
    # FPPFLAGS    pre-processing flags
    # AR          assembler
    # ARFLAGS     assembler flags
    # MK          make
    # USER_INC    additional include files for the compiler,  e.g. -I<include dir>
    # USER_LIB    additional libraries to pass to the linker, e.g. -l<library>


    %LIB_MPI             MPI2
    %LIB_MPI_BIS        
    %NCDF_INC            -I/lustre/jasper/software/netcdf/netcdf-4.1.3/include -lhdf5_hl -lhdf5 -lz -lsz
    %NCDF_LIB            -L/lustre/jasper/software/netcdf/netcdf-4.1.3/lib -lnetcdf -lnetcdff -lhdf5_hl -lhdf5 -lz -lsz
    %FC                  mpiifort
    %FCFLAGS 	         -c -fpp -r8 -O3 -assume byterecl -convert big_endian -heap-arrays
    %F_O                 -O3 -r8 $(F_P)  -I$(MODDIR) -module $(MODDIR) -assume byterecl -convert big_endian -heap-arrays $(NCDF_INC)
    %FFLAGS 	         $(F_O) -extend_source
    %LD                  mpiifort
    %PC                  cpp
    %FPPFLAGS            -P -C -traditional 
    %LDFLAGS
    %AR                  ar 
    %ARFLAGS             -r
    %MK                  make
    %USER_INC            %NCDF_INC
    %USER_LIB            %NCDF_LIB 
    %MODDIR  	         ../../../lib

*   then change directory and make a project, e.g. 
    then for a new GYRE configuration using your new arch file ocean
*   Add the netcdf4 key to use netcdf4 capabilities
*   Exactly the same (except GYRE -> AMM12) for AMM12 

    .. code-block:: bash

        cd ../CONFIG
        ./makenemo -m ifort_jasper -r GYRE -n MY_GYRE add_key "key_netcdf4"

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

   .. code-block:: bash
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



.. _nemo: http://www.nemo-ocean.eu/
