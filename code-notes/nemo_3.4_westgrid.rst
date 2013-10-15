Notes on Downloading/Running NEMO 3.4 on Jasper
===============================================

Note, this set-up runs on one core on one processor.

Getting the Code
----------------

see `Notes on Downloading/Running NEMO 3.4 on Ocean Cluster including Salish`_

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
    %F_O                 -O3 -r8 $(F_P)  -I$(MODDIR) -I$(MODDIR)/oce -module $(MODDIR) -assume byterecl -convert big_endian -heap-arrays $(NCDF_INC)
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
    %LIBDIR 	         ../../../lib
    %MODDIR  	         $(LIBDIR)

*   then change directory and make a project, e.g. 
    then for a new GYRE configuration using your new arch file ocean
*   Add the netcdf4 key to use netcdf4 capabilities

    .. code-block:: bash

        cd ../CONFIG
        ./makenemo -m ifort_jasper -r GYRE -n MY_GYRE add_key "key_netcdf4"

Running the Code
----------------

* Go to your version (where you want the results to end up)

    .. code-block:: bash

       cd MY_GYRE/EXP00
      
* Created a .pbs run file. For a simple run of GYRE that could be

* PBS file: :file:`GYRE.pbs` containing::

# Script for running simple GYRE configuration

   #PBS -l procs=1
   #PBS -l pmem=500mb
   #PBS -l walltime=00:05:00 

   echo "Current working directory is `pwd`"
   cd dev_v3_4_STABLE_2012/NEMOGCM/CONFIG/MY_GYRE/EXP00
   echo "Current working directory is `pwd`"

   echo "Starting run at: `data`"
   ./opa
   echo "Program opa finished with exit code $? at: `date`"


.. _nemo: http://www.nemo-ocean.eu/
