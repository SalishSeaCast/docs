.. _WorkingOnMacOSX:

*****************************
Working on :kbd:`macOSX`
*****************************

This section describe things that are different from Salish or Sable
in getting NEMO to run on a mac running OSX.

Get Your Libraries
==================

Unless you are already using a different package manager, use brew.  First, as you probably have Anaconda set-up so you can do analysis in ipython notebooks on your mac, remove anaconda from your path.

.. code-block:: bash

    SAVEPATH = $PATH
    echo $PATH

copy everything other than /Users/yourname/anaconda/bin:

.. code-block:: bash

    export PATH=what you just copied

and check it looks good

.. code-block:: bash

    echo $PATH

Then see if the brewmaster is happy

.. code-block:: bash

    brew doctor

It will probably tell you to update

.. code-block:: bash

    brew update

Then install fortran

.. code-block:: bash

    brew install gfortran
    brew install gcc

Then to get netcdf we need to change the "tap"

.. code-block:: bash

    brew tap homebrew/science
    brew install netcdf --with-fortran

and put your path back

.. code-block:: bash

    export PATH=$SAVEPATH

Get the Correct Arch File
==========================

If you are on our SalishSea MEOPAR team, if you cloned the NEMO-code
repo you will get the arch file you need: arch-gfortran_osx.fcm with
the one modification from the standard file.  If you are not a member, our macosx arch file is:
 
:file:`arch-gfortran_osx.fcm`:

.. code-block:: bash

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

    %NCDF_INC            -I/usr/local/include
    %NCDF_LIB            -L/usr/local/lib -lnetcdf -lnetcdff
    %FC                  gfortran
    %FCFLAGS             -fdefault-real-8 -O3 -funroll-all-loops -fcray-pointer -ffree-line-length-none 
    %FFLAGS              %FCFLAGS
    %LD                  gfortran
    %LDFLAGS
    %FPPFLAGS            -P -C -traditional 
    %AR                  libtool
    %ARFLAGS             -c -s -o
    %MK                  make
    %USER_INC            %NCDF_INC
    %USER_LIB            %NCDF_LIB

Fix the Blanks
===============

If you then compile (e.g.):

.. code-block:: bash

    ./makenemo -m gfortran_osx -r GYRE -n myGYRE add_key "key_nosignedzero key_netcdf4"

you will get errors where fortran complains about spaces between a
variable and a bracket.  You have to go through and remove these.
(Seriously).  Tedious.  There are a set of files for the GYRE
configuration in myGYRE/MY_SRC with the blanks removed.  Other
configurations may need more corrections.

Run
===

All set, just remember that you only have compiled for only one processor.  So no need to run through mpiexec, you can directly run opa.exe.



