Notes on Downloading/Running NEMO 3.4 on Ocean Cluster including Salish
=======================================================================

Note, this set-up runs on one core on one processor.

.. _GettingTheCodeNM34:

Getting the Code
----------------

* Goto http://www.nemo-ocean.eu/
* Logon.
* Goto Using NEMO, then User Guides, the NEMO Quick Start Guide
* locally switch to BASH as your shell (e.g. type bash)
*    check perl is installed (type perl and cntrl C when it sits on the new line wang)
* check svn is installed (type svn, it will suggest help for you)
* check you have a fortran compiler (on our systems gfortran)
* netcdf (not sure yet how you check this)
* then type

  .. code-block:: bash

      svn --username "sallen@eos.ubc.ca" co -r 3819 http://forge.ipsl.jussieu.fr/nemo/svn/branches/2012/dev_v3_4_STABLE_2012

* EXCEPT change my username to your username on the NEMO system.
* NOTE no < before svn unlike what they have on the website
* You will be prompted for your password
* and now you have the code

Making a Project
----------------

* Nemo uses an ARCH (architecture) file to determine compiler, maker, netcdf library location.

  .. code-block:: bash

      cd dev_v3_4_STABLE_2012/NEMOGCM/ARCH

* NEMO ships with a gfortran_linux file.  This file needs some edits to work on ocean. Changes in bold.

* New ARCH file: :file:`arch-ocean.fcm` containing::

    # generic gfortran compiler options for linux
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
    %NCDF_INC            **-I/usr/include**
    %NCDF_LIB            **-L/usr/lib -lnetcdff**
    %FC                  gfortran
    %FCFLAGS             -fdefault-real-8 -O3 -funroll-all-loops -fcray-pointer
    %FFLAGS              %FCFLAGS
    %LD                  gfortran
    %LDFLAGS
    %FPPFLAGS            -P -C -traditional
    %AR                  ar
    %ARFLAGS             -rs
    %MK                  **make**
    %USER_INC            %NCDF_INC
    %USER_LIB            %NCDF_LIB


*   then change directory and make a project, e.g.
    then for a new GYRE configuration using your new arch file ocean
*   Note we need to add the key_nosignedzero for our fortran 90 compiler
*   Add the netcdf4 key to use netcdf4 capabilities

    .. code-block:: bash

        cd ../CONFIG
        ./makenemo -m ocean -r GYRE -n MY_GYRE add_key "key_nosignedzero key_netcdf4"

*   If the following error comes up:

    .. code-block:: bash

       /* Copyright (C) 1991-2012 Free Software Foundation, Inc.
        1
       Error: Invalid character in name at (1)

    then modify the arch-ocean.fcm file line:

    .. code-block:: bash

       %FPPFLAGS	-P -C -traditional

    to:

    .. code-block:: bash

       %FPPFLAGS	-P -traditional

Running the Code
----------------

.. code-block:: bash

   cd MY_GYRE/EXP00
   nice ./opa

Notes on NEMO 3.6
------------------
When getting the code, follow the instructions for NEMO 3.4 but instead type:

  .. code-block:: bash

      svn --username "username@eos.ubc.ca" co http://forge.ipsl.jussieu.fr/nemo/svn/branches/2015/nemo_v3_6_STABLE/NEMOGCM