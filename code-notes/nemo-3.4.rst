Notes on Downloading NEMO 3.4 from NEMO
=======================================

Getting the Code
----------------

*    Goto : nemo_
*    Logon.

*    Goto Using NEMO, then User Guides, the NEMO Quick Start Guide

*    locally switch to BASH as your shell (e.g. type bash)
*    check perl is installed (type perl and cntrl C when it sits on the new line waiting)
*    check svn is installed (type svn, it will suggest help for you)
*    check you have a fortran compiler (on our systems gfortran)
*    netcdf (not sure yet how you check this)

*    then type 

    .. code-block:: bash

        svn --username "sallen@eos.ubc.ca" co -r 3819 http://forge.ipsl.jussieu.fr/nemo/svn/branches/2012/dev_v3_4_STABLE_2012

*    EXCEPT change my username to your username on the NEMO system.
*    NOTE no < before svn unlike what they have on the website
*    You will be prompted for your password
*    and now you have the code

Making a Project
----------------


*   then change directory and make a project, e.g. 
    then for a new AMM12 configuration using gfortran on linux

    .. code-block:: bash

        cd dev_v3_4_STABLE_2012/NEMOGCM/CONFIG
        ./makenemo -m gfortran_linux -r AMM12 -n MY_AMM12


    .. note::

        stuck again

.. _nemo: http://www.nemo-ocean.eu/
