Notes on Downloading NEMO 3.4 from NEMO
=======================================

Getting the Code
----------------

.. note::

    Goto : nemo_
    Logon.

    Goto Using NEMO, then User Guides, the NEMO Quick Start Guide

    switch to BASH as your shell (e.g. type bash)
    check perl is installed (type perl and cntrl C when it sits on the new line waiting)
    check svn is installed (type svn, it will suggest help for you)
    check you have a fortran compiler (on our systems gfortran)
    netcdf (not sure yet how you check this)

    then type 
    svn --username "sallen@eos.ubc.ca" co -r 3819 http://forge.ipsl.jussieu.fr/nemo/svn/branches/2012/dev_v3_4_STABLE_2012
    EXCEPT change my username to your username on the NEMO system.
    NOTE no < before svn unlike what they have on the website

    then cd dev_v3_4_STABLE_2012/NEMOGCM/CONFIG
    then for a new AMM12 configuration using gfortran on linux
    ./makenemo -m gfortran_linux -r AMM12 -n MY_AMM12

    stuck again

.. _nemo: http://www.nemo-ocean.eu/
