Notes on NEMO 3.6 Bugs and Quirks We Have Found
=================================================

The issues described here are clearly bugs in some cases,
and in other cases perhaps just things about NEMO that take time to understand.
In either case,
we hope that this page this will save someone some time!


Writes out to ocean.output that its 3.4
---------------------------------------

Top of the ocean.output file announces version number and it still says 3.4.  This can be changed
in nemogcm.F90 line 343.

Internally Calculated Vertical Grid Spacing
-------------------------------------------

If you want to have the code calculate the vertical grid spacing, you need to set

.. code-block:: fortran

   ppsur       = 999999.
   ppa0        = 999999.
   ppa1        = 999999.

as 999999. is the value of pp_to_be_computed.  However, if the code writes out that it is
calculating the vertical grid spacing if you set these values to 0 (but does not do anything).
This can be fixed in domzgr.F90 by changing the 0._wp's in this if statement to pp_to_be_computed.

.. code-block:: fortran

   IF( ppa1 == 0._wp .AND. ppa0 == 0._wp .AND. ppsur == 0._wp ) THEN

Can't reduce the number of Tidal Constituents
---------------------------------------------

The base namelist_ref has 11 tidal constituents.  The code sets the number of tidal constituents to
the larger of that in namelist_ref or namelist_cfg.  So with the base namelist it is not possible
to run less than 11 tidal constituents.  To fix this, remove all but one tidal constituent from
namelist_ref.

Tidal Harmonics not Written Out Correctly
-----------------------------------------

Tidal harmonics for bdy are now taken from tide.h90 in SBC.  However, there is no call to tide_harmo
before the tides are written out to ocean.output.  To fix this, in bdytides.F90

* remove the bang (!) in front of USE tide_mod

* add

.. code-block:: fortran

   call tide_harmo(omega_tide, v0tide, utide, ftide, ntide, nb_harmo)

before the tides are written.

* correct the units in the write statement, they are not deg/hr

.. code-block:: fortran

   WRITE(numout,*) '             Tidal cpt name    -     Phase speed (/s)'

Straight Boundary Segments from Namelist Crash
----------------------------------------------

The allocated size of the arrays are not set correctly if you use straight boundary segments of
less than a whole side and set them in the namelist.  The variable jpbdtau does not get set and this
causes a memory error when the boundary data is read.  To fix this add

.. code-block:: fortran

   jpbdtau = jpbdtas

in bdyini.F90, right after jpbdtas is calculated on line 482.
