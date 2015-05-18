Notes on NEMO 3.6 Bugs and Quirks We Have Found
===========================================

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
This can be fixed in domzgr.F90 by changing the 0.d0's in this if statement to pp_to_be_computed.

.. code-block:: fortran

   IF( ppa1 == 0._wp .AND. ppa0 == 0._wp .AND. ppsur == 0._wp ) THEN

Can't reduce the number of Tidal Constituents
---------------------------------------------

The base namelist_ref has 11 tidal constituents.  The code sets the number of tidal constituents to
the larger of that in namelist_ref or namelist_cfg.  So with the base namelist it is not possible
to run less than 11 tidal constituents.  To fix this, remove all but one tidal constituent from
namelist_ref.
