.. _NowcastResults:

*********************
Nowcast Model Results
*********************

:file:`/results/SalishSea/nowcast/` holds the results from the daily nowcast runs.

:file:`/results/SalishSea/forecast/` holds the results from the daily forecast runs.

:file:`/results/SalishSea/forecast2/` holds the results from the daily forecast2 runs.

Initial run used:

* NEMO-3.4
* Bathymetry #2 (truncated Fraser River)
* TS4 tides
* practical salinity


Model Parameter Changes Over Time
=================================

.. |br| raw:: html

    <br>

===========  ===================================================  ==============  ==================
 Date                       Change                                New Value       Changeset
===========  ===================================================  ==============  ==================
27-Oct-2014  1st :file:`nowcast/` run results                     N/A
20-Nov-2014  1st :file:`forecast/` run results                    N/A
26-Nov-2014  Changed to tidal forcing tuned for better |br|       see changeset   efa8c39a9a7c_
             accuracy at Point Atkinson
28-Nov-2014  1st :file:`forecast2/` run results                   N/A
07-Dec-2014  Changed temperature of run-off from all rivers |br|  see changeset   e691e0c99dff_
             to that of the Fraser (one day only)
14-Dec-2014  Changed temperature of run-off from all rivers |br|  see changeset   e691e0c99dff_
             to that of the Fraser (ongoing)
28-Mar-2015  Horizontal turbulent diffusivity (kappa) |br|        10              89d2c2653d9e_
             reduced from 20.5
06-Aug-2015  Changed lateral momentum diffusion from |br|         see changeset   064a3be69f54_
             horizontal to iso-neutral direction
29-Sep-2015  Vertical background turbulent viscosity |br|         1.0e-5          9fdb426ea91f_
             reduced from 1.0e-4
03-Nov-2015  Increased deep salinity of western boundary |br|     see changeset   956b5587d773_
             conditions based on climatological comparisons |br|
             with IOS & WOD data
22-Nov-2015  Corrected and moved river run-off grid |br|          see changeset   5d1e00c2f44e_
             locations: |br|
             Oyster & 4 Jervis Inlet rivers were on land, |br|
             southern-most Fraser portion corrected from |br|
             Deas Slough to Canoe Pass
27-Nov-2015  Changed from using Neah Bay forecast residuals |br|  see changeset   65ce47429291_
             to using that forecast and calculating our own |br|
             residuals via ttide
15-Dec-2015  One day only: no weather available, used |br|        N/A             N/A
             14-Dec-2015 forecast
24-Jan-2016  Add vertical eddy viscosity & diffusion |br|         see changesets  e927e26ebe34_ |br|
             coefficients to :file:`*grid_W.nc` output |br|
             files. |br|
             Remove snowfall rate from :file:`*_grid_T.nc` |br|                   71946bd297a4_
             output files.
15-Oct-2016  Changed to NEMO-3.6 at SVN revision 6036. |br|
             Changed to :file:`bathy_downonegrid2.nc` |br|
             bathymetry and mesh mask. |br|
             Changed to conservative temperature for rivers |br|
             runoff and boundary conditions. |br|
             Changed to TEOS-10 reference salinity for |br|
             boundary conditions. |br|
             Restart file was |br|
             :file:`SalishSea_00553680_restart.nc` from |br|
             :file:`nowcast-green/14oct16/` run results.

26-Oct-2016  Updated NEMO code to NEMO-3.6r6204+                                  967d5a19d820_
29-Oct-2016  Moved :kbd:`nowcast` run results since |br|
             15oct16 into :file:`nowcast-blue/` directory. |br|
             Moved :kbd:`forecast` run results prior to |br|
             15oct16 into :file:`forecast-3.4/` directory. |br|
             Moved :kbd:`forecast2` run results prior to |br|
             15oct16 into :file:`forecast2-3.4/` directory.

28-Nov-2016  Updated NEMO code to NEMO-3.6r6459+ |br|                             a79d64786baa_ |br|
             Change to rebuild_nemo that writes |br|                              f9a8d03de741_ |br|
             deflated netcdf4/hdf5 files directly. |br|
             Start using land processor elimination in runs.                      fb724851d9f7_

09-Jan-2017  Updated NEMO code to NEMO-3.6r6770+ |br|                             3b0accb0e693_ |br|
             Change to Ceph/NFS shared storage on west.cloud.

27-Apr-2017  Increased lateral viscosity and diffusivity          2.2 m2/s        2c892a5bae80_

27-Aug-2017  Changed to v201702 bathymetry and parameters
             (see :ref:`HindcastResults` for details).
             Restart file from :file:`nowcast-green/26aug17`.
             nowcast-blue and nowcast-dev runs henceforth use
             previous day's nowcast-green restart file.

07-Oct-2017  Updated bathymetry to correct smoothing in Puget
             Sound, etc.
             
30-Dec-2017  New LiveOcean boundary condition processor with 
             longer western boundary and NO3 from LiveOcean
             
19-Jun-2018  Apply Neah Bay ssh to north boundary as well as                       6b89d1af1aec
             west boundary
             
16-Aug-2018  Change Orlanksi boundary conditions to calculate                      7c6a30e8bbf0
             along boundary wave on the boundary not one grid
             cell in.
===========  ===================================================  ==============  ==================

.. _efa8c39a9a7c: https://bitbucket.org/salishsea/ss-run-sets/commits/efa8c39a9a7c
.. _e691e0c99dff: https://bitbucket.org/salishsea/ss-run-sets/commits/e691e0c99dff
.. _89d2c2653d9e: https://bitbucket.org/salishsea/ss-run-sets/commits/89d2c2653d9e
.. _064a3be69f54: https://bitbucket.org/salishsea/ss-run-sets/commits/064a3be69f54
.. _9fdb426ea91f: https://bitbucket.org/salishsea/ss-run-sets/commits/9fdb426ea91f
.. _956b5587d773: https://bitbucket.org/salishsea/ss-run-sets/commits/956b5587d773
.. _5d1e00c2f44e: https://bitbucket.org/salishsea/nemo-forcing/commits/5d1e00c2f44e
.. _65ce47429291: https://bitbucket.org/salishsea/tools/commits/65ce47429291
.. _e927e26ebe34: https://bitbucket.org/salishsea/ss-run-sets/commits/e927e26ebe34
.. _71946bd297a4: https://bitbucket.org/salishsea/ss-run-sets/commits/71946bd297a4
.. _967d5a19d820: https://bitbucket.org/salishsea/nemo-3.6-code/commits/967d5a19d820
.. _a79d64786baa: https://bitbucket.org/salishsea/nemo-3.6-code/commits/a79d64786baa
.. _f9a8d03de741: https://bitbucket.org/salishsea/nemo-3.6-code/commits/f9a8d03de741
.. _fb724851d9f7: https://bitbucket.org/salishsea/tools/commits/fb724851d9f7
.. _3b0accb0e693: https://bitbucket.org/salishsea/nemo-3.6-code/commits/3b0accb0e693
.. _2c892a5bae80: https://bitbucket.org/salishsea/ss-run-sets/commits/2c892a5bae80
.. _6b89d1af1aec: https://bitbucket.org/salishsea/ss-run-sets/commits/6b89d1af1aec
.. _7c6a30e8bbf0: https://bitbucket.org/salishsea/nemo-3.6-code/commits/7c6a30e8bbf0
