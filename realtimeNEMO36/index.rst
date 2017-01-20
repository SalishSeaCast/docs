.. _realtimeNEMO36:

**************************
RealTime with Nemo 3.6
**************************

Started running NEMO 3.6 in realtime on Dec 5, 2015.

Physics only code is configuration SalishSea: compiled with keys:
key_bdy key_tide key_dynspg_ts key_vvl key_diainstant key_mpp_mpi
key_iomput key_netcdf4 key_nosignedzero key_zdfgls
key_ldfslp

Physics plus SOG code is configuration SOG: compiled with keys:
key_bdy key_dynspg_ts  key_vvl key_diainstant key_mpp_mpi key_iomput
key_netcdf4 key_nosignedzero  key_zdfgls key_ldfslp
key_top key_pisces key_tide

Namelists, iodef.xml and field_def.xml  are in SS-run-set/SalishSea/nemo3.6/nowcast

Currently running in /data/sallen/MEOPAR/nowcast-green

Results are (hand-moved) to /results/SalsishSea/nowcast-green

more details in the tools docs results section.
