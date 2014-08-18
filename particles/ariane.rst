.. _Ariane:

Ariane
======================================================================================================
Overview of Ariane...

Getting the code
--------------------------------------
Include link to website and version number to download

Installing and running on :kbd:`salish`
------------------------------------------


Installing and running on :kbd:`orcinus` 
------------------------------------------
On :kbd:`orcinus` create an Ariane working directory:

.. code-block:: bash

	mkdir $HOME/MEOPAR/Ariane

Place the :kbd:`ariane-2.2.6_00.tar.gz` package in that directory and unpack it:

.. code-block:: bash

	cd $HOME/MEOPAR/Ariane
	gunzip ariane-2.2.6_00.tar.gz
	tar -xf ariane-2.2.6_00.tar
	
Like :kbd:`salish`, we need to specify the locations of the :kbd:`netcdf` libraries on :kbd:`orcinus`:

.. code-block:: bash

	module load intel/14.0/netcdf_hdf5
        export NETCDF_INC=/global/software/lib64/ncsa-tools/include
        export NETCDF_LIB=/global/software/lib64/ncsa-tools/lib

Make and install Ariane:

.. code-block:: bash

	cd ariane-2.2.6_00
	./configure --prefix=$HOME/MEOPAR/Ariane
	make
	make check
	install

The :kbd:`prefix` flag indicates where Ariane will be installed.
Here we have chosen :kbd:`$HOME/MEOPAR/Ariane`.
There should be several new directories in this folder: :kbd:`bin:`, :kbd:`docs`, :kbd:`examples`.
To run Ariane, add the path for the Ariane executable to your :kbd:`PATH` environment variable:

.. code-block:: bash

	export PATH=SHOME/MEOPAR/Ariane/bin:$PATH

Now you can run Ariane from any directory by typing:

.. code-block:: bash

	ariane

To test that you have everything set up correctly, run one of the Ariane examples. 
For instance, try:

.. code-block:: bash

       cd examples/qualitative
       ariane

You should notice several new files, such as :kbd:`ariane_trajectories_qualitative.nc` and :kbd:`traj.txt`.
These files contain the trajectory information.
:kbd:`ariane_trajectories_qualitative.nc` can be loaded into a notebook to plot the particle locations over time and starting/finishing points, etc. 
A quick look at :kbd:`traj.txt` can be useful as well.

To run your own trajectory simulation with Salish Sea model output, create a run directory:

.. code-block:: bash

	mkdir -p  $HOME/MEOPAR/Ariane/results/myexperiment
	cd $HOME/MEOPAR/Ariane/results/myexperiment
	
You will need :kbd:`namelist` and :kbd:`initial_positions.txt` files in this run directory (see below). 
Type :kbd:`ariane` to  run the code. 

On :kbd:`orcinus` Ariane runs can also be sumbitted to the queue.
An example :kbd:`.pbs` will be included soon. 


Example run files for Salish Sea model
------------------------------------------------------------------------------

:kbd:`namelist`
^^^^^^^^^^^^^^^

:kbd:`intitial_positions.txt`
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Looking at Ariane output
------------------------

References
-------------------------------



