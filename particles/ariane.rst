.. _Ariane:

Ariane
======================================================================================================
Ariane is a Fortran code used to compute 3D streamlines in a given velocity field.

Getting the code
--------------------------------------
* Go to  http://stockage.univ-brest.fr/~grima/Ariane/
* Under “What's new”, choose Ariane-v2.2.6

* Register and use your username and password to download the zip file with Ariane's source code

Installing on :kbd:`salish`
------------------------------------------
On :kbd:`salish` create an Ariane working directory:

.. code-block:: bash

	mkdir $HOME/MEOPAR/Ariane

Place the :kbd:`ariane-2.2.6_00.tar.gz` package in that directory and unpack it

.. code-block:: bash

	cd $HOME/MEOPAR/Ariane
	gunzip ariane-2.2.6_00.tar.gz
	tar -xf ariane-2.2.6_00.tar

Specify the locations of the :kbd:`netcdf` libraries to help the :kbd:`configure` script find them:

.. code-block:: bash

	cd ariane-2.2.6_00
        export NETCDF_INC=/usr/include
        export NETCDF_LIB=/usr/lib

Configure the installation:

.. code-block:: bash

	./configure --prefix=$HOME/MEOPAR/Ariane
The :kbd:`prefix` argument overwrites the default install directory into a customized directory.

Make and install Ariane:

.. code-block:: bash

	make
	make check
	make install
:kbd:`make` compiles source files, :kbd:`make check` tests Ariane's qualitative and quantitative modes, and :kbd:`make install` installs Ariane.

Add the path for the Ariane executable to your :kbd:`PATH` environment variable:

.. code-block:: bash

	export PATH=$HOME/MEOPAR/Ariane/bin:$PATH

Now you can run Ariane from any directory by typing :kbd:`ariane`.


Installing on :kbd:`orcinus` 
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

	export PATH=$HOME/MEOPAR/Ariane/bin:$PATH

Now you can run Ariane from any directory by typing :kbd:`ariane`.

On :kbd:`orcinus` Ariane runs can also be sumbitted to the queue.
An example :kbd:`.pbs` will be included soon. 


Testing Ariane
------------------------
To test that you have everything set up correctly, run one of the Ariane examples. 
For instance, try:

.. code-block:: bash

       cd examples/qualitative
       ariane

You should notice several new files, such as :kbd:`ariane_trajectories_qualitative.nc` and :kbd:`traj.txt`.
These files contain the trajectory information.

* :kbd:`ariane_trajectories_qualitative.nc` can be loaded into a notebook to plot the particle locations over time and starting/finishing points, etc. 
* :kbd:`traj.txt` is helpful if you want to get a general idea of what the resulting trajectory coordinates look like or to check if the simulation ran properly.


Running Ariane: An example for Salish Sea model
------------------------------------------------------------------------------
To run your own trajectory simulation with Salish Sea model output, create a run directory:

.. code-block:: bash

	mkdir -p  $HOME/MEOPAR/Ariane/results/myexperiment
	cd $HOME/MEOPAR/Ariane/results/myexperiment
	
You will need :kbd:`namelist` and :kbd:`initial_positions.txt` files in this run directory (see below). 

Type :kbd:`ariane` to  run the code.


:kbd:`intitial_positions.txt`
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
The :kbd:`initial_positions.txt` contains 5 columns and as many rows as there are particles in the simulation you are running.


   .. code-block:: txt

       310  360   -1  1.000    1.0
       310  370   -2  1.000    1.0
       310  380   -1  1.000    1.0
       310  410   -1  1.000    1.0
       331  415   -1  1.000    1.0
       
This simulation, for example, will have 5 particles.

* Column 1: Spatial index (X)
* Column 2: Spatial index (Y)
* Column 3: Spatial index (Z)

	* A negative value tells Ariane to confine the particle to its original depth throught its trajectory. If you would like to have the particle trajectory include vertical movement, enter positive values and provide Ariane with the W velocity components in :kbd:`namelist` if using NEMO data. 
	* Since Ariane starts counting at 1, a "-1" or "1" in this column represents the surface.
* Column 4: Time index
* Column 5: Fifth parameter = 1.0


:kbd:`namelist`
^^^^^^^^^^^^^^^



 .. note::

      Condition 1:
      Condition 2:




Ariane output
------------------------

References
-------------------------------



