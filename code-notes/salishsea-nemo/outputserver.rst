***************************
Output Server Configuration
***************************

This section describes how to control the output files from NEMO.
We will discuss how to output certain fields at a specific grid point.
First, some overview on the NEMO output configuration file :file:`iodef.xml` is provided.


:file:`iodef.xml`
==================

NEMO's output configuration is controlled by a file called :file:`iodef.xml`.
It is found in the :file:`SS-run-sets/SalishSea/` directory along with all of the namelists.
This file allows the user to specify which fields should be outputted, how often, and at which grid points.
It is written in an xml language and the details can be found in section 11.2 of the NEMO documentation.

Here is an example snippet from the :file:`<file definition>` segment of the default :file:`iodef.xml` file.

.. code-block:: xml

     <group id="4h" output_freq="14400" output_level="10" enabled=".true.">
        <file id="4h_grid_T" name="auto">
          <field ref="ssh" name="sossheig"/>
          <field ref="toce" name="votemper"/>
          <field ref="soce" name="vosaline"/>
          <field ref="rain" name="rain_rate"/>
          <field ref="snowpre" name="snow_rate"/>
        </file>
        <file id="4h_grid_U" name="auto">
          <field ref="uoce" name="vozocrtx"/>
          <field ref="utau" name="u_wind_stress"/>
        </file>
        <file id="4h_grid_V" name="auto">
          <field ref="voce" name="vomecrty"/>
          <field ref="vtau" name="v_wind_stress"/>
        </file>
      </group>

This part of the code defines a group of variables that will be saved every 4 hours.
A few notes on the first line:

* :file:`output_freq="14400"` specifies that this group of variables will be saved every 4 hours (14400 seconds).
* :file:`enabled=".true."` tells NEMO that this group of variables should be saved. If this were set to :file:`enabled=".false."` then NEMO would not save this group.

In the next set of lines, we define several files that will contain the output.

* The first file is tagged :file:`id="4h_grid_T"` and contains the T fields listed.
* The second file is tagged :file:`id="4h_grid_U"` and contains the U fields listed.
* The third file is tagged :file:`id="4h_grid_V"` and contains the V fields listed.
* :file:`name="auto"` means that the file name is automatically generated based on the id tags.

In order to learn how to output at a specific grid point we need to visit the :file:`<grid definition>` segment of the :file:`iodef.xml` file. Here is an example snippet:

.. code-block:: xml

      <grid id="grid_T" description="grid T" >
        <!--   Eq section   -->
        <zoom id="EqT" ibegin="1" jbegin="0000" ni="0000" nj="1" />
        <!--   TAO   -->
          <!--   137e   -->
        <zoom id="2n137eT" ibegin="0000" jbegin="0000" ni="1" nj="1" />
        <zoom id="5n137eT" ibegin="0000" jbegin="0000" ni="1" nj="1" />
        <zoom id="8n137eT" ibegin="0000" jbegin="0000" ni="1" nj="1" />
        ...
      </grid>

This part of the code defines a tag for the T grid and highlights the "zoom" feature.
The zoom lines in the code create "zoom" tags that can be used when defining groups as outlined in the above section.

* :file:`id="2n137eT"` defines the zoom_ref tag to be used later.
* :file:`ibegin="0000"` states that the zoom tag should begin at grid point i=0.
* :file:`jbegin="0000"` states that the zoom tag should begin at grid point j=0.
* :file:`ni="1"` states that the zoom tag should span one grid point in the x direction.
* :file:`nj="1"` states that the zoom tag should span one grid point in the y direction.

There are many predefined zoom tags in this section of the code.
However, they all seem to begin at (i,j)=(0,0).
In the next sections we will work on adding our own.

These zoom tags are defined on the T grid.
You could also define zoom tags on the U or V grid by editing those sections of the grid definition.

Outputting a specified grid point
=================================
In order to output at a specified point we should first define a new zoom tag. We will modify the above section of code to include a new zoom tag.

.. code-block:: xml

      <grid id="grid_T" description="grid T" >
        <!--   Eq section   -->
        <zoom id="EqT" ibegin="1" jbegin="0000" ni="0000" nj="1" />
        <!--   TAO   -->
          <!--   137e   -->
        <zoom id="2n137eT" ibegin="0000" jbegin="0000" ni="1" nj="1" />
        <zoom id="5n137eT" ibegin="0000" jbegin="0000" ni="1" nj="1" />
        <zoom id="8n137eT" ibegin="0000" jbegin="0000" ni="1" nj="1" />
        ...
       <!--   Storm Surge Points -->
        <zoom id="PointAtkinson"  ibegin="329" jbegin="469" ni="1" nj="1" />
      </grid>

We have added a zoom tag with :file:`id="PointAtkinson"` at the grid point (329,469).

Next, we will go back to the :file:`<file definition>` segment to define a new group of variables to be saved at this grid point.

.. code-block:: xml

   <group id="1h_freq" output_freq="3600" output_level="10" enabled=".true." >
        <file id="1h_PointAtkinson" name="1h_PointAtkinson" enabled=".true." description="Point Atkinson 1h outputs">
        <group id="1h_PointAtkinson" zoom_ref="PointAtkinson" >
          <field ref="ssh" name="sossheig"/>
          <field ref="toce" name="votemper"/>
          <field ref="soce" name="vosaline"/>
          <field ref="rain" name="rain_rate"/>
          <field ref="snowpre" name="snow_rate"/>
          <field ref="uoce" name="vozocrtx"/>
          <field ref="utau" name="u_wind_stress"/>
          <field ref="voce" name="vomecrty"/>
          <field ref="vtau" name="v_wind_stress"/>
	</group>
        </file>
   </group>

Here we have added a group of variables tagged as "1h_freq" that will be saved every hour.

* The second line defines a file for saving the Point Atkinson data. The file will be called :file:`1h_PointAtkinson.nc`.
* The third line defines a group of variables tagged as "1h_PointAtkinson". These variables will be taken at the grid defined by zoom tag :file:`zoom_ref="PointAtkinson"`. Since this group of variables is within the "1h_freq" group they will also be saved every hour.
* The rest of the lines define the fields that will be saved.
* Other files and groups can be added to the "1h_freq" group as is done in :file:`iodef_freq.xml` in :file:`SS-run-sets/SalishSea/`.

Storm Surge Outputs
===================
The file :file:`iodef.xml` has been set up to give one hour outputs at known storm surge locations.
The storm surge locations are outlined in the :ref:`StormSurge` doc.
The "enabled" attributes for these files and group must be set to true in order to produce the new output files.
After NEMO is run, the output should include four new files :file:`1h_PointAtkinson.nc`, :file:`1h_Victoria.nc`, :file:`1h_PatriciaBay.nc`, and :file:`1h_CampbellRiver.nc`.

Other Notes
===========
Users can also change how the output is calculated (instantaneous vs. average fields) with the "operation" attribute.
This feature is outlined in the NEMO documentation section 11.2.
