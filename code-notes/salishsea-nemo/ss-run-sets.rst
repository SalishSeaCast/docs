.. _SS-run-sets:

*************************
Salish Sea Run Sets Files
*************************

The collection of NEMO namelist,
:program:`salishsea` command processor run description,
and NEMO output server control files used to run NEMO for the Salish Sea are maintained in the :ref:`SS-run-sets-repo` repo.

We choose to call these run-set files but they are also referred to as "experiments" in the NEMO community.
That term is difficult for the laboratory modelers on the Salish Sea MEOPAR team to apply to computational model runs.

The :program:`salishsea` command processor run description files are YAML_ files that are used by our :ref:`SalishSeaCmdProcessor` tool for managing NEMO runs and results.
The :ref:`RunDescriptionFileStructure` section describes the file syntax.

.. _YAML: http://pyyaml.org/wiki/PyYAMLDocumentation

The namelist files contain collections of related namelists.
They are concatenated to form a complete namelist for a NEMO run by the :program:`salishsea` :ref:`salishsea-prepare`.

The output server control files is like those found in the :file:`EXP00/` directories of the reference configurations in the :ref:`NEMO-code-repo` repo.


Getting the Repo
================

If you use SSH key authentication on Bitbucket you may clone the `SS-run-sets`_ repo with:

.. code-block:: bash

    hg clone ssh://hg@bitbucket.org/salishsea/ss-run-sets SS-run-sets

For password authentication use:

.. code-block:: bash

    hg clone https://<you>@bitbucket.org/salishsea/ss-run-sets SS-run-sets

where :kbd:`<you>` is your Bitbucket user id.


Repo Contents
=============

.. _SS-run-sets-SalishSea:

:file:`SalishSea/` Directory
----------------------------

the :file:`SalishSea` directory contains the run-set files for the initial full domain runs:

* :file:`SalishSea.yaml`: Sample run description file for use with the :ref:`SalishSeaCmdProcessor`
* :file:`SalishSea_no_IOM.yaml`: Sample run description file that uses the :file:`SalishSea_no_IOM` configuration to execute minimal output runs without using the IOM output server
* :file:`namelist`: Sample namelist containing the current recommended model parameter values
* :file:`namelist_no_IOM`: Sample namelist for running without the IOM output server
* :file:`namelist.bottom`: Sample bottom boundary conditions namelists
* :file:`namelist.compute`: Sample compute parameters namelists
* :file:`namelist.domain`: Sample domain configuration nameslists
* :file:`namelist.dynamics`: Sample dynamics parameter namelists
* :file:`namelist.lateral`: Sample lateral boundary conditions and forcing namelists
* :file:`namelist.surface`: Sample surface boundary conditions namelists
* :file:`namelist.time`: Sample model time parameters namelists
* :file:`namelist.tracers`: Sample tracer quantities configuration namelist
* :file:`iodef.xml`: Sample IOM output server definitions
* :file:`xmlio_server.def`: IOM output server control settings


:file:`JPP/` Directory
-----------------------

The :file:`JPP/` directory contains the run-set files for the initial sub-domain test case also known as :kbd:`WCSD_RUN_tide_M2_OW_ON_file_DAMP_ANALY`:

* :file:`JPP.yaml`
* :file:`namelist`
