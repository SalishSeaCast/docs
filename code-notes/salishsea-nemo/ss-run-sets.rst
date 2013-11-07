*************************
Salish Sea Run Sets Files
*************************

The collection of NEMO namelist,
:command:`salishsea` command processor run description,
and NEMO output server control files used to run NEMO for the Salish Sea are maintained in the `SS-run-sets`_ repo.

.. _SS-run-sets: https://bitbucket.org/salishsea/ss-run-sets/

We choose to call these run-set files but they are also referred to as "experiments" in the NEMO community.
That term is difficult for the laboratory modelers on the Salish Sea MEOPAR team to apply to computational model runs.

The namelist and output server control files are like those found in the :file:`EXP00/` directories of the reference configurations in the :ref:`NEMO-code-repo`.

The :command:`salishsea` command processor run description files are YAML_ files that are used by our tool for managing NEMO runs and results.

.. _YAML: http://pyyaml.org/wiki/PyYAMLDocumentation


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

:file:`SalishSea/` Directory
----------------------------

the :file:`SalishSea` directory contains the run-set files for the initial full domain runs:

* :file:`SalishSea.yaml`: Sample run description file for use with the :ref:`SalishSeaCmdProcessor`
* :file:`namelist`: Sample namelist containing the current recommended model parameter values
* :file:`iodef.xml`: Sample IOM output server definitions
* :file:`xmlio_server.def`: IOM output server control settings


:file:`JPP/` Directory
-----------------------

The :file:`JPP/` directory contains the run-set files for the initial sub-domain test case also known as :kbd:`WCSD_RUN_tide_M2_OW_ON_file_DAMP_ANALY`:

* :file:`JPP.yaml`
* :file:`namelist`
