.. _WorkingEnvironment:

*******************
Working Environment
*******************

Instructions and recommendations for setting up your computational working environment for the Salish Sea MEOPAR project,
and information about the major tools that we use.

Most team members will eventually need to set up working environments on several machines.
Typically,
the first one will be on a Waterhole workstation running Linux.
Other environments that you may need later will be on:

* :kbd:`salish.eos.ubc.ca`: The project computer server that runs Linux and has several disk mounts in common with Waterhole workstations.
  :kbd:`salish` is primarily used for short development runs.
* One or more of the Westgrid_ HPC clusters such as :kbd:`jasper.westgrid.ca`, :kbd:`orcinus.westgrid.ca`, or :kbd:`bugaboo.westgrid.ca` that run Linux.
  The Westgrid clusters
  (primarily :kbd:`jasper`)
  are used for longer research runs.

.. _Westgrid: https://www.westgrid.ca/

.. toctree::
   :maxdepth: 2

   bash_config
   hg_version_control
   anaconda_python
   salishsea_pkgs
   sphinx_docs
   emacs_config

It is also possible to set up a working environment for most things other than running the NEMO model on your own laptop,
if you wish.
The instructions and recommendations above are most applicable to doing that on a laptop that runs OS/X or Linux.
Our best advice for Windows is to install puTTY_ and use it to connect remotely
(via :program:`ssh`) to your Linux environments
(though additions to these docs by experienced Windows users who set up a working environment similar to the one described above are *most* welcome).

.. _puTTY: http://www.chiark.greenend.org.uk/~sgtatham/putty/
