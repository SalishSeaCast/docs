.. _WorkingEnvironment:

*******************
Working Environment
*******************

Instructions and recommendations for setting up your computational working environment for the Salish Sea MEOPAR project,
and information about the major tools that we use.

Team members at UBC will typically set up their first working environment on a Waterhole workstation running Linux.
If you are running the Salish Sea NEMO model you will eventually need to set up working environments on several machines.

The Waterhole workstations are a collection of Linux workstations owned co-operatively by Susan Allen,
Rich Pawlowicz,
Stephanie Waterman,
and Roger Pieters.
They are maintained by EOAS Comp Staff,
primarily Charles Krzysik.
They are mostly located in ESB 3024/3026 but some are located in users' offices.
The Waterhole machines are nominally identically configured so that your should be able to log-in on all of them and find the same software available for use.
They are set up so that your user configuration is identical on all machines.
All machines have a :file:`/ocean/` partition which provides storage that is large
(several terabytes),
redundant (RAID), and
automatically backed up.
That storage space is on the :kbd:`ocean.eos.ubc.ca` storage server
(also co-operatively owned and maintained by the same group).
Each user has their own directory on :file:`/ocean/` named with their EOAS user id;
e.g. :file:`/ocean/sallen/`.
You should use your :file:`/ocean/` directory as your primary file storage area.

Other machines that you may need working environments on later include:

* :kbd:`salish.eos.ubc.ca`: Our group compute server that runs Linux and has several disk mounts in common with Waterhole workstations.
  :kbd:`salish` is primarily used for short development runs of the Salish Sea NEMO model.
  :kbd:`salish` has several terabytes of storage in its :file:`/data/` filesystem.
  If your Waterhole workstation does not have access to :file:`/data/` you should open a ticket via the :kbd:`Helpdesk` link on https://helpdesk.eoas.ubc.ca/ to request that EOAS Comp Staff add a :kbd:`salish /data/` mount on the workstation you are using.
  That will enable you to read/write files on the :kbd:`salish /data/` filesystem without have to sign on to :kbd:`salish` or copy the files from one machine to another.
* :kbd:`skookum.eos.ubc.ca`: Our group results storage and web server.
  The Salish Sea Nowcast system results are stored on the :file:`/results/` file system on :kbd:`skookum`.
  If your Waterhole workstation does not have access to :file:`/results/` you should open a ticket via the :kbd:`Helpdesk` link on https://helpdesk.eoas.ubc.ca/ to request that EOAS Comp Staff add a :kbd:`skookum /results/` mount on the workstation you are using.
  That will enable you to read files from the :kbd:`skookum /results/` filesystem without have to sign on to :kbd:`skookum` or copy the files from one machine to another.
* One or more of the Westgrid_ HPC clusters such as :kbd:`jasper.westgrid.ca`, :kbd:`orcinus.westgrid.ca`, or :kbd:`bugaboo.westgrid.ca` that run Linux.
  The Westgrid clusters
  (primarily :kbd:`orcinus`)
  are used for longer research runs of the model.

  .. _Westgrid: https://www.westgrid.ca/

When you are ready to run on those machines,
please see the :ref:`QuickStartGuide` for instructions on setting working environments on them.

.. toctree::
   :maxdepth: 2

   bash_config
   hg_version_control
   anaconda_python
   salishsea_pkgs
   python_notes
   bitbucket_notebook_readme
   westgrid_account
   sphinx_docs
   ssh_config
   emacs_config
   python3_conda_environment
   porting_to_python3

It is also possible to set up a working environment for most things other than running the NEMO model on your own laptop,
if you wish.
The instructions and recommendations above are most applicable to doing that on a laptop that runs OS/X or Linux.
Our best advice for Windows is to install puTTY_ and use it to connect remotely
(via :program:`ssh`) to your Linux environments
(though additions to these docs by experienced Windows users who set up a working environment similar to the one described above are *most* welcome).

.. _puTTY: http://www.chiark.greenend.org.uk/~sgtatham/putty/
