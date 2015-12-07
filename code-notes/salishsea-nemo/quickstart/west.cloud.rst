.. _WorkingOnWestCloud:

****************************
Working on :kbd:`west.cloud`
****************************

.. warning::

    This section contains work in progress notes.
    `west.cloud`_ is not yet ready to run the Salish Sea NEMO model.

These are working notes about the process of getting the Salish Sea NEMO model set up to run on  the `Ocean Networks Canada`_ private cloud computing facility `west.cloud`_ that run on `OpenStack`_.

.. _Ocean Networks Canada: http://www.oceannetworks.ca/
.. _west.cloud: https://www.westgrid.ca/support/systems/Nefos
.. _OpenStack: http://www.openstack.org/

The `OpenStack dashboard`_ provides a web interface to provision and report on cloud resources.
The :kbd:`west.cloud` dashboard is at https://west.cloud.computecanada.ca/dashboard/.

.. _OpenStack dashboard: http://docs.openstack.org/user-guide/content/ch_dashboard.html

Authentication and authorization for :kbd:`west.cloud` is managed by `Westgrid`_,
so use your Westgrid user id and password to log in to the dashboard.

.. _Westgrid: https://www.westgrid.ca/


Web Interface
=============

Initial setup was done via the https://west.cloud.computecanada.ca/dashboard/ web interface with guidance from the
(unpublished at time of writing)
`nefos.westgrid.ca Quickstart Guide`_ and the `OpenStack End User Guide`_.

.. _nefos.westgrid.ca Quickstart Guide: https://www.westgrid.ca/support/quickstart/nefos_openstack _cloud_quick start_guide
.. _OpenStack End User Guide: http://docs.openstack.org/user-guide/content/openstack_user_guide.html

The project (aka tenant) name for the the Salish Sea NEMO model is :kbd:`NEMO`.

Network
-------

The network configuration was done for us by ONC.
The configuration from the :guilabel:`Network` section of the web interface is recorded here for reference.

Network:

* Network Name: NEMO-network
* Shared: No
* Admin State: Up

Subnet:

* Subnet Name: NEMO-subnet
* Network Address: 192.168.0.0/23
* IP Version: IPv4
* Gateway IP: 192.168.0.1

Subnet Details:

* Enable DHCP: Yes
* Allocation Pools: 192.168.0.2, 192.168.1.254
* DNS Servers: 142.104.6.1 142.104.80.2

Router:

* Router Name: NEMO-gw
* External Network: VLAN3337

Interface:

* Subnet: NEMO-network (NEMO-subnet)
* IP Address: 192.168.0.1
* Router: NEMO-gw


Images
------

An Ubuntu Server 14.04 image was loaded via the :guilabel:`Compute > Images > Create Image` button with the following parameters:

* Name: ubuntu-server-14.04-amd64
* Description: Ubuntu 14.04 64-bit for Salish Sea NEMO project
* Image Source: Image Location
* Image Location: http://cloud-images.ubuntu.com/trusty/current/trusty-server-cloudimg-amd64-disk1.img
* Format: QCOW2 - QEMU Emulator
* Architecture: x86_64
* Minimum Disk (GB): blank
* Minimum RAM (MB): blank
* Public: Yes
* Protected: Yes


Access & Security
-----------------

Generate an ssh key pair on a Linux or OS/X system using the command:

.. code-block:: bash

    $ cd $HOME/.ssh/
    $ ssh -t rsa -f west.cloud_id_rsa -c <yourname>-west.cloud

Assign a string passphrase to the key pair when prompted.
Passphraseless keys have their place,
but they are a bad idea for general use.

List the public key with the command:

.. code-block:: bash

    $ cat west.cloud_id_rsa.pub

and use copy-paste to import it into the web interface via the :guilabel:`Compute > Access & Security > Key Pairs > Import Key Pair` button.

Use the :guilabel:`Compute > Access & Security > Security Groups > Manage Rules` button associated with the :guilabel:`default` security group to add security rules to allow :command:`ssh` and :command:`ping` access to the image instances.

:command:`ssh` Rule:

* Rule: SSH
* Remote: CIDR
* CIDR: 0.0.0.0/0

:command:`ping` Rule:

* Rule: ALL ICMP
* Direction: Ingress
* Remote: CIDR
* CIDR: 0.0.0.0/0


Instances
---------

Use the :guilabel:`Compute > Instances` section of the web interface to manage instances.

To launch an instance to use as a login node use the :guilabel:`Launch Instance` button.
On the :guilabel:`Details` tab set the following parameters:

* Availability Zone: nova
* Instance Name: manage
* Flavor: c4-7.5g-35
* Instance Count: 1
* Instance Boot Soure: Boot from image
* Image Name: ubuntu-server-14.04-amd64

On the :guilabel:`Access & Security` tab set the following parameters:

* Key Pair: the name of the key pair that you imported
* Security Groups: default enabled

.. note::

    If only 1 key pair has been imported it will be used by default.
    If there is more than 1 key pair available,
    one must be selected.
    Only 1 key can be loaded automatically into an instance on launch.
    Additional public keys can be loaded once an instance is running.

On the :guilabel:`Networking` tab ensure that :guilabel:`NEMO-network` is selected.

Click the :guilabel:`Launch` button to launch the instance.

Once the instance is running use the :guilabel:`More > Associate Floating IP` menu item to associate a public IP address with the instance.


:command:`ssh` Access
=====================

Log in to the publicly accessible instance with the command:

.. code-block:: bash

    $ ssh -i $HOME/.ssh/west.cloud_id_rsa ubuntu@<ip-address>

The first time you connect to an instance you will be prompted to accept its RSA host key fingerprint.
You can verify the fingerprint by looking for the :kbd:`SSH HOST KEY FINGERPRINT` section in the instance log in the :guilabel:`Instances > Instance Details > Log` tab.
If you have previously associated a different instance with th IP address you may receive a message about host key verification failure and potential man-in-the-middle attacks.
To resolve the issue delete the prior host key from your :file:`$HOME/.ssh/known_hosts` file.
The message will tell you what line it is on.

You will also be prompted for the pasphrase that you assigned to the ssh key pair when you created it.
On Linux and OS/X authenticating the ssh key with your pasphrase has the side-effect of adding it to the :command:`ssh-agent` instance that was started when you logged into the system.
You can add the key to the agent yourself with the command:

.. code-block:: bash

    $ ssh-add $HOME/.ssh/west.cloud_id_rsa

You can list the keys that the agent is managing for you with:

.. code-block:: bash

    $ ssh-add -l

You can simplify logins to the instance by adding the following lines to your :file:`$HOME/.ssh/config` file:

.. code-block:: ini

    Host west.cloud
        Hostname        <ip-address>
        User            ubuntu
        IdentityFile    ~/.ssh/west.cloud_id_rsa
        ForwardAgent    yes

With that in place you should be able to connect to the instance with:

.. code-block:: bash

    $ ssh west.cloud


Provisioning and Configuration
==============================

Launch an :kbd:`c4-7.5g-35` flavour instance from the :kbd:`ubuntu-server-14.04-amd64` image,
associate a floating IP address with it,
and provision it with the following packages:

.. code-block:: bash

    $ sudo apt-get update
    $ sudo apt-get install mercurial
    $ sudo apt-get install gfortran
    $ sudo apt-get install libopenmpi1.6 libopenmpi-dev
    $ sudo apt-get install openmpi-bin
    $ sudo apt-get install libnetcdf-dev netcdf-bin
    $ sudo apt-get install libhdf5-dev
    $ sudo apt-get install nco
    $ sudo apt-get install liburi-perl
    $ sudo apt-get install make ksh emacs24
    $ sudo apt-get install sshfs
    $ sudo apt-get install python-pip python-dev
    $ sudo apt-get install python-matplotlib python-pandas python-cliff
    $ sudo apt-get install python-zmq python-paramiko

Use:

.. code-block:: bash

    sudo dpkg-reconfigure tzdata

to set the timezone.

Set the network interface MTU
(Maximum Transmission Unit)
to 1500 with:

.. code-block:: bash

    sudo ip link set dev eth0 mtu 1500

Copy the public key of the passphrase-less ssh key pair that will be used for nowcast cloud operations into :file:`$HOME/.ssh/` and append it to the :file:`authorized_keys` file:

.. code-block:: bash

    # on a system where they key pair is stored
    $ scp -Cp $HOME/.ssh/SalishSeaNEMO-nowcast_id_rsa.pub west.cloud:.ssh/

    # on west.cloud
    $ cd $HOME/.ssh/
    $ cat SalishSeaNEMO-nowcast_id_rsa.pub >> authorized_keys
    $ rm SalishSeaNEMO-nowcast_id_rsa.pub

The nowcast operations key pair could have been used as the default key pair in the OpenStack web interface,
but using a key pair with a passphrase there allows for more flexibility:
in particular,
the possibliity of revoking the passphrase-less key pair without loosing access to the instances.

Copy the ssh key pair that will be used to access the :ref:`ShareStorageViaSSHFS` and rename them to the default key names:

.. code-block:: bash

    # on a system where they key pair is stored
    $ scp -Cp $HOME/.ssh/nefos-sshfs_id_rsa* west.cloud:.ssh/

    # on west.cloud
    $ cd $HOME/.ssh/
    $ mv nefos-sshfs_id_rsa id_rsa
    $ mv nefos-sshfs_id_rsa.pub id_rsa.pub

Edit :file:`$HOME/.profile` to add code that puts :file:`$HOME/.local/bin` at the front of :envvar:`PATH`:

.. code-block:: bash

    # set PATH so it includes user's private and local bins
    # if they exists
    if [ -d "$HOME/bin" ] ; then
        PATH="$HOME/bin:$PATH"
    fi
    if [ -d "$HOME/.local/bin" ] ; then
        PATH="$HOME/.local/bin:$PATH"
    fi

Create :file:`$HOME/.bash_aliases` containing a command to set the command-line prompt to show the host name and the final directory of the :kbd:`pwd` path:

.. code-block:: bash

    PS1="\h:\W\$ "


.. _HeadNodeSpecificConfiguration:

Head Node Specific Configuration
--------------------------------

Mount the :ref:`ShareStorageViaSSHFS`:

.. code-block:: bash

    $ sudo mkdir -p $HOME/MEOPAR
    $ sudo chown ubuntu:ubuntu $HOME/MEOPAR
    $ sshfs -o idmap=user nemo@ncnfs1.neptune.uvic.ca:/gss_onc/NEMO $HOME/MEOPAR

If they do not already exist on :file:`$HOME/MEOPAR/`,
clone the Salish Sea NEMO project code,
forcing,
run sets,
and tools repos from Bitbucket:

.. code-block:: bash

    $ mkdir -p $HOME/MEOPAR/SalishSea
    $ cd MEOPAR
    $ hg clone ssh://hg@bitbucket.org/salishsea/nemo-code NEMO-code
    $ hg clone ssh://hg@bitbucket.org/salishsea/nemo-forcing NEMO-forcing
    $ hg clone ssh://hg@bitbucket.org/salishsea/ss-run-sets SS-run-sets
    $ hg clone ssh://hg@bitbucket.org/salishsea/tools tools

Install the :ref:`SalishSeaToolsPackage` and :ref:`SalishSeaCmdProcessor` as editable user packages and add :file:`$HOME/.local/bin/`:

.. code-block:: bash

    $ cd $HOME/MEOPAR/tools/
    $ pip install --user -e SalishSeaTools
    $ pip install --user -e SalishSeaCmd
    $ export PATH=$HOME/.local/bin:$PATH

Unmount the SSHFS:

.. code-block:: bash

    $ cd $HOME
    $ fusermount -u $HOME/MEOPAR

and use the OpenStack web interface to create a snapshot of the instance for use as the "head" node for running the Salish Sea NEMO nowcast and forecast runs.
The head node is the one that will have the public IP address associated with it and it will be used for commands,
uploads,
and downloads.
It is also used as a compute node.

Because the provisioning and configuration has been done on a small flavour instance the snapshot image captured from it can be launched on larger instances for production use.


.. _ComputeNodeConfiguration:

Compute Node Configuration
--------------------------

On an instance launched from the head node image remove the installation of the :ref:`SalishSeaToolsPackage` and :ref:`SalishSeaCmdProcessor` as editable user packages and the :file:`$HOME/.local/bin/`:

.. code-block:: bash

    $ rm -rf $HOME/.local

Remove :file:`$HOME/.local/` from :envvar:`PATH`:

.. code-block:: bash

    export PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games

Delete the nowcast operations public key from the :file:`authorized_keys` file and replace it with the :file:`id_rsa.pub` SSHFS key.

Ensure that the SSHFS is not mounted:

.. code-block:: bash

    $ cd $HOME
    $ fusermount -u $HOME/MEOPAR

and use the OpenStack web interface to create a snapshot of the instance for use as compute nodes for running the Salish Sea NEMO nowcast and forecast runs.
Compute nodes provide cores and RAM for the runs.

Because the provisioning and configuration has been done on a small flavour instance the snapshot image captured from it can be launched on larger instances for production use.


.. _ShareStorageViaSSHFS:

Shared Storage via SSHFS
------------------------

.. warning::

    Instances with an SSHFS mounted are fragile with respect to rebooting and image creation,
    that is,
    if an SSHFS is mounted when an instance is rebooted,
    the instance will be unreachable.
    Likewise,
    inatances booted from an image created from an instance with an SSHFS mounted are unreachable.

Shared, persistent storage accessible via SSHFS is set up for user :kbd:`nemo` on :kbd:`nemo@ncnfs1.neptune.uvic.ca`.
It is only accessible from :kbd:`west.cloud` instances.

Generate a pasphrase-less ssh key-pair with the default (:file:`.ssh/id_rsa`) name and copy it to to :kbd:`nemo@ncnfs1.neptune.uvic.ca`:

.. code-block:: bash

    $ ssh-keygen -t rsa
    $ ssh-copy-id nemo@ncnfs1.neptune.uvic.ca

Create a mount point with appropriate ownership and mount the SSHFS filesystem:

.. code-block:: bash

    $ sudo mkdir $HOME/MEOPAR
    $ sudo chown ubuntu:ubuntu $HOME/MEOPAR
    $ sshfs -o idmap=user nemo@ncnfs1.neptune.uvic.ca:/gss_onc/NEMO $HOME/MEOPAR

To unmount the filesystem use:

.. code-block:: bash

    $ fusermount -u $HOME/MEOPAR

Set up the shared storage:

.. code-block:: bash

    $ mkdir -p $HOME/MEOPAR/SalishSea
    $ cd MEOPAR
    $ hg clone ssh://hg@bitbucket.org/salishsea/nemo-code NEMO-code
    $ hg clone ssh://hg@bitbucket.org/salishsea/nemo-forcing NEMO-forcing
    $ hg clone ssh://hg@bitbucket.org/salishsea/ss-run-sets SS-run-sets
    $ hg clone ssh://hg@bitbucket.org/salishsea/tools tools


Command-line Interface
======================

To automate creation and management of cloud resources OpenStack provides a collection of `command-line clients`_.
There is also a `Python Software Development Kit (SKD)`_.
The SDK implements Python bindings to the OpenStack API,
which enables you to perform automation tasks in Python by making calls on Python objects rather than making REST calls directly.
All OpenStack command-line tools are implemented using the Python SDK.

.. _command-line clients: http://docs.openstack.org/user-guide/content/ch_cli.html
.. _Python Software Development Kit (SKD): http://docs.openstack.org/user-guide/content/ch_sdk.html


Local System Setup
------------------

To isolate the installation of the OpenStack command-line clients and the Python SDK on our local machine
(Waterhole workstation,
laptop,
etc.)
we'll use a `Conda environment`_.
This presumes that you have the :ref:`AnacondaPythonDistro` installed on your local machine.

.. _Conda environment: http://conda.pydata.org/docs/intro.html

At the time of writing OpenStack only supports Python 2.7,
so create and activate an environment with Python 2.7 and `pip`_ installed:

.. code-block:: none

    $ conda create -n west.cloud python=2.7 pip
    ...
    $ source activate west.cloud

.. _pip: https://pip.readthedocs.org/en/latest/

Install a collection of packages that the command-line clients and SDK depend on and that are included in the Anaconda distribution.
Doing this avoids compilation and linking issues.

.. code-block:: none

    (west.cloud)$ conda install requests pyopenssl six pytz cryptography cffi pycparser
