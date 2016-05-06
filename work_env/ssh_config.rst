.. _sshConfiguration:

****************************
:command:`ssh` Configuration
****************************

This section assumes that you have completed the :command:`ssh-keygen` step of the `Bitbucket ssh Set-up`_ instructions referenced in the :ref:`vc-with-hg` section.

.. _Bitbucket ssh Set-up: https://confluence.atlassian.com/bitbucket/set-up-ssh-for-mercurial-728138122.html

If you set up working environments to run the Salish Sea NEMO model on :kbd:`salish` or on a Westgrid cluster,
or need to access model results files from a Westgrid cluster,
you should set up :command:`ssh-agent` forwarding to minimize the need to repeatedly type your :command:`ssh` key pass phrase and to minimize the number of machines on which your private key is stored.

ssh keys work by having a public key and a private key pair with the public key on remote machines and the private key
on the local machine you log into first.
Your private key is usually protected by a long passphrase that you only have to enter once per login session on the local machine.
After that an ssh-agent program on the local machine uses the private key to exchange encrypted authentication information
with the remote machines.

The public key needs to be stored in the :file:`.ssh/authorized_keys` file on every machine that you want to ssh into.
Because your home directory is shared across all of the Waterhole machines (and salish, and skookum)
you only need to put your public key in :file:`.ssh/authorized_keys` on any one of those machines.
On WestGrid you need to put your public key in :file:`.ssh/authorized_keys` on each machine that you use
(orcinus, jasper, bugaboo, etc.).


:command:`ssh-agent` Forwarding for :kbd:`salish`
=================================================

To set up agent forwarding for :kbd:`salish` create a :file:`$HOME/.ssh/config` file on your Waterhole machine containing the following (or append the following if :file:`$HOME/.ssh.config` already exists):

.. code-block:: cfg

    Host salish
      Hostname  salish.eos.ubc.ca
      User  userid
      ForwardAgent  yes

where :kbd:`userid` is your EOAS user id.

The first two lines establish :kbd:`salish` as a short alias for :kbd:`salish.eos.ubc.ca` so that you can just type :command:`ssh salish`.

The third line sets the user id to use on the remote system,
which is convenient if it differs from your EOAS user id.

The last line enables agent forwarding so that authentication requests received on the remote system are passed back to your Waterhole machine for handling.
That means that connections to Bitbucket (for instance) in your session on :kbd:`salish` will be authenticated by your Waterhole machine.
So,
after you type your :command:`ssh` key pass phrase in to your Waterhole machine once,
you should not have to type it again until you log off and log in again.

The other thing that is required for agent forwarding to work is that your :command:`ssh` public key be stored in the :file:`$HOME/.ssh/authorized_keys` file on the remote system.
Thanks to shared storage between the Waterhole machines and :kbd:`salish` that is *really* easy to do:

.. code-block:: bash

    cd $HOME/.ssh
    cat id_rsa.pub >> authorized_keys


:command:`ssh-agent` Forwarding for :kbd:`jasper` or for :kbd:`orcinus`
=======================================================================

Orcinus is exactly the same as jasper, just change the name throughout.

To set up agent forwarding for :kbd:`jasper` append the following to the :file:`$HOME/.ssh/config` file on your Waterhole machine:

.. code-block:: cfg

    Host jasper
      Hostname  jasper.westgrid.ca
      User  userid
      ForwardAgent  yes

where :kbd:`userid` is your Westgrid user id.

If you do not have a Westgrid account follow the instructions on this page to create one:

https://www.westgrid.ca/support/accounts/registering_ccdb

When prompted to select an institution, choose :kbd:`Westgrid: University of British Columbia`.
If you are creating an account as a sponsored user ask your supervisor for their CCRI code.


Install your :command:`ssh` public key on :kbd:`jasper`:

.. code-block:: bash

    cd $HOME/.ssh/
    scp id_rsa.pub jasper:.ssh/authorized_keys

Note: You may have to create the :file:`.ssh` directory in your home directory on :kbd:`jasper` first. You will be prompted for your Westgrid password,
but after the key has been installed you should be able to use :command:`ssh`,
:command:`scp`,
and :command:`sftp` to connect to :kbd:`jasper` without having to type your password.
Likewise,
Mercurial commands on :kbd:`jasper` should not require your to type your :command:`ssh` key pass phrase.
