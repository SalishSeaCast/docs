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

The public key needs to be stored on every machine that you want to ssh into.
Because your home directory is shared across all of the Waterhole machines
(and :kbd:`salish`, and :kbd:`skookum`)
you only need to put your public key on any one of those machines.
On WestGrid and ComputeCanada you need to put your public key on each machine that you use
(:kbd:`cedar`, :kbd:`graham`, :kbd:`orcinus`, etc.).
The sections below include instructions for how to store your public key on various machines.


:command:`ssh-agent` Forwarding for :kbd:`salish`
=================================================

To set up agent forwarding for :kbd:`salish` create a :file:`$HOME/.ssh/config` file on your Waterhole machine containing the following (or append the following if :file:`$HOME/.ssh/config` already exists)::

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


:command:`ssh-agent` Forwarding for Westgrid and ComputeCanada Clusters
=======================================================================

To set up agent forwarding for a Westgrid or ComputeCanada machine append the appropriate block below to the :file:`$HOME/.ssh/config` file on your Waterhole machine::

    Host cedar
      Hostname  cedar.computecanada.ca
      User  userid
      ForwardAgent  yes

    Host graham
      Hostname  graham.computecanada.ca
      User  userid
      ForwardAgent  yes

    Host orcinus
      Hostname  orcinus.westgrid.ca
      User  userid
      ForwardAgent  yes

where :kbd:`userid` is your Westgrid or ComputeCanada user id.

.. note:: If you do not have a Westgrid or ComputeCanada account follow the instructions here to make one: :ref:`westgridAccount`.

Install your :command:`ssh` public key on the remote machine;
:kbd:`cedar`, for example:

.. code-block:: bash

    ssh-copy-id -i $HOME/.ssh/id_rsa cedar

You will be prompted for your Westgrid or ComputeCanada password.
After the key has been installed you should be able to use :command:`ssh`,
:command:`scp`,
and :command:`sftp` to connect to the remote machine without having to type your password.
Likewise,
Mercurial commands on the remove machine should not require your to type your :command:`ssh` key pass phrase.
