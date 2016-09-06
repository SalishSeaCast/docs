.. _batch_runs:

=================================================
SMELT Sensitivity Analysis: Batch Simulation Runs
=================================================

The first task was to create a dataset of simulation results for the different parameter changes. It was decided to test each parameter at 10%, 50%, 90%, 110%, 200% and 1000% of its default value.
There are 6 changes per parameter and over 100 parameters in total, so this is about 600 simulation runs. The SMELT model has a large physical domain (398x898x40) and so it is fairly slow to run. Due to the number of runs required it was not feasible to use the full model. Instead we created a smaller domain (5x5x40) with simplified physics that was much faster to run (3 months simulation time in about 30 minutes on one processor). Ideally trends observed in the smaller model will also be applicable to the full model.

With a minimum of several hundred simulation runs it made sense to automate this process. A python script was created to modify the parameters one at a time and run the model for each change. The code can be found `here <https://bitbucket.org/salishsea/analysis-james/src/tip/batch_5x5.py?fileviewer=file-view-default/>`_.

This script was run on the local computer 'Salish'. It would start up simulation runs such that the total CPU usage did not exceed a preset threshold. This was intended to be less obtrusive to other users of the same machine. Shown below is a point form overview of how the script works.

       - create a list of parameter change sets. Each item specifies which parameters should be different from the default namelist
       - for each parameter change set

             - wait until there are enough processors available

             - create a biological namelist file with the default values and the change applied

             - create a run identifier- typically the name of the changed parameter and its new value
             - call salishsea_cmd.api.run_in_subprocess(), specifying the name of the result directory as the run identifier and using the modified namelist

The various datasets created using this method are currently stored in /data/jpetrie/MEOPAR/SalishSea/results/ on the local filesystem. All individual result directories are stored with the long name name of the parameter and the value used for that parameter, eg. "nampismezo_zz_rate_mesozoo_alpha_0.5". Detailed biological parameter information for the run can be found in the file namelist_pisces_cfg stored in the run directory. The combined dataset contains results for every parameter that doesn't contain the string "zz_frac_waste" in its name and does not have a default value of 0.

The same modifications were made with February, April, and June initial conditions. The April and June result directories are stored in all_params_AprIC_june_22 and all_params_JunIC_june_23 respectively. The February results are stored differently- they are sorted by section and the file names do not explicitly say that they are using February initial conditions. The following list details the directories the February runs are split into: nampiszoo_june_14/, nampisopt_june_14/, nampismes_june_14/, nampissink_june_17/, nampisprod_june_16/, nampismort_june_17/, nampisrem_june_17, nampismezo_june_20/. They have a format of [section name]_[date computed].

If the model is changed and the results need to be regenerated follow the steps below:

	- Compile the 5x5 model if you haven't already
        - Open the python `script <https://bitbucket.org/salishsea/analysis-james/src/tip/batch_5x5.py?fileviewer=file-view-default/>`_
	- Create a modified version of the reference yaml file to match your directory organisation
	- Change the reference_yaml variable in the script to point to your new yaml file
	- Change the results_dir variable to wherever you want the output
	- If only certain parameters are needed, modify the first for loop to only include these in the patch list
	- ssh onto Salish (optional)
	- Run the script (using "python batch_5x5.py")

