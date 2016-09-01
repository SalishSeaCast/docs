.. _batch_runs:

=======================
SMELT Sensitivity Analysis: Batch Simulation Runs
=======================

The first task was to create a dataset of simulation results for the different parameter changes. There are 6 changes per parameter and over 100 parameters in total, so this is about 600 simulation runs. The SMELT model has a large physical domain (398x898x40) and so it is fairly slow to run. Due to the number of runs required it was not feasible to use the full model. Instead we created a smaller domain (5x5x40) with simplified physics that was much faster to run (3 months simulation time in about 30 minutes on one processor). Ideally trends observed in the smaller model will also be applicable to the full model.

With a minimum of several hundred simulation runs it made sense to automate this process. A python script was created to modify the parameters one at a time and run the model for each change. The code can be found `here <https://bitbucket.org/salishsea/analysis-james/src/c1e9a3bd21a080a917338986fe788ddd0a037900/batch_5x5.py?fileviewer=file-view-default/>`_.

Currently utilizes 'salish' to run tens or hundreds of 20 minute jobs.                                                                                       
                                                                                                                                                                                                            
How it works:                                                                                                                                                                                                
       - create a list of parameter change sets. Each item specifies which parameters should be different from the default namelist                                                                          
       - for each parameter change set:                                                                                                                                                                      
             - wait until there are enough processors available                                                                                                                                              
             - create a biological namelist file with the default values and the changes applied                                                                                                             
             - create a run identifier- typically the name of the changed parameter and its new value                                                                                                        
             - call salishsea_cmd.api.run_in_subprocess(), specifying the name of the result directory as the run identifier and using the modified namelist                                                

