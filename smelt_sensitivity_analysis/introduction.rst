SMELT Sensitivity Analysis: Introduction
=======================

SMELT (Salish Model Ecosystem- Lower Trophic) is a one dimensional biological model (SOG) coupled to a physical model of the Salish Sea (NEMO). This model tracks the interactions and movement of several types of plankton and nutrients. 

In the model parameters are used to describe things like plankton growth functions, sinking rates, mortality rates, light requirements and nutrient requirements. There are over one hundred biological parameters with values that have been chosen based on related research.

A sensitivity analysis was performed on this model to better understand the impact of individual parameter choices on model results. These pages describes the methodology used and current findings.

The entirety of the analysis is based around the set of default values. The goal is to identify the effect of individual parameters; this can be done by modifying one parameter at a time and comparing the simulation results. It was decided to test each parameter at 10%, 50%, 90%, 110%, 200% and 1000% of its default value. Parameter notebooks and metric functions were then used to examine the impact of these changes.
