# Final Project for Phylo-Class 563
	(Last updated 2021-04-29)


## Project Overview
In this project I simulate genetic data subject to intralocus recombination
under various parameter regimes (mutation rate, species tree branch lengths,
recombination rates, etc) in order evaluate the robustness of summary coalescent
methods to violations of the "no intralocus recombination" assumption.

For my project report and detailed instructions for reproducing my results, see
[report.md](report.md).

For further information on the scripts used, see the [scripts
readme](scripts/readme.md).

For detailed description of the format and structure of the datafiles, the [data
readme](data/readme.md). Datafiles are cotnained in the `data/` directory. Plots
are contained in the `analysis/` directory.


## Todo
* Check that all correct datafiles are uploaded.

* Finish project report. 

* Plan some longer simulations. 

* Redo the simulation from part 2 of the project report with more values for f,
  more values for recombination rate in A, no recombination in AB, and only one
  mutation rate).
  
* Section 3 plots would be better illustrated by a surface in three dimensions
  with an intersecting plane given by z:= P[A(BC)]-P[(AB)C]=0
