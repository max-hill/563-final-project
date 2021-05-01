# Script Readme
	(Last updated: 2021-04-30)

A short descriptions of each script is given. Additional, more complete
documentation for each script can be found as commentary in the files
themselves.

# Scripts
* [simulate.sh](simulate.sh) contains the script for starting up a simulation
  under a set of parameter regimes as specified in
  [simulation-parameters.lisp](simulation-parameters.lisp). It is intended to be
  run from the command line by the user (i.e. you). There are three simulation
  types to choose from:
  
     0. **ML-Sequence:** binary seqeuences with maximum likielihood gene trees under
        the symmetric mutation process (ie 0 --> 1 and 1 --> 0 at equal rates).
	 1. **JC-Expected:** *expected* evolutionary (Hamming) distances
        under the Jukes-Cantor 1969 model of site evolution;
     2. **JC-Sequence:** evolutionary (Hamming) distances under the Jukes-Cantor
        1969 model of site evolution. 
		
  Detailed descripton of these three modes, as well as more technical
  documentation, can be found as commentary within the file itself.
  
* [simulation-parameters.lisp](simulation-parameters.lisp) is a document which
  specifies the range of parameter regimes to be simulated when
  [simulate.sh](simulate.sh) is executed. Detailed instructions for how to
  customize the parameters is provided as commentary in the file
  [simulation-parameters.lisp](simulation-parameters.lisp) itself.
  
* [initiate-simulation.lisp](initiate-simulation.lisp) runs the user-selected
  inference method over all parameter regimes specified in
  [simulation-parameters.lisp](simulation-parameters.lisp) and records the
  results in a csv file. This file is executed automatically by the script
  [simulate.sh](simulate.sh).

* [simulator.lisp](simulator.lisp) contains all general lisp functions for
  simulating the multispecies coalescent with recombination on a three-taxa
  species tree with topology ((AB)C), as well as those functions for inferring
  gene 'tree' topologies from the simulated data. It is loaded automatically by
  [simulate.sh](simulate.sh).

* [make-plots.R](make-plots.R) contains R code for creating all plots in my
  project report from the simulation data contained in the `data/` directory. The
  plots are output to the `analysis/` directory. This file can be run from bash
  using Rscript (see project.md for instructions), or individual plots can be
  created by copying parts of the code into an R REPL.

* [plotmaker.R](plotmaker.r) is an outdated script that contains some basic
  notes from when I was learning R to plot and examples of running the simulator
  functions from the LISP REPL. It is not used by the pipeline and can be safely
  ignored.
