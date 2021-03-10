#!/bin/bash

#!/bin/bash
#_______________________________________________________________________________
#
# consensus-jc.sh --- Estimate species tree topology probabilities for a range
#                     of parameter regimes.
#_______________________________________________________________________________
#
# Author: max hill
# (Last updated 2021-03-09)
#
# INPUT: specified species-tree parameters for a species tree with three leaves
# and topology ((AB)C), number of loci to sample, and number of base pairs per
# loci.
#
# OUTPUT: Several .csv files in the data/ directory containing the information
# about the runs (both the estimated inference probabilities as well as the
# parameters associated with each run. See execute-consensus-jc.lisp for
# additional details.
#
# PRODECURE: This script compiles simulator.lisp and execute-consensus-jc.lisp,
# producing simulator.fasl files and execute-consensus-jc.lisp. It then runs the
# compiled files. In particular, simulator.lisp contains the 'guts' including
# all functions to implement the simulation and inference methods, whereas
# execute-consensus-jc.lisp contains the 'brains', choosing which parameter
# values to use and directing the execution of the simulation.


sbcl  --dynamic-space-size 10000 --noinform --eval '
(progn (compile-file "simulator.lisp" :print nil)
       (compile-file "execute-consensus-jc.lisp" :print nil)
       (load "simulator.fasl")
       (time (load "execute-consensus-jc.fasl"))
       (quit))'

# Output is timed.
# Additional data processing commands will be included, witten in R.

# 2021-03-09 UPDATE: it appears that compiling or not does not affect runtime.
