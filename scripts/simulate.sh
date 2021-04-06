#!/bin/bash
#_______________________________________________________________________________
#
# simulate.sh --- command line script to easily run simulations 
#_______________________________________________________________________________

# Author: Max Hill
# (Last updated 2021-04-06)
#
# DESCRIPTION: This file contains the script for starting up a simulation under
# a pre-specified parameter regime. It is intended to be run from the command
# line by the user (i.e. you). There are three simulation types to choose from
# (see INSTRUCTIONS and INPUT sections below).
#
# INSTRUCTIONS: To run this script, navigate to the scripts/ directory and run
# the command 'bash simulate.sh X', where X=1,2, or 3. The input X determines
# which type of simulation is performed and is described in the INPUT section
# below.
#
# INPUT: The value of X determines what type of simulation is performed, what
# data is generated, and how the species tree is inferred. The precise
# specficiation for X = 0, 1 and 2 is as follows:
#
#   (0) Binary Sequences + Yang-ML inference:
#
#       Simulate an ancestral recombation graph with parameter regime specified
#       in 'execute-consensus-ml.lisp'. Generate binary sequences for each
#       extant species. Infer a species tree using maximum-likelihood. The
#       maximum-likelihood tree is obtained analytically using site-frequency
#       data and the method described in Table 4 of Yang 2000.
#
#   (1) Expected JC69 distance + R* inference:
#
#       Simulate an ancestral recombination graph with parameter regime
#       specified in 'execute-consensus-sh.lisp'. Compute expected genetic
#       distances between extant species using the Jukes-Cantor 1969 model of
#       site evolution. Infer a species tree with R* consensus method based on
#       those expected distances.
#
#   (2) JC69 sequences + R* inference:
#
#       Simulate an ancestral recombination graph with parameter regime
#       specified in 'execute-consensus-jc-seq.lisp' Generate a sequence (with
#       letters A,T,C,G) for each extant species. Infer species tree with R*
#       consensus method based on actual sequence distances.


### ---Begin script---

################################################################################


# Name the input variable. 
inference_method="$1"

# Check that the input variable is an element of {0,1,2}.
if [[ $inference_method != [012] ]]
then
    echo "error: argument must be an element of {0,1,2}"
    exit
fi




# (0) Binary Sequences + Yang-ML inference:
if [[ $inference_method == 0 ]]
then
    echo "Running a simulation of the following type:"
    echo "Binary Sequences + Yang-ML inference"
    sbcl  --dynamic-space-size 10000 --noinform --eval '
    (progn (load "simulator.lisp")
           (load "simulation-parameters.lisp")
           (load "execute-consensus-ml.lisp")
           (quit))' 2>/dev/null 
    exit
fi
# Note that SBCL ('steel bank common lisp') is an implmentation of common lisp,
# the language our simulator is written in. When loading files, SBCL outputs
# style warnings by default. Since this might confuse users not familiar with
# lisp, such behavior is surpressed by piping standard error into the black hole
# of /dev/null. Surely this won't have any unintended consequences.





# (1) Expected JC69 distance + R* inference:

if [[ $inference_method == 1 ]]
then
    echo "Running a simulation of the following type:"
    echo "Expected JC69 distance + R* inference"
    sbcl  --dynamic-space-size 10000 --noinform --eval '
    (progn (compile-file "simulator.lisp" :print nil)
	   (compile-file "execute-consensus-jc.lisp" :print nil)
	   (load "simulator.fasl")
	   (time (load "execute-consensus-jc.fasl"))
	   (quit))'
    exit
fi

# (2) JC69 sequences + R* inference:
if [[ $inference_method == 2 ]]
then
    echo "Running a simulation of the following type:"
    echo "JC69 sequences + R* inference"
    echo "just kidding! It hasn't been implemented yet" 
    exit
fi

################################################################################

### ---Script ends here---
