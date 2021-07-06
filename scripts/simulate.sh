#!/bin/bash
#_______________________________________________________________________________
#
# simulate.sh --- command line script to easily run simulations 
#_______________________________________________________________________________

# Author: Max Hill
# (Last updated 2021-06-21)
#
# INSTRUCTIONS: This file is intended to be run from the command line. To run
# this script, navigate to the scripts/ directory and run the command 'bash
# simulate.sh X', where X=0,1, or 2. The input X determines which type of
# simulation is performed and is described in the DESCRIPTION section below.
#
# DESCRIPTION: This file contains the script for running a simulation under each
# of the parameter regimes specified in the file simulation-parameters.lisp. The
# simulation seeks to model the evolution of a locus of DNA consisting of *L*
# base pairs on a species tree with topology ((AB)C) when the possibility of
# intralocus recombination is present. There are three simulation/inference
# modes to choose from. The three modes are ml-sequence, jc-expected, and
# jc-sequence. The results for each parameter regime consists of information
# about how well the inference procedure performed (eg how often the correct
# species tree topology was inferred under that regime). The results for each
# regime are recorded as a row in an automatically-named output csv file in the
# data directory.
#

### ---Begin script---

################################################################################

# Name the input variable. 
inference_method="$1"

# Check that the input variable is an element of {0,1,2}.
if [[ $inference_method != [0123] ]]
then
    echo "error: argument must be an element of {0,1,2}"
    echo "0 == Yang's maximum likelihood procedure on binary sequences ('ml-sequence')"
    echo "1 == R* inference using *expected* uncorrected sequence distances under JC69 substitution model ('jc-expected')"
    echo "2 == R* inference using uncorrected sequence distances under JC69 substitution model ('jc-sequence')"
    echo "3 == STEAC inference using uncorrected sequence distances under JC69 substitution model ('steac')"
    exit
fi

echo "Running a simulation of the following type:"

if [[ $inference_method == 0 ]]; then echo "Binary Sequences + Yang-ML inference"; fi
if [[ $inference_method == 1 ]]; then echo "R* using expected distances with JC69"; fi
if [[ $inference_method == 2 ]]; then echo "R* using sequence data with JC69"; fi
if [[ $inference_method == 3 ]]; then echo "STEAC with JC69"; fi
   
   
   
sbcl --noinform --eval '
(progn (load "simulator.lisp")
       (load "simulation-parameters.lisp")
       (load "initiate-simulation.lisp")
       (quit))' $inference_method 2>/dev/null
# Note that SBCL ('steel bank common lisp') is an implmentation of common lisp,
# the language our simulator is written in. When loading files, SBCL outputs
# style warnings by default. Since this might confuse users not familiar with
# lisp, such behavior is surpressed by piping standard error into the black hole
# of /dev/null. Surely this won't have any unintended consequences.

# NOTE ABOUT MEMORY ALLOCATION: By default, SBCL allocates about 1GB of RAM to
# be used. If *N* or *L* is very large, this may not be enough. To allocated a
# specified X megabytes of RAM, one can add the option '--dynamic-space-size X'
# when calling sbcl.

################################################################################

### ---Script ends here---
