#!/bin/bash
#_______________________________________________________________________________
#
# simulate.sh --- command line script to easily run simulations 
#_______________________________________________________________________________

# Author: Max Hill
# (Last updated 2021-04-28)
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


# 
# There are three simulation modes, essentially corresponding to three different
# ways of inferring the gene tree topology from the generated data. We shall
# refer to these three modes as ml-sequence, jc-expected, and jc-sequence. The
# three modes differ primarily in the type of sequence data they produce and the
# way that they infer a 'gene tree' topology from each individually simulated
# locus. All three modes are consensus-based in the sense that we assume that
# the ultimate estimate of the species tree topology is chosen to be the most
# commonly occuring gene tree topology. A full description of the simulation
# procedure under each of the three modes is as follows:
#
#   (0) ml-sequence (maximum-likelihood with binary sequences):
#
#       Given a fixed parameter regime, do the following three steps *N* times:
#       first, simulate an ancestral recombation graph (ARG) with the given
#       parameter regime. Second, use the ARG to generate binary DNA sequences
#       for each taxa by modeling mutations under the symmetric model of site
#       evolution. Third, infer a gene tree topology from the sequence data
#       using maximum-likelihood. In particular, the maximum-likelihood tree is
#       obtained from site-frequency data using results from Yang 2000 (ie Table
#       4 in that paper).
#
#   (1) jc-expected (expected Hamming distances under the JC69 model of site
#                    evolution):
#
#       Given a fixed parameter regime, do the following three steps *N* times:
#       first, simulate an ARG with the given parameter regime. Second,
#       conditional on the simulated ARG, and assuming the Jukes-Cantor (1969)
#       model of site evolution, compute the expected Hamming distances between
#       sequences. (This step uses a formula based on coalescent times and
#       locations of recombination events on the ARG and does not require
#       sequences to be generated). Third, infer a topology using the rule that
#       the two taxa with the smallest expected difference are most closely
#       related.
#
#   (2) jc-sequence (Hamming distances under the JC69 model of site evolution)
#
#       For each parameter do the following four steps *N* times: first,
#       simulate an ARG with the given parameter regime. Second, for each taxa
#       generate a nucleotide sequence with possible bases A,T,C, and G by
#       simulating the Jukes-Cantor process on each marginal gene tree of the
#       ARG. Third, compute the Hamming distance between the sequences for each
#       taxa. Fourth, infer a tree topology using the rule that the two taxa
#       with the smallest Hamming distance are most closely related.
#
#   Having completed (0), (1), or (2), the program then records in the csv the
#   proportion of the *N* samples which resulted in correct topological
#   inference vs inferred incorrect topologies. That is, for each of the three
#   possible topologies ((AB)C), ((AC)B) and ((BC)A), it records the fraction of
#   samples for which that topology was inferred. Such information is recorded
#   in a single row in the csv, along with the information about the parameter
#   regime that was used. The simulator then moves on to the next parameter
#   regime and repeat the process to generate the next row of the csv.
#
# OUTPUT: A single .csv file located in the data/ directory named
# 'inference-method-N-L.csv', where 'inference method' is replaced by an element
# of the set {ml-sequence, jc-expected, jc-sequence}, and N, L are specified in
# the table below. If a file of that name already exists, then the results will
# be appended to the existing file. The csv will have 14 columns, indicated in
# the following table:
#
# | Column Number | Symbol | Description or definition                       |
# |---------------+--------+-------------------------------------------------|
# |             1 | P-ab   | estimated probability of inferring ((AB)C)      |
# |             2 | P-ac   | estimated probability of inferring ((AC)B)      |
# |             3 | P-bc   | estimated probability of inferring ((BC)A)      |
# |             4 | τ_ab   | divergence time of species A and B              |
# |             5 | τ_abc  | divergence time of species AB and C             |
# |             6 | τ_max  | maximum height of the tree (pick large)         |
# |             7 | ρ_a    | recombination rate in population A              |
# |             8 | ρ_b    | recombination rate in population B              |
# |             9 | ρ_c    | recombination rate in population C              |
# |            10 | ρ_ab   | recombination rate in population AB             |
# |            11 | ρ_abc  | recombination rate in population ABC            |
# |            12 | θ      | mutation rate per site per coalescent unit      |
# |            13 | N      | number of sampled loci (ie # of ARGS simulated) |
# |            14 | L      | length of each locus in base pairs              |

### ---Begin script---

################################################################################

# Name the input variable. 
inference_method="$1"

# Check that the input variable is an element of {0,1,2}.
if [[ $inference_method != [012] ]]
then
    echo "error: argument must be an element of {0,1,2}"
    echo "0 == Yang's maximum likelihood procedure on binary sequences ('ml-sequence')"
    echo "1 == R* inference using *expected* uncorrected sequence distances under JC69 substitution model ('jc-expected')"
    echo "2 == R* inference using uncorrected sequence distances under JC69 substitution model ('jc-sequence')"
    exit
fi

echo "Running a simulation of the following type:"
echo "Binary Sequences + Yang-ML inference"
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
