# Project Notebook
(Last updated: 2021-04-08)

# Rubric
* Does the phylogenetic analysis have a clear goal and a clear guideline of steps to follow?
* Are the methods chosen justified for the data at hand?
* Are the specific assumptions and limitations of each method acknowledged?
* Does the analysis have a reproducible script?
* Is the reproducible script easy to follow and understand?

# Instructions
The draft due on April 9 should be a skelethon version of ideas, not a complete written product. The goal of the peer evaluation is to provide feedback on the analysis steps, not on grammar/wording. The peer evaluation should also address how reproducible the current state of the scripts are.

Questions the peer reviewer should consider:

* Does the phylogenetic analysis have a clear goal and a clear guideline of steps to follow?
* Are the methods chosen justified for the data at hand?
* Are the specific assumptions and limitations of each method acknowledged?
* Does the analysis have a reproducible script?
* Is the reproducible script easy to follow and understand?

# Data

## Overview and research question


Broadly speaking, phylogenetic inference using summary tree methods involves a two-step process. First, observed gene sequences are used to construct a collection of 'gene trees', each representing the evolution of a single gene (or 'locus'). Second, a species tree is inferred from the gene trees by looking at some informative property of the gene trees, such as the most commonly-observed rooted species tripets or unrooted species quartets. Provided that the gene trees are estimated without error, such methods are generally statistically consistent, meaning that the inferred species tree converges to the true species tree as the number of sampled genes increases (Warnow 2018, Chapter 10.5). 

In my project, I investigate the robustness of such methods when certain assumptions they make about gene trees are violated. Each gene tree is thought to represent the ancestry of a collection of orthologous genes observed in the sequence data. For example, a protein-coding gene which existed in the genome of the MRCA of humans, chimps, and gorillas may have evolved differently in these three daughter species in such a way that the human, chimp, and gorilla genomes that exist today exhibit three slightly different (but related) versions of this gene. If the human and chimp versions are the most similar, then the corresponding gene tree would have topology ((HC)G) and branch lengths determined by the degree of genetic difference between the three gene versions. In real biological terms, this gene tree represents the idea that the two versions of the gene observed in humans and chimps descend from an ancestor gene that existed more recently than any common ancestor with gorillas.

One assumption here is that gene ancestries can be accurately represented by _trees_ (as opposed to more general graphs). In order to be valid, this assumption requires that offspring always inherits the _entire gene_ from one parents or the other. However this may not be biologically reasonable. For example, recombination may cause a child to inherit part of their gene from one parent and part from the other. To what extent does such intralocus recombination present a challenge to phylogenetic inference? In particular, does it present a significant impediment to correct topological inference when using summary gene tree coalescent methods? 

I hope to make a contribution to the ongoing debate regarding these questions. On one hand, it is well-established that summary methods based on the most commonly occuring species triplets or species quartets are consistent for estimating the unrooted species tree from gene trees generated under the multispecies coalescent, a model does not allow for intralocus recombination (see Warnow 2018, Chapter 10.5). By contrast, I have previously shown (not in this project) that when when intralocus recombination is allowed, there exist parameter regimes in which those summary methods (e.g. based on the most commonly-occuring rooted triplets, unrooted quartets, and maximum-likelihood trees) fail to infer the correct species tree. In this project I seek to use simulation to characterize the zone of parameters in which these methods fail in order to better understand the effect of intralocus recombination on species tree inference. 



### Methods and Data
The data consists of simulated gene trees and sequences since this allows us to precisely specify the parameter regime (e.g. the species tree, mutation rate, and recombination rates) in order to test their effects on inference. 

In order to allow for recombination, we simulate gene trees using a modification of the multispecies coalescent based on an important model of gene recombination called the Ancestral Recombination Graph (Griffiths and Marjoram 1997). 

Inference methods considered include (1) species tree inference using R*, a summary method based on rooted triples (see Degnan et al 2009 at https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2909780/ for complete description), 

Further, an important paper in this area (Lanier and Knowles 2012) concluded that intralocus recombination is not a significant source of phylogenetic conflict and may be safely disregarded. I would like to replicate some of their analyses (using *BEAST) but have not got to that. I suspect that certain parameter reg

All simulations assume a species tree with three species A, B, and C, having topology ((AB)C).



# Reproducing the pipeline
Note: this pipeline is written for Linux (I'm running Debian 10 stable), so I can't guarantee things will work for other operating systems. By default, the simulation sets aside 10GB of RAM, but this is not necessary and can be modified. 

## Step 1. Install dependencies. 

Install Steel Bank Common Lisp (SBLC). For Ubuntu/Debian, run the command

```sudo apt-get install sbcl```

If you are running a different operating system, instructions can be found at https://lisp-lang.org/learn/getting-started/


## Step 2. Run a simulation.

To run a simulation using the default simluation parameters (i.e. those specified in the file [simulation-parameters.lisp](scripts/simulation-parameters.lisp)), navigate to the scripts/ directory and run the command

```bash simulate.sh X```

where X=0,1, or 2. The input X determines
which type of simulation is performed and is described in the INPUT section
below.
The value of X determines what type of simulation is performed, what
data is generated, and how the species tree is inferred. The precise
specficiation for X = 0, 1 and 2 is as follows:

  (0) Binary Sequences + Yang-ML inference:

      Simulate an ancestral recombation graph with parameter regime specified
      in 'execute-consensus-ml.lisp'. Generate binary sequences for each
      extant species. Infer a species tree using maximum-likelihood. The
      maximum-likelihood tree is obtained analytically using site-frequency
      data and the method described in Table 4 of Yang 2000.

  (1) Expected JC69 distance + R* inference:

      Simulate an ancestral recombination graph with parameter regime
      specified in 'execute-consensus-sh.lisp'. Compute expected genetic
      distances between extant species using the Jukes-Cantor 1969 model of
      site evolution. Infer a species tree with R* consensus method based on
      those expected distances.

  (2) JC69 sequences + R* inference:

      Simulate an ancestral recombination graph with parameter regime
      specified in 'execute-consensus-jc-seq.lisp' Generate a sequence (with
      letters A,T,C,G) for each extant species. Infer species tree with R*
      consensus method based on actual sequence distances.



## Consensus R* -- weighted JC distance
consensus-jc.lisp

## Consensus R* -- with mutations
consensus-mut.lisp

## Maximum Likelihood -- Yang

Navigate to /scripts/, load simulator.lisp and run 
``(time (ml-estimate-topology-probabilities 1 1.01 999999 10 0 0 0 0 .1 2000000 100))``

Evaluation took:
  1085.782 seconds of real time
  1086.609552 seconds of total run time (1085.461362 user, 1.148190 system)
  [ Run times consist of 6.597 seconds GC time, and 1080.013 seconds non-GC time. ]
  100.08% CPU
  3,153,167,832,745 processor cycles
  192,159,098,272 bytes consed
  
"0.32665983,0.35443458,0.3189056,1,1.01,999999,10,0,0,0,0,0.1,2000000,100"
