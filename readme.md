# Readme
	(Last updated 2021-04-30)


## Project Overview
In this project I simulate genetic data subject to intralocus recombination
under various parameter regimes (mutation rate, species tree branch lengths,
recombination rates, etc) in order evaluate the robustness of summary coalescent
methods to violations of the "no intralocus recombination" assumption.

For my project report, see
[project report](report.md). To replicate those results, follow the step-by-step
instructions in the section "reproducing the pipeline" below.

For further information on the scripts used, see the [scripts
readme](scripts/readme.md).

For detailed description of the format and structure of the datafiles, see the [data
readme](data/readme.md). Datafiles are contained in the `data/` directory. Plots
are contained in the `analysis/` directory.

## Reproducing the pipeline
To replicate or extend my results detailed in the [project report](report.md),
clone this repository and follow the step-by-step instructions below.

### Step 1. Setup

#### Install SBCL
We need to install SBCL, the Common Lisp implementation used by our
simulator. If you are running Ubuntu/Debian, simply run the command

```
sudo apt-get install sbcl
```

If you are running OS X, run the command 

```
brew install sbcl
```

If you are running a different operating system, instructions can be found at
[https://lisp-lang.org/learn/getting-started/](https://lisp-lang.org/learn/getting-started/).
(Note that you do not need to install Quicklisp, Emacs, or SLIME.)


#### Install R and ggplot2
We will use R and the R package ggplot2 to make plots. To install R on Debian
(and probably Ubuntu), run the command

```
sudo apt-get install r-base
```

More detailed instructions for installing R can be found at [https://cloud.r-project.org/](https://cloud.r-project.org/).

To install the R package ggplot2, run the command

```
sudo apt-get install r-basesudo su - -c "R -e \"install.packages('ggplot2', repos='http://cran.rstudio.com/')\""
```

I have no idea how to do this on other operatings systems.



### Step 2. Simulate the Data

Since they are reasonably small, I have opted to include all simulated datafiles
used for this project in the `data/` directory. To replicate any particular
datafile, you will need to run a simulation using the following steps, using the
appropriate parameter regime specified in the [project report](report.md) (also listed below).


#### Step 2a. Choose simulation parameters

Simulation global parameters are specified within the file
[simulation-parameters.lisp](scripts/simulation-parameters.lisp). If you wish to
supply your own custom parameter values, you will need to edit this file to
assign them. Instructions and examples for how to do so are provided as
commentary in the file itself. (If you want to run the simulation just using the
parameters already specified in the file, then you don't need to do anything and
can move to step 2b.)

Note that the file
[simulation-parameters.lisp](scripts/simulation-parameters.lisp) allows you to
specify a *list* of values for each parameter, making it possible to simulate a
range of parameter regimes, rather than just a single parameter regime at a
time. By default, when executed with [simulate.sh](scripts/simulate.sh), the
simulator will loop over *all possible combinations* of parameters specified in
[simulation-parameters.lisp](scripts/simulation-parameters.lisp).

If you wish to replicated all of the simulations in the project report, you will
need to do run Step 2b with each each of the three following parameter ranges:

````
(defparameter *τ_ab-values* '(1))
(defparameter *f-values* '(.01))
(defparameter *ρ_a-values* '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))
(defparameter *ρ_b-values* '(0))
(defparameter *ρ_c-values* '(0))
(defparameter *ρ_ab-values* '(0))
(defparameter *ρ_abc-values* '(0))
(defparameter *θ-values* '(.1)) 
(defparameter *N* 15000)
(defparameter *L* 500)
(defparameter *τ_max* 999999)
````
and
```` 
(defparameter *τ_ab-values* '(1))
(defparameter *f-values* '(.001 .01 .1))
(defparameter *ρ_a-values* '(0 5 10))
(defparameter *ρ_b-values* '(0))
(defparameter *ρ_c-values* '(0))
(defparameter *ρ_ab-values* '(0 5 10))
(defparameter *ρ_abc-values* '(0)) 
(defparameter *θ-values* '(.001 .01 .05 .1 .2))
(defparameter *N* 10000)
(defparameter *L* 500)
(defparameter *τ_max* 999999)
````
and
````
(defparameter *τ_ab-values* '(1))
(defparameter *f-values* '(.01 .02 .03 .04 .05 .06 .07 .08 .09 .1 .11 .12 .13 .14 .15 .16 .17 .18 .19 .2))
(defparameter *ρ_a-values* '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))
(defparameter *ρ_b-values* '(0))
(defparameter *ρ_c-values* '(0))
(defparameter *ρ_ab-values* '(0))
(defparameter *ρ_abc-values* '(0)) 
(defparameter *θ-values* '(.1 .01)) 
(defparameter *N* 10000) 
(defparameter *L* 50) 
(defparameter *τ_max* 999999)
````



#### Step 2b. Run simulate.sh

In the terminal, navigate to the `scripts/` directory and run the command
```
bash simulate.sh X
```
where X=0,1, or 2. The input X determines which type of inference method is
performed, according to the following table:
```
 X  Inference method
(0) ML-sequence 
(1) JC-expected
(2) JC-sequence
```

Detailed descriptions of these options are found in the [project
report](report.md).

Alternatively, to run the simulation under each of the three methods
consecutively, run the command
```
bash simulate.sh 0; bash simulate.sh 1; bash simulate.sh 2
```

Depending on the parameter values that you chose, the simulation may take some
time. (Replicating the datafiles here will take a few hours or more depending on
your computer hardware.) The simulator will track its progress by printing out a
number each time it completes simulation of a parameter regime.

Upon completion, a single .csv file is created, located in the `data/`
directory. The name of this output file will depend on the choice of parameter
regime and inference type chosen. For further information, including how to
interpret the rows and columns of the datafiles, see the [data
readme](data/readme.md).


### Step 3. Generate Plots

After running at simulations under all parameter regimes specified in parts 1-4
of the results section, we can automatically generate all plots contained in
this report by executing the R script [make-plots.R](scripts/make-plots.R). To
run this file from the command line, navigate to the `scripts/` directory and
run the command

```
Rscript make-plots.R
```

This will output all the plots to the `analysis/` directory.

Alternatively, if you wish to do one plot at a time, you can copy and paste
snippets of the code from [make-plots.R](scripts/make-plots.R) into an R REPL
(just make sure you are in the right directory with the correct libraries
loaded).




## Todo

* Plan some longer simulations with smaller mutation rates

* Simulations with virus-specific parameter values

* Redo the simulation from part 2 of the project report with more values for f,
  more values for recombination rate in A, no recombination in AB, and only one
  mutation rate).
  
* Section 3 plots would be better illustrated by a surface in three dimensions
  with an intersecting plane given by z:= P[A(BC)]-P[(AB)C]=0

