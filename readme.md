# Final Project for Phylo-Class 563
	(Last updated 2021-04-29)


## Project Overview
In this project I simulate genetic data subject to intralocus recombination
under various parameter regimes (mutation rate, species tree branch lengths,
recombination rates, etc) in order evaluate the robustness of summary coalescent
methods to violations of the "no intralocus recombination" assumption.

For my project report and detailed instructions for reproducing my results, see
[report.md](report.md). To replicate those results, follow the step-by-step
instructions in the section "reproducing the pipeline" below.

For further information on the scripts used, see the [scripts
readme](scripts/readme.md).

For detailed description of the format and structure of the datafiles, the [data
readme](data/readme.md). Datafiles are cotnained in the `data/` directory. Plots
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
appropriate parameter regime specified in the [project report](report.md).


#### Step 2a. Choose simulation parameters

If you wish to run a simulation using the default simulation parameters (i.e.
those already specified in the file
[simulation-parameters.lisp](scripts/simulation-parameters.lisp)), then you
don't need to do anything and can move to step 2b.

If you wish to supply your own parameter values, you will need to edit the file
[simulation-parameters.lisp](scripts/simulation-parameters.lisp) to assign
custom parameter values. Instructions and examples for how to do so are provided
as commentary in the file itself.

The file [simulation-parameters.lisp](scripts/simulation-parameters.lisp) allows
you to specify a list of values for each parameter. This makes it possible to
simulate a *range* of parameter regimes, rather than just a single parameter
regime at a time. By default, when executed with
[simulate.sh](scripts/simulate.sh), the simulator will loop over *all possible
combinations* of parameters specified in
[simulation-parameters.lisp](scripts/simulation-parameters.lisp).

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

To run the simulation under each of the three methods consecutively, run the command
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
snippets of the code from [make-plots.R](scripts/make-plots.R) into an R REPL.




## Todo
* Finish project report. 

* Plan some longer simulations with smaller mutation rates

* Simulations with virus-specific parameter values

* Redo the simulation from part 2 of the project report with more values for f,
  more values for recombination rate in A, no recombination in AB, and only one
  mutation rate).
  
* Section 3 plots would be better illustrated by a surface in three dimensions
  with an intersecting plane given by z:= P[A(BC)]-P[(AB)C]=0

