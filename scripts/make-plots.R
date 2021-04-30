#!/usr/bin/env Rscript


## This file uses the R package ggplot2 to create plots from the simulated data.


## Install and load necessary R packages

packages <- c("ggplot2")

installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages]) # Install packages
}

invisible(lapply(packages, library, character.only = TRUE)) # Packages loading


## Set working directory to 'data/' 
setwd('../data/')

## Plot 1
## Simulation run with parameters

## (defparameter *τ_ab-values* '(1))
## (defparameter *f-values* '(.01))
## (defparameter *ρ_a-values* '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))
## (defparameter *ρ_b-values* '(0))
## (defparameter *ρ_c-values* '(0))
## (defparameter *ρ_ab-values* '(0))
## (defparameter *ρ_abc-values* '(0))
## (defparameter *θ-values* '(.1)) 
## (defparameter *N* 15000)
## (defparameter *L* 500)
## (defparameter *τ_max* 999999)

## we ran this with each of the three settings

## jc-expected
df = read.csv("jc-expected-N15000-L500.csv")
plot1 = ggplot(df, aes(x=ρ_a, y= P.bc - P.ab)) +
    geom_point() + 
    labs(title="Recombination effect on most likely inferred gene topology under JC-expected", x="recombination rate in population A",y="Pr[A(BC)]-Pr[(AB)C]")
ggsave("plot1-jce.jpeg",path="../analysis/")

## jc-sequence
df = read.csv("jc-sequence-N15000-L500.csv")
plot1jce = ggplot(df, aes(x=ρ_a, y= P.bc - P.ab)) +
    geom_point() + 
    labs(title="Recombination effect on most likely inferred gene topology under JC-sequence",x="recombination rate in population A",y="Pr[A(BC)]-Pr[(AB)C]")
ggsave("plot1-jcs.jpeg",path="../analysis/")

## ml-sequence
df = read.csv("ml-sequence-N15000-L500.csv")
plot1jce = ggplot(df, aes(x=ρ_a, y= P.bc - P.ab)) +
    geom_point() + 
    labs(title="Recombination effect on most likely inferred gene topology under ML-sequence",x="recombination rate in population A",y="Pr[A(BC)]-Pr[(AB)C]")
ggsave("plot1-mls.jpeg",path="../analysis/")




## Plot 2

## Simulation parameters

## (defparameter *τ_ab-values* '(1))
## (defparameter *f-values* '(.001 .01 .1))
## (defparameter *ρ_a-values* '(0 5 10))
## (defparameter *ρ_b-values* '(0))
## (defparameter *ρ_c-values* '(0))
## (defparameter *ρ_ab-values* '(0 5 10))
## (defparameter *ρ_abc-values* '(0)) 
## (defparameter *θ-values* '(.001 .01 .05 .1 .2))
## (defparameter *N* 10000)
## (defparameter *L* 500)
## (defparameter *τ_max* 999999)


## The next three plots give some indication of relative the effects of recombination rate of species A vs internal branch length on the species tree. In these simulations, the recombination rate in populations A and AB were either 0, 5, or 10, and zero recombination elsewhere. The color gradient represents the length of the internal branch in coalescent units (taking values 0.001, 0.01, or 0.1), with light blue being longer internal branch and black being shorter. The somewhat complicated expression for the x-axis is the expected number of recombination events which are ancestral to the sampled sequence from A under the root of the tree. The Y axis the the probability that the gene tree from a single locus is inferred to have topology A(BC) minus the probability that it is inferred to have topology (AB)C. The methods considered here are statistically consistent only if this difference is negative.  While a positive relationship is observed as the recombination rates increase, the effect appears overwhelmed by the relatively small absolute (but order-of-magnitude) increases in branch length.

## jc-expected
df = read.csv("jc-expected-N10000-L500.csv")
plot2 = ggplot(df, aes(x=ρ_a + (τ_abc-τ_ab)*ρ_ab/2 , y= P.bc - P.ab , color=τ_abc-τ_ab)) +
    geom_point() + 
    labs(title="Recomb., internal branch, and gene topology probabilities under JC-expected", x="ρ_a + (τ_abc-τ_ab)*ρ_ab/2",y="Pr[A(BC)]-Pr[(AB)C]")
ggsave("plot2-jce.jpeg",path="../analysis/")

## jc-sequence
df = read.csv("jc-sequence-N10000-L500.csv")
plot2 = ggplot(df, aes(x=ρ_a + (τ_abc-τ_ab)*ρ_ab/2 , y= P.bc - P.ab , color=τ_abc-τ_ab)) +
    geom_point() + 
    labs(title="Recomb., internal branch, and gene topology probabilities under JC-sequence", x="ρ_a + (τ_abc-τ_ab)*ρ_ab/2",y="Pr[A(BC)]-Pr[(AB)C]")
ggsave("plot2-jcs.jpeg",path="../analysis/")

## ml-sequence
df = read.csv("ml-sequence-N10000-L500.csv")
plot2 = ggplot(df, aes(x=ρ_a + (τ_abc-τ_ab)*ρ_ab/2 , y= P.bc - P.ab , color=τ_abc-τ_ab)) +
    geom_point() + 
    labs(title="Recomb., internal branch, and gene topology probabilities under ML-sequence", x="ρ_a + (τ_abc-τ_ab)*ρ_ab/2",y="Pr[A(BC)]-Pr[(AB)C]")
ggsave("plot2-mls.jpeg",path="../analysis/")

## Note for future: redo this simulation with more values for f, more values for recombination rate in A, no recombination in AB, and only one mutation rate


## Plot 3 -- Identifying the anomaly zones with gradient illustrations
## (defparameter *τ_ab-values* '(1))
## (defparameter *f-values* '(.01 .02 .03 .04 .05 .06 .07 .08 .09 .1 .11 .12 .13 .14 .15 .16 .17 .18 .19 .2))
## (defparameter *ρ_a-values* '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))
## (defparameter *ρ_b-values* '(0))
## (defparameter *ρ_c-values* '(0))
## (defparameter *ρ_ab-values* '(0))
## (defparameter *ρ_abc-values* '(0)) 
## (defparameter *θ-values* '(.1 .01)) 
## (defparameter *N* 10000) 
## (defparameter *L* 50) 
## (defparameter *τ_max* 999999)

## In the next twelve plots, we seek a graphical representation of the anomaly zone for inference of the rooted triplet toplogy under each of the three methods. The first six plots provide a color gradient which indicates the difference P[A(BC)]-P[(AB)C], with x and y axes representing recombination rate in population A and internal branch length of the species tree respectively. Each dot represents an estimate of P[A(BC)]-P[(AB)C] obtained by simulated 10,000 ARGs for loci of length 50bp. 

## To understand the meaning of the color gradient in the following plots, note that if P[A(BC)]-P[(AB)C]>0 then P[A(BC)]>P[(AB)C], and hence the most frequently-observed gene tree topology will not be the topology A(BC) which matches the species tree. Hence the light blue areas (bottom right on all six graphs) are the area where we expect consensus-based methods to fail.

## Although not visible from these graphs, in almost all the cases simulated, the region of parameter space satisfying P[A(BC)]<P[(AB)C] tends to coincide with the region satisfying P[A(BC)] > max(P[(AB)C], P[(AC)B]). Exceptions to this appear to be the result of random noise owing to insufficent convergence of the simulation's probability estimates, such as in cases where recombination and internal branch length as very small and hence all three topologies have inference probabilities extremely close to 1/3. This suggests that the higher differential rate of recombination occurring in population A compared to other populations tends to make it more probable that A is inferred to be the outgroup in the triplet topology. However the effect is very small: even when P[A(BC)]-P[(AB)C] is positive, in the the graphs presented we have P[A(BC)]-P[(AB)C]<0.1. [As an additional note, the most extreme values I have found in any simulations are in the realm of P[A(BC)] ~ 0.4 and P[(AB)C] ~ 0.3. In no simulation have I yet observed P[A(BC)] > 0.5.]


## ML Sequence
df = subset(read.csv("ml-sequence-N10000-L50.csv"), θ==0.01)
plot3 = ggplot(df, aes(x = ρ_a, y = τ_abc-τ_ab, color = P.bc - P.ab)) +
    geom_point() + 
    labs(title="Gradient illustration of anomaly zone under ML-sequence with θ=0.01", x="ρ_a",y="f=τ_abc-τ_ab")
ggsave("plot3-mls-th0.01.jpeg",path="../analysis/")

df = subset(read.csv("ml-sequence-N10000-L50.csv"), θ==0.1)
plot3 = ggplot(df, aes(x = ρ_a, y = τ_abc-τ_ab, color = P.bc - P.ab)) +
    geom_point() + 
    labs(title="Gradient illustration of anomaly zone under ML-sequence with θ=0.1", x="ρ_a",y="f=τ_abc-τ_ab")
ggsave("plot3-mls-th0.1.jpeg",path="../analysis/")

## JC expected
df = subset(read.csv("jc-expected-N10000-L50.csv"), θ==0.01)
plot3 = ggplot(df, aes(x = ρ_a, y = τ_abc-τ_ab, color = P.bc - P.ab)) +
    geom_point() + 
    labs(title="Gradient illustration of anomaly zone under JC-expected with θ=0.01", x="ρ_a",y="f=τ_abc-τ_ab")
ggsave("plot3-jce-th0.01.jpeg",path="../analysis/")

df = subset(read.csv("jc-expected-N10000-L50.csv"), θ==0.1)
plot3 = ggplot(df, aes(x = ρ_a, y = τ_abc-τ_ab, color = P.bc - P.ab)) +
    geom_point() + 
    labs(title="Gradient illustration of anomaly zone under JC-expected with θ=0.1",x="ρ_a",y="f=τ_abc-τ_ab")
ggsave("plot3-jce-th0.1.jpeg",path="../analysis/")

## JC Sequence
df = subset(read.csv("jc-sequence-N10000-L50.csv"), θ==0.01)
plot3 = ggplot(df, aes(x = ρ_a, y = τ_abc-τ_ab, color = P.bc - P.ab)) +
    geom_point() + 
    labs(title="Gradient illustration of anomaly zone under JC-sequence with θ=0.01", x="ρ_a",y="f=τ_abc-τ_ab")
ggsave("plot3-jcs-th0.01.jpeg",path="../analysis/")

df = subset(read.csv("jc-sequence-N10000-L50.csv"), θ==0.1)
plot3 = ggplot(df, aes(x = ρ_a, y = τ_abc-τ_ab, color = P.bc - P.ab)) +
    geom_point() + 
    labs(title="Gradient illustration of anomaly zone under JC-sequence with θ=0.1", x="ρ_a",y="f=τ_abc-τ_ab")
ggsave("plot3-jcs-th0.1.jpeg",path="../analysis/")



## Plot 4 -- Discrete illustrationss of anomaly zone for inference of rooted triplet topology

## parameters same as for plot 3

## (defparameter *τ_ab-values* '(1))
## (defparameter *f-values* '(.01 .02 .03 .04 .05 .06 .07 .08 .09 .1 .11 .12 .13 .14 .15 .16 .17 .18 .19 .2))
## (defparameter *ρ_a-values* '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))
## (defparameter *ρ_b-values* '(0))
## (defparameter *ρ_c-values* '(0))
## (defparameter *ρ_ab-values* '(0))
## (defparameter *ρ_abc-values* '(0)) 
## (defparameter *θ-values* '(.1 .01)) 
## (defparameter *N* 10000) 
## (defparameter *L* 50) 
## (defparameter *τ_max* 999999)

## The following graphs help to illustrate the approximate anomaly zone for
## θ=0.1, where anomaly zone is defined as the set of parameters for which the
## topology ((AB)C) [i.e. matching that of the species tree] is not the most
## likely topology to be exhibited by individual gene trees. In all cases
## observed, the anomaly zone coincides almost entirely with the region in which
## the most likely gene tree topology is (A(BC)). For JCE, exceptions to this
## rule appear to be the result of random noise due to insufficent convergence
## when f is very close to zero. In all cases, the anomaly zone is indicated by
## the color teal.

df = subset(read.csv("jc-expected-N10000-L50.csv"), θ==0.1)
p <- df$P.bc>df$P.ab
q <- df$P.ac>df$P.ab
anomaly_zone <- p | q
plot3 = ggplot(df, aes(x = ρ_a, y = τ_abc-τ_ab, color = anomaly_zone)) +
    geom_point() + 
    labs(title="Approximate anomaly zone under JC-expected for θ=0.1",x="ρ_a",y="f=τ_abc-τ_ab")
ggsave("plot4-jce-th0.1.jpeg",path="../analysis/")

df = subset(read.csv("jc-expected-N10000-L50.csv"), θ==0.01)
p <- df$P.bc>df$P.ab
q <- df$P.ac>df$P.ab
anomaly_zone <- p | q
plot3 = ggplot(df, aes(x = ρ_a, y = τ_abc-τ_ab, color = anomaly_zone)) +
    geom_point() + 
    labs(title="Approximate anomaly zone under JC-expected for θ=0.01",x="ρ_a",y="f=τ_abc-τ_ab")
ggsave("plot4-jce-th0.01.jpeg",path="../analysis/")

## in these two above cases, the anomaly zone is somewhat larger when θ is
## smaller. This appears consistent with analytical results (not included in this
## project report) that I have previously obtained which indicate that in the
## JC-expected case, the difference P[A(BC)]-P[(AB)C] is a decreasing function
## of θ on the interval [0,3/4].

### Anomaly zone for MLS

df = subset(read.csv("ml-sequence-N10000-L50.csv"), θ==0.1)
p <- df$P.bc>df$P.ab
q <- df$P.ac>df$P.ab
anomaly_zone <- p | q
plot3 = ggplot(df, aes(x = ρ_a, y = τ_abc-τ_ab, color = anomaly_zone)) +
    geom_point() + 
    labs(title="Approximate anomaly zone under ML-sequence for θ=0.1",x="ρ_a",y="f=τ_abc-τ_ab")
ggsave("plot4-mls-th0.1.jpeg",path="../analysis/")

df = subset(read.csv("ml-sequence-N10000-L50.csv"), θ==0.01)
p <- df$P.bc>df$P.ab
q <- df$P.ac>df$P.ab
anomaly_zone <- p | q
plot3 = ggplot(df, aes(x = ρ_a, y = τ_abc-τ_ab, color = anomaly_zone)) +
    geom_point() + 
    labs(title="Approximate anomaly zone under ML-sequence for θ=0.01",x="ρ_a",y="f=τ_abc-τ_ab")
ggsave("plot4-mls-th0.01.jpeg",path="../analysis/")

### Anomaly zone for JCS

df = subset(read.csv("jc-sequence-N10000-L50.csv"), θ==0.1)
p <- df$P.bc>df$P.ab
q <- df$P.ac>df$P.ab
anomaly_zone <- p | q
plot3 = ggplot(df, aes(x = ρ_a, y = τ_abc-τ_ab, color = anomaly_zone)) +
    geom_point() + 
    labs(title="Approximate anomaly zone under JC-sequence for θ=0.1",x="ρ_a",y="f=τ_abc-τ_ab")
ggsave("plot4-jcs-th0.1.jpeg",path="../analysis/")

df = subset(read.csv("jc-sequence-N10000-L50.csv"), θ==0.01)
p <- df$P.bc>df$P.ab
q <- df$P.ac>df$P.ab
anomaly_zone <- p | q
plot3 = ggplot(df, aes(x = ρ_a, y = τ_abc-τ_ab, color = anomaly_zone)) +
    geom_point() + 
    labs(title="Approximate anomaly zone under JC-sequence for θ=0.01",x="ρ_a",y="f=τ_abc-τ_ab")
ggsave("plot4-jcs-th0.01.jpeg",path="../analysis/")

## The anomaly zone for JCS is smaller than that for JCE, but appears to follow
## the same qualitative pattern. When the internal branch is short (e.g. less
## than .05 coalescent units), more recombination in population A has the effect
## of producing anomolous gene trees, but this effect is small and completely
## overwhelmed by the effect of increasing the internal branch length.
##
## One difference between this case and that of JCE is that for JCS, the anomaly
## zone appears to decrease in size as we go from θ=0.1 down to θ=0.01. This
## also appears to be the case in the graphs for MLS. However the opposite
## behavior occurs in the JCE case. The most plausible explanation that I have
## for such behavior is that the sample size of the simulation is too low for
## JCS to overcome higher variance in the probability estimates inherent under
## JCS and MLS compared to JCE. This is due to their actually simulating
## sequences with random mutations from the marginal gene trees, whereas JCE
## infers gene tree topologies using expected differences between sequences.
