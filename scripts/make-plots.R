#!/usr/bin/env Rscript
##
## make-plots.R --- use the R package ggplot2 to create plots from simulated data
##
## Author: Max Hill
## (Last updated 2021-04-30)





##______________________________________________________________________________
##
## Setup -- Install and load necessary R packages.
##______________________________________________________________________________


packages <- c("ggplot2")

installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages]) # Install packages
}

invisible(lapply(packages, library, character.only = TRUE)) # Packages loading


setwd('../data/') # Set working directory to 'data/' 





##______________________________________________________________________________
##
## Part 1 -- Effect of recombination when all other variables are fixed and
##           internal branch length is small.
##______________________________________________________________________________

## Simulation parameters:
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


##______________________________________________________________________________
##
## Part 2 -- Relative effects of internal branch length and recombination rate
##           on correct topological inference for gene triplets.
##______________________________________________________________________________

## Simulation parameters:
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


##______________________________________________________________________________
##
## Part 3 -- Identifying the anomaly zones with gradient illustrations.
##______________________________________________________________________________

## Simulation parameters:
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

## ML-sequence
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

## JC-expected
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

## JC-sequence
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


##______________________________________________________________________________
##
## Part 4 -- Discrete illustrationss of anomaly zone for inference of rooted
##           triplet topology.
##______________________________________________________________________________

## parameters same as for plot 3

# JC-expected
df = subset(read.csv("jc-expected-N10000-L50.csv"), θ==0.1)
p <- df$P.bc>df$P.ab
q <- df$P.ac>df$P.ab
anomaly_zone <- p | q # the vertical bar | is a vectorized logical 'or'
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

### ML-sequence
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

### JC-sequence
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



##______________________________________________________________________________
##
## Part 5 -- Discrete illustrationss of anomaly zone for inference of rooted
##           triplet topology.
##______________________________________________________________________________

## parameters same as for plot 3,4, but with L=500 rather than L=50
## made with datafiles in folder 2020-05-14

# JC expected color test
df = subset(read.csv("jc-expected-N10000-L500.csv"), θ==0.1)
p <- df$P.bc>df$P.ab
q <- df$P.ac>df$P.ab
anomaly_zone <- p | q # the vertical bar | is a vectorized logical 'or'
plot3 = ggplot(df, aes(x = ρ_a, y = τ_abc-τ_ab, color = anomaly_zone, size = P.bc - P.ab)) +
    geom_point() + 
    labs(title="Approximate anomaly zone under JC-expected for θ=0.1,L=500",x="ρ_a",y="f=τ_abc-τ_ab")
ggsave("color-test-plot5-jce-th0.1.jpeg",path="../analysis/")

# JC-expected
df = subset(read.csv("jc-expected-N10000-L500.csv"), θ==0.1)
p <- df$P.bc>df$P.ab
q <- df$P.ac>df$P.ab
anomaly_zone <- p | q # the vertical bar | is a vectorized logical 'or'
plot3 = ggplot(df, aes(x = ρ_a, y = τ_abc-τ_ab, color = anomaly_zone)) +
    geom_point() + 
    labs(title="Approximate anomaly zone under JC-expected for θ=0.1,L=500",x="ρ_a",y="f=τ_abc-τ_ab")
ggsave("plot5-jce-th0.1.jpeg",path="../analysis/")

df = subset(read.csv("jc-expected-N10000-L500.csv"), θ==0.01)
p <- df$P.bc>df$P.ab
q <- df$P.ac>df$P.ab
anomaly_zone <- p | q
plot3 = ggplot(df, aes(x = ρ_a, y = τ_abc-τ_ab, color = anomaly_zone)) +
    geom_point() + 
    labs(title="Approximate anomaly zone under JC-expected for θ=0.01,L=500",x="ρ_a",y="f=τ_abc-τ_ab")
ggsave("plot5-jce-th0.01.jpeg",path="../analysis/")

### ML-sequence
df = subset(read.csv("ml-sequence-N10000-L500.csv"), θ==0.1)
p <- df$P.bc>df$P.ab
q <- df$P.ac>df$P.ab
anomaly_zone <- p | q
plot3 = ggplot(df, aes(x = ρ_a, y = τ_abc-τ_ab, color = anomaly_zone)) +
    geom_point() + 
    labs(title="Approximate anomaly zone under ML-sequence for θ=0.1,L=500",x="ρ_a",y="f=τ_abc-τ_ab")
ggsave("plot5-mls-th0.1.jpeg",path="../analysis/")

df = subset(read.csv("ml-sequence-N10000-L500.csv"), θ==0.01)
p <- df$P.bc>df$P.ab
q <- df$P.ac>df$P.ab
anomaly_zone <- p | q
plot3 = ggplot(df, aes(x = ρ_a, y = τ_abc-τ_ab, color = anomaly_zone)) +
    geom_point() + 
    labs(title="Approximate anomaly zone under ML-sequence for θ=0.01,L=500",x="ρ_a",y="f=τ_abc-τ_ab")
ggsave("plot5-mls-th0.01.jpeg",path="../analysis/")

### JC-sequence
df = subset(read.csv("jc-sequence-N10000-L500.csv"), θ==0.1)
p <- df$P.bc>df$P.ab
q <- df$P.ac>df$P.ab
anomaly_zone <- p | q 
plot3 = ggplot(df, aes(x = ρ_a, y = τ_abc-τ_ab, color = anomaly_zone)) +
    geom_point() + 
    labs(title="Approximate anomaly zone under JC-sequence for θ=0.1,L=500",x="ρ_a",y="f=τ_abc-τ_ab")
ggsave("plot5-jcs-th0.1.jpeg",path="../analysis/")

df = subset(read.csv("jc-sequence-N10000-L500.csv"), θ==0.01)
p <- df$P.bc>df$P.ab
q <- df$P.ac>df$P.ab
anomaly_zone <- p | q
plot3 = ggplot(df, aes(x = ρ_a, y = τ_abc-τ_ab, color = anomaly_zone)) +
    geom_point() + 
    labs(title="Approximate anomaly zone under JC-sequence for θ=0.01,L=500",x="ρ_a",y="f=τ_abc-τ_ab")
ggsave("plot5-jcs-th0.01.jpeg",path="../analysis/")




##______________________________________________________________________________
##
## Part 6 -- Effect of recombination when all other variables are fixed and
##          internal branch length is small and τ_ab varies
##______________________________________________________________________________

## Simulation parameters:
## (defparameter *τ_ab-values* '(.1 .5 1 2))
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

## jc-expected
df = read.csv("jc-expected-N15000-L500.csv")
plot1 = ggplot(df, aes(x=ρ_a, y= P.bc - P.ab)) +
    geom_point() + 
    labs(title="Recombination effect on most likely inferred gene topology under JC-expected", x="recombination rate in population A",y="Pr[A(BC)]-Pr[(AB)C]")
ggsave("plot6-jce.jpeg",path="../analysis/")

## jc-sequence
df = read.csv("jc-sequence-N15000-L500.csv")
plot1jce = ggplot(df, aes(x=ρ_a, y= P.bc - P.ab)) +
    geom_point() + 
    labs(title="Recombination effect on most likely inferred gene topology under JC-sequence",x="recombination rate in population A",y="Pr[A(BC)]-Pr[(AB)C]")
ggsave("plot6-jcs.jpeg",path="../analysis/")

## ml-sequence
df = read.csv("ml-sequence-N15000-L500.csv")
plot1jce = ggplot(df, aes(x=ρ_a, y= P.bc - P.ab)) +
    geom_point() + 
    labs(title="Recombination effect on most likely inferred gene topology under ML-sequence",x="recombination rate in population A",y="Pr[A(BC)]-Pr[(AB)C]")
ggsave("plot6-mls.jpeg",path="../analysis/")

