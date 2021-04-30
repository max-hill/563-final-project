#!/usr/bin/env Rscript


# This file is the template for automated plot creation using ggplot2. 


packages <- c("ggplot2")


# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))





## Use these if you want to include arguments for the script
## args=commandArgs(trailingOnly=TRUE)
## print(args[1])


## Make the plot
file_list = list.files(path="../data",
                       pattern="*.csv", full.names=TRUE)

for (file in file_list){
    file_name = basename(file)
    file_name_sans_ext = tools::file_path_sans_ext(file_name)
    output_name=paste(file_name_sans_ext, ".jpeg", sep="")
    df=read.csv(file,header=TRUE)

    plot1 = ggplot(df, aes(x=ρ_a - ρ_b, y= P.bc - P.ab)) +
        geom_point() + 
        labs(x="(recombination rate in population A) - (recomb rate in population B)",y="Pr[A(BC)]-Pr[(AB)C]")
    ggsave(output_name, plot1, path="../analysis/")
    print(paste("graph made for: ",file_name,sep=""))
    print(paste("output file path: ../analysis/",output_name,sep=""))
}



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
    labs(x="recombination rate in population A",y="Prob[A(BC)]-Pr[(AB)C]")
ggsave("plot1-jce.jpeg",path="../analysis/")

## jc-sequence
df = read.csv("jc-sequence-N15000-L500.csv")
plot1jce = ggplot(df, aes(x=ρ_a, y= P.bc - P.ab)) +
    geom_point() + 
    labs(x="recombination rate in population A",y="Pr[A(BC)]-Pr[(AB)C]")
ggsave("plot1-jcs.jpeg",path="../analysis/")

## ml-sequence
df = read.csv("ml-sequence-N15000-L500.csv")
plot1jce = ggplot(df, aes(x=ρ_a, y= P.bc - P.ab)) +
    geom_point() + 
    labs(x="recombination rate in population A",y="Pr[A(BC)]-Pr[(AB)C]")
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


## jc-expected

df = read.csv("jc-expected-N10000-L500.csv")
plot2 = ggplot(df, aes(x=ρ_a + (τ_abc-τ_ab)*ρ_ab/2 , y= P.bc - P.ab , color=τ_abc-τ_ab)) +
    geom_point() + 
    labs(x="ρ_a + (τ_abc-τ_ab)*ρ_ab/2",y="Prob[A(BC)]-Pr[(AB)C]")

ggsave("plot2-jce.jpeg",path="../analysis/")



## jc-sequence
df = read.csv("jc-sequence-N10000-L500.csv")
plot2 = ggplot(df, aes(x=ρ_a + (τ_abc-τ_ab)*ρ_ab/2 , y= P.bc - P.ab , color=τ_abc-τ_ab)) +
    geom_point() + 
    labs(x="ρ_a + (τ_abc-τ_ab)*ρ_ab/2",y="Prob[A(BC)]-Pr[(AB)C]")

ggsave("plot2-jcs.jpeg",path="../analysis/")


## ml-sequence
df = read.csv("ml-sequence-N10000-L500.csv")
plot2 = ggplot(df, aes(x=ρ_a + (τ_abc-τ_ab)*ρ_ab/2 , y= P.bc - P.ab , color=τ_abc-τ_ab)) +
    geom_point() + 
    labs(x="ρ_a + (τ_abc-τ_ab)*ρ_ab/2",y="Prob[A(BC)]-Pr[(AB)C]")

ggsave("plot2-mls.jpeg",path="../analysis/")



## analysis: these plots show a slight positive correlation. Note the division by two is rough heuristic since half the recombination events occuring in population AB will occur along lineages of A and half along lineages of B.
## If time, you should redo this simulation with more values for f, more values for recombination rate in A, no recombination in AB, and only one mutation rate


## Plot 3
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

## ML Sequence
df = subset(read.csv("ml-sequence-N10000-L50.csv"), θ==0.01)
plot3 = ggplot(df, aes(x = ρ_a, y = τ_abc-τ_ab, color = P.bc - P.ab)) +
    geom_point() + 
    labs(x="ρ_a",y="f=τ_abc-τ_ab")
ggsave("plot3-mls-th0.01.jpeg",path="../analysis/")

df = subset(read.csv("ml-sequence-N10000-L50.csv"), θ==0.1)
plot3 = ggplot(df, aes(x = ρ_a, y = τ_abc-τ_ab, color = P.bc - P.ab)) +
    geom_point() + 
    labs(x="ρ_a",y="f=τ_abc-τ_ab")
ggsave("plot3-mls-th0.1.jpeg",path="../analysis/")


### Anomaly zone for MLS

df = subset(read.csv("ml-sequence-N10000-L50.csv"), θ==0.1)
p <- df$P.bc>df$P.ab
q <- df$P.ac>df$P.ab
anomaly_zone <- p | q
plot3 = ggplot(df, aes(x = ρ_a, y = τ_abc-τ_ab, color = anomaly_zone)) +
    geom_point() + 
    labs(title="Approximate anomaly zone for θ=0.1",x="ρ_a",y="f=τ_abc-τ_ab")
ggsave("plot3-mls-th0.1-bool.jpeg",path="../analysis/")


df = subset(read.csv("ml-sequence-N10000-L50.csv"), θ==0.01)
p <- df$P.bc>df$P.ab
q <- df$P.ac>df$P.ab
anomaly_zone <- p | q
plot3 = ggplot(df, aes(x = ρ_a, y = τ_abc-τ_ab, color = anomaly_zone)) +
    geom_point() + 
    labs(title="Approximate anomaly zone for θ=0.01",x="ρ_a",y="f=τ_abc-τ_ab")
ggsave("plot3-mls-th0.01-bool.jpeg",path="../analysis/")



## JC Sequence
df = read.csv("jc-sequence-N10000-L50.csv")

df = subset(subset(df, θ==0.01))
plot3 = ggplot(df, aes(x = ρ_a, y = τ_abc-τ_ab, color = P.bc - P.ab)) +
    geom_point() + 
    labs(x="ρ_a",y="f=τ_abc-τ_ab")
ggsave("plot3-jcs-th0.01.jpeg",path="../analysis/")

df = subset(subset(df, θ==0.1))
plot3 = ggplot(df, aes(x = ρ_a, y = τ_abc-τ_ab, color = P.bc - P.ab)) +
    geom_point() + 
    labs(x="ρ_a",y="f=τ_abc-τ_ab")
ggsave("plot3-jcs-th0.1.jpeg",path="../analysis/")



### Anomaly zone for JCS

df = subset(read.csv("jc-sequence-N10000-L50.csv"), θ==0.1)
p <- df$P.bc>df$P.ab
q <- df$P.ac>df$P.ab
anomaly_zone <- p | q
plot3 = ggplot(df, aes(x = ρ_a, y = τ_abc-τ_ab, color = anomaly_zone)) +
    geom_point() + 
    labs(title="Approximate anomaly zone for θ=0.1",x="ρ_a",y="f=τ_abc-τ_ab")
ggsave("plot3-jcs-th0.1-bool.jpeg",path="../analysis/")


df = subset(read.csv("jc-sequence-N10000-L50.csv"), θ==0.01)
p <- df$P.bc>df$P.ab
q <- df$P.ac>df$P.ab
anomaly_zone <- p | q
plot3 = ggplot(df, aes(x = ρ_a, y = τ_abc-τ_ab, color = anomaly_zone)) +
    geom_point() + 
    labs(title="Approximate anomaly zone for θ=0.01",x="ρ_a",y="f=τ_abc-τ_ab")
ggsave("plot3-jcs-th0.01-bool.jpeg",path="../analysis/")

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



## JC expected
df = read.csv("jc-expected-N10000-L50.csv")

df01 = subset(subset(df, θ==0.01))
plot3 = ggplot(df01, aes(x = ρ_a, y = τ_abc-τ_ab, color = P.bc - P.ab)) +
    geom_point() + 
    labs(x="ρ_a",y="f=τ_abc-τ_ab")
ggsave("plot3-jce-th0.01.jpeg",path="../analysis/")

df1 = subset(subset(df, θ==0.1))
plot3 = ggplot(df1, aes(x = ρ_a, y = τ_abc-τ_ab, color = P.bc - P.ab)) +
    geom_point() + 
    labs(x="ρ_a",y="f=τ_abc-τ_ab")
ggsave("plot3-jce-th0.1.jpeg",path="../analysis/")

# We make six plots, should be albe to see the gradient very clearly which indicates the nomaly zone.


x = subset(df, θ==0.01)
x = subset(df, θ==0.001)

## I would like to try this with various values of theta, as I know the effect decays as theta -> 3/4



## anomaly zone

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
    labs(title="Approximate anomaly zone for θ=0.1",x="ρ_a",y="f=τ_abc-τ_ab")
ggsave("plot3-jce-th0.1-bool.jpeg",path="../analysis/")


df = subset(read.csv("jc-expected-N10000-L50.csv"), θ==0.01)
p <- df$P.bc>df$P.ab
q <- df$P.ac>df$P.ab
anomaly_zone <- p | q
plot3 = ggplot(df, aes(x = ρ_a, y = τ_abc-τ_ab, color = anomaly_zone)) +
    geom_point() + 
    labs(title="Approximate anomaly zone for θ=0.01",x="ρ_a",y="f=τ_abc-τ_ab")
ggsave("plot3-jce-th0.01-bool.jpeg",path="../analysis/")

## in these two above cases, the anomaly zone is somewhat larger when θ is
## smaller. This appears consistent with analytical results (not included in this
## project report) that I have previously obtained which indicate that in the
## JC-expected case, the difference P[A(BC)]-P[(AB)C] is a decreasing function
## of θ on the interval [0,3/4].
