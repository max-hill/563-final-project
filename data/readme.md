# Data Readme
	(Last updated: 2021-04-30)

Datafiles have been included because (1) they are reasonably small and (2) they
may take a signifiant amount of time (hours to days depending on your hardware)
to reproduce.


## Datafile Names

Datafiles are automatically named by the simulation. The name consist of three parts:

* the simulation mode used to generate them (i.e. ml-sequence, jc-expected, or
  jc-sequence);

* a number N, representing the number of ancestral recombination graphs
  simulated per parameter regime (i.e. per row of the datafile);

* a number L, representing the number of sites per locus represented by an
  ancestral recombination graph.

If you run a simulation with the same mode, N, and L as a previous simulation,
the existing datafile will not be overwritten. Instead, the simulation output
will be *appended* to the existing datafile. This allows large scale (i.e.
time-consuming) simulations to be broken up into chunks (of rows). 

## Datafile Structure

Datafiles are .csv files. Each row corresponds to the res
ult from a simulated
parameter regime. The parameter regime is specied in the row itself. The csv
will have 14 columns, indicated as in the following table:



```
| Column | Symbol | Definition                                                           |
|--------+--------+----------------------------------------------------------------------|
|      1 | P-ab   | estimated probability of inferring ((AB)C) as the gene tree topology |
|      2 | P-ac   | estimated probability of inferring ((AC)B) as the gene tree topology |
|      3 | P-bc   | estimated probability of inferring ((BC)A) as the gene tree topology |
|      4 | τ_ab   | divergence time of species A and B                                   |
|      5 | τ_abc  | divergence time of species AB and C                                  |
|      6 | τ_max  | maximum height of the tree (pick large)                              |
|      7 | ρ_a    | recombination rate in population A                                   |
|      8 | ρ_b    | recombination rate in population B                                   |
|      9 | ρ_c    | recombination rate in population C                                   |
|     10 | ρ_ab   | recombination rate in population AB                                  |
|     11 | ρ_abc  | recombination rate in population ABC                                 |
|     12 | θ      | mutation rate per site per coalescent unit                           |
|     13 | N      | number of sampled loci (ie # of ARGS simulated)                      |
|     14 | L      | length of each locus in base pairs                                   |
```
