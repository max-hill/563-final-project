;; This file sets the global parameters for the ancestral recombination graph
;; simulation.

(defparameter *τ_ab* 1) ; age of most recent species divergence
(defparameter *f* .01) ; internal branch length: τ_abc-τ_ab.
(defparameter *τ_abc* (+ *τ_ab* *f*)) ; age of oldest divergence time
(defparameter *τ_max* 999999) ; maximum height of species tree
(defparameter *θ* .02) ; mutation parameter
(defparameter *possible-ρ-values* '(0 4 8))
(defparameter *N* 10); sample size (number of sampled loci)
(defparameter *L* 500) ; locus length (in base pairs)



;; Note: Choose τ_max large enough to ensure all coalescent events occur long before
;; this time occurs (we could just choose this to be most-positive-long-float).

;; | Column Number | Symbol | Description or definition                       |
;; |---------------+--------+-------------------------------------------------|
;; |             1 | P-ab   | estimated probability of inferring ((AB)C)      |
;; |             2 | P-ac   | estimated probability of inferring ((AC)B)      |
;; |             3 | P-bc   | estimated probability of inferring ((BC)A)      |
;; |             4 | τ_ab   | divergence time of species A and B              |
;; |             5 | τ_abc  | divergence time of species AB and C             |
;; |             6 | τ_max  | maximum height of the tree (pick large)         |
;; |             7 | ρ_a    | recombination rate in population A              |
;; |             8 | ρ_b    | recombination rate in population B              |
;; |             9 | ρ_c    | recombination rate in population C              |
;; |            10 | ρ_ab   | recombination rate in population AB             |
;; |            11 | ρ_abc  | recombination rate in population ABC            |
;; |            12 | θ      | mutation rate per site per coalescent unit      |
;; |            13 | N      | number of sampled loci (ie # of ARGS simulated) |
;; |            14 | L      | length of each locus in base pairs              |
