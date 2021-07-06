;; simulation-parameters.lisp --- Customizable document which specifies the
;; range of paramter regimes under which simulations are run by simulate.sh.
;;
;; Author: max hill
;; (Last updated 2021-04-29)
;;
;; DESCRIPTION: This file sets the global parameters for the ancestral
;; recombination graph simulation executed by the script simulate.sh.
;; In particular, when run using simulate.sh, the simulation will simulate N
;; ancestral recombination graphs (each corresponding to a locus consisting of L
;; base pairs) for each possible configuration of the other parameters.
;;
;; Recall that the species tree always has three taxa (ie leaves): A, B, and C
;; and topology ((AB)C). The model parameters are summarized in the following
;; table:
;;
;; | Symbol | Description or definition                       |
;; |--------+-------------------------------------------------|
;; | τ_ab   | divergence time of species A and B              |
;; | τ_abc  | divergence time of species AB and C             |
;; | f      | internal branch length. f= τ_abc-τ_ab           |
;; | τ_max  | maximum height of the tree (pick large)         |
;; | ρ_a    | recombination rate in population A              |
;; | ρ_b    | recombination rate in population B              |
;; | ρ_c    | recombination rate in population C              |
;; | ρ_ab   | recombination rate in population AB             |
;; | ρ_abc  | recombination rate in population ABC            |
;; | θ      | mutation rate per site per coalescent unit      |
;; | N      | number of sampled loci (ie # of ARGS simulated) |
;; | L      | length of each locus in base pairs              |
;;
;;
;; HOW TO SET YOUR OWN PARAMETERS: To customize N, change the number in the line
;; containing (defparameter *N* 100) to a different number. Similarly, to
;; customize L, change the number in the line with (defparameter *L* 500) to a
;; different number.
;;
;; To specify the other parameters, simply modify their corresponding lists of
;; numeric values below. For example, to instruct the simulation to use mutation
;; rates .01, .02, and .05, we would replace the code
;; (defparameter *θ-values* '(.02))
;; by the code (defparameter *θ-values* '(.01 .02 .05))
;;
;; Note that the list entries must be separated by spaces, not commas. 

;;______________________________________________________________________________
;;
;; ---Begin script---
;;______________________________________________________________________________

(defparameter *τ_ab-values* '(1)) ; age of most recent species divergence

(defparameter *f-values* '(.01))
				 ; f is the internal branch length on the
				 ; species tree: f=τ_abc-τ_ab.

;; The following parameters define population-specific recombination rates.
;; There are 5 populations, corresponding to edges on the species tree (A, B, C,
;; AB) as well as the root of the species tree (ABC). The simulator will
;; consider all possible combinations of the listed values.
(defparameter *ρ_a-values* '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))
(defparameter *ρ_b-values* '(0))
(defparameter *ρ_c-values* '(0))
(defparameter *ρ_ab-values* '(0))
(defparameter *ρ_abc-values* '(0))  ; Note that choosing a non-zero values for
				    ; ρ_abc can significantly increase computing
				    ; time.

(defparameter *θ-values* '(.01)) ; mutation rate

(defparameter *N* 100000) ; sample size (number of sampled loci)
(defparameter *L* 500) ; locus length (in base pairs)


(defparameter *τ_max* 999999) ; maximum height of species tree.
;; Note: technically τ_max is the maximum allowable duration of the final
;; population ABC. It's not really a model parameter, but instead an artifact of
;; how I wrote the simulation (since the root population ABC will simulate an
;; ARG until one of the following occur: (1) there exists one lineage or (2)
;; time has exceeded τ_max). Just choose a really big number (we could also
;; replace 999999 with most-positive-long-float but I have to test). As long as
;; it is large enought to ensure all coalesecent events occur long before this
;; time occurs, there is no problem.

;; Output some helpful messages to the user
(format t "~%Parameter regime is as follows (this is saved in output file): 
~%τ_ab ∈ ~a ~%f ∈ ~a ~%τ_max=~a ~%θ ∈ ~a ~%N=~a ~%L=~a ~%ρ_a ∈ ~a ~%ρ_b ∈ ~a ~%ρ_c ∈ ~a ~%ρ_ab ∈ ~a ~%ρ_abc ∈ ~a"
	*τ_ab-values*	*f-values*	*τ_max* 	*θ-values*	*N*	*L*	*ρ_a-values*	*ρ_b-values*
	*ρ_c-values*	*ρ_ab-values*	*ρ_abc-values*)

(format t "~%(Simulation parameters can be modified by modifying the file scripts/simulation-parameters.lisp)")


;;______________________________________________________________________________
;;
;; ---End script---
;;______________________________________________________________________________
