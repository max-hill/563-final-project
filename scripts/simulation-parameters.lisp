
;; DESCRIPTION: This file sets the global parameters for the ancestral
;; recombination graph simulation executed by the script simulate.sh.
;;
;; Recall that the species tree alwasy has three leaves: A, B, and C and
;; topology ((AB)C). The other model parameters are summarized in the following
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
;; HOW TO SET YOUR OWN PARAMETERS: Modify any of the numeric values in the
;; script below. For example, to change the mutation rate to .01, replace the
;; code '(defparameter *θ* .02)' with '(defparameter *θ* .01)'.


;;______________________________________________________________________________
;;
;; ---Begin script---
;;______________________________________________________________________________

(defparameter *τ_ab* 1) ; age of most recent species divergence
(defparameter *f* .01) ; internal branch length on the species tree: τ_abc-τ_ab.
(defparameter *τ_abc* (+ *τ_ab* *f*)) ; age of oldest divergence time (don't
					; modify this)

(defparameter *τ_max* 999999) ; maximum height of species tree.
;; Note: technically τ_max is the maximum allowable duration of the final
;; population ABC. It's not really a model parameter, but instead an artifact of
;; how I wrote the simulation (since the root population ABC will simulate an
;; ARG until one of the following occur: (1) there exists one lineage or (2)
;; time has exceeded τ_max). Just choose a really big number (we could also
;; replace 999999 with most-positive-long-float but I have to test). As long as
;; it is large enought to ensure all coalesecent events occur long before this
;; time occurs, there is no problem.

(defparameter *possible-ρ-values* '(0 4 8)) ; allowable population-specific
					    ; recombination rates in the
					    ; non-root populations
;; Note: runtime increases exponentially as the length of this list increases
;; because the simulator has all 4 non-root populations (A, B, C, AB) try all
;; possible combinations of these values. The recombination rate in the root
;; population is specified separately to improve computation times (see next
;; line).

(defparameter *possible-root-ρ-values* '(0)) ; recombination rate in
;; the root population. Note that choosing a non-zero values for this tends to
;; significantly increase computing time.

(defparameter *θ* .02) ; mutation rate
(defparameter *N* 50) ; sample size (number of sampled loci)
(defparameter *L* 100) ; locus length (in base pairs)



;; Output some helpful messages to the user
(format t "~%Parameter regime is as follows (this is saved in output file): ~%τ_ab=~a ~%τ_abc=~a ~%τ_max=~a ~%θ=~a ~%N=~a ~%L=~a ~%ρ_a ρ_b ρ_c ρ_ab chosen independently from the set ~a" *τ_ab* *τ_abc* *τ_max* *θ* *N* *L* *possible-ρ-values*)
(format t "~%ρ_abc chosen from the set ~a" *possible-root-ρ-values*)
(format t "~%(Simulation parameters can be modified by modifying the file scripts/simulation-parameters.lisp)")


;;______________________________________________________________________________
;;
;; ---End script---
;;______________________________________________________________________________
