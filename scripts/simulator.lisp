;;;; simulator.lisp --- Lisp functions to simulate multispecies coalescent with
;;                      recombination and attempt to infer species trees from it
;; 
;; Author: max hill 
;; (Last updated 2021-04-28)

;; DESCRIPTION: This file contains all general functions for simulating the
;; multispecies coalescent with recombination on a species triplet with topology
;; ((AB)C). It also contains the functions necessary for inference, including:
;;   - R* consensus method using weighted Jukes-Cantor distances;
;;   - R* consensus method using mutations generated by a Jukes-Cantor process;
;;   - A summary method which infers the maximum likelihood tree.
;; For reproducibility, we will load, compile, and execute these functions using
;; shell scripts.
;;
;; You can also evaluate the functions individually using a REPL (I like sbcl
;; with emacs+slime). In that case, to load all the functions in this file, open
;; the REPL and run the command (load "/home/.../scripts/simulator.lisp")
;; where '...' is replaced by the appropriate directory. You might get some
;; output about undefined functions and style warnings, but don't worry about
;; any of that. You can now run commands like (estimate-topology-probabilities 1
;; 1.1 100 5 0 0 0 0 .2 10000 2)



;;______________________________________________________________________________
;;
;; --- GENERAL AUXILLARY FUNCTIONS ---
;;______________________________________________________________________________

(defun convert-to-cdf (π)
  "Converts a distribution vector into a cumulative distribution vector.
Example: (convert-pdf-to-cdf '(.23 .25 .26 .26))"
  (cdf-aux π nil))

(defun cdf-aux (π cdf-vector)
  "Auxillary function to convert-to-cdf"
  (if (null π)
      cdf-vector
      (cdf-aux (butlast π)
	       (cons (reduce #'+ π) cdf-vector))))

(defun draw-exponential (λ)
  "Return a number drawn according to a rate λ exponential random variable.
Based on code from https://github.com/tpapp/cl-random. If λ=0, return positive
infinity, represented by most-positive-long-float (1.7976931348623157d308)."
  (if (zerop λ)
      most-positive-long-float
      (- (/ (log (- 1 (random 1d0))) λ))))

(defun interval (a b)
  (loop for i from a to b collecting i))

;; (defun intervalnn (a b &optional (x nil))
;;   "Attempt to optimize #'interval by declaring types. No improvement observed."
;;   (declare (optimize (speed 3) (safety 0)))
;;   (declare (type integer a b))
;;   (if (= a b)
;;       (cons b x)
;;       (intervalnn (1+ a) b (cons a x))))

(defun randomly-choose (x &optional (n 1) (with-replacement nil))
  "Randomly choose n elements from a list x without (or with) replacement. If
the number n is unspecified (or n=1), output a single randomly-chosen element.
If n>1, output a *list* of elements. By default, the selection of multiple
elements is done *without* replacement. Selection *with* replacement can be
specified by setting the optional variable 'with-replacement' to a non-nil
value."
  (cond ((null x) nil)
	((= n 1) (nth (random (length x)) x))
	(t (randomly-choose-several x n with-replacement))))

(defun randomly-choose-several (x &optional (n 1) (with-replacement nil))
  "This is an auxillary function to the function randomly-choose. It outputs a
*list* of n elements chosen randomly without (or with) replacement from the list
x. It can also be run standalone. The only difference in output compared to
randomly-choose occurs when n=1, in which case randomly-choose outputs only the
randomly-chosen element, whereas randomly-choose-several outputs a list of
length one containing the element. I don't know if that will ever be of
interest, but I record it here just in case."
  (cond ((null x) nil)
        ((<= n 0) nil)
	(with-replacement
	    (cons (randomly-choose x)
		  (randomly-choose-several x (1- n) with-replacement)))
	((>= n (length x)) x)
        (t (let ((element (randomly-choose x)))
	     (cons element
		   (randomly-choose-several
		    (remove element x :count 1)
		    (1- n)))))))

(defun remove-elements (elements-to-remove initial-set)
  "Code based on function found at
http://www-users.cselabs.umn.edu/classes/Spring-2018/csci4511/lisp/mapping.html"
  (remove-if
   #'(lambda (element) (member element elements-to-remove :test #'equal))
   initial-set))


;;______________________________________________________________________________
;;
;; --- FUNCTIONS FOR COMPUTING JUKES-CANTOR DISTANCE ---
;;______________________________________________________________________________

(defun common-coordinate (species1 species2 edge position)
  "test whether the genetic material contained at a specified position on an
edge is an ancestor to the sampled individual from species1 and species2"
  (and (member position (nth species1 edge))
       (member position (nth species2 edge))))
;; COMMENTARY: The inputs 'species1' and 'species2' take values 1,2,3,
;; corresponding to species A,B,C respectively. To compute all distances, we
;; will need to loop over all pairs (species1,species2) where species1 and
;; species2 are not equal
;;
;; Example code: (common-coordinate 1 3 '(0 (1 2 3) NIL (2 3)) 2) would return
;; true, since 2 is contained in both (1 2 3) and (2 3).

(defun compute-marginal-tmrcas (species1 species2 output-edges number-of-base-pairs)
  "Compute a list of positive numbers such that the kth entry is the time until
most recent common ancestor of species1 and species2 in the marginal gene tree
corresponding to the kth base pair (or position) on the locus. Note that
species1 and species2 take values 1 2 or 3, corresponding to species A B C. Note
that for a given marginal gene tree, if the ancestors of species1 and species2
did not coalese during the simulation, this function returns a t_mrca of ZERO
for those two species. This may be a problem. One workaround is to choose τ_max
so large that full coalescence must occur below the top of the tree."
  (let* ((list-of-output-edges (union (first output-edges) (second output-edges))))
    (loop for position from 1 to number-of-base-pairs
	  collect (loop for x in list-of-output-edges
			if (common-coordinate species1 species2 x position)
			  minimizing (first x)))))

(defun compute-weighted-jc-distance (species1 species2 output-edges θ number-of-base-pairs)
  "Compute the weighted Jukes-Cantor distance between species1 and species2
given mutation parameter θ"
  (let ((list-of-t_mrcas (compute-marginal-tmrcas species1 species2 output-edges number-of-base-pairs)))
    (flet ((compute-integrand (t_mrca θ)
	     (- 1 (exp (* -1 (/ (* 8 θ t_mrca) 3))))))
    (* (/ 3 (* 4 number-of-base-pairs))
       (loop for t_mrca in list-of-t_mrcas
	     summing (compute-integrand t_mrca θ))))))


(defun jc-expected-estimate-topology-probabilities (τ_ab τ_abc τ_max ρ_a ρ_b ρ_c ρ_ab ρ_abc θ number-of-samples number-of-base-pairs)
    "For each sample, construct an ancestral recombination graph on three
species and use it to compute the weighted JC distances d-ab, d-ac, d-bc. Then
count the number of samples for which each of the topologies (AB)C, (AC)B,
and (BC)A are inferred. Dividing by the total number of samples gives an
estimate for the probability of inferring each species tree (with R*/STAR/MDC
consensus method). The output takes the form of a 14 comma-separated float
numbers: the fraction of samples which estimate the topology as ((AB)C), 
((AC)B), and ((BC)A) respectively, in that order, followed by the 11 original
inputs to this function. The output is written to standard output (for shell
integration)."
  (let* ((counts
	   (loop for i from 1 to number-of-samples
		 for output-edges =  (simulate-three-species
				      τ_ab τ_abc τ_max ρ_a ρ_b ρ_c ρ_ab ρ_abc number-of-base-pairs)
		 for d-ab = (compute-weighted-jc-distance 1 2 output-edges θ number-of-base-pairs)
		 for d-ac = (compute-weighted-jc-distance 1 3 output-edges θ number-of-base-pairs)
		 for d-bc = (compute-weighted-jc-distance 2 3 output-edges θ number-of-base-pairs)
		 counting (and (< d-ab d-ac) (< d-ab d-bc)) into ab-count
		 counting (and (< d-ac d-ab) (< d-ac d-bc)) into ac-count
		 counting (and (< d-bc d-ab) (< d-bc d-ac)) into bc-count
		 finally (return (list ab-count ac-count bc-count)))))
    (format nil "~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a"
	    (float (/ (first counts) number-of-samples))
	    (float (/ (second counts) number-of-samples))
	    (float (/ (third counts) number-of-samples))
	    τ_ab τ_abc τ_max ρ_a ρ_b ρ_c ρ_ab ρ_abc θ number-of-samples number-of-base-pairs)))
    



;;______________________________________________________________________________
;;
;;  --- FUNCTIONS FOR GENERATING ANCESTRAL RECOMBINATION GRAPH ---
;;______________________________________________________________________________

(defun simulate-three-species (τ_ab τ_abc τ_max ρ_a ρ_b ρ_c ρ_ab ρ_abc number-of-base-pairs)
  "Construct an ARG on a 3-taxa species tree with given parameters"
  (let* ((sample-A (cons (cons `(0 ,(interval 1 number-of-base-pairs) nil nil) nil) nil))
	 (sample-B (cons (cons `(0 nil ,(interval 1 number-of-base-pairs) nil) nil) nil))
	 (sample-C (cons (cons `(0 nil nil ,(interval 1 number-of-base-pairs)) nil) nil))
	 (edges-from-A (arg-builder ρ_a 0 τ_ab number-of-base-pairs sample-A))
	 (edges-from-B (arg-builder ρ_b 0 τ_ab number-of-base-pairs sample-B))
	 (edges-from-C (arg-builder ρ_c 0 τ_abc number-of-base-pairs sample-C))
	 (edges-from-AB (arg-builder ρ_ab τ_ab τ_abc number-of-base-pairs
				     (list (union (first edges-from-A)
						  (first edges-from-B))
					   (union (second edges-from-A)
						  (second edges-from-B))))))
    (arg-builder ρ_abc τ_abc τ_max number-of-base-pairs
		 (list (union (first edges-from-AB)
			      (first edges-from-C))
		       (union (second edges-from-AB)
			      (second edges-from-C)))
		 t)))

(defun arg-builder (ρ t_0 t_end number-of-base-pairs edge-sets &optional (stop-at-mrca nil))
  "Build an ancestral recombination graph in a single population."
  (let* ((k (length (first edge-sets)))
	 (coales-rate (* .5 k (- k 1)))
	 (recomb-rate (* .5 k ρ))
	 (total-rate (+ coales-rate recomb-rate))
	 (t_1 (+ t_0 (draw-exponential total-rate))))
    (if (or (> t_1 t_end) (and stop-at-mrca (= k 1)))
	edge-sets
	(arg-builder ρ t_1 t_end number-of-base-pairs
		     (if (< (random 1d0) (/ recomb-rate total-rate))
			 (implement-recombination t_1 edge-sets number-of-base-pairs)
			 (implement-coalescence t_1 edge-sets))
		     stop-at-mrca))))

;; COMMENTARY: The input ρ is the recombination parameter. The input t_0 and
;; t_end are the starting and ending times of the population. The optional
;; variable 'stop-at-mrca' determines whether the process continues simulating
;; events in the event that only a single ancestor remains. If this variable is
;; set to non-nil result, then the process will stop. If set to a nil result
;; (the default) there remains a chance that the single remaining ancestor
;; undergoes a recombination. In general, you should set this to a non-nil value
;; when simulating the root population of the species tree, and leave it nil
;; otherwise.
;;
;; The input variable 'edge-sets' is a two-element list of the form (P,Q) where
;; P is the set of edges which are possible candidates for recombination and
;; coalescence at the present time, and Q is the set of edges which are not, but
;; which need to be saved for later analysis of the genealogy.
;;
;; In particular, elements of P and Q are 4-tuples of the form (t,I_A,I_B,I_C)
;; where t is the starting age of the edge, and I_X is a subset of [n] where n
;; is a specified number of bases on the locus (e.g. n=1000 or something tbd).
;; (It is perhaps more correct to think of elements of P and Q as vertices on
;; the ARG rather than edges.)
;;
;; The variable t_1 is the time at which the next event (either recombination or
;; coalescence) is scheduled to occur; if this is scheduled to occur after the
;; end of the population (i.e. if t_1>t_end), then the event is not implemented
;; and the simulation is over. Also note that if total-rate=0 then
;; draw-exponential returns positive infinity (represented by
;; most-positive-long-float), in which case t_1>t_end (for any finite value of
;; t_end), which ends the simulation.


(defun implement-recombination (time edge-sets number-of-base-pairs)
  "Updates the edge-sets (p,q) appropriately for when a recombination occurs at
the given time."
  (let* ((recombination-child (randomly-choose (first edge-sets)))
	 (breakpoint (+ 1 (random (- number-of-base-pairs 1))))
	 (recombination-parents (make-recombination-parents time recombination-child breakpoint))
	 (new-p (cons (first recombination-parents)
		      (cons (second recombination-parents)
			    (remove-elements
			     (cons recombination-child nil)
			     (first edge-sets))))))
    (progn
;      (format t "~%~%RECOMBINATION at time ~a~%One child edge removed: ~a~%Two parent edges added: ~a~%                        ~a"
;	      time recombination-child (first recombination-parents) (second recombination-parents))
      (list new-p (second edge-sets)))))


(defun make-recombination-parents (time edge breakpoint)
  "Creates both parents of a specified recombining child edge. Outputs a list
containing the left and right parent, where the left (right) parent contains
only genetic labels less than or equal to (greater than) the breakpoint. Working
example: (make-recombination-parents .3 `(.1 ,(interval 1 7) ,(interval 3 6)
,(interval 2 10)) 10)"
  (let* ((paired-list
	   (mapcar
	    #'(lambda (label-set)
		(loop for item in label-set
		      if (<= item breakpoint) collect item into left-part
			else collect item into right-part
		      finally (return (list left-part right-part))))
	    (rest edge))))
    (list
     (cons time (mapcar #'first paired-list))
     (cons time (mapcar #'second paired-list)))))

(defun make-coalescent-parent (time coalescing-pair)
  "Creates parent edge of two coalescing edges. The input coalescing-edges is of
the form (x y) where x and y are the edges"
  (let ((edge1 (first coalescing-pair))
	(edge2 (second coalescing-pair)))
    (list time
	  (union (second edge1) (second edge2))
	  (union (third edge1) (third edge2))
	  (union (fourth edge1) (fourth edge2)))))


(defun implement-coalescence (time edge-sets)
  "Updates the edge-sets (p,q) appropriately for when a recombination occurs at
the given time."
  (let* ((coalescing-pair (randomly-choose (first edge-sets) 2))
	 (coalescent-parent (make-coalescent-parent time coalescing-pair))
	 (new-p (remove-elements coalescing-pair (cons coalescent-parent (first edge-sets))))
	 (new-q (cons (first coalescing-pair) (cons (second coalescing-pair) (second edge-sets)))))
    (progn
;      (format t "~%~%COALESCENCE at time ~a~%Two child edges removed: ~a~%                         ~a~%One parent edge created: ~a"
;	      time (first coalescing-pair) (second coalescing-pair) coalescent-parent)
      (list new-p new-q))))


;;______________________________________________________________________________
;;
;; --- Maximum Likelihood method (from Yang 2000) ---
;;______________________________________________________________________________

;; This section implements a maximum-likelihood consensus tree method for
;; inferring the species tree (as opposed to estimating the species tree using
;; weighted JC distances like in the previous section). Similarly to previous
;; section, for every sampled locus, we generate an ancestral recombination
;; graph and compute pairwise distances between species for each site using
;; marginal gene trees. What is different is that now instead of computing
;; weighted JC distances, for each locus we implement a mutational process to
;; generate binary 'gene' sequences for every species. Then, following Table 4
;; in Yang 2000, we infer the ML tree topology based on the site pattern
;; frequencies (i.e. the frequency of observing a muational pattern xxx, xxy,
;; yxx, or xyx across species A, B and C). Then, having done this process for
;; all of our sampled loci, we choose the most commonly-occurring ML tree
;; topology as our 'inferred species tree topology'.
;;
;; As usual, our species tree has three species. To run a simulation for a given
;; parameter regime, input use function #'ml-estimate-topology-probabilities.
;; Working examples:
;; (ml-estimate-topology-probabilities 1 1.01 10000 10 0 0 0 0 .2 10000 100)
;; or
;; (time (ml-estimate-topology-probabilities 1 1.01 999999 10 0 0 0 0 .1 100000 100))

(defun list-all-pairwise-marginal-distances (τ_ab τ_abc τ_max ρ_a ρ_b ρ_c ρ_ab ρ_abc number-of-base-pairs)
  "Simulate a tree and return a list of triplets, each corresponding to a base
pair on the locus. Each triplet is of the form (t_ab,t_ab,t_bc) where t_xy is
the time of mrca of species x and y in marginal gene tree corresponding to the
base pair."
  (let* ((output-edges (simulate-three-species τ_ab τ_abc τ_max ρ_a ρ_b ρ_c ρ_ab ρ_abc number-of-base-pairs))
	 (t_ab-list (compute-marginal-tmrcas 1 2 output-edges number-of-base-pairs))
	 (t_ac-list (compute-marginal-tmrcas 1 3 output-edges number-of-base-pairs))
	 (t_bc-list (compute-marginal-tmrcas 2 3 output-edges number-of-base-pairs)))
    (loop for position from 0 to (- number-of-base-pairs 1)
	  collecting (list (nth position  t_ab-list) (nth position  t_ac-list) (nth position  t_bc-list)))))


(defun draw-random-binary-nucleotide ()
  "Choose 0 or 1 uniformly"
  (randomly-choose '(0 1)))

(defun binary-mutate (base)
  "Switches 0 to 1 and 1 to zero."
  (declare (fixnum base))
  (logxor base 1))

(defun implement-binary-substitutions-along-edge (edge-length base θ)
  "Input: an edge length, mutation parameter, and starting base pair. Output: a
base pair, possibly different from the starting base, having been subject to
mutations"
  (let ((waiting-time
	  (draw-exponential θ)))
    (if (> waiting-time edge-length)
	base
	(implement-binary-substitutions-along-edge
	 (- edge-length waiting-time)
	 (binary-mutate base)
	 θ))))

(defun generate-binary-nucleotide-for-species-triplet (t_ab t_ac t_bc θ)
  "Input: mutation parameter and time of MRCA (on a marginal gene tree) for each
pair of species (from A,B,C). Output: a triplet of nucleotides of the form (A T
C) or (G G C) corresponding to species A, B, and C respectively."
  (let* ((min-tmrca (min t_ab t_ac t_bc))
	 (max-tmrca (max t_ab t_ac t_bc))
	 (internal-edge-length (- max-tmrca min-tmrca))
	 (grand-mrca (draw-random-binary-nucleotide))
	 (internal-mrca (implement-binary-substitutions-along-edge
			 internal-edge-length grand-mrca θ)))
    (cond ((= min-tmrca t_ab)
	   (list (implement-binary-substitutions-along-edge min-tmrca internal-mrca θ)
		 (implement-binary-substitutions-along-edge min-tmrca internal-mrca θ)
		 (implement-binary-substitutions-along-edge max-tmrca grand-mrca θ)))
           ((= min-tmrca t_ac)
	    (list (implement-binary-substitutions-along-edge min-tmrca internal-mrca θ)
		  (implement-binary-substitutions-along-edge max-tmrca grand-mrca θ)
		  (implement-binary-substitutions-along-edge min-tmrca internal-mrca θ)))
           ((= min-tmrca t_bc)
	    (list (implement-binary-substitutions-along-edge max-tmrca grand-mrca θ)
		  (implement-binary-substitutions-along-edge min-tmrca internal-mrca θ)
		  (implement-binary-substitutions-along-edge min-tmrca internal-mrca θ))))))


(defun generate-binary-nucleotide-triplets-for-sampled-locus
    (τ_ab τ_abc τ_max ρ_a ρ_b ρ_c ρ_ab ρ_abc number-of-base-pairs θ)
  "Generate an ARG, then for each base pair and each pair of species A,B,C,
compute the marginal times until MRCA and use those to generate nucleotide
sequences for each species. Output is a list of triplets of the form (A A T)
or (A C T) where the nth element of the list gives the nth base pair sampled
from species A, B and C respectively."
  (let* ((pairwise-marginal-distances
	   (list-all-pairwise-marginal-distances
	    τ_ab τ_abc τ_max ρ_a ρ_b ρ_c ρ_ab ρ_abc number-of-base-pairs)))
    (loop for position from 0 to (- number-of-base-pairs 1)
	  collecting (generate-binary-nucleotide-for-species-triplet
		      (first (nth position pairwise-marginal-distances))
		      (second (nth position pairwise-marginal-distances))
		      (third (nth position pairwise-marginal-distances))
		      θ))))

(defun compute-site-pattern-frequencies (list-of-triplets)
  "Input: a list of nucleotide triplets. Output: a list (n₁ n₂ n₃ n₄) of site
pattern frequencies, where the entries of the list correspond to patterns xxx, xxy,
yxx, and xyx respectively. Example usage: (compute-site-pattern-frequencies '((G
G G) (A A T) (G C C))) or (compute-site-pattern-frequencies '((1
1 1) (1 1 0) (0 1 1) (0 0 0)))"
  (loop for triplet in list-of-triplets 
 	counting (and (equal (first triplet) (second triplet))
		      (equal (second triplet) (third triplet))) into xxx
        counting (and (equal (first triplet) (second triplet))
		      (not (equal (second triplet) (third triplet)))) into xxy
	counting (and (equal (second triplet) (third triplet))
		      (not (equal (first triplet) (second triplet)))) into yxx
	counting (and (equal (first triplet) (third triplet))
		      (not (equal (first triplet) (second triplet)))) into xyx
 	finally (return (mapcar #'(lambda (x) (/ x (+ xxx xxy yxx xyx)))
				(list xxx xxy yxx xyx)))))



(defun T₀-condition (f₁ f₂ f₃)
  "Tests the condition for inferring the 3-polytomy T₀ provided in Table 4 Yang
2000. Evaluates to t if T₀ is the ML tree and nil otherwise."
  (declare (rational f₁ f₂ f₃))
  (or (and (> f₁ f₂) (> f₁ f₃)
	   (>= (+ f₂ f₃) .5))
      (and (> f₂ f₁) (> f₂ f₃)
	   (>= (+ f₁ f₃) .5))
      (and (> f₃ f₁) (> f₃ f₂)
	   (>= (+ f₂ f₁) .5))
      (and (= f₁ f₂) (= f₂ f₃))))

(defun ml-sequence-estimate-topology-probabilities
    (τ_ab τ_abc τ_max ρ_a ρ_b ρ_c ρ_ab ρ_abc θ number-of-samples
    number-of-base-pairs)
  "For each sampled locus, construct an ancestral recombination graph on three
species, then compute pairwise distances for each marginal gene tree, then use
binary Jukes-Cantor process to model evolution of nucleotides on each gene tree,
thus generating a nucleotide triplet (one nucleotide for each species) for every
site on the locus. Then count the number of site patterns and use the counts to
infer a topology for the locus."
  (let* ((inference-probabilities
	   (loop for i from 1 to number-of-samples
		 for list-of-triplets = (generate-binary-nucleotide-triplets-for-sampled-locus
					 τ_ab τ_abc τ_max ρ_a ρ_b ρ_c ρ_ab ρ_abc number-of-base-pairs θ)
		 for site-pattern-frequencies = (compute-site-pattern-frequencies
						 list-of-triplets)
		 for f₁ = (second site-pattern-frequencies) ; 'second' is not a typo.
					                    ; The f-notation is consistent
		                                            ; with Table 4 in Yang 2000.
		 for f₂ = (third site-pattern-frequencies)
		 for f₃ = (fourth site-pattern-frequencies)
		 if (T₀-condition f₁ f₂ f₃)
		   count 1 into T₀-count ; T₀ is a 3-polytomy
		 else
		   count (and (> f₁ f₂) (> f₁ f₃)) into T₁-count ; T₁ is (AB)C
		   and count (and (> f₂ f₃) (> f₂ f₁)) into T₂-count ; T₂ is A(BC)
		   and count (and (> f₃ f₁) (> f₃ f₂)) into T₃-count ; T₃ is (AC)B
		   and count (and (= f₁ f₂) (> f₂ f₃)) into T₁₂-count
		   and count (and (= f₂ f₃) (> f₃ f₁)) into T₂₃-count
		   and count (and (= f₃ f₁) (> f₁ f₂)) into T₁₃-count
		 finally (return
			   (mapcar #'(lambda (x) (float (/ x number-of-samples)))
				   (list (+ T₁-count (/ T₁₂-count 2) (/ T₁₃-count 2) (/ T₀-count 3))
					 (+ T₂-count (/ T₁₂-count 2) (/ T₂₃-count 2) (/ T₀-count 3))
					 (+ T₃-count (/ T₁₃-count 2) (/ T₂₃-count 2) (/ T₀-count 3)))))))
	 (P-ab (first inference-probabilities))
	 (P-bc (second inference-probabilities))
	 (P-ac (third inference-probabilities)))
    (format nil "~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a"
	    P-ab P-ac P-bc τ_ab τ_abc τ_max ρ_a ρ_b ρ_c ρ_ab
	    ρ_abc θ number-of-samples number-of-base-pairs)))

;;______________________________________________________________________________
;;
;;  --- FUNCTIONS FOR SIMULATING JC69-SEQUENCES + R* INFERENCE ---
;;______________________________________________________________________________

;; These functions are for generating DNA sequences consisting of letters A,T,C,
;; and G under the ancestral recombination graph + JC 69 substitution model.
;; Most of the functions are similar to those found in the previous section for
;; Maximum Likelihood method (from Yang 2000), but with minor modifications. In
;; this case, sequences are generated for each species and R* inference is based
;; on the pairwise Hamming distances.


(defun draw-random-ATCG-nucleotide (&optional (π '(.25 .25 .25 .25)))
  "Output a nucleotide A,T,C or G according to the given distribution (density)
vector π (which should sum to one). By default, if no probability distribution
is given, output a uniformly chosen nucleotide."
  (let ((x (random 1d0))
	(cdf (convert-to-cdf π)))
    (cond ((< x (first cdf)) "A")
	  ((and (>= x (first cdf)) (< x (second cdf))) "T")
	  ((and (>= x (second cdf)) (< x (third cdf))) "C")
	  (t "G"))))

(defun ATCG-mutate (base)
  "Mutate nucleotide of the form A,T,C, or G to a different nucleotide of a
different letter. The new nucleotide is chosen uniformly."
  (let ((new-base (randomly-choose '("A" "T" "C" "G"))))
    (if (equal base new-base)
	(ATCG-mutate base)
	new-base)))


(defun implement-ATCG-substitutions-along-edge (edge-length base θ)
  "Input: an edge length, mutation parameter, and starting base pair. Output: a
base pair, possibly different from the starting base, having been subject to
mutations"
  (let ((waiting-time
	  (draw-exponential θ)))
    (if (> waiting-time edge-length)
	base
	(implement-ATCG-substitutions-along-edge
	 (- edge-length waiting-time)
	 (ATCG-mutate base)
	 θ))))


(defun generate-ATCG-nucleotide-for-species-triplet (t_ab t_ac t_bc θ)
  "Input: mutation parameter and time of MRCA (on a marginal gene tree) for each
pair of species (from A,B,C). Output: a triplet of nucleotides of the form (A T
C) or (G G C) corresponding to species A, B, and C respectively."
  (let* ((min-tmrca (min t_ab t_ac t_bc))
	 (max-tmrca (max t_ab t_ac t_bc))
	 (internal-edge-length (- max-tmrca min-tmrca))
	 (grand-mrca (draw-random-ATCG-nucleotide))
	 (internal-mrca (implement-ATCG-substitutions-along-edge
			 internal-edge-length grand-mrca θ)))
    (cond ((= min-tmrca t_ab)
	   (list (implement-ATCG-substitutions-along-edge min-tmrca internal-mrca θ)
		 (implement-ATCG-substitutions-along-edge min-tmrca internal-mrca θ)
		 (implement-ATCG-substitutions-along-edge max-tmrca grand-mrca θ)))
           ((= min-tmrca t_ac)
	    (list (implement-ATCG-substitutions-along-edge min-tmrca internal-mrca θ)
		  (implement-ATCG-substitutions-along-edge max-tmrca grand-mrca θ)
		  (implement-ATCG-substitutions-along-edge min-tmrca internal-mrca θ)))
           ((= min-tmrca t_bc)
	    (list (implement-ATCG-substitutions-along-edge max-tmrca grand-mrca θ)
		  (implement-ATCG-substitutions-along-edge min-tmrca internal-mrca θ)
		  (implement-ATCG-substitutions-along-edge min-tmrca internal-mrca θ))))))


(defun generate-ATCG-nucleotide-triplets-for-sampled-locus
    (τ_ab τ_abc τ_max ρ_a ρ_b ρ_c ρ_ab ρ_abc number-of-base-pairs θ)
  "Generate an ARG, then for each base pair and each pair of species A,B,C,
compute the marginal times until MRCA and use those to generate nucleotide
sequences for each species. Output is a list of triplets of the form (A A T)
or (A C T) where the nth element of the list gives the nth base pair sampled
from species A, B and C respectively."
  (let* ((pairwise-marginal-distances
	   (list-all-pairwise-marginal-distances
	    τ_ab τ_abc τ_max ρ_a ρ_b ρ_c ρ_ab ρ_abc number-of-base-pairs)))
    (loop for position from 0 to (- number-of-base-pairs 1)
	  collecting (generate-ATCG-nucleotide-for-species-triplet
		      (first (nth position pairwise-marginal-distances))
		      (second (nth position pairwise-marginal-distances))
		      (third (nth position pairwise-marginal-distances))
		      θ))))


(defun make-dna-sequences (list-of-triplets)
  "Converts the nucleotide triplets into a list of three nucleotide sequences,
corresponding to species A, B, and C. Example code: (make-dna-sequences (generate-atcg-nucleotide-triplets-for-sampled-locus 1 1.3 19999 3 0 0 0 0 5 3))"
  (list (format nil "~{~a~}" (loop for x in list-of-triplets collecting (first x)))
	(format nil "~{~a~}" (loop for x in list-of-triplets collecting (second x)))
	(format nil "~{~a~}" (loop for x in list-of-triplets collecting (third x)))))
;; Note the input variable list-of-triplets take the form
;; '(("A" "T" "T") ("T" "T" "T") ("G" "A" "T"))

(defun hamming-distance (dna1 dna2)
  "Determine number of mutations between DNA strands by computing the Hamming
Distance. This function was copied verbatim from Shinmera's solution (2018)
online at
https://exercism.io/tracks/common-lisp/exercises/hamming/solutions/f76cb73b07254c118cc91d800e70021d"
  (when (= (length dna1) (length dna2))
    (loop for nucleotide1 across dna1
          for nucleotide2 across dna2
          when (not (eql nucleotide1 nucleotide2))
          count it)))

(defun jc-sequence-estimate-topology-probabilities
    (τ_ab τ_abc τ_max ρ_a ρ_b ρ_c ρ_ab ρ_abc θ number-of-samples
     number-of-base-pairs)
  "For each sampled locus, construct an ancestral recombination graph on three
species, then compute pairwise distances for each marginal gene tree, then use
Jukes-Cantor process to model evolution of nucleotides ATCG on each gene tree,
thus generating a nucleotide triplet (one nucleotide for each species) for every
site on the locus. Then reformat the data into DNA sequences, one for each
species (ie dna-X is a string of nucleotides corresponding to the sequence from
species X). Then count the number pairwise differences between the three
sequences to infer a topology for the locus. (In the code below h-xy is the
hamming distance between the sequences from species x and y)."
  (let* ((inference-probabilities
	  (loop for i from 1 to number-of-samples
		for list-of-triplets = (generate-ATCG-nucleotide-triplets-for-sampled-locus
					τ_ab τ_abc τ_max ρ_a ρ_b ρ_c ρ_ab ρ_abc
					number-of-base-pairs θ)
                for dna-A = (format nil "~{~a~}" (loop for x in list-of-triplets collecting (first x)))
		for dna-B = (format nil "~{~a~}" (loop for x in list-of-triplets collecting (second x)))
		for dna-C = (format nil "~{~a~}" (loop for x in list-of-triplets collecting (third x)))
		for h-ab = (hamming-distance dna-A dna-B)
		for h-ac = (hamming-distance dna-A dna-C)
		for h-bc = (hamming-distance dna-B dna-C) 
     	        if (= h-ab h-ac h-bc)
		   count 1 into T₀-count ; T₀ is a 3-polytomy
		 else
		   count (and (> h-ac h-ab) (> h-bc h-ab)) into T₁-count ; T₁ is (AB)C
		   and count (and (> h-ab h-bc) (> h-ac h-bc)) into T₂-count ; T₂ is A(BC)
		   and count (and (> h-ab h-ac) (> h-bc h-ac)) into T₃-count ; T₃ is (AC)B
		   and count (and (= h-ab h-bc) (> h-ac h-ab)) into T₁₂-count
		   and count (and (= h-bc h-ac) (> h-ab h-bc)) into T₂₃-count
		   and count (and (= h-ab h-ac) (> h-bc h-ab)) into T₁₃-count
		 finally (return
			   (mapcar #'(lambda (x) (float (/ x number-of-samples)))
				   (list (+ T₁-count (/ T₁₂-count 2) (/ T₁₃-count 2) (/ T₀-count 3))
					 (+ T₂-count (/ T₁₂-count 2) (/ T₂₃-count 2) (/ T₀-count 3))
					 (+ T₃-count (/ T₁₃-count 2) (/ T₂₃-count 2) (/ T₀-count 3)))))))
	 (P-ab (first inference-probabilities))
	 (P-bc (second inference-probabilities))
	 (P-ac (third inference-probabilities)))
    (format nil "~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a"
	    P-ab P-ac P-bc τ_ab τ_abc τ_max ρ_a ρ_b ρ_c ρ_ab
	    ρ_abc θ number-of-samples number-of-base-pairs)))
;;  Edge cases in the above are handled in the following way: if all three
;;  hamming distances are equal, we assume that one of the three possible binary
;;  tree topologies is chosen uniformly at random. If h-xy = h-xz < h-yz, then
;;  we assume that one of ((xy)z) or ((xz)y) is chosen uniformly at random.
;;  However, to reduce estimate variance, we do not implement this step
;;  directly, but instead assing weights of 1/3 to all cases or 1/2 to the two
;;  more plausible trees respectively."
