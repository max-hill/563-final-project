;;
;; --- infer-topology-with-jc-distances.lisp ---
;;
;; Script to run from command line. INPUT: specified species-tree parameters for
;; a species tree with three leaves and topology ((AB)C), number of loci to
;; sample, and number of base pairs per loci. FUNCTION: For each loci, generate
;; an ARG, compute marginal gene trees, and then compute JC distances based on
;; the heights of marginal gene trees. Based on the computed JC distances, use
;; the R* consensus method to infer a topology. OUTPUT: for each of the three
;; possible topologies ((AB)C), ((AC)B) and ((BC)A), output the fraction of loci
;; that inferred that topology.
;;
;; To run this from the shell, there are two steps. First navigate to the
;; directory containing this file. Then compile the file by running the command

;; >sbcl --noinform --eval '(progn (compile-file "infer-topology-with-jc-distances.lisp") (exit))'


;; Next, to run the compiled file with specified paramters, execute the command

;; > sbcl --noinform --load infer-topology-with-jc-distances.fasl --eval '(progn (estimate-topology-probabilities 1 1.1 100 5 0 0 0 0 .2 100 30) (exit))' > inference-probabilities.out


;; UPDATED RUN INSTRUCTIONS: compile and run all in one command:
; > sbcl --noinform --eval '(progn (compile-file "infer-topology-with-jc-distances.lisp") (load "infer-topology-with-jc-distances.fasl") (quit))'


;;
;; --- GENERAL AUXILLARY FUNCTIONS ---
;;

(defun convert-to-cdf (π)
  "Converts a distribution vector into a cumulative distribution vector.
  Example: (convert-pdf-to-cdf '(.23 .25 .26 .26))"
  (cdf-aux π nil))

(defun cdf-aux (π cdf-vector)
  "Auxillary function to convert-to-cdf"
  (if (null π)
      cdf-vector
      (cdf-aux (butlast π)
	       (cons (apply '+ π) cdf-vector))))

(defun draw-exponential (λ)
  "Return a number drawn according to a rate λ exponential random variable.
  Based on code from https://github.com/tpapp/cl-random. If λ=0, return positive
  infinity, represented by most-positive-long-float (1.7976931348623157d308)."
  (if (zerop λ)
      most-positive-long-float
      (- (/ (log (- 1 (random 1d0))) λ))))

(defun interval (a b)
  (loop for i from a to b collecting i))

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
  *list* of n elements chosen randomly without (or with) replacement from the
  list x. It can also be run standalone. The only difference in output compared
  to randomly-choose occurs when n=1, in which case randomly-choose outputs only
  the randomly-chosen element, whereas randomly-choose-several outputs a list of
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
  "Code based on function found at http://www-users.cselabs.umn.edu/classes/Spring-2018/csci4511/lisp/mapping.html"
  (remove-if
   #'(lambda (element) (member element elements-to-remove :test #'equal))
   initial-set))
	   

;;
;;  --- FUNCTIONS FOR GENERATING ANCESTRAL RECOMBINATION GRAPH ---
;;

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

;; COMMENTARY: The input ρ is the recombination paramter. The input t_0 and
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
	 (recombination-parents (make-recombination-parents time recombination-child number-of-base-pairs))
	 (new-p (cons (first recombination-parents)
		      (cons (second recombination-parents)
			    (remove-elements
			     (cons recombination-child nil)
			     (first edge-sets))))))
    (progn
;      (format t "~%~%RECOMBINATION at time ~a~%One child edge removed: ~a~%Two parent edges added: ~a~%                        ~a"
;	      time recombination-child (first recombination-parents) (second recombination-parents))
      (list new-p (second edge-sets)))))


(defun make-recombination-parents (time edge number-of-base-pairs)
  "Creates both parent edges of a recombining child edge.  Working example: 
(make-recombinant-parents .3 `(.1 ,(interval 1 7) ,(interval 3 6) ,(interval 2 10)))"
  (let ((breakpoint (+ 1 (random (1- number-of-base-pairs)))))
    (list
     (list time
	  (keep-left-part (second edge) breakpoint)
	  (keep-left-part (third edge) breakpoint)
	  (keep-left-part (fourth edge) breakpoint))
     (list time
	  (keep-right-part (second edge) breakpoint)
	  (keep-right-part (third edge) breakpoint)
	  (keep-right-part (fourth edge) breakpoint)))))

(defun keep-left-part (initial-set breakpoint)
  "Remove elements to the right of the breakpoint."
  (remove-if #'(lambda (element) (> element breakpoint)) initial-set))

(defun keep-right-part (initial-set breakpoint)
  "Remove elements less than or equal to (i.e. to the left of) the breakpoint."
  (remove-if #'(lambda (element) (<= element breakpoint)) initial-set))

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

(defun make-coalescent-parent (time coalescing-pair)
  "Creates parent edge of two coalescing edges. The input coalescing-edges is of
  the form (x y) where x and y are the edges"
  (let ((edge1 (first coalescing-pair))
	(edge2 (second coalescing-pair)))
    (list time
	  (union (second edge1) (second edge2))
	  (union (third edge1) (third edge2))
	  (union (fourth edge1) (fourth edge2)))))





;;
;; --- FUNCTIONS FOR COMPUTING JUKES-CANTOR DISTANCE ---
;;

(defun common-coordinate (species1 species2 edge position)
  "test whether the genetic material contained at a specified position on an edge"
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
  species1 and species2 take values 1 2 or 3, corresponding to species A B C"
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

(defun sample-one-locus (τ_ab τ_abc τ_max ρ_a ρ_b ρ_c ρ_ab ρ_abc θ number-of-base-pairs)
  "Input a parameter regime. Construct an ancestral recombination graph on three
  species and then output a binary vector indicating which JC distance is the smallest. (1_AB, 1_AC, 1_BC)."
  (let* ((output-edges (simulate-three-species τ_ab τ_abc τ_max ρ_a ρ_b ρ_c ρ_ab ρ_abc number-of-base-pairs))
	 (d-ab (compute-weighted-jc-distance 1 2 output-edges θ number-of-base-pairs))
	 (d-ac (compute-weighted-jc-distance 1 3 output-edges θ number-of-base-pairs))
	 (d-bc (compute-weighted-jc-distance 2 3 output-edges θ number-of-base-pairs)))
    (cond ((and (< d-ab d-ac) (< d-ab d-bc))
	   (list 1 0 0))
	  ((and (< d-ac d-ab) (< d-ac d-bc))
	   (list 0 1 0))
	  ((and (< d-bc d-ab) (< d-bc d-ac))
	   (list 0 0 1))
	  (t (list 0 0 0)))))

(defun estimate-topology-probabilities (τ_ab τ_abc τ_max ρ_a ρ_b ρ_c ρ_ab ρ_abc θ number-of-samples number-of-base-pairs)
  "Use SLLN to estimate the probabilities of inferring each species tree
   topology via R*/STAR/MCD consensus method. The output, which is written to the standard output, is given as three comma-separated float numbers: the fraction of samples which estimate the topology as ((ab)c), ((ac)b), and ((bc)a) respectively, in that order."
  (let* ((vote-list (loop for i from 1 to number-of-samples
			       collecting (sample-one-locus τ_ab τ_abc τ_max ρ_a ρ_b ρ_c ρ_ab ρ_abc θ number-of-base-pairs)))
	 (probability-of-ab (/ (loop for v in vote-list summing (first v)) number-of-samples))
	 (probability-of-ac (/ (loop for v in vote-list summing (second v)) number-of-samples))
	 (probability-of-bc (/ (loop for v in vote-list summing (third v)) number-of-samples)))
    (format t "~a,~a,~a~%"
	    (float probability-of-ab)
	    (float probability-of-ac)
	    (float probability-of-bc))))


(estimate-topology-probabilities 1 1.1 100 5 0 0 0 0 .2 10000 2)
(estimate-topology-probabilities 1 1.1 100 5 0 0 0 0 .2 10000 4)
(estimate-topology-probabilities 1 1.1 100 5 0 0 0 0 .2 10000 8)
(estimate-topology-probabilities 1 1.1 100 5 0 0 0 0 .2 10000 16)
(estimate-topology-probabilities 1 1.1 100 5 0 0 0 0 .2 10000 32)
(estimate-topology-probabilities 1 1.1 100 5 0 0 0 0 .2 10000 64)
;(estimate-topology-probabilities 1 1.1 100 5 0 0 0 0 .2 1000 128)
;(estimate-topology-probabilities 1 1.1 100 5 0 0 0 0 .2 1000 256)
;(estimate-topology-probabilities 1 1.1 100 5 0 0 0 0 .2 1000 512)
;(estimate-topology-probabilities 1 1.1 100 5 0 0 0 0 .2 1000 1024)

