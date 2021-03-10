;; execute-consensus-jc.lisp --- collect inference data from many simulations
;;
;; Author: max hill
;; (Last updated 2021-03-09)
;;
;; DESCRIPTION: Simulate the evolution a length of DNA (the 'sampled locus')
;; across three sister species A, B, and C whose species tree has topology
;; ((AB)C). For each sampled locus (i.e., each simulation run), attempt to infer
;; the original topology using weighted Jukes-Cantor distances. Record all
;; information, in a csv file to be analyzed later.
;;
;; OUTPUT: A single .csv file named 'consensus-jc-N-L-F.csv', where N is the
;; sample size ( = number of simulation runs = number of rows in csv file), L is
;; the length of the each sampled loci (in base pairs), and F is the internal
;; branch length of the species tree. The csv will have 14 columns, indicated as
;; follows:
;; 
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
;;
;;
;; PROCEDURE: For each loci, generate an ARG, compute marginal gene trees,
;; and then compute JC distances based on the heights of marginal gene trees.
;; Based on the computed JC distances, use the R* consensus method to infer a
;; topology. OUTPUT: for each of the three possible topologies ((AB)C), ((AC)B)
;; and ((BC)A), output the fraction of loci that inferred that topology.

;; ADDITIONAL COMMENTARY: The multispecies coalescent with recombination is used
;; to generate each locus. The functions used to implement this process are
;; defined in simulator.lisp. Additional information about the process can be
;; found as commentary in that file.


(defparameter *τ_ab* 1) ; age of most recent divergence time (in coalescent units)
(defparameter *f* .1) ; internal branch length: τ_abc-τ_ab. It's convenient to write this explicitly
(defparameter *τ_abc* (+ *τ_ab* *f*)) ; age of oldest divergence time
(defparameter *τ_max* 999999) ; maximum height of species tree -- choose this large enough to ensure all coalescent events occur long before this time occurs (we could just choose this to be most-positive-long-float).
(defparameter *θ* .1) ; mutation parameter
(defparameter *possible-ρ-values* '(0 1 2 4 8))
(defparameter *N* 1000); sample size (number of sampled loci)
(defparameter *L* 16) ; locus length (in base pairs)
(defparameter *counter* 0) ; counter for tracking how long we have to wait for the simulation to finish

(defun try-various-recombination-rates (N L &optional (θ *θ*))
  "Run simulations for N loci each of length L with every possible combination of species recombination rates taken from the list *possible-ρ-values*. User must input N and L, where N is the sample size and L is the locus length."
  (let ((output-csv-file (format nil "../data/consensus-jc-N~a-L~a-F~a.csv" N L *f*)))
    (with-open-file (output output-csv-file
			    :direction :output
                            :if-does-not-exist :create
			    :if-exists :append)
      (progn
	(format output "P-ab,P-ac,P-bc,τ_ab,τ_abc,τ_max,ρ_a,ρ_b,ρ_c,ρ_ab,ρ_abc,θ,N,L~%")
	(loop for ρ₁ in *possible-ρ-values* do
	  (loop for ρ₂ in *possible-ρ-values* do
	    (loop for ρ₃ in *possible-ρ-values* do
	      (loop for ρ₄ in *possible-ρ-values* do
		(loop for ρ₅ in '(0) do
		  (progn
		    (print (setf *counter* (1+ *counter*))) ; counter for tracking how much longer to wait
		    (sb-ext:gc :full t) ; initiate full garbage collection. Prevents running out of memory.
		    (format output "~a~%"
			    (estimate-topology-probabilities
			     *τ_ab* *τ_abc* *τ_max* ρ₁ ρ₂ ρ₃ ρ₄ ρ₅ θ N L))))))))))))

;; Now execute the above function with some various values of N and L. The output can be found in the data/ directory in the form of csv files for each input pair of N and L (e.g. N320000-L2, N160000-L4, etc).
(mapcar #'try-various-recombination-rates '(320000 160000 80000 40000 20000) '(2 4 8 16 32))

