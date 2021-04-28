;; initiate-simulation.lisp --- runs the approprate inference methods over
;; various parameter regims and records the results in a csv file.
;;
;; Author: max hill
;; (Last updated 2021-04-27)
;;
;; DESCRIPTION: Simulate the evolution a length of DNA (the 'sampled locus')
;; across three sister species A, B, and C whose species tree has topology
;; ((AB)C). For each sampled locus (i.e., each simulation run), attempt to infer
;; the original topology using one of three possible methods (see documentation,
;; but there correspond roughly to: maximum-liklihood with binary sequences,
;; expected weighted Jukes-Cantor distances, and uncorrected Hamming distances).
;; Record all information in a csv file to be analyzed later.
;;
;; OUTPUT: A single .csv file named 'inference-method-N-L-F-θ.csv', where
;; 'inference method' is replaced by an element of the set {ml-sequence,
;; jc-expected, jc-sequence}, N, L, and θ are specified in the table below, and
;; F:=τ_abc-τ_ab is the internal branch length of the species tree. The csv will
;; have 14 columns, indicated as follows:
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
;; More specific descriptions of the procedures (incomplete -- also maybe move these somewhere else)
;; PROCEDURE: For each loci, generate an ARG, compute marginal gene trees, and
;; then compute generate nucleotide sequence (A,T,C,G) using the Jukes-Cantor
;; process on the marginal gene trees. Then use the R* consensus method to infer
;; a topology (i.e. look at the uncorrected distances of the three sequences).
;; PROCEDURE: For each loci, generate an ARG, compute marginal gene trees,
;; and then compute JC distances based on the heights of marginal gene trees.
;; Based on the computed JC distances, use the R* consensus method to infer a
;; topology. OUTPUT: for each of the three possible topologies ((AB)C), ((AC)B)
;; and ((BC)A), output the fraction of loci that inferred that topology.


;; ADDITIONAL COMMENTARY: The multispecies coalescent with recombination is used
;; to generate each locus. The functions used to implement this process are
;; defined in simulator.lisp. Additional information about the process can be
;; found as commentary in that file.

;; Set the counter
(defparameter *counter* 0)

;; Import user-specified inference method (= "0" "1" or "2")
(defparameter *inference_method* (second *posix-argv*))

;; Set name of output-file (depends on the inference method)
(defparameter *output-filename*
  (cond ((equal "0" *inference_method*)
	 (format nil "../data/ml-sequence-N~a-L~a.csv" *N* *L*))
	((equal "1" *inference_method*)
	 (format nil "../data/jc-expected-N~a-L~a.csv" *N* *L*))
	((equal "2" *inference_method*)
	 (format nil "../data/jc-sequence-N~a-L~a.csv" *N* *L*))))

;; Identify which function in simulator.lisp to use (depends onthe inference method)
(defparameter *main-inference-function*
  (cond ((equal "0" *inference_method*) 'ml-sequence-estimate-topology-probabilities)
	((equal "1" *inference_method*) 'jc-expected-estimate-topology-probabilities)
	((equal "2" *inference_method*) 'jc-sequence-estimate-topology-probabilities)))

;; Run the simulation, looping over all parameter regimes specified in the file
;; simulation-parameters.lisp.
(if (probe-file *output-filename*)
    (format t "~%~%Error: file with the intented output name already exists. ~a ~a ~a~%"
	    "You have already simulated with these parameters. You must choose new"
	    "parameters or rename/delete the file" *output-filename*)
    (progn
      (format t "~%~%This will take some time.")
      (with-open-file (output *output-filename*
			      :direction :output
			      :if-does-not-exist :create
			      :if-exists :append)
	(progn
	  (format output "P-ab,P-ac,P-bc,τ_ab,τ_abc,τ_max,ρ_a,ρ_b,ρ_c,ρ_ab,ρ_abc,θ,N,L~%")
	  (loop for τ_ab in *τ_ab-values* do
	       (loop for f in *f-values* do
		    (loop for ρ₁ in *ρ_a-values* do
			 (loop for ρ₂ in *ρ_b-values* do
			      (loop for ρ₃ in *ρ_c-values* do
				   (loop for ρ₄ in *ρ_ab-values* do
					(loop for ρ₅ in *ρ_abc-values* do
					     (loop for θ in *θ-values* do
						  (print (setf *counter* (1+ *counter*))) ; counter for tracking how much longer to wait
						  (sb-ext:gc :full t) ; initiate full garbage collection. Prevents running out of memory.
						  (format output "~a~%"
							  (funcall *main-inference-function*
								   τ_ab (+ f τ_ab) *τ_max* ρ₁ ρ₂ ρ₃ ρ₄ ρ₅ θ *N* *L*))))))))))))
	  (format t "~%~%Output written to file ~a~%" *output-filename*)))

;; COMMENTARY: The above code does the following. First, we introduce a
;; conditional to resolve certain naming issues (by printing an error if a file
;; with the given output filename already exists). Then for each parameter
;; regime, we simulate *N* loci each of length *L*. Since *N* is large, this
;; allows us to determine the likelihood of inferring the correct species tree
;; topology compared to the incorrect topologies using the given method. The
;; results for each parameter regime are written as a row in the output csv
;; file. Note that (+ f τ_ab) = τ_abc
