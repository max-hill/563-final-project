;; initiate-simulation.lisp --- runs the user-selected inference method over all
;; parameter regimes specified in simulation-parameters.lisp and records the
;; results in a csv file. This file is executed automatically by the script
;; simulate.sh.
;;
;; Author: max hill
;; (Last updated 2021-06-21)


;; Set the progress-tracking counter
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
	 (format nil "../data/jc-sequence-N~a-L~a.csv" *N* *L*))
	((equal "3" *inference_method*)
	 (format nil "../data/steac-N~a-L~a.csv" *N* *L*))))

;; Identify which function in simulator.lisp to use (depends on the inference
;; method)
(defparameter *main-inference-function*
  (cond ((equal "0" *inference_method*) 'ml-sequence-estimate-topology-probabilities)
	((equal "1" *inference_method*) 'jc-expected-estimate-topology-probabilities)
	((equal "2" *inference_method*) 'jc-sequence-estimate-topology-probabilities)
	((equal "3" *inference_method*) 'steac-estimate-topology-probabilities)))

;; Create the output file and add a header. Give a warning message for when the
;; output filename already exists.
(if (probe-file *output-filename*)
    (format t "~%~%Warning: file with the intented output name already exists. ~a ~a ~%"
	    "Results from this simulation will be appended to the existing file: "
	    *output-filename*)
    (with-open-file (output *output-filename*
			    :direction :output
			    :if-does-not-exist :create
			    :if-exists :append)
      (if (equal *inference_method* "3")
	  (format output "Expected d-ab,Expected d-ac,Expected d-bc,τ_ab,τ_abc,τ_max,ρ_a,ρ_b,ρ_c,ρ_ab,ρ_abc,θ,N,L~%")
	  (format output "P-ab,P-ac,P-bc,τ_ab,τ_abc,τ_max,ρ_a,ρ_b,ρ_c,ρ_ab,ρ_abc,θ,N,L~%"))))

;; Progress indicator message
(format t "~%~%This will take some time. Progress counter tracks total number of parameter regimes completed (ie number of rows added to output file) so far.")
  
;; Run the simulation, looping over all parameter regimes specified in the file
;; simulation-parameters.lisp.
(with-open-file (output *output-filename*
			:direction :output
			:if-does-not-exist :create
			:if-exists :append)
  (loop for τ_ab in *τ_ab-values* do
    (loop for f in *f-values* do
      (loop for ρ₁ in *ρ_a-values* do
	(loop for ρ₂ in *ρ_b-values* do
	  (loop for ρ₃ in *ρ_c-values* do
	    (loop for ρ₄ in *ρ_ab-values* do
	      (loop for ρ₅ in *ρ_abc-values* do
		(loop for θ in *θ-values* do
		  (print (setf *counter* (1+ *counter*))) ; update and print progress counter
		  (sb-ext:gc :full t) ; do full garbage collection
		  (format output "~a~%"
			  (funcall *main-inference-function*
				   τ_ab (+ f τ_ab) *τ_max* ρ₁ ρ₂ ρ₃ ρ₄ ρ₅ θ *N* *L*)))))))))))
;; COMMENTARY: The above code does the following. For each parameter regime, we
;; simulate *N* loci each of length *L*. Since *N* is large, this allows us to
;; determine the likelihood of inferring the correct species tree topology
;; compared to the incorrect topologies using the given method. The results for
;; each parameter regime are written as a row in the output csv file. Note that
;; (+ f τ_ab) = τ_abc.

;; NOTE ABOUT GARBAGE COLLECTION: The garbage collection step clears RAM in
;; between computing rows of the csv. This was necessary to prevent memory
;; overload in an earlier version of the simulator, and while it probably isn't
;; necessary any more, I am opting to leave it in since it contributes very
;; little to run time.

;; Completion message
(format t "~%~%Output written to file ~a~%" *output-filename*)
