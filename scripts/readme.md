# Script Readme
(Last updated: 2021-04-06)

I'm not sure what my final pipeline will look like yet. As of now, the file [simulator.lisp](simulator.lisp) contains all lisp funtions for simulating the Ancestral Recombination Graph on a 3-taxa species tree. I am running slime in emacs, with SBCL.

Short descriptions of each script is given. Further documentation can be found as commentary in the script files themselves (i.e. open them with a text editor).

# Scripts

* [consensus-jc.sh](consensus-jc.sh) estimates the species tree topology probabilities for a range of prameter regimes using the file 'execute-consensus-jc.lisp'. To run, navigate to the `scripts/` directory and execute the command ``` bash consensus-jc.sh```



# To do: 
* Finish implementing mutations. To do this you should think clearly about what you want your pipeline to look like. What outputs should be saved as files?
* Write a program to explore the parameter space for consistency of R*. Still waiting on some computer parts to arrive before I implements this.
* Implement data analysis in some language other than lisp (i.e. python, julia, or R). There are two reasons for this: (1) it's prettier, since math equations are kind of ugly in lisp; (2) I need to make plots. Ultimately I want a pretty, graphical depiction of the anomolous zone.
* Figure out what format the output sequences should be for, say, *BEAST.
* Write lisp programs so that they can be piped (i.e. with standard input and standard output). I want to be able to run my pipeline entirely from the terminal, without opening a lisp REPL.
* Implement the maximum likelihood method from Yang.
