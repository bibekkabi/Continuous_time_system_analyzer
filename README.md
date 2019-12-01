# Continuous_time_system_analyzer
A prototype analyzer, written in OCaml connected to APRON Abstract Domain Library for finding positive invariant of continuous time systems

The prime purpose of this analyzer is to infer postive invariant for continuous time systems. The analyzer discretizes the continuous time system and applies the constraint programming algorithm on the discretized map. 

Note that for a continuous time system Taylor based discretization method provides only approximate values for the solution. So, we will be using **Taylor models** [5] that provide guaranteed bounds for the flow of an ordinary diffential equations. We evaluate a Taylor model on each time step and apply the continuous programming algorithm on every new Taylor model. For computing a Taylor model we use the toolbox Flow* [6]. 

The analyzer was originally developed by Antoine Miné which implemented the constraint programming algorithm of [1] 
using the boxes and octagons abstract domains of the Apron [2] library for finding inductive invariants of numeric programs.  
The present version was modified to use the **zonotopic abstract domain Taylor1+** [3] and polyhedra abstract domain New Polka of Apron. 

In particular, the new elements included in the analyzer are: 

1. Now, the analyzer can handle multiple sub-parts after every split operation (see [1] for more information about the algorithm). 

2. A cheaper coverage (see [1] for more information about the algorithm) metric has been implemented which no longer relies on volume computation
   of abstract elements.
   
The steps to install and use the analyzer can be found in the document `00README.txt`. 
The modified version of the analyzer has been tested for different abstract domains namely boxes, octagons, polyhedra and zonotopes.

The analyzer relies on Apron library for the numerical abstract domains. 
For installing Apron, please follow the steps provided in the document `Pointers_apron.pdf`. 

The analyzer uses Taylor1+ implementation of Apron for the zonotope abstract domain. 
In order to extend the analyzer with zonotope abstract domain few new operations were implemented in Taylor1+ [4]. 
The details about them can be found in: https://github.com/bibekkabi/taylor1plus


# References

[1] Miné, A., Breck, J., & Reps, T. (2016, April). An algorithm inspired by constraint solvers to infer inductive invariants in numeric programs. In European Symposium on Programming (pp. 560-588). Springer, Berlin, Heidelberg.

[2] Jeannet, B., & Miné, A. (2009, June). Apron: A library of numerical abstract domains for static analysis. In International Conference on Computer Aided Verification (pp. 661-667). Springer, Berlin, Heidelberg.

[3] Ghorbal, K., Goubault, E., & Putot, S. (2009, June). The zonotope abstract domain taylor1+. In International Conference on Computer Aided Verification (pp. 627-633). Springer, Berlin, Heidelberg.

[4] Kabi, B. (to be submitted). Synthesizing invariants: a constraint programming approach based on zonotopic abstraction (Doctoral dissertation, Ecole Polytechnique X, Universite Paris Saclay).

[5] Martin Berz and Kyoko Makino, Verified integration of odes and flows using differential algebraic methods on high-order taylor models, Reliable computing 4 (1998), no. 4, 361–369.

[6] Xin Chen, Erika Ábrahám, and Sriram Sankaranarayanan, Flow*: An analyzer for non-linear hybrid systems, International Conference on Computer Aided Verification, Springer, 2013, pp. 258–263.
