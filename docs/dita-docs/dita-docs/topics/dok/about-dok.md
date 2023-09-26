# Dok

Dok is programming language based on stepwise refinements of source code.

Higher-level code is transformed to low level executable code, through incremental refinements. At each refinement step, the higher level code can be seen as a model and specification for the lower level code to produce. These refinements can be done manually, and/or using compiler extensions. Annotations to the higher-level code can guide these transformations.

These refinements can form a tree: at each step, different versions of low-level code can be produced from the same higher-level code. Every version can support different requirements about the run-time on which the low-level code had to run, or the size of data to process, and so on.

Some rationales for this approach are: better code comprehension and documentation; incremental analysis of requirements; reuse of high-level models for different usage scenario.

Dok libraries can be compiler plugins, i.e. they can analyze and transform the code where they are used. They can also define domain specific languages (DSL). 

Dok libraries can also specialize the IDE with ad-hoc tools.

In Dok, the source code is a first-class citizen, because it can be directly analyzed and transformed by user definable tools. This approach simplifies the automation of complex programming tasks.


