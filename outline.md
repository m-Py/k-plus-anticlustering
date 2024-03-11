## Outline

- Start with use case: Stimulus assignment in within-subjects experiments (and stick with it throughout the paper, only briefly mention other use cases)
  * Needed: high similarity between stimulus set to reduce variance in responses attributed to other factors than the experimental manipulation. This is possible using `anticlust`, which has been applied many times since its introduction in @papenberg2020 (cite references)
  * Also needed (point of this paper!): High pairwise dissimilarity of stimuli in the same set. 
- Simultaneous optimization of between-set similarity and pairwise within-set dissimilarity was introduced by Brusco et al.'s algorithm, which is also available in `anticlust` since 2021 (version 0.6.0)
- Outline: 
  * Here, we present several alternatives of a bicriterion optimization that are based on optimally solving the max dispersion problem before maximizing the diversity, and we compare them to "vanilla" BILS
    (1) Always return optimal solution and combine optimal dispersion with maximum diversity (BILS, LCW, optimal ILP)
    (2) Use different distance matrix for dispersion and diversity (if different information is needed for both criteria, or to induce custom cannot-link constraints)
    (3) Extend the BILS's capacities towards other measures of between-group similarity (k-means / k-plus)
  * Simulation study as evaluation of the different bicriterion approaches
- Technical introduction to "vanilla" BILS
- Technical introduction to our extensions
  * Optimal dispersion method: Basic algorithm + graph coloring ILP
    * Computational Feasibility
    * How to maintain optimal dispersion while the BILS runs? (Optimal ILP, BILS, LCW)
    * Obtaining an (optimal) Pareto set
  * Different distance matrices: does not change algorithm, just computation / implementation
  * Extend towards k-means: Use squared Euclidean distance + "average diversity" (k-plus is basically the same, but do not use the additional k-plus variables for the dispersion distance matrix)
- Simulation Study
  (1) How often does vanilla BILS find optimal solutions
  (2) Which of the alternatives is best for optimizing between-group similarity while maintaining optimal dispersion?
- Application (presenting R code)
  * Optimize k-plus criterion using norming data; dispersion is based on pairwise orthographic dissimilarity between words
- Discussion
  * Future extensions: Graph coloring returns several solutions, LCW as first phase in BILS
  