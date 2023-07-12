# k-plus anticlustering

This repository contains materials for the manuscript

Papenberg, M. (2023). K-plus Anticlustering: An Improved k-means Criterion for Maximizing Between-Group Similarity. *British Journal of Mathematical and Statistical Psychology*. Advance online publication. https://doi.org/10.1111/bmsp.12315

The preprint of the manuscript is available from the PsyArXiv preprint server: https://psyarxiv.com/7jw6v.

- The file "paper.Rmd" is the R-Markdown file which renders the manuscript. To reproduce the manuscript...
  * ... ensure that the packages that are loaded in the first R code chunk are installed (in particular, use the development version of [papaja](https://github.com/crsh/papaja) to render it in APA 7th ed. style)
  * ... use `rmarkdown::render("paper.Rmd")` in the R console or the Knit button in the RStudio IDE
- The Rmd file paper.Rmd contains all required code to reproduce the anticlustering examples in the paper
- The directories `./Simulation_1` and `./Simulation_2` contain the code and all information needed to to reproduce the simulation studies reported in the manuscript
