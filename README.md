# k-plus anticlustering

This repository contains materials for the manuscript:

k-plus Anticlustering: An Improved k-means Criterion for Maximizing Between-Group Similarity (Papenberg, 2023)

The manuscript has not yet been submitted for publication (but will soon be).

- The file "paper.Rmd" is the R-Markdown file which renders the manuscript. To reproduce the manuscript...
  * ... use `rmarkdown::render("paper.Rmd")` in the R console or the Knit button in the RStudio IDE
  * ... ensure that the packages that are loaded in the first R code chunk are installed (in particular, use the development version of [papaja](https://github.com/crsh/papaja) to render it in APA 7th ed. style)
- The Rmd file paper.Rmd contains all required code to reproduce the anticlustering examples in the paper
- The directory `./Simulation_Study` contains the code and all information needed to to reproduce the simulation study reported in the manuscript
