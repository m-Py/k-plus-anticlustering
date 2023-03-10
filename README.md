# k-plus anticlustering

This repository contains materials for the manuscript

k-plus Anticlustering: An Improved k-means Criterion for Maximizing Between-Group Similarity (Papenberg, 2023)

The manuscript has been uploaded to the PsyArXiv preprint server (available via https://psyarxiv.com/7jw6v) and has been submitted for publication.

- The file "paper.Rmd" is the R-Markdown file which renders the manuscript. To reproduce the manuscript...
  * ... ensure that the packages that are loaded in the first R code chunk are installed (in particular, use the development version of [papaja](https://github.com/crsh/papaja) to render it in APA 7th ed. style)
  * ... use `rmarkdown::render("paper.Rmd")` in the R console or the Knit button in the RStudio IDE
- The Rmd file paper.Rmd contains all required code to reproduce the anticlustering examples in the paper
- The directory `./Simulation_Study` contains the code and all information needed to to reproduce the simulation study reported in the manuscript
