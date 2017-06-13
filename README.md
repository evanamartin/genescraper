# geneScrapeR

geneScrapeR is an R package that mines NCBI databases for genes mentioned in connection with diseases and other medical conditions.

### Motivation

geneScrapeR is meant to make it easy to find the genes associated with a disease or condition and the number of times they are mentioned and return them in a useful format.

### Installation

It is recommended that you install org.Hs.eg.db prior to installing geneScrapeR.
To install org.Hs.eg.db
```{r,eval = FALSE}
source('http://bioconductor.org/biocLite.R')     
biocLite('org.Hs.eg.db')
```
Also, if you do not have a package that geneScrapeR depends on you may get an error as it installs. If that happens you will have to install that package separately before you can successfully install geneScrapeR.
To install geneScrapeR
```{r,eval = FALSE}
devtools::install_github('Evatar/geneScrapeR')
```
