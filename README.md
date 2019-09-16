[![Build Status](https://travis-ci.org/evanamartin/genescraper.svg?branch=master)](https://travis-ci.org/evanamartin/genescraper)
[![Coverage Status](https://img.shields.io/codecov/c/github/evanamartin/genescraper/master.svg?precision=1)](https://codecov.io/github/evanamartin/genescraper?branch=master)

# genescraper

genescraper is an R package that mines NCBI databases for genes mentioned in connection with diseases and other medical conditions. It counts the number of articles that mention each gene cited and returns them in a data frame. 

### Motivation

genescraper is meant to make it easy to extract the genes mentioned in articles available in NCBI databases. It returns them in a format that is easy to use and read.

### Installation

It is recommended that you install org.Hs.eg.db and org.Mm.eg.db prior to installing genescraper.
To install them run
```r
source('http://bioconductor.org/biocLite.R')
biocLite('org.Hs.eg.db')
biocLite('org.Mm.eg.db')
```

If you do not have a package that genescraper depends on you may get an error as genescraper installs. Before being able to successfully install genescraper you will have to install the missing package separately. After installing all of the packages that genescraper depends on you will be able to successfully install genescraper.

To install genescraper
```r
devtools::install_github('evanamartin/genescraper')
```
