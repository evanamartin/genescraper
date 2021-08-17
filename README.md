[![Build Status](https://travis-ci.org/evanamartin/genescraper.svg?branch=master)](https://travis-ci.org/evanamartin/genescraper)
[![Coverage Status](https://img.shields.io/codecov/c/github/evanamartin/genescraper/master.svg?precision=1)](https://codecov.io/github/evanamartin/genescraper?branch=master)

# genescraper

The R package genescraper performs text mining on the PubMed database for genes mentioned in connection with user specified search keywords. It can be used to identify the genes that appear the most often in articles associated with diseases and other medical conditions. It retrieves all genes mentioned in any of the articles associated with the search, counts the number of articles in which each gene appears, and returns the genes and their counts in a table.

The search keywords are used to find all papers in the PubMed database that match any word or combination of words in the search. The NCBI (National Center for Biotechnology Information) search options are all compatible with genescraper. For example, MeSH (Medical Subject Headings) terms; logical operators such as AND, OR, or NOT; dates; quotation marks (used to group words together); and other terms can all be used to refine the search. 

After finding all papers that match the search criteria, genescraper then accesses their title and abstract. Next, genescraper text mines each title and abstract for human or mouse genes. The number of papers that mention each gene are then counted. The genes and their counts are separated by organism (either human or mouse) and reported in two tables: the first contains all human genes and their associated counts and the second contains the mouse genes and their counts.

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
