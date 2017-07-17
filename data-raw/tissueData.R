library (readr)
library (stringr)
library (tibble)
library (dplyr)
library (tidyr)
library (org.Hs.eg.db)

# Get the entrez gene id and gene symbol to link with tissues data
symbolDF <- as.data.frame (org.Hs.egSYMBOL)


### When the normal_tissue.csv file is downloaded from the Human Protein Atlas
### website the first and the last columns must be deleted before running the
### rest of this R script. These columns were deleted before putting the file on
### GitHub to make it smaller than 50 mb.
# read in the csv that contains the tissues that each gene is expressed in
tissues <- read_csv (file = '~/normal_tissue.csv')

# Leave out the genes that don't have any tissues associated with them and
# combine the 'tissue', 'cell type', and 'level' columns
tissues <- tissues %>%
  filter(Level != 'Not detected') %>%
  unite (col = Tissue, Tissue:Level, sep = ':')

Tissues <- vector(mode = 'character', length = length (symbolDF[, 1]))

for (e in 1:length (symbolDF[, 1])) {

  # Get the index of the rows in symbolDF that match each gene in symbolDF
  # and combine the tissue/cell type column into one row
  geneIDX <- grep (str_c ('^', symbolDF[e, 2], '$', sep = ''), tissues[[1]])

  if (length(geneIDX) != 0) {

    temp2 <- NULL

    # condense the list of tissues to one string
    for (v in 1:length (geneIDX)) {

      temp <- tissues[geneIDX[v], 2]
      temp2 <- str_c(temp2, temp, sep = '; ', collapse = '')

    }

    Tissues[[e]] <- temp2

  } else {

    Tissues[[e]] <- 'No Tissues'

  }

  if (e %% 100 == 0) print (e)

}

# Add tissues to the symbolDFs data frame
tissueDF <- tibble (geneSymbol = symbolDF[[2]], 'tissue/cellType/level' = Tissues)

devtools::use_data (tissueDF, internal = TRUE, overwrite = TRUE)
