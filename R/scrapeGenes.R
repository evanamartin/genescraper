#' Scrape Genes
#'
#' Mines the genes mentioned in each of the articles from \code{scrapeIDs}.
#'
#' @param IDs A list of article IDs from the NCBI website.
#' @param nCores An integer for the number of cores to use to mine the NCBI articles.
#' @param nArticles An integer for the number of articles to be mined.
#'
#' @return A tibble. The first column is the gene name and
#' the second column is the number of times that gene is mentioned.
#'
#' @examples
#' pmids <- scrapeIDs (dataBase = 'pubmed',
#'                     term = '(vivax malaria[MeSH]) AND (folic acid antagonists[MeSH])')
#'
#' geneNames <- scrapeGenes (IDs = pmids,
#'                           nCores = 2,
#'                           nArticles = length (pmids))
#'
#' @export
#'
#' @import dplyr
#' @import foreach
#' @import magrittr
#' @import org.Hs.eg.db
#' @import tibble
#'
#' @importFrom AnnotationDbi keys
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel makeCluster stopCluster
#' @importFrom pubmed.mineR pubtator_function
#' @importFrom stringr str_to_lower
#'
scrapeGenes <- function (IDs,
                          nCores,
                          nArticles) {

  clusters <- makeCluster (nCores)
  registerDoParallel (clusters)

  i <- value <- geneNames <- NULL

  # genes is a list to hold the names of the genes found in the abstract of each article that matched the search.
  # Loop through each article individually to extract the names of the genes in the abstarct.
  genes <- foreach (i = 1:nArticles, .export = 'pubtator_function') %dopar% {

    # Use try() to return a try-error if the pubtator_function doesn't communicate with the pubmed website.
    pubtatorOutput <- try (pubtator_function (IDs[i]),
                           silent = TRUE)

    # Continue to try to retrive data from pubmed until it is successful.
    counter_i <- 0

    while (inherits (pubtatorOutput,
                     'try-error') == TRUE) {

      pubtatorOutput <- try (pubtator_function (IDs[i]),
                             silent = TRUE)
      counter_i <- counter_i + 1

      if (counter_i > 5) {

        stop ('Unknown SSL protocol error in connection to www.ncbi.nlm.nih.gov:443')

      }

    }

    # Check if the pubtator_function returns any data.
    listCheck <- is.list (pubtatorOutput)

    if (listCheck == TRUE) {

      # Check if there is a gene mentioned in the abstract.
      geneCheck <- is.null (pubtatorOutput$Genes)

      if (geneCheck == TRUE) {

        0

      } else {

        pubtatorOutput$Genes

      }

    } else {

      0

    }

  }

  stopCluster(clusters)

  uniqueGenes <- genes %>%
    lapply (str_to_lower) %>% # Convert all letters to lower case
    lapply (unique) %>% # Keep only one mention of each gene from each article
    unlist () %>%
    as_tibble () %>%
    dplyr::rename (geneNames = value) %>%
    filter (geneNames %in% tolower (keys (org.Hs.egSYMBOL2EG))) %>% # Only keep the genes that match genes lised in the org.Hs.egSYMBOL2EG database.
    count (geneNames) %>%
    mutate (geneNames = factor (geneNames, geneNames),
            geneNames = toupper (geneNames))

  return (uniqueGenes)

}
