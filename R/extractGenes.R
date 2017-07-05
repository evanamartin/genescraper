#' Extract Genes
#'
#' Extracts the gene ids from the information scraped from the PubTator website.
#'
#' @param IDs A list of article IDs from the NCBI website.
#' @param nCores An integer for the number of cores to use to mine the NCBI articles.
#' @param nTries An integer for the number of times to attempt to connect to the NCBI website.
#'
#' @return A list. Each element in the list is also a list. It contains the
#' entrez gene ids for all the genes found in each abstract.
#'
#' @examples
#' pmids <- scrapeIDs (dataBase = 'pubmed',
#'                     term = '(vivax malaria[MeSH]) AND (folic acid antagonists[MeSH])')
#'
#' geneNames <- extractGenes (IDs = pmids,
#'                            nCores = 2,
#'                            nTries = 5)
#'
#' @export
#'
#' @import dplyr
#' @import foreach
#' @import magrittr
#' @import stringr
#' @import tibble
#'
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel makeCluster stopCluster
#' @importFrom RCurl getURL
#'
extractGenes <- function (IDs,
                         nCores = 2,
                         nTries = 5) {

  clusters <- makeCluster (nCores)
  registerDoParallel (clusters)

  i <- value <- geneNames <- NULL

  genes <- foreach (i = seq_along(IDs),
                    .export = c('getURL',
                                'scrapePubTator',
                                'str_c',
                                'str_split')) %dopar% {

                                  # Use try() to return a try-error if the pubtator_function doesn't communicate with the pubmed website.
                                  pubtatorOutput <- try (scrapePubTator (IDs[i]),
                                                         silent = TRUE)

                                  # Continue to try to retrive data from pubmed until it is successful.
                                  counter_i <- nTries

                                  while (inherits (pubtatorOutput,
                                                   'try-error') == TRUE) {

                                    pubtatorOutput <- try (scrapePubTator (IDs[i]),
                                                           silent = TRUE)

                                    counter_i <- counter_i + 1

                                    if (counter_i > nTries) {

                                      stop ('Unknown SSL protocol error in connection to www.ncbi.nlm.nih.gov:443')

                                    }

                                  }

                                  pubtatorOutput

                                }

  stopCluster(clusters)

  return (genes)

}
