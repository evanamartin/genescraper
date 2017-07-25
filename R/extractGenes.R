#' Extract Genes
#'
#' Extracts the gene ids from the information scraped from the PubTator website.
#'
#' @param IDs A list of article ids from the NCBI website.
#' @param nCores An integer for the number of cores to use to mine the NCBI articles.
#' @param nTries An integer for the number of times to attempt to connect to the NCBI website.
#'
#' @return A list. Each element in the list is also a list. It contains the
#' entrez gene ids for all the genes found in each abstract.
#'
#' @examples
#' pmids <- scrapeIDs(dataBase = 'pubmed',
#'                    term = '(vivax malaria[MeSH Terms]) AND (folic acid antagonists[MeSH Terms])')
#'
#' geneNames <- extractGenes(IDs = pmids,
#'                           nCores = 2,
#'                           nTries = 5)
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

  clusters <- makeCluster(nCores)
  registerDoParallel(clusters)

  # A warning is thrown when checking the package if the following values are
  # not set to NULL before they are created.
  i <- value <- geneNames <- NULL

  genes <- foreach(i = seq_along(IDs),
                   .export = c('getURL',
                               'map',
                               'scrapePubTator',
                               'seq_along',
                               'str_c',
                               'str_split')) %dopar% {

                                 # The NCBI website will occasionally throw
                                 # an error when the scrapePubTator function
                                 # tries to extract information from it.
                                 pubtatorOutput <- try (scrapePubTator(IDs[i]),
                                                        silent = TRUE)

                                 # Continue to try to scrape data from NCBI
                                 counter_i <- nTries
                                 while (inherits(pubtatorOutput,
                                                 'try-error') == TRUE) {

                                   pubtatorOutput <- try (scrapePubTator(IDs[i]),
                                                          silent = TRUE)

                                   counter_i <- counter_i + 1

                                   if (counter_i > nTries) {

                                     stop ('Unknown SSL protocol error in connection to www.ncbi.nlm.nih.gov:443')

                                   }

                                 }

                                 pubtatorOutput

                               }

  # All sorts of things go terribly wrong when checking the package and
  # creating the vignettes if stopCluster isn't used at the end of the function.
  stopCluster(clusters)

  return (genes)

}
