#' Extract Genes
#'
#' Extracts the gene ids from the information scraped from the PubTator website.
#'
#' @param IDs A list of article IDs from the NCBI website.
#' @param nCores An integer for the number of cores to use to mine the NCBI articles.
#'
#' @return A list. Each element in the list is also a list. It contains the
#' entrez gene ids for all the genes found in each abstract.
#'
#' @examples
#' pmids <- scrapeIDs (dataBase = 'pubmed',
#'                     term = '(vivax malaria[MeSH]) AND (folic acid antagonists[MeSH])')
#'
#' geneNames <- extractGenes (IDs = pmids,
#'                            nCores = 2)
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
                         nCores = 2) {

  clusters <- makeCluster (nCores)
  registerDoParallel (clusters)

  i <- value <- geneNames <- NULL

  genes <- foreach (i = seq_along(IDs),
                    .export = c('getURL',
                                'scrapePubTator',
                                'str_c',
                                'str_split')) %dopar% {

                                  scrapePubTator(IDs[i])

                                }

  stopCluster(clusters)

  return (genes)

}
