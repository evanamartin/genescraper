#' Scrape IDs
#'
#' Mines article IDs from NCBI databases according to the seach criteria.
#'
#' @param dataBase A string. Indicates the database from which you want to search.
#' @param term A string. The terms used to search the specified database.
#'
#' @return A list containing all of the ids of the articles that matched the search criteria.
#'
#' @examples
#' pmids2017 <- scrapeIDs(dataBase = 'pubmed',
#'                        term = '(prostate cancer[MeSH Terms]) AND 2017[Date - Publication]')
#'
#' head(pmids2017)
#'
#' @export
#'
#' @importFrom rentrez entrez_search
#'
scrapeIDs <- function (dataBase = 'pubmed',
                       term = 'prostate cancer[MeSH]') {

  # Run an initial search to get the total number of articles that match
  # the search criteria.
  initialRun <- entrez_search(db = dataBase,
                              term = term)

  # Use the number of articles from the first search to return the PubMed id
  # for all of the articles that match the search criteria.
  IDS <- entrez_search(db = dataBase,
                       term = term,
                       retmax = initialRun$count)

  return (IDS$ids)

}
