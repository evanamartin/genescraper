#' Clean genes
#'
#' Converts the entrez gene ids into the approved gene symbol and
#' counts the number of times each gene is mentioned.
#'
#' @param geneList A list containing the entrez gene IDs for all of the
#' genes found in each article.
#'
#' @return A tibble. The first column is the gene name and
#' the second column is the number of times that gene is mentioned.
#'
#' @examples
#' pmids <- scrapeIDs (dataBase = 'pubmed',
#'                     term = '(vivax malaria[MeSH]) AND (folic acid antagonists[MeSH])')
#'
#' geneids <- extractGenes (IDs = pmids,
#'                          nCores = 2)
#'
#' geneNames <- cleanGenes (geneList = geneids)
#'
#' @export
#'
#' @import dplyr
#' @import magrittr
#' @import tibble
#'
cleanGenes <- function (geneList) {

  value <- geneSymbol <- NULL

  geneTibble <- geneList %>%
    unlist () %>%
    lapply (function (x) geneConversion[grep (str_c ('^', x, '$'), geneConversion[[2]]), c(1, 3, 4)]) %>%
    unlist () %>%
    as_tibble () %>%
    dplyr::rename (geneSymbol = value) %>%
    dplyr::count (geneSymbol) %>%
    dplyr::arrange (-n) %>%
    dplyr::mutate (geneSymbol = factor (x = geneSymbol))

  return (geneTibble)

}
