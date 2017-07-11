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
#'                     term = '(vivax malaria[MeSH Terms]) AND (folic acid antagonists[MeSH Terms])')
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
#' @import org.Hs.eg.db
#' @import tibble
#'
#' @importFrom AnnotationDbi as.data.frame
#' @importFrom purrr map_chr map_int map_lgl
#'
cleanGenes <- function (geneList) {

  value <- geneSymbol <- NULL

  symbolDF <- AnnotationDbi::as.data.frame (org.Hs.egSYMBOL)
  nameDF <- AnnotationDbi::as.data.frame (org.Hs.egGENENAME)
  keyDF <- cbind (symbolDF, 'gene_name' = nameDF[, 2])

  geneList <- unlist(geneList)

  # Return FALSE for the genes that aren't human
  isHuman <- geneList %>%
    unlist () %>%
    map_lgl (~isTRUE(grep (str_c ('^', ., '$'), keyDF[, 1]) >= 1))

  humanIDs <- geneList[c (isHuman)]

  geneDF <- humanIDs %>%
    map_chr (~keyDF[grep (str_c ('^', ., '$'), keyDF[, 1]), 2]) %>%
    as_tibble () %>%
    dplyr::rename (geneSymbol = value) %>%
    dplyr::count (geneSymbol) %>%
    dplyr::arrange (-n) %>%
    dplyr::mutate (geneSymbol = factor (x = geneSymbol))

  geneIDX <- map_int (geneDF[[1]], ~grep (str_c ('^', geneDF[[1]][.], '$'), keyDF[, 2]))
  geneDF[, 3] <- as.character (keyDF[geneIDX, 3])
  geneDF[, 4] <- as.character(tissueDF[geneIDX, 2])

  names(geneDF) <- c('geneSymbol', 'n', 'geneName', 'tissue')

  return (geneDF)

}
