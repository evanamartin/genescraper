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
#'
cleanGenes <- function (geneList) {

  value <- gene_symbol <- NULL

  symbolDF <- AnnotationDbi::as.data.frame (org.Hs.egSYMBOL)
  nameDF <- AnnotationDbi::as.data.frame (org.Hs.egGENENAME)
  keyDF <- cbind (symbolDF, 'gene_name' = nameDF[, 2])

  geneDF <- geneList %>%
    unlist () %>%
    lapply (function (x) keyDF[grep (str_c ('^', x, '$'), keyDF[, 1]), 2]) %>%
    unlist () %>%
    as_tibble () %>%
    dplyr::rename (gene_symbol = value) %>%
    dplyr::count (gene_symbol) %>%
    dplyr::arrange (-n) %>%
    dplyr::mutate (gene_symbol = factor (x = gene_symbol))

  geneDF[, 3:4] <- NA

  for (v in 1:length (geneDF[[1]])) {

    geneIDX <- grep (str_c ('^', geneDF[[1]][v], '$'), keyDF[, 2])
    geneDF[v, 3] <- as.character (keyDF[geneIDX, 3])
    geneDF[v, 4] <- tissueDF[geneIDX, 2]

  }

  return (geneDF)

}
