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
#' @import org.Mm.eg.db
#' @import tibble
#'
#' @importFrom AnnotationDbi as.data.frame
#' @importFrom purrr map_chr map_int map_lgl
#'
cleanGenes <- function (geneList) {

  value <- geneSymbol <- NULL

  # Get gene symbols and names from org.Hs.eg.db
  symbolHsDF <- AnnotationDbi::as.data.frame (org.Hs.egSYMBOL)
  nameHsDF <- AnnotationDbi::as.data.frame (org.Hs.egGENENAME)
  keyHsDF <- cbind (symbolHsDF, 'geneName' = nameHsDF[, 2])

  # Get gene symbols and names from org.Mm.eg.db
  symbolMmDF <- AnnotationDbi::as.data.frame (org.Mm.egSYMBOL)
  nameMmDF <- AnnotationDbi::as.data.frame (org.Mm.egGENENAME)
  keyMmDF <- cbind (symbolMmDF, 'geneName' = nameMmDF[, 2])

  # By unlisting geneList all of the NULL elements are removed
  geneList <- unlist (geneList)

  # Return FALSE for the genes that aren't human because only human genes
  # are listed in the keyDF data frame
  isHuman <- geneList %>%
    map_lgl (~isTRUE (grep (str_c ('^', ., '$'), keyHsDF[, 1]) >= 1))

  isMouse <- geneList %>%
    map_lgl (~isTRUE (grep (str_c ('^', ., '$'), keyMmDF[, 1]) >= 1))

  # Separate human and mouse genes to report them in different tibbles
  humanIDs <- geneList[c (isHuman)]
  mouseIDs <- geneList[c (isMouse)]

  # Get the counts of the human genes and order them by count
  geneHsDF <- humanIDs %>%
    map_chr (~keyHsDF[grep (str_c ('^', ., '$'), keyHsDF[, 1]), 2]) %>%
    as_tibble () %>%
    dplyr::rename (geneSymbol = value) %>%
    dplyr::count (geneSymbol) %>%
    dplyr::arrange (-n) %>%
    dplyr::mutate (geneSymbol = factor (x = geneSymbol))

  # Take only the first element returned because of duplicate gene symbols
  # in the org.Hs.egSYMBOL data base.
  geneIDX <- geneHsDF[[1]] %>%
    map_int (~grep (str_c ('^', ., '$'), keyHsDF[, 2])[[1]])
  geneHsDF[, 3] <- as.character (keyHsDF[geneIDX, 3])
  geneHsDF[, 4] <- as.character (tissueDF[geneIDX, 2])

  names (geneHsDF) <- c ('geneSymbol', 'n', 'geneName', 'tissue')

  # Get the counts of the mouse genes and order them by count
  geneMmDF <- mouseIDs %>%
    map_chr (~keyMmDF[grep (str_c ('^', ., '$'), keyMmDF[, 1]), 2]) %>%
    as_tibble () %>%
    dplyr::rename (geneSymbol = value) %>%
    dplyr::count (geneSymbol) %>%
    dplyr::arrange (-n) %>%
    dplyr::mutate (geneSymbol = factor (x = geneSymbol))

  geneMmIDX <- geneMmDF[[1]] %>%
    map_int (~grep (str_c ('^', ., '$'), keyMmDF[, 2])[[1]])
  geneMmDF[, 3] <- as.character (keyMmDF[geneMmIDX, 3])

  names (geneMmDF) <- c ('geneSymbol', 'n', 'geneName')

  return (list (human = geneHsDF, mouse = geneMmDF))

}
