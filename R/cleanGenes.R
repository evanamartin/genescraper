#' Clean genes
#'
#' Converts the entrez gene ids into the approved gene symbol and
#' counts the number of times each gene is mentioned.
#'
#' @param geneList A list containing the Entrez gene ids for all of the
#' genes found in each article.
#'
#' @return A tibble. The first column is the gene name and
#' the second column is the number of times that gene is mentioned.
#'
#' @examples
#' pmids <- scrapeIDs(dataBase = 'pubmed',
#'                    term = '(vivax malaria[MeSH Terms]) AND (folic acid antagonists[MeSH Terms])')
#'
#' geneids <- extractGenes(IDs = pmids,
#'                         nCores = 2)
#'
#' geneNames <- cleanGenes(geneList = geneids)
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
#' @importFrom purrr map map_chr map_int map_lgl
#'
cleanGenes <- function (geneList) {

  # If value and geneID aren't set to NULL a warning is returned
  # when building the package
  value <- geneID <- NULL

  # Get current human gene symbols and names from org.Hs.eg.db to match
  # the entrez gene id to the gene symbol and name
  symbolHsDF <- AnnotationDbi::as.data.frame(org.Hs.egSYMBOL)
  nameHsDF <- AnnotationDbi::as.data.frame(org.Hs.egGENENAME)
  keyHsDF <- tibble('geneID' = symbolHsDF[, 1],
                    'geneSymbol' = symbolHsDF[, 2],
                    'geneName' = nameHsDF[, 2])

  # Get current mouse gene symbols and names from org.Mm.eg.db to match
  # the entrez gene id to the gene symbol and name
  symbolMmDF <- AnnotationDbi::as.data.frame(org.Mm.egSYMBOL)
  nameMmDF <- AnnotationDbi::as.data.frame(org.Mm.egGENENAME)
  keyMmDF <- tibble('geneID' = symbolMmDF[, 1],
                    'geneSymbol' = symbolMmDF[, 2],
                    'geneName' = nameMmDF[, 2])

  # By unlisting geneList all of the NULL elements are removed
  geneList <- unlist(geneList)

  # Separete human gene ids from all other gene ids
  isHuman <- geneList %>%
    map_lgl(~isTRUE(grep(str_c('^', ., '$'), keyHsDF[[1]]) >= 1))

  # Select mouse gene ids from the ids that returned FALSE in isHuman
  isMouse <- geneList[!isHuman] %>%
    map_lgl(~isTRUE(grep(str_c('^', ., '$'), keyMmDF[[1]]) >= 1))

  # Separate human and mouse gene ids to report them in different tibbles
  humanIDs <- geneList[c(isHuman)]

  # To get the mouse gene ids from geneList first filter geneList when isHuman
  # is FALSE because isMouse is not the same length as geneList. Then filter
  # these indices when isMouse is TRUE.
  mouseIDs <- geneList[c(!isHuman)]
  mouseIDs <- mouseIDs[c(isMouse)]

  # Get the counts of each human gene id and order them in
  # descending order by count
  geneHsDF <- humanIDs %>%
    as_tibble() %>%
    dplyr::rename(geneID = value) %>%
    dplyr::count(geneID) %>%
    dplyr::arrange(dplyr::desc(n))

  # Match each human gene id with its symbol, name, and tissue it is expressed in.
  # Take only the first element returned by grep because of potential duplicate
  # gene symbols in the org.Hs.egSYMBOL data base.
  geneIDX <- geneHsDF[[1]] %>%
    map_int(~grep(str_c('^', ., '$'), keyHsDF[[1]]))
  geneHsDF[, 3] <- keyHsDF[geneIDX, 2]
  geneHsDF[, 4] <- keyHsDF[geneIDX, 3]
  geneHsDF[, 5] <- tissueDF[geneIDX, 2]

  # Get the counts of mouse gene ids and place them in descending order
  geneMmDF <- mouseIDs %>%
    as_tibble() %>%
    dplyr::rename(geneID = value) %>%
    dplyr::count(geneID) %>%
    dplyr::arrange(dplyr::desc(n))

  # Match each mouse gene id with its symbol and name.
  # Tissues will be added in the future.
  # Take only the first element returned by grep because of  potential
  # duplicate gene symbols in the org.Mm.egSYMBOL data base.
  geneMmIDX <- geneMmDF[[1]] %>%
    map_int(~grep(str_c('^', ., '$'), keyMmDF[[1]]))
  geneMmDF[, 3] <- keyMmDF[geneMmIDX, 2]
  geneMmDF[, 4] <- keyMmDF[geneMmIDX, 3]

  return (list(human = geneHsDF, mouse = geneMmDF))

}
