scrapePubTator <- function (IDs) {

  # Get the text from the PubTator website for the specified NCBI article id
  rawOutput <- getURL(str_c('https://www.ncbi.nlm.nih.gov/CBBresearch/Lu/Demo/RESTful/tmTool.cgi/BioConcept/',
                            IDs,
                            '/PubTator'))

  # Create a list of elements from the PubTator website (title, abstract, ...)
  listOutput <- rawOutput %>%
    str_split('\n') %>%
    unlist() %>%
    map(~str_split(., '\t'))

  # If the following for loop doesn't see any genes return null
  genes <- NULL

  # Loop through each element of listOutput and extract the gene id
  # if there is a gene present
  for (e in seq_along(listOutput)) {

    if (length(listOutput[[e]][[1]]) == 6 && listOutput[[e]][[1]][5] == 'Gene') {

      temp <- listOutput[[e]][[1]][6]
      genes <- c(genes, temp)

    }

  }

  return (unique(genes))

}
