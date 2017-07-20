scrapePubTator <- function (IDs) {

  rawPTOutput <- getURL(str_c('https://www.ncbi.nlm.nih.gov/CBBresearch/Lu/Demo/RESTful/tmTool.cgi/BioConcept/',
                              IDs,
                              '/PubTator'))

  listPTOutput <- unlist(str_split(rawPTOutput,
                                   '\n'))

  PTMatrix <- NULL

  for (e in 3:length(listPTOutput)) {

    PTElements <- unlist(str_split(listPTOutput[e], '\t'))

    if (length(PTElements) == 5) {

      PTElements <- c(PTElements, '-')

    }

    PTMatrix <- rbind(PTMatrix, PTElements)

  }

  if (ncol(PTMatrix) == 6) {

    PTGenes <- NULL

    for (v in 1:length(PTMatrix[, 1])) {

      if (PTMatrix[v, 5] == 'Gene') {

        PTGenes <- c(PTGenes, PTMatrix[v, 6])

      }

    }

    uniqueGenes <- base::unique(PTGenes)

  } else {

    return (NULL)

  }

  return (list(uniqueGenes))

}

