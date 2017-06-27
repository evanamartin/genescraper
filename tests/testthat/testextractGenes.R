library (geneScrapeR)

test_that ('A list is returned', {

  pmids <- scrapeIDs (dataBase = 'pubmed',
                      term = 'maladaptive daydreaming[Title/Abstract]')

  geneids <- extractGenes (IDs = pmids,
                           nCores = 2)

  # Expect the new search will contain the same five articles in basepmids
  expect_that (str (geneids),
               prints_text ('List of'))

})
