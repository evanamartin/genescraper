library (geneScrapeR)

test_that ('A tibble with two columns is returned', {

  pmids <- scrapeIDs (dataBase = 'pubmed',
                      term = 'maladaptive daydreaming[Title/Abstract]')

  geneids <- extractGenes(IDs = pmids,
                          nCores = 2)

  geneSymbols <- cleanGenes(geneids)

  expect_that (str (geneSymbols),
               prints_text ('tbl_df'))

  expect_that (dim (geneSymbols)[2],
               is_equivalent_to (2))

})

