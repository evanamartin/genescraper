context ('cleanGenes')

test_that ('A tibble with two columns is returned', {

  pmids <- scrapeIDs (dataBase = 'pubmed',
                      term = '(vivax malaria[MeSH]) AND (folic acid antagonists[MeSH])')

  geneids <- extractGenes(IDs = pmids,
                          nCores = 2)

  geneSymbols <- cleanGenes(geneids)

  # Check that a tibble is returned
  expect_that (str (geneSymbols),
               prints_text ('tbl_df'))

  # Check that the tibble has the correct number of columns
  expect_that (dim (geneSymbols)[2],
               is_equivalent_to (2))

})
