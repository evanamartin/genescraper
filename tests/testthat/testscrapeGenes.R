library (geneScrapeR)

test_that ('A tibble with two columns is returned', {

  pmids <- scrapeIDs (dataBase = 'pubmed',
                      term = 'maladaptive daydreaming[Title/Abstract]')

  genes <- scrapeGenes(IDs = pmids,
                       nCores = 2,
                       nTries = 5)

  expect_that (str (genes),
               prints_text ('tbl_df'))

  expect_that (dim (genes)[2],
               is_equivalent_to (2))

})

