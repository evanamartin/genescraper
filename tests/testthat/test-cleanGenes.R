context ('cleanGenes')

test_that ('A tibble with two columns is returned', {

  geneids <- extractGenes(IDs = 20810806,
                          nCores = 2,
                          nTries = 5)

  geneSymbols <- cleanGenes(geneids)

  # Check that a tibble is returned
  expect_that (str (geneSymbols$human),
               prints_text ('tbl_df'))

  # mouse: check that a tibble is returned
  expect_that (str (geneSymbols$mouse),
               prints_text ('tbl_df'))

  # Check that the human tibble has the correct number of columns
  expect_that (dim (geneSymbols$human)[2],
               is_equivalent_to (4))

  # Check that the mouse tibble has the correct number of columns
  expect_that (dim (geneSymbols$mouse)[2],
               is_equivalent_to (3))

})
