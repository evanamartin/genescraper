context ('extractGenes')

test_that ('extractGenes returns a list of the correct length', {

  geneids <- extractGenes (IDs = c(28614680, 28614679, 28538811, 28443495, 28433972),
                           nCores = 2,
                           nTries = 5)

  # Check that the new search will contain the same five article ids that are in basepmids
  expect_that (str (geneids),
               prints_text ('List of'))

  # Check that the list is the correct length
  expect_that (length (geneids) == 5,
               is_true ())

  # Check that NULL is returned when the article output doesn't have the correct form
  expect_that (is.null (extractGenes (IDs = 20275638,
                                      nCores = 1)[[1]]),
               is_true())

})
