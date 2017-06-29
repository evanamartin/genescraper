context ('extractGenes')

test_that ('extractGenes returns a list of the correct length', {

  pmids <- scrapeIDs (dataBase = 'pubmed',
                      term = 'maladaptive daydreaming[Title/Abstract]')

  geneids <- extractGenes (IDs = pmids,
                           nCores = 2)

  # Check that the new search will contain the same five article ids that are in basepmids
  expect_that (str (geneids),
               prints_text ('List of'))

  # Check that the list is the correct length
  expect_that (length (pmids) == length (geneids),
               is_true ())

  # Check that NULL is returned when the article output doesn't have the correct form
  expect_that (is.null (extractGenes (IDs = 20275638,
                                      nCores = 1)[[1]]),
               is_true())

})
