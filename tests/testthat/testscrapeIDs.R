library (geneScrapeR)

test_that ('scrapeIDs returns the correct ids in the correct form', {

  pmids <- scrapeIDs (dataBase = 'pubmed',
                      term = 'maladaptive daydreaming[Title/Abstract]')

  # IDs returned from the search 'maladaptive daydreaming[Title/Abstract]' on 06/15/2017
  basepmids <- c ("28598955", "27082138", "27002749", "26943233", "26707384")

  # Check that the new search will contain the same five articles in basepmids
  expect_that (sum (pmids %in% basepmids) == 5,
               is_true ())

  # Check if the output from scrapeIDs is a list
  expect_that (str (pmids),
               prints_text ('chr'))

})


