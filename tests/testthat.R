Sys.setenv("R_TESTS" = "")

library(testthat)
library(genescraper)

test_check("genescraper")
