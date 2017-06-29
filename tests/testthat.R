Sys.setenv("R_TESTS" = "")

library(testthat)
library(geneScrapeR)

test_check("geneScrapeR")
