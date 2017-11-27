
library(testthat)

source('code/functions.R')

sink('output/test-reporter.txt')
  test_file('code/tests.R')
sink()