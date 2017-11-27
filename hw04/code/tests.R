
library(testthat)


context("test remove_missing function")
test_that("remove NA", {
  a <- c(1,2,3,4,5,NA)
  b <- c(NA, 1,2,3,4,5, NA)
  expect_equal(length(remove_missing(a)), length(na.omit(a)))
  expect_false(anyNA(remove_missing(a)))
  expect_equal(remove_missing(a), remove_missing(b))
  expect_false(anyNA(remove_missing(b)))
})



context("test get_minimum function")
test_that("get_minimum", {
  a <- c(1,2,3,4,5)
  b <- c(NA, 1,2,3,4,5,NA)
  c <- c("a", "b")
  expect_equal(get_minimum(a), min(a))
  expect_equal(as.numeric(get_minimum(b, na.rm = F)), min(b))
  expect_equal(get_minimum(a, na.rm = F), min(a))
  expect_error(get_minimum(c))
  expect_that(get_minimum(a), is_a("numeric"))
})



context("test get_maximum function")
test_that("get_maximum", {
  a <- c(1,2,3,4,5)
  b <- c(NA, 1,2,3,4,5,NA)
  c <- c("a", "b")
  expect_equal(get_maximum(a), max(a))
  expect_equal(as.numeric(get_maximum(b, na.rm = F)), max(b))
  expect_equal(get_maximum(a, na.rm = F), max(a))
  expect_error(get_maximum(c))
  expect_that(get_maximum(a), is_a("numeric"))
})



context("test get_range function")
test_that("get_range", {
  a <- c(1,2,3,4,5)
  b <- c(NA, 1,2,3,4,5,NA)
  c <- c("a", "b")
  expect_equal(get_range(a), range(a)[2] - range(a)[1])
  expect_equal(as.numeric(get_range(b, na.rm = F)), range(b)[2] - range(b)[1])
  expect_equal(get_range(a, na.rm = F), range(a)[2] - range(a)[1])
  expect_error(get_range(c))
  expect_that(get_range(a), is_a("numeric"))
})



context("test get_percentile10 function")
test_that("get_percentile10", {
  a <- c(1,4,7,10)
  b <- c(1,2,3,4,5,NA)
  c <- c("a", "b")
  expect_equal(get_percentile10(a), 1.9)
  expect_equal(get_percentile10(b), 1.4)
  expect_equal(get_percentile10(b, na.rm = F), NA)
  expect_that(get_percentile10(a), is_a("numeric"))
  expect_error(get_percentile10(c))
  expect_equal(get_percentile10(a, na.rm = F), 1.9)
})



context("test get_percentile90 function")
test_that("get_percentile90", {
  a <- c(1,4,7,10)
  b <- c(1,2,3,4,5,NA)
  c <- c("a", "b")
  expect_equal(get_percentile90(a), 9.1)
  expect_equal(get_percentile90(b), 4.6)
  expect_equal(get_percentile90(b, na.rm = F), NA)
  expect_that(get_percentile90(a), is_a("numeric"))
  expect_error(get_percentile90(c))
  expect_equal(get_percentile90(a, na.rm = F), 9.1)
})



context("test get_median function")
test_that("get_median", {
  a <- c(1,2,3,4,5)
  b <- c(NA, 1,2,3,4,5,NA)
  c <- c("a", "b")
  expect_equal(get_median(a), median(a))
  expect_equal(as.numeric(get_median(b, na.rm = F)), median(b))
  expect_that(get_median(b), is_a("numeric"))
  expect_error(get_median(c))
  expect_equal(get_median(a, na.rm = F), median(a))
})



context("test get_average function")
test_that("get_average", {
  a <- c(1,2,3,4,5)
  b <- c(NA, 1,2,3,4,5,NA)
  c <- c("a", "b")
  expect_equal(get_average(a), mean(a))
  expect_equal(as.numeric(get_average(b, na.rm = F)), mean(b))
  expect_that(get_average(b), is_a("numeric"))
  expect_error(get_average(c))
  expect_equal(get_average(a, na.rm = F), mean(a))
})




context("test get_stdev function")
test_that("get_stdev", {
  a <- c(1,2,3,4,5)
  b <- c(NA, 1,2,3,4,5,NA)
  c <- c("a", "b")
  expect_equal(get_stdev(a), sd(a))
  expect_equal(as.numeric(get_stdev(b, na.rm = F)), sd(b))
  expect_that(get_stdev(b), is_a("numeric"))
  expect_error(get_stdev(c))
  expect_equal(get_stdev(a, na.rm = F), sd(a))
})



context("test get_quartile1 function")
test_that("get_quartile1", {
  a <- c(1,4,7,10)
  b <- c(NA, 1,2,3,4,5,NA)
  c <- c("a", "b")
  expect_equal(get_quartile1(a), 3.25)
  expect_equal(get_quartile1(b, na.rm = F), NA)
  expect_that(get_quartile1(a), is_a("numeric"))
  expect_error(get_quartile1(c))
  expect_equal(get_quartile1(a, na.rm = F), 3.25)
})



context("test get_quartile3 function")
test_that("get_quartile3", {
  a <- c(1,4,7,10)
  b <- c(NA, 1,2,3,4,5,NA)
  c <- c("a", "b")
  expect_equal(get_quartile3(a), 7.75)
  expect_equal(get_quartile3(b, na.rm = F), NA)
  expect_that(get_quartile3(a), is_a("numeric"))
  expect_error(get_quartile3(c))
  expect_equal(get_quartile3(a, na.rm = F), 7.75)
})




context("test count_missing function")
test_that("count_missing", {
  a <- c(1,4,7,10)
  b <- c(NA, 1,2,3,4,5,NA)
  expect_equal(count_missing(a), 0)
  expect_equal(count_missing(b), 2)
  expect_that(count_missing(a), is_a("numeric"))
  expect_that(count_missing(b), is_a("numeric"))
})



context("test summary_stats function")
test_that("count_missing", {
  a <- c(1,4,7,10)
  b <- c(NA, 1,2,3,4,5,NA)
  expect_equal(names(summary_stats(a)), c("minimum", "percent10", 
    "quartile1", "median", "mean", "quartile3",
     "percent90", "maximum", "range", "stdev", "missing"))
  expect_equal(names(summary_stats(b)), c("minimum", "percent10", 
     "quartile1", "median", "mean", "quartile3",
      "percent90", "maximum", "range", "stdev", "missing"))
  expect_true(is.list(summary_stats(a)))
  expect_true(is.list(summary_stats(a)))
})



context("test rescale100 function")
test_that("rescale100", {
  d <- c(18,15,16,4,17,9)
  expect_equal(rescale100(d, xmin = 0, xmax = 20), c(90,75,80,20,85,45))
  expect_true(max(rescale100(d, xmin = 0, xmax = 20)) <= 100)
  expect_true(min(rescale100(d, xmin = 0, xmax = 20)) >= 0)
  expect_true(is.vector(rescale100(d, xmin = 0, xmax = 20)))
})



context("test drop_lowest function")
test_that("drop_lowest", {
  e <- c(10,10,8.5,4,7,9)
  f <- c(10,10,8.5,4,7,9,4)
  expect_equal(drop_lowest(e), c(10.0,10.0, 8.5, 7.0, 9.0))
  expect_equal(drop_lowest(f), c(10.0,10.0, 8.5, 7.0, 9.0, 4.0))
  expect_true(length(drop_lowest(e)) < length(e))
  expect_true(is.vector(drop_lowest(e)))
})



context("test score_homework function")
test_that("score_homework", {
  g <- c(100,80,30,70, 75, 85)
  expect_equal(score_homework(g, drop = T), 82)
  expect_equal(score_homework(g, drop = F), mean(c(100,80,30, 70,75,85)))
  expect_true(score_homework(g) > score_homework(g, drop = F))
  expect_true(is.numeric(score_homework(g)))
})



context("test score_quiz function")
test_that("score_quiz", {
  g <- c(100,80,70,0)
  expect_equal(score_quiz(g, drop = T), mean(c(100,80,70)))
  expect_equal(score_quiz(g, drop = F), 62.5)
  expect_true(score_quiz(g) > score_quiz(g, drop = F))
  expect_true(is.numeric(score_quiz(g)))
})



context("test score_lab function")
test_that("score_lab", {
  expect_equal(score_lab(12), 100)
  expect_equal(score_lab(11), 100)
  expect_equal(score_lab(10), 80)
  expect_equal(score_lab(8), 40)
  expect_equal(score_lab(6), 0)
  expect_equal(score_lab(3), 0)
  expect_equal(score_lab(0), 0)
})

