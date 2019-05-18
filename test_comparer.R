source("rating_fctn.R")

context("test check_diff")

test_that("test check_diff", {
  expect_equal(check_diff(1,2), 100)
  expect_equal(check_diff(1,2.5), 0)
})


context("test get_mean_of_diffs")

test_that("test get_mean_of_diffs", {
  expect_equal(get_mean_of_diffs(1, 2, 2.5, 1,5), 50)
  expect_equal(get_mean_of_diffs(1, 2, 1,5, NA, NA), 100*2/3)
  expect_equal(get_mean_of_diffs(1, 2, NA), 100)
  expect_equal(get_mean_of_diffs(1.5, 2, 3), 50)
  expect_equal(get_mean_of_diffs(1, NA), NA)
})


context("test get_diff_to_gold_if_available")

test_that("get_diff_to_gold_if_available", {
  expect_equal(get_diff_to_gold_if_available(1, 2, 3,4), 100)
  expect_equal(get_diff_to_gold_if_available(NA, 2, 3,4), 50)
})


context("test add_reliability_column")

testdata <- data.frame( "id"        = c(1,1,1,2,2,3),
                        "condition" = c(1,2,1,2,1,2),
                        "standard"  = c(1,2,3,4,NA,NA),
                        "rater1"    = c(0.5,2, 4.5, 3.5, 1, 1.5),
                        "rater2"    = c(2.5, 0.5, 3.5, 4, 2, 3),
                        "rater3"    = c(NA, NA, 3, 4, NA, 2)
                       )

test_that("test add_reliability_column", {
  expect_equal(add_reliability_column(testdata, "standard", "rater1", "rater1_rel", "rater2", "rater3")$rater1_rel, c(100,100,0,100,100,50))
  expect_equal(add_reliability_column(testdata, "standard", "rater2", "rater2_rel", "rater1", "rater3")$rater2_rel, c(0,0,100,100,100,50))
  expect_equal(add_reliability_column(testdata, "standard", "rater3", "rater3_rel", "rater1", "rater2")$rater3_rel, c(NA,NA,100,100,NA,100))
})


context("test add_gold_rated_column")

test_that("test add_gold_rated_column", {
  expect_equal(add_gold_rated_column(testdata,"standard")$gold_has_rated, c(TRUE,TRUE,TRUE,TRUE,FALSE,FALSE))
})


context("test get_rating_valid")

expectedResult <- data.frame(
  "id" = c(1,1,2,2,3),
  "condition" = c(1,2,1,2,2),
  "rater1_reliability" = c(50,100,100,100,0),
  "rater2_reliability" = c(50,0,100,100,0),
  "rater3_reliability" = c(100,NA,NA,100,100),
  "gold_has_rated"     = c(1,1,0,1,0)
  )
expectedResultGroupById <- data.frame(
  "id" = c(1,2,3),
  "rater1_reliability" = c(200/3,100,0),
  "rater2_reliability" = c(100/3,100,0),
  "rater3_reliability" = c(100,100,100),
  "gold_has_rated"     = c(1,0.5,0)
)
expectedResultGroupByCondition <- data.frame(
  "condition" = c(1,2),
  "rater1_reliability" = c(200/3,200/3),
  "rater2_reliability" = c(200/3,100/3),
  "rater3_reliability" = c(100,100),
  "gold_has_rated"     = c(2/3,2/3)
)

test_that("test get_rating_valid", {
  expect_equal(suppressWarnings(get_rating_valid(testdata,"standard", c("rater1","rater2","rater3"))), expectedResult)
  expect_equal(suppressWarnings(get_rating_valid(testdata,"standard", c("rater1","rater2","rater3"),grouping_vars = "id")), expectedResultGroupById)
  expect_equal(suppressWarnings(get_rating_valid(testdata,"standard", c("rater1","rater2","rater3"),grouping_vars = "condition")), expectedResultGroupByCondition)
})

