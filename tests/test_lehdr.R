library(testthat)
library(lehdr)
library(dplyr)

context("Testing lehdr w/ OR data")

test_that("test grabbing wac", {
  expect_equal(grab_wac('or', year=2014) %>% dim, c(35219, 55))
  expect_equal(grab_wac('or', year=2014, tract=TRUE) %>% dim, c(826, 54))
})

test_that("test grabbing rac", {
  expect_equal(grab_rac('or', year=2014) %>% dim, c(77936, 45))
  expect_equal(grab_rac('or', year=2014, tract=TRUE) %>% dim, c(834, 44))
})

test_that("test grabbing od", {
  expect_equal(grab_od('or', year=2014) %>% dim, c(1448143, 14))
  expect_equal(grab_od('or', year=2014, tract=TRUE) %>% dim, c(231320, 13))
})
