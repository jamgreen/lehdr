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


test_that("test grab lodes od", {
  expect_equal(grab_lodes(state = 'or', year = 2014, lodes_type = "od", job_type = "JT01", 
                          segment = "SA01", state_part = "main", tract = TRUE) %>% 
                 dim, c(217792, 13))
  expect_equal(grab_lodes(state = "or", year = "2015", lodes_type = "od", job_type = "JT01", 
                          segment = "SA01", state_part = "main" ) %>% 
                 dim, c(1410831, 15))
})