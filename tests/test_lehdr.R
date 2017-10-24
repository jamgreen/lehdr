library(testthat)
library(lehdr)
library(dplyr)

context("Testing lehdr w/ OR data")


test_that("test grab lodes od", {
  expect_equal(grab_lodes(state = 'or', year = 2014, lodes_type = "od", job_type = "JT01", 
                          segment = "SA01", state_part = "main", tract = TRUE) %>% 
                 dim, c(223119, 14))
  expect_equal(grab_lodes(state = "or", year = "2015", lodes_type = "od", job_type = "JT01", 
                          segment = "SA01", state_part = "main" ) %>% 
                 dim, c(1410831, 15))
})

test_that("test grab lodes rac", {
  expect_equal(grab_lodes(state = 'or', year = 2014, lodes_type = "rac", job_type = "JT01", 
                          segment = "SA01", tract = TRUE) %>% 
                 dim, c(834, 44))
  expect_equal(grab_lodes(state = "or", year = "2015", lodes_type = "rac", job_type = "JT01", 
                          segment = "SA01") %>% 
                 dim, c(60226, 45))
})

test_that("test grab lodes wac", {
  expect_equal(grab_lodes(state = 'or', year = 2014, lodes_type = "wac", job_type = "JT01", 
                          segment = "SA01", tract = TRUE) %>% 
                 dim, c(825, 54))
  expect_equal(grab_lodes(state = "or", year = "2015", lodes_type = "wac", job_type = "JT01", 
                          segment = "SA01") %>% 
                 dim, c(23367, 55))
})

test_that("test grab lodes od for multiple states and years", {
  expect_equal(grab_lodes(state = c('or', "ri"), year = c(2013, 2014), lodes_type = "od", job_type = "JT01", 
                          segment = "SA01", state_part = "main", tract = TRUE) %>% 
                 dim, c(511071, 14))
  expect_equal(grab_lodes(state = c('or', "ri"), year = c(2013, 2014), lodes_type = "od", job_type = "JT01", 
                          segment = "SA01", state_part = "main" ) %>% 
                 dim, c(3326655, 15))
})

test_that("test grab lodes wac for multiple states and years", {
  expect_equal(grab_lodes(state = c('or', "ri"), year = c(2013, 2014), lodes_type = "wac", job_type = "JT01", 
                          segment = "SA01", tract = TRUE) %>% 
                 dim, c(2132, 54))
  expect_equal(grab_lodes(state = c('or', "ri"), year = c(2013, 2014), lodes_type = "wac", job_type = "JT01", 
                          segment = "SA01") %>% 
                 dim, c(55307, 55))
})