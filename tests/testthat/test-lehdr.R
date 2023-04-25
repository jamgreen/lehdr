test_that("test grab lodes od", {
  expect_equal(grab_lodes(state = 'or', year = 2014, lodes_type = "od", job_type = "JT01", 
                          segment = "SA01", state_part = "main", agg_geo = "tract") %>% 
                 dim, c(261761, 14))
  expect_equal(grab_lodes(state = "or", year = "2015", lodes_type = "od", job_type = "JT01", 
                          segment = "SA01", state_part = "main" ) %>% 
                 dim, c(1409076, 15))
})

test_that("test grab lodes rac", {
  expect_equal(grab_lodes(state = 'or', year = 2014, lodes_type = "rac", job_type = "JT01", 
                          segment = "SA01", agg_geo = "tract") %>% 
                 dim, c(1001, 44))
  expect_equal(grab_lodes(state = "or", year = "2015", lodes_type = "rac", job_type = "JT01", 
                          segment = "SA01") %>% 
                 dim, c(57007, 45))
})

test_that("test grab lodes wac", {
  expect_equal(grab_lodes(state = 'or', year = 2014, lodes_type = "wac", job_type = "JT01", 
                          segment = "SA01", agg_geo = "tract") %>% 
                 dim, c(992, 54))
  expect_equal(grab_lodes(state = "or", year = "2015", lodes_type = "wac", job_type = "JT01", 
                          segment = "SA01") %>% 
                 dim, c(23611, 55))
})

test_that("test grab lodes od for multiple states and years", {
  expect_equal(grab_lodes(state = c('or', "ri"), year = c(2013, 2014), lodes_type = "od", job_type = "JT01", 
                          segment = "SA01", state_part = "main", agg_geo = "tract") %>% 
                 dim, c(590935, 14))
  expect_equal(grab_lodes(state = c('or', "ri"), year = c(2013, 2014), lodes_type = "od", job_type = "JT01", 
                          segment = "SA01", state_part = "main" ) %>% 
                 dim, c(3347967, 15))
})

test_that("test grab lodes wac for multiple states and years", {
  expect_equal(grab_lodes(state = c('or', "ri"), year = c(2013, 2014), lodes_type = "wac", job_type = "JT01", 
                          segment = "SA01", agg_geo = "tract") %>% 
                 dim, c(2478, 54))
  expect_equal(grab_lodes(state = c('or', "ri"), year = c(2013, 2014), lodes_type = "wac", job_type = "JT01", 
                          segment = "SA01") %>% 
                 dim, c(59718, 55))
})


test_that("test using LODES7 for RAC", {
  expect_equal(grab_lodes(state = 'or', year = 2014, version = "LODES7", lodes_type = "rac", job_type = "JT01", 
                          segment = "SA01", agg_geo = "tract") %>% 
                 dim, c(834, 44))
})