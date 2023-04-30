test_that("test grab lodes od", {
  expect_equal(grab_lodes(state = "or", 
                          year = 2014, 
                          version = "LODES7", 
                          lodes_type = "od", 
                          state_part = "main", 
                          agg_geo = "tract") %>% 
                 dim, c(231320, 14))
  expect_equal(grab_lodes(state = "or", 
                          year = 2020,
                          version = "LODES8",
                          lodes_type = "od", 
                          job_type = "JT00", 
                          segment = "SA01", 
                          state_part = "main", 
                          agg_geo = "tract") %>% 
                 dim, c(281967, 14))
  expect_equal(grab_lodes(state = "or", 
                          year = 2009,
                          version = "LODES5",
                          lodes_type = "od",
                          state_part = "main", 
                          agg_geo = "tract") %>% 
                 dim, c(191561, 14))
  expect_equal(grab_lodes(state = "or", 
                          year = "2015",
                          version = "LODES7",
                          lodes_type = "od", 
                          job_type = "JT01", 
                          segment = "SA01", 
                          state_part = "main") %>% 
                 dim, c(1410831, 15))
  expect_equal(grab_lodes(state = "or", 
                          year = "2015",
                          version = "LODES8",
                          lodes_type = "od", 
                          job_type = "JT00", 
                          segment = "SE01", 
                          state_part = "main") %>% 
                 dim, c(1506773, 15))
  expect_equal(grab_lodes(state = "or", 
                          year = "2009",
                          version = "LODES5",
                          lodes_type = "od", 
                          state_part = "aux") %>% 
                 dim, c(89861, 15))
})

test_that("test grab lodes rac", {
  expect_equal(grab_lodes(state = "or", 
                          year = 2014,
                          version = "LODES7",
                          lodes_type = "rac", 
                          agg_geo = "tract") %>% 
                 dim, c(834, 44))
  expect_equal(grab_lodes(state = "or", 
                          year = 2020,
                          version = "LODES8",
                          lodes_type = "rac", 
                          agg_geo = "tract") %>% 
                 dim, c(994, 44))
  expect_equal(grab_lodes(state = "or", 
                          year = "2015", 
                          version = "LODES8",
                          lodes_type = "rac") %>% 
                 dim, c(73113, 45))
  expect_equal(grab_lodes(state = "or", 
                          year = "2004",
                          version = "LODES7",
                          lodes_type = "rac", 
                          job_type = "JT01", 
                          segment = "SA01") %>% 
                 dim, c(61015, 45))
})

test_that("test grab lodes wac", {
  expect_equal(grab_lodes(state = "or", 
                          year = 2014,
                          version = "LODES7",
                          lodes_type = "wac", 
                          job_type = "JT01", 
                          agg_geo = "tract") %>% 
                 dim, c(826, 54))
  expect_equal(grab_lodes(state = "or", 
                          year = 2009,
                          version = "LODES5",
                          lodes_type = "wac", 
                          job_type = "JT01", 
                          agg_geo = "tract") %>% 
                 dim, c(754 , 42))
  expect_equal(grab_lodes(state = "or", 
                          year = "2015",
                          version = "LODES7",
                          lodes_type = "wac") %>% 
                 dim, c(36067, 55))
  expect_equal(grab_lodes(state = "or", 
                          year = "2020", 
                          version = "LODES8",
                          lodes_type = "wac") %>% 
                 dim, c(38785, 55))
})

test_that("test grab lodes od for multiple states and years", {
  expect_equal(grab_lodes(state = c("or", "ri"), 
                          year = c(2013, 2014),
                          version = "LODES7",
                          lodes_type = "od", 
                          job_type = "JT01", 
                          segment = "SA01", 
                          state_part = "aux", 
                          agg_geo = "tract") %>% 
                 dim, c(162573, 14))
  expect_equal(grab_lodes(state = c("wa", "ri"), 
                          year = c(2007, 2008),
                          version = "LODES5",
                          lodes_type = "od", 
                          job_type = "JT01", 
                          segment = "SA01", 
                          state_part = "main", 
                          agg_geo = "tract") %>% 
                 dim, c(853294, 14))
  expect_equal(grab_lodes(state = c("or", "ri"), 
                          year = c(2013, 2014), 
                          version = "LODES8",
                          lodes_type = "od", 
                          job_type = "JT01", 
                          segment = "SA01", 
                          state_part = "main") %>% 
                 dim, c(3347967, 15))
  expect_equal(grab_lodes(state = c("or", 'md'), 
                          year = c(2013, 2020),
                          version = "LODES8",
                          lodes_type = "od", 
                          job_type = "JT01", 
                          segment = "SA01", 
                          state_part = "main") %>% 
                 dim, c(6384095, 15))
})

test_that("test grab lodes wac for multiple states and years", {
  expect_equal(grab_lodes(state = c("or", "ri"), 
                          year = c(2013, 2014),
                          version = "LODES7",
                          lodes_type = "wac", 
                          agg_geo = "tract") %>% 
                 dim, c(2134, 54))
  expect_equal(grab_lodes(state = c("or", "ri"), 
                          year = c(2007, 2009), 
                          version = "LODES5",
                          lodes_type = "wac", 
                          agg_geo = "tract") %>% 
                 dim, c(1974, 42))
  expect_equal(grab_lodes(state = c("or", "ri"), 
                          year = c(2013, 2014), 
                          version = "LODES7",
                          lodes_type = "wac") %>% 
                 dim, c(86037, 55))
  expect_equal(grab_lodes(state = c("or", "ri"), 
                          year = c(2017, 2018, 2019, 2020), 
                          # version = "LODES8",
                          lodes_type = "wac", 
                          job_type = "JT01", 
                          segment = "S000") %>%
                 dim, c(204443, 55))
})