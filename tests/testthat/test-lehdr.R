test_that("test grab lodes od", {
  expect_equal(grab_lodes(state = 'or', year = 2014, lodes_type = "od", job_type = "JT01", 
                          segment = "SA01", state_part = "main", agg_geo = "tract", lodes_ver = "7") %>% 
                 dim, c(223119, 14))
  expect_equal(grab_lodes(state = 'or', year = 2014, lodes_type = "od", job_type = "JT01", 
                          segment = "SA01", state_part = "main", agg_geo = "tract", lodes_ver = "8") %>% 
                 dim, c(261761, 14))
  expect_equal(grab_lodes(state = 'or', year = 2009, lodes_type = "od", job_type = "JT01", 
                          segment = "SA01", state_part = "main", agg_geo = "tract", lodes_ver = "5") %>% 
                 dim, c(185646, 14))
  expect_equal(grab_lodes(state = "or", year = "2015", lodes_type = "od", job_type = "JT01", 
                          segment = "SA01", state_part = "main", lodes_ver = "7") %>% 
                 dim, c(1410831, 15))
  expect_equal(grab_lodes(state = "or", year = "2015", lodes_type = "od", job_type = "JT01", 
                          segment = "SA01", state_part = "main", lodes_ver = "8") %>% 
                 dim, c(1409076, 15))
  expect_equal(grab_lodes(state = "or", year = "2009", lodes_type = "od", job_type = "JT01", 
                          segment = "SA01", state_part = "main", lodes_ver = "5") %>% 
                 dim, c(1246010, 15))
})

test_that("test grab lodes rac", {
  expect_equal(grab_lodes(state = 'or', year = 2014, lodes_type = "rac", job_type = "JT01", 
                          segment = "SA01", agg_geo = "tract", lodes_ver = "7") %>% 
                 dim, c(834, 44))
  expect_equal(grab_lodes(state = 'or', year = 2020, lodes_type = "rac", job_type = "JT01", 
                          segment = "SA01", agg_geo = "tract", lodes_ver = "8") %>% 
                 dim, c(994, 44))
  expect_equal(grab_lodes(state = "or", year = "2015", lodes_type = "rac", job_type = "JT01", 
                          segment = "SA01", lodes_ver = "7") %>% 
                 dim, c(60227, 45))
  expect_equal(grab_lodes(state = "or", year = "2004", lodes_type = "rac", job_type = "JT01", 
                          segment = "SA01", lodes_ver = "5") %>% 
                 dim, c(53026, 43))
})

test_that("test grab lodes wac", {
  expect_equal(grab_lodes(state = 'or', year = 2014, lodes_type = "wac", job_type = "JT01", 
                          segment = "SA01", agg_geo = "tract", lodes_ver = "7") %>% 
                 dim, c(825, 54))
  expect_equal(grab_lodes(state = 'or', year = 2009, lodes_type = "wac", job_type = "JT01", 
                          segment = "SA01", agg_geo = "tract", lodes_ver = "5") %>% 
                 dim, c(754 , 42))
  expect_equal(grab_lodes(state = "or", year = "2015", lodes_type = "wac", job_type = "JT01", 
                          segment = "SA01", lodes_ver = "7") %>% 
                 dim, c(23367, 55))
  expect_equal(grab_lodes(state = "or", year = "2020", lodes_type = "wac", job_type = "JT01", 
                          segment = "SA01", lodes_ver = "8") %>% 
                 dim, c(24793, 55))
})

test_that("test grab lodes od for multiple states and years", {
  expect_equal(grab_lodes(state = c('or', "ri"), year = c(2013, 2014), lodes_type = "od", job_type = "JT01", 
                          segment = "SA01", state_part = "main", agg_geo = "tract", lodes_ver = "7") %>% 
                 dim, c(511071, 14))
  expect_equal(grab_lodes(state = c('wa', "ri"), year = c(2007, 2008), lodes_type = "od", job_type = "JT01", 
                          segment = "SA01", state_part = "main", agg_geo = "tract", lodes_ver = "5") %>% 
                 dim, c(853294, 14))
  expect_equal(grab_lodes(state = c('or', 'ri'), year = c(2013, 2014), lodes_type = "od", job_type = "JT01", 
                          segment = "SA01", state_part = "main", lodes_ver = "7") %>% 
                 dim, c(3326655, 15))
  expect_equal(grab_lodes(state = c('or', 'md'), year = c(2013, 2020), lodes_type = "od", job_type = "JT01", 
                          segment = "SA01", state_part = "main", lodes_ver = "8") %>% 
                 dim, c(6384095, 15))
})

test_that("test grab lodes wac for multiple states and years", {
  expect_equal(grab_lodes(state = c('or', 'ri'), year = c(2013, 2014), lodes_type = "wac", job_type = "JT01", 
                          segment = "SA01", agg_geo = "tract", lodes_ver = "7") %>% 
                 dim, c(2132, 54))
  expect_equal(grab_lodes(state = c('or', 'ri'), year = c(2007, 2009), lodes_type = "wac", job_type = "JT01", 
                          segment = "SA01", agg_geo = "tract", lodes_ver = "5") %>% 
                 dim, c(1974, 42))
  expect_equal(grab_lodes(state = c('or', 'ri'), year = c(2013, 2014), lodes_type = "wac", job_type = "JT01", 
                          segment = "SA01", lodes_ver = "7") %>% 
                 dim, c(55307, 55))
  expect_equal(grab_lodes(state = c('or', 'ri'), year = c(2017, 2018, 2019, 2020), lodes_type = "wac", job_type = "JT01", 
                          segment = "SA01", lodes_ver = "8") %>% 
                 dim, c(130857, 55))
})