test_that("test grab lodes od", {
  withr::local_options(list(lehdr_use_cache = TRUE))
  
  expect_equal(grab_lodes(state = "de", 
                          year = 2020,
                          version = "LODES8",
                          lodes_type = "od", 
                          job_type = "JT00", 
                          segment = "SA01", 
                          state_part = "main", 
                          agg_geo = "tract") %>% 
                 dim, c(36671 , 14))
  expect_equal(grab_lodes(state = "de", 
                          year = 2009,
                          version = "LODES5",
                          lodes_type = "od",
                          state_part = "main", 
                          agg_geo = "tract") %>% 
                 dim, c(25805 , 14))
  expect_equal(grab_lodes(state = "de", 
                          year = "2015",
                          version = "LODES8",
                          lodes_type = "od", 
                          job_type = "JT00", 
                          segment = "SE01", 
                          state_part = "main") %>% 
                 dim, c(290314 , 15))
  expect_equal(grab_lodes(state = "de", 
                          year = "2009",
                          version = "LODES5",
                          lodes_type = "od", 
                          state_part = "aux") %>% 
                 dim, c(68588 , 15))
})

test_that("test grab lodes rac", {
  withr::local_options(list(lehdr_use_cache = TRUE))
  
  expect_equal(grab_lodes(state = "de", 
                          year = 2014,
                          version = "LODES7",
                          lodes_type = "rac", 
                          agg_geo = "tract") %>% 
                 dim, c(218 , 44))
  expect_equal(grab_lodes(state = "de", 
                          year = "2015", 
                          version = "LODES8",
                          lodes_type = "rac") %>% 
                 dim, c(14436 , 45))
  expect_equal(grab_lodes(state = "de", 
                          year = "2004",
                          version = "LODES7",
                          lodes_type = "rac", 
                          job_type = "JT01", 
                          segment = "SA01") %>% 
                 dim, c(12596 , 45))
})

test_that("test grab lodes wac", {
  withr::local_options(list(lehdr_use_cache = TRUE))
  
  expect_equal(grab_lodes(state = "de", 
                          year = 2009,
                          version = "LODES5",
                          lodes_type = "wac", 
                          job_type = "JT01", 
                          agg_geo = "tract") %>% 
                 dim, c(197  , 42))
  expect_equal(grab_lodes(state = "de", 
                          year = "2015",
                          version = "LODES7",
                          lodes_type = "wac") %>% 
                 dim, c(5476 , 55))
  expect_equal(grab_lodes(state = "de", 
                          year = "2020", 
                          version = "LODES8",
                          lodes_type = "wac") %>% 
                 dim, c(6421 , 55))
})

test_that("test grab lodes od for multiple states and years", {
  withr::local_options(list(lehdr_use_cache = TRUE))
  
  expect_equal(grab_lodes(state = c("nd", "vt"), 
                          year = c(2007, 2008),
                          version = "LODES5",
                          lodes_type = "od", 
                          job_type = "JT01", 
                          segment = "SA01", 
                          state_part = "main", 
                          agg_geo = "tract") %>% 
                 dim, c(65262 , 14))
  expect_equal(grab_lodes(state = c("de", "vt"), 
                          year = c(2013, 2014), 
                          version = "LODES8",
                          lodes_type = "od", 
                          job_type = "JT01", 
                          segment = "SA01", 
                          state_part = "main") %>% 
                 dim, c(929717 , 15))
  expect_equal(grab_lodes(state = c("de", 'sd'), 
                          year = c(2013, 2020),
                          version = "LODES8",
                          lodes_type = "od", 
                          job_type = "JT01", 
                          segment = "SA01", 
                          state_part = "main") %>% 
                 dim, c(1125184 , 15))
})

test_that("test grab lodes wac for multiple states and years", {
  withr::local_options(list(lehdr_use_cache = TRUE))
  
  expect_equal(grab_lodes(state = c("de", "vt"), 
                          year = c(2013, 2014),
                          version = "LODES7",
                          lodes_type = "wac", 
                          agg_geo = "tract") %>% 
                 dim, c(798, 54))
  expect_equal(grab_lodes(state = c("de", "vt"), 
                          year = c(2007, 2009), 
                          version = "LODES5",
                          lodes_type = "wac", 
                          agg_geo = "tract") %>% 
                 dim, c(752 , 42))
  expect_equal(grab_lodes(state = c("de", "vt"), 
                          year = c(2013, 2014), 
                          version = "LODES7",
                          lodes_type = "wac") %>% 
                 dim, c(24132 , 55))
  expect_equal(grab_lodes(state = c("de", "vt"), 
                          year = c(2017, 2018, 2019, 2020), 
                          version = "LODES8",
                          lodes_type = "wac", 
                          job_type = "JT01", 
                          segment = "S000") %>%
                 dim, c(55696 , 55))
})

test_that("test grab crosswalk", {
  withr::local_options(list(lehdr_use_cache = TRUE))
  
  expect_equal(grab_crosswalk('vt') %>% 
                 dim, c(24611, 41))
  expect_equal(grab_crosswalk(c("wy", "ND")) %>% 
                 dim, c(138335, 41))
})

test_that("test join_lodes_geometry", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  rac_data <- grab_lodes(
    state = "vt",
    year = 2008,
    version = "LODES5",
    lodes_type = "rac",
    job_type = "JT01",
    segment = "SA01",
    state_part = "main",
    agg_geo = "county",
    geometry = TRUE
  )

  wac_data <- grab_lodes(
    state = "vt",
    year = 2008,
    version = "LODES5",
    lodes_type = "wac",
    job_type = "JT01",
    segment = "SA01",
    state_part = "main",
    agg_geo = "county",
    geometry = TRUE
  )

  expect_s3_class(rac_data, "sf")
  expect_s3_class(wac_data, "sf")

  rac_data_bg <- grab_lodes(
    state = "vt",
    year = 2022,
    version = "LODES8",
    lodes_type = "rac",
    job_type = "JT01",
    segment = "SA01",
    state_part = "main",
    agg_geo = "bg",
    geometry = TRUE
  )

  wac_data_bg <- grab_lodes(
    state = "vt",
    year = 2022,
    version = "LODES8",
    lodes_type = "wac",
    job_type = "JT01",
    segment = "SA01",
    state_part = "main",
    agg_geo = "bg",
    geometry = TRUE
  )

  expect_s3_class(rac_data_bg, "sf")
  expect_s3_class(wac_data_bg, "sf")

  rac_data_block <- grab_lodes(
    state = "vt",
    year = 2008,
    version = "LODES5",
    lodes_type = "rac",
    job_type = "JT01",
    segment = "SA01",
    state_part = "main",
    agg_geo = "block",
    geometry = TRUE
  )

  wac_data_block <- grab_lodes(
    state = "vt",
    year = 2008,
    version = "LODES5",
    lodes_type = "wac",
    job_type = "JT01",
    segment = "SA01",
    state_part = "main",
    agg_geo = "block",
    geometry = TRUE
  )

  expect_s3_class(rac_data_block, "sf")
  expect_s3_class(wac_data_block, "sf")

  expect_message(
    grab_lodes(
      state = "vt",
      year = 2008,
      version = "LODES5",
      lodes_type = "wac",
      job_type = "JT01",
      segment = "SA01",
      state_part = "main",
      agg_geo = "county",
      geometry = TRUE
    )
  )
})
