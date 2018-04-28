
library("actigraph.sleepr")
library("dplyr")
library("readr")

context("General tests")
if (requireNamespace("lintr", quietly = TRUE)) {
  context("lints")
  test_that("Package Style", {
    lintr::expect_lint_free()
  })
}

context("Read agd files")
test_that("read_agd returns a tbl_agd", {
  file <- system.file("extdata", "GT3XPlus-RawData-Day01.agd",
                      package = "actigraph.sleepr")
  agdb <- read_agd(file)
  expect_s3_class(agdb, "tbl_agd")
})
test_that("Error if file doesn't exist", {
  file <- system.file("extdata", "dummy.agd",
                      package = "actigraph.sleepr")
  expect_error(read_agd(file))
})

context("Manipulate agdb data frame")
test_that("all dplyr verbs work on a tbl_agd", {
  file <- system.file("extdata", "GT3XPlus-RawData-Day01.agd",
                      package = "actigraph.sleepr")
  agdb <- read_agd(file) %>%
    mutate(magnitude = sqrt(axis1 ^ 2 + axis2 ^ 2 + axis3 ^ 2))
  expect_true(exists("magnitude", where = agdb))
  agdb <- agdb %>%
    group_by(day = lubridate::day(timestamp)) %>%
    summarise_at(vars(starts_with("axis")), sum)
  expect_named(agdb, c("day", "axis1", "axis2", "axis3"))
})
test_that("group_by works as expected on tbl_agd with time gap", {
  file <- system.file("extdata", "GT3XPlus-RawData-Day01.agd",
                      package = "actigraph.sleepr")
  agdb <- read_agd(file) %>%
    collapse_epochs(60) %>%
    filter(lubridate::hour(timestamp) != 0)
  expect_true(agdb %>% has_missing_epochs)
  agdb <- agdb %>%
    mutate(date = lubridate::date(timestamp)) %>%
    group_by(date)
  expect_false(agdb %>% has_missing_epochs)
})

context("Collapse to 60s epochs")
test_that("collapse_epochs returns same result as ActiLife 6", {
  agd_file <- system.file("extdata", "GT3XPlus-RawData-Day01.agd",
                          package = "actigraph.sleepr")
  csv_file <- system.file("extdata", "GT3XPlus-RawData-Day01-10sec60sec.csv",
                          package = "actigraph.sleepr")
  actilife <- read_csv(csv_file)
  agdb_60s <- read_agd(agd_file) %>%
    collapse_epochs(60, use_incomplete = TRUE)
  for (var in setdiff(colnames(agdb_60s), "timestamp")) {
    expect_identical(agdb_60s[[var]], actilife[[var]])
  }
  # The last epoch in the example dataset is incomplete
  agdb_60s <- read_agd(agd_file) %>%
    collapse_epochs(60, use_incomplete = FALSE)
  n <- attr(agdb_60s, "epochcount")
  for (var in setdiff(colnames(agdb_60s), "timestamp")) {
    expect_identical(agdb_60s[[var]], actilife[[var]][1:n])
  }
})
test_that("collapse_epochs errors if unexpected epoch length", {
  dummy_agdb <- tbl_agd(data_frame(timestamp = 1:12, axis1 = 0),
                        data_frame(epochlength = 10))
  expect_error(collapse_epochs(dummy_agdb, 75))
  dummy_agdb <- tbl_agd(data_frame(timestamp = 1:12, axis1 = 0),
                        data_frame(epochlength = 9))
  expect_error(collapse_epochs(dummy_agdb, 60))
})

context("Impute epochs")
test_that("impute_epochs fills in NA epoch counts", {
  data("gtxplus1day", package = "actigraph.sleepr")

  gtxplus1day <- gtxplus1day %>%
    mutate_if(is.numeric, function(x) {
      x[5:10] <- NA
      x
    })
  expect_false(noNA(gtxplus1day$axis1))

  gtxplus1day <- gtxplus1day %>% impute_epochs(axis1)
  expect_true(noNA(gtxplus1day$axis1))
})

context("Combine epochs and periods")
test_that("combine_epochs_periods adds period id column", {
  data("gtxplus1day", package = "actigraph.sleepr")

  agdb <- gtxplus1day %>% collapse_epochs(60) %>% apply_sadeh()
  periods <- agdb %>% apply_tudor_locke(min_sleep_period = 60)
  agdb_with_periods <-
    combine_epochs_periods(agdb, periods, in_bed_time, out_bed_time)

  expect_true(has_name(agdb_with_periods, "period_id"))
  expect_equal(agdb, agdb_with_periods %>% select(- period_id))

  x <- agdb_with_periods %>% drop_na() %>% count(period_id) %>% .$n
  y <- periods %>% .$duration
  expect_identical(x, y + 1L)
})
