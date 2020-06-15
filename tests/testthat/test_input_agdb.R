
library("actigraph.sleepr")
library("dplyr")
library("readr")

testthat::context("General tests")
if (requireNamespace("lintr", quietly = TRUE)) {
  testthat::context("lints")
  testthat::test_that("Package Style", {

    # from https://github.com/jimhester/lintr/issues/421
    # A duplicate copy of the find_package function from lintr
    find_package <- function(path) {
      path <- normalizePath(path, mustWork = FALSE)

      while (!file.exists(file.path(path, "DESCRIPTION"))) {
        path <- dirname(path)
        if (identical(path, dirname(path))) {
          return(NULL)
        }
      }

      path
    }

    if (!is.null(find_package("."))) {
      lintr::expect_lint_free()
    }
  })
}

testthat::context("Read agd files")
testthat::test_that("read_agd returns a tibble", {
  file <- system.file("extdata", "GT3XPlus-RawData-Day01.agd",
                      package = "actigraph.sleepr")
  agdb <- read_agd(file)
  testthat::expect_s3_class(agdb, "tibble")
})
testthat::test_that("Error if file doesn't exist", {
  file <- system.file("extdata", "dummy.agd",
                      package = "actigraph.sleepr")
  testthat::expect_error(read_agd(file))
})

testthat::context("Manipulate agdb data frame")
testthat::test_that("all dplyr verbs work on a tibble", {
  file <- system.file("extdata", "GT3XPlus-RawData-Day01.agd",
                      package = "actigraph.sleepr")
  agdb <- read_agd(file) %>%
    mutate(magnitude = sqrt(axis1 ^ 2 + axis2 ^ 2 + axis3 ^ 2))
  testthat::expect_true(exists("magnitude", where = agdb))
  agdb <- agdb %>%
    group_by(day = lubridate::day(timestamp)) %>%
    summarise_at(vars(starts_with("axis")), sum)
  testthat::expect_named(agdb, c("day", "axis1", "axis2", "axis3"))
})
testthat::test_that("group_by works as expected on tibble with time gap", {
  file <- system.file("extdata", "GT3XPlus-RawData-Day01.agd",
                      package = "actigraph.sleepr")
  agdb <- read_agd(file) %>%
    collapse_epochs(60) %>%
    filter(lubridate::hour(timestamp) != 0)
  testthat::expect_true(agdb %>% has_missing_epochs)
  agdb <- agdb %>%
    mutate(date = lubridate::date(timestamp)) %>%
    group_by(date)
  testthat::expect_false(agdb %>% has_missing_epochs)
})

testthat::context("Collapse to 60s epochs")
testthat::test_that("collapse_epochs returns same result as ActiLife 6", {
  agd_file <- system.file("extdata", "GT3XPlus-RawData-Day01.agd",
                          package = "actigraph.sleepr")
  csv_file <- system.file("extdata", "GT3XPlus-RawData-Day01-10sec60sec.csv",
                          package = "actigraph.sleepr")
  # Read all columns but the first one (the timestamps) as integers
  actilife <- readr::read_csv(csv_file, col_types = "?iiiiiiiii")
  agdb_60s <- read_agd(agd_file) %>%
    collapse_epochs(60, use_incomplete = TRUE)
  for (var in setdiff(colnames(agdb_60s), "timestamp")) {
    expect_identical(agdb_60s[[var]], actilife[[var]])
  }
  # The last epoch in the example dataset is incomplete
  agdb_60s <- read_agd(agd_file) %>%
    collapse_epochs(60, use_incomplete = FALSE)
  n <- nrow(agdb_60s)
  for (var in setdiff(colnames(agdb_60s), "timestamp")) {
    testthat::expect_identical(agdb_60s[[var]], actilife[[var]][1:n])
  }
})
testthat::test_that("collapse_epochs errors if unexpected epoch length", {
  dummy_agdb <- tbl_agd(tibble::tibble(timestamp = 1:12, axis1 = 0),
                        tibble::tibble(epochlength = 10))
  testthat::expect_error(collapse_epochs(dummy_agdb, 75))
  dummy_agdb <- tbl_agd(tibble::tibble(timestamp = 1:12, axis1 = 0),
                        tibble::tibble(epochlength = 9))
  testthat::expect_error(collapse_epochs(dummy_agdb, 60))
})

testthat::context("Impute epochs")
test_that("impute_epochs fills in NA epoch counts", {
  data("gtxplus1day", package = "actigraph.sleepr")

  gtxplus1day <- gtxplus1day %>%
    mutate_if(is.numeric, function(x) {
      x[5:10] <- NA
      x
    })
  testthat::expect_false(assertthat::noNA(gtxplus1day$axis1))

  gtxplus1day <- gtxplus1day %>% impute_epochs(axis1)
  testthat::expect_true(assertthat::noNA(gtxplus1day$axis1))
})

testthat::context("Combine epochs and periods")
testthat::test_that("combine_epochs_periods adds period id column", {
  data("gtxplus1day", package = "actigraph.sleepr")

  agdb <- gtxplus1day %>% collapse_epochs(60) %>% apply_sadeh()
  periods <- agdb %>% apply_tudor_locke(min_sleep_period = 60)
  agdb_with_periods <-
    combine_epochs_periods(agdb, periods, in_bed_time, out_bed_time)

  testthat::expect_true(assertthat::has_name(agdb_with_periods, "period_id"))
  aa <- agdb
  attr(aa, "sleep_algorithm") <- NULL
  testthat::expect_equal(aa, agdb_with_periods %>%
                           select(- period_id) %>%
                           arrange(timestamp))

  x <- agdb_with_periods %>% tidyr::drop_na() %>% count(period_id) %>% .$n
  y <- periods %>% .$duration
  testthat::expect_identical(x, y + 1L)
})
