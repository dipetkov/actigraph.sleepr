# Internal replacements for functions previously imported from RcppRoll
# and zoo. Each is checked against the original implementation.

roll_sum_ <- actigraph.sleepr:::roll_sum_
roll_mean_ <- actigraph.sleepr:::roll_mean_
roll_sd_ <- actigraph.sleepr:::roll_sd_
na_locf <- actigraph.sleepr:::na_locf
na_trim <- actigraph.sleepr:::na_trim
na_spline <- actigraph.sleepr:::na_spline

# Random test vectors: activity-like counts and values whose spread is small
# relative to their mean (which stresses the variance computation).
random_vectors <- function(n_vectors, min_len = 11, max_len = 300) {
  set.seed(20261109)
  lapply(seq_len(n_vectors), function(i) {
    len <- sample(min_len:max_len, 1)
    x <- switch(i %% 3 + 1,
      as.numeric(rpois(len, 50)),
      rnorm(len, mean = 1e6, sd = 1),
      as.numeric(pmin(rpois(len, 2) * 100, 300))
    )
    if (i %% 4 == 0) x[sample(len, 2)] <- NA
    x
  })
}

testthat::context("Rolling window statistics (RcppRoll replacements)")

testthat::test_that("roll_sum_, roll_mean_, roll_sd_ match RcppRoll", {
  testthat::skip_if_not_installed("RcppRoll")
  for (x in random_vectors(150)) {
    for (n in c(2, 3, 6, 11)) {
      testthat::expect_equal(roll_sum_(x, n), RcppRoll::roll_sum(x, n))
      testthat::expect_equal(roll_mean_(x, n), RcppRoll::roll_mean(x, n))
      testthat::expect_equal(roll_sd_(x, n), RcppRoll::roll_sd(x, n))
    }
  }
})

testthat::test_that("rolling stats match RcppRoll as called by apply_sadeh", {
  testthat::skip_if_not_installed("RcppRoll")
  half_window <- 5
  padding <- rep(0, half_window)
  for (x in random_vectors(50)) {
    x[is.na(x)] <- 0
    centered <- c(padding, x, padding)
    right <- c(padding, x)
    testthat::expect_equal(
      roll_mean_(centered, n = 11),
      RcppRoll::roll_mean(centered, n = 11, align = "center", partial = FALSE)
    )
    testthat::expect_equal(
      roll_sum_(centered, n = 11),
      RcppRoll::roll_sum(centered, n = 11, align = "center", partial = FALSE)
    )
    testthat::expect_equal(
      roll_sd_(right, n = 6),
      RcppRoll::roll_sd(right, n = 6, align = "right", partial = FALSE)
    )
  }
})

testthat::test_that("rolling stats handle edge cases", {
  testthat::expect_identical(roll_sum_(c(1, 2, 3), 3), 6)
  testthat::expect_identical(roll_mean_(c(1, 2, 3), 3), 2)
  testthat::expect_identical(roll_sd_(c(1, 2, 3), 3), 1)
  testthat::expect_identical(roll_sd_(rep(5, 6), 6), 0)
  testthat::expect_identical(roll_sum_(c(1, 2), 3), numeric())
  testthat::expect_identical(roll_sum_(c(1, NA, 3, 4), 2), c(NA, NA, 7))
  testthat::expect_identical(roll_sd_(c(1, NA, 3, 4), 2), c(NA, NA, sd(3:4)))
  testthat::expect_identical(roll_sd_(c(1, 2), 1), c(NA_real_, NA_real_))
  testthat::expect_error(roll_sum_(1:3, 0))
})

testthat::context("NA handling (zoo replacements)")

testthat::test_that("na_spline matches zoo::na.spline", {
  testthat::skip_if_not_installed("zoo")
  examples <- list(
    interior_gap = c(1, 2, NA, 4, 5),
    long_gap = c(10, 12, NA, NA, NA, NA, 30, 31),
    leading_na = c(NA, NA, 3, 5, 4, 8),
    trailing_na = c(3, 5, 4, 8, NA, NA),
    both_ends = c(NA, 3, NA, 5, NA),
    no_na_double = c(0.5, 2, 3.25, 1),
    no_na_integer = 1:10,
    integer_gaps = c(5L, NA, 7L, NA, NA, 3L),
    one_value = c(NA, 3, NA),
    two_values = c(NA, 3, NA, 5),
    all_na = c(NA_real_, NA_real_),
    all_na_integer = c(NA_integer_, NA_integer_, NA_integer_),
    empty = numeric(),
    single = 7
  )
  for (nm in names(examples)) {
    x <- examples[[nm]]
    testthat::expect_identical(na_spline(x), zoo::na.spline(x), info = nm)
  }
})

testthat::test_that("na_spline matches zoo::na.spline on random counts", {
  testthat::skip_if_not_installed("zoo")
  set.seed(1)
  for (i in 1:200) {
    x <- rpois(sample(5:300, 1), 20)
    x[sample(length(x), sample(seq_len(length(x) - 2), 1))] <- NA
    testthat::expect_identical(na_spline(x), zoo::na.spline(x))
  }
})

testthat::test_that("na_trim matches zoo::na.trim on vectors", {
  testthat::skip_if_not_installed("zoo")
  examples <- list(
    no_na = c(1, 2, 3),
    leading = c(NA, NA, 1, 2),
    trailing = c(1, 2, NA),
    both = c(NA, 1, NA, 2, NA, NA),
    interior_only = c(1, NA, NA, 2),
    all_na = c(NA_real_, NA_real_),
    single_na = NA_real_,
    single_value = 5,
    empty = numeric(),
    integer = c(NA, 1L, 2L, NA),
    character = c(NA, "a", NA, "b", NA),
    logical = c(NA, TRUE, FALSE, NA),
    named = c(a = NA, b = 1, c = 2, d = NA)
  )
  for (nm in names(examples)) {
    x <- examples[[nm]]
    testthat::expect_identical(na_trim(x), zoo::na.trim(x), info = nm)
  }
})

testthat::test_that("na_trim matches zoo::na.trim on data frames", {
  testthat::skip_if_not_installed("zoo")
  examples <- list(
    no_na = data.frame(a = 1:3, b = 4:6),
    na_first_col_leading = data.frame(a = c(NA, 2, 3), b = 4:6),
    na_second_col_trailing = data.frame(a = 1:4, b = c(1, 2, NA, NA)),
    staggered = data.frame(
      a = c(NA, 2, 3, 4, 5, 6),
      b = c(1, NA, 3, 4, 5, NA),
      c = c(1, 2, 3, NA, 5, 6)
    ),
    interior_only = data.frame(a = c(1, NA, 3), b = c(1, 2, 3)),
    all_rows_incomplete = data.frame(a = c(NA, 1), b = c(1, NA)),
    all_na = data.frame(a = c(NA, NA), b = c(NA, NA)),
    one_column = data.frame(a = c(NA, 1, 2, NA)),
    one_row_complete = data.frame(a = 1, b = 2),
    one_row_na = data.frame(a = NA, b = 2),
    zero_rows = data.frame(a = numeric(), b = numeric()),
    mixed_types = data.frame(
      x = c(NA, "a", "b", "c"),
      y = c(1L, 2L, 3L, NA),
      z = factor(c("u", "v", NA, "w")),
      stringsAsFactors = FALSE
    ),
    row_names = data.frame(
      a = c(NA, 1, 2, NA),
      row.names = c("w", "x", "y", "z")
    )
  )
  for (nm in names(examples)) {
    df <- examples[[nm]]
    testthat::expect_identical(na_trim(df), zoo::na.trim(df), info = nm)
    tbl <- tibble::as_tibble(df)
    testthat::expect_identical(na_trim(tbl), zoo::na.trim(tbl), info = nm)
  }
})

testthat::test_that("na_trim matches zoo::na.trim on matrices", {
  testthat::skip_if_not_installed("zoo")
  m <- cbind(a = c(NA, 1, 2, 3, NA), b = c(1, 2, NA, 4, 5))
  testthat::expect_identical(na_trim(m), zoo::na.trim(m))
  m1 <- m[, "a", drop = FALSE]
  testthat::expect_identical(na_trim(m1), zoo::na.trim(m1))
})

testthat::test_that("na_trim matches zoo::na.trim on random data frames", {
  testthat::skip_if_not_installed("zoo")
  set.seed(2)
  for (i in 1:100) {
    n <- sample(1:30, 1)
    df <- data.frame(a = rnorm(n), b = rpois(n, 3))
    df$a[runif(n) < 0.2] <- NA
    df$b[runif(n) < 0.2] <- NA
    testthat::expect_identical(na_trim(df), zoo::na.trim(df))
  }
})

testthat::test_that("na_locf matches zoo::na.locf", {
  testthat::skip_if_not_installed("zoo")
  examples <- list(
    no_na = c(1L, 0L, 1L),
    interior = c(1L, NA, NA, 0L, NA, 1L),
    trailing = c(0L, 1L, NA, NA),
    leading = c(NA, NA, 1L, NA, 0L),
    character = c("S", NA, "W", NA, NA, "S"),
    leading_character = c(NA, "W", NA),
    all_na = c(NA_integer_, NA_integer_),
    empty = integer(),
    single = "S"
  )
  for (nm in names(examples)) {
    x <- examples[[nm]]
    testthat::expect_identical(na_locf(x), zoo::na.locf(x), info = nm)
  }
})

testthat::test_that("impute_epochs matches the zoo-based implementation", {
  testthat::skip_if_not_installed("zoo")
  data("gtxplus1day", package = "actigraph.sleepr")
  agdb <- gtxplus1day
  agdb$axis1[c(1:3, 50:60, 500)] <- NA
  agdb$axis2[c(2, 900:905, nrow(agdb))] <- NA

  expected <- agdb |>
    dplyr::select(timestamp, axis1, axis2) |>
    zoo::na.trim() |>
    dplyr::inner_join(agdb, by = c("timestamp", "axis1", "axis2")) |>
    dplyr::mutate(dplyr::across(
      c(axis1, axis2),
      function(x) pmax(round(zoo::na.spline(x)), 0)
    ))

  testthat::expect_identical(impute_epochs(agdb, axis1, axis2), expected)
})

testthat::context("Expanding periods (tidyr/purrr replacements)")

testthat::test_that("expand_periods keeps time zone and handles empty input", {
  tz <- "America/New_York"
  periods <- tibble::tibble(
    start = as.POSIXct(c("2020-01-01 00:00", "2020-01-02 12:00"), tz = tz),
    end = start + c(120, 60)
  )
  expanded <- expand_periods(periods, start, end)
  testthat::expect_identical(expanded$period_id, c(1L, 1L, 1L, 2L, 2L))
  testthat::expect_identical(attr(expanded$timestamp, "tzone"), tz)
  testthat::expect_identical(
    expanded$timestamp,
    c(periods$start[1] + c(0, 60, 120), periods$start[2] + c(0, 60))
  )

  none <- expand_periods(periods[0, ], start, end)
  testthat::expect_identical(nrow(none), 0L)
  testthat::expect_s3_class(none$timestamp, "POSIXct")
})
