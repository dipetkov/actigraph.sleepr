library("dplyr")
library("ggplot2")
library("tibble")

context("Plotting helpers")

test_that("plot_activity uses a constant fill when color is not a column", {
  start <- as.POSIXct("2020-01-01 00:00:00", tz = "UTC")
  agdb <- tibble(
    timestamp = start + 60 * 0:5,
    axis1 = c(0, 1, 2, 1, 0, 3),
    sleep = c("S", "S", "W", "W", "S", "S")
  )

  p <- plot_activity(agdb, axis1, color = "steelblue")

  expect_s3_class(p, "ggplot")
  expect_s3_class(p$facet, "FacetNull")

  built <- ggplot_build(p)
  expect_length(unique(built$data[[1]]$fill), 1)
})

test_that("plot_activity maps fill and facets grouped data", {
  start <- as.POSIXct("2020-01-01 00:00:00", tz = "UTC")
  agdb <- tibble(
    subject = c("a", "a", "b", "b"),
    timestamp = start + 60 * 0:3,
    axis1 = c(1, 2, 3, 4),
    sleep = c("S", "W", "S", "W")
  ) %>%
    group_by(subject)

  p <- plot_activity(agdb, axis1, color = "sleep", nrow = 1, ncol = 2)

  expect_s3_class(p, "ggplot")
  expect_s3_class(p$facet, "FacetWrap")

  built <- ggplot_build(p)
  expect_gt(length(unique(built$data[[1]]$fill)), 1)
})

test_that("plot_activity_period adds a rectangle layer for the periods", {
  start <- as.POSIXct("2020-01-01 00:00:00", tz = "UTC")
  agdb <- tibble(
    timestamp = start + 60 * 0:5,
    axis1 = c(0, 1, 2, 1, 0, 3)
  )
  periods <- tibble(
    start = start + c(60, 240),
    end = start + c(120, 300)
  )

  p <- plot_activity_period(agdb, periods, axis1, start, end, fill = "tomato")

  expect_s3_class(p, "ggplot")
  expect_length(p$layers, 2)
  expect_s3_class(p$layers[[2]]$geom, "GeomRect")
  expect_identical(p$layers[[2]]$aes_params$fill, "tomato")
})

context("Cole-Kripke scoring")

test_that("apply_cole_kripke scores a 60 second series and sets metadata", {
  start <- as.POSIXct("2020-01-01 00:00:00", tz = "UTC")
  agdb <- tibble(
    timestamp = start + 60 * 0:6,
    axis1 = c(0, 0, 0, 0, 10000, 0, 0)
  )

  res <- apply_cole_kripke(agdb)

  expect_equal(attr(res, "sleep_algorithm"), "Cole-Kripke")
  expect_equal(res$sleep[1], "S")
  expect_equal(res$sleep[5], "W")
  expect_true(all(res$sleep %in% c("S", "W")))
})

test_that("apply_cole_kripke errors on invalid inputs", {
  start <- as.POSIXct("2020-01-01 00:00:00", tz = "UTC")

  bad_epoch <- tibble(
    timestamp = start + c(0, 30, 60, 90, 120, 150, 180),
    axis1 = rep(1, 7)
  )
  expect_error(apply_cole_kripke(bad_epoch), "60sec epochs")

  missing_ts <- tibble(
    timestamp = c(
      start, start + 60, NA, start + 180, start + 240, start + 300, start + 360
    ),
    axis1 = rep(1, 7)
  )
  expect_error(apply_cole_kripke(missing_ts), "Missing timestamps")

  missing_count <- tibble(
    timestamp = start + 60 * 0:6,
    axis1 = c(1, 1, NA, 1, 1, 1, 1)
  )
  expect_error(apply_cole_kripke(missing_count), "Missing axis1 counts")
})

test_that("internal Cole-Kripke helpers transform and score counts", {
  start <- as.POSIXct("2020-01-01 00:00:00", tz = "UTC")
  data <- tibble(
    timestamp = start + 60 * 0:6,
    axis1 = c(0, 0, 0, 0, 10000, 0, 0)
  )

  adjusted <- actigraph.sleepr:::actigraph_adjustment(data)
  expect_equal(adjusted$count, c(0, 0, 0, 0, 100, 0, 0))

  scored_1min <- actigraph.sleepr:::apply_cole_kripke_1min_(adjusted)
  expect_equal(scored_1min$sleep[1], "S")
  expect_equal(scored_1min$sleep[5], "W")

  scored_30sec <- actigraph.sleepr:::apply_cole_kripke_30sec_(adjusted)
  expect_equal(scored_30sec$sleep[1], "S")
  expect_equal(scored_30sec$sleep[5], "W")

  scored_10sec <- actigraph.sleepr:::apply_cole_kripke_10sec_(adjusted)
  expect_equal(scored_10sec$sleep[1], "S")
  expect_equal(scored_10sec$sleep[5], "W")
})

context("Utility helpers")

test_that("expand_timestamp returns a regular sequence and validates inputs", {
  start <- as.POSIXct("2020-01-01 00:00:00", tz = "UTC")
  end <- as.POSIXct("2020-01-01 01:00:00", tz = "UTC")

  out <- expand_timestamp(start, end, "15 mins")
  expect_identical(out, seq(start, end, by = "15 mins"))

  expect_error(expand_timestamp(1, end))
})

test_that("expand_periods expands intervals into equally spaced timestamps", {
  start <- as.POSIXct("2020-01-01 00:00:00", tz = "UTC")
  periods <- tibble(
    start = start + c(0, 180),
    end = start + c(60, 240)
  )

  out <- expand_periods(periods, start, end, units = "1 min")

  expect_s3_class(out, "tbl_df")
  expect_true(all(c("period_id", "timestamp") %in% names(out)))
  expect_gt(nrow(out), nrow(periods))
})

test_that("complement_periods handles empty and non-empty periods", {
  start <- as.POSIXct("2020-01-01 00:00:00", tz = "UTC")
  epochs <- tibble(timestamp = start + 60 * 0:12)

  no_periods <- tibble(
    start = as.POSIXct(character(), tz = "UTC"),
    end = as.POSIXct(character(), tz = "UTC")
  )
  empty_out <- complement_periods(no_periods, epochs, start, end)
  expect_equal(empty_out$period_start, start)
  expect_equal(empty_out$period_end, start + 60 * 12)
  expect_equal(empty_out$length, 12)

  periods <- tibble(
    start = start + 60 * c(1, 8),
    end = start + 60 * c(3, 10)
  )
  out <- complement_periods(periods, epochs, start, end)

  expect_equal(out$period_start, start + 60 * c(0, 5))
  expect_equal(out$period_end, start + 60 * c(0, 7))
  expect_equal(out$length, c(1L, 3L))
})

test_that("get_epoch_length infers the spacing and validates timestamps", {
  start <- as.POSIXct("2020-01-01 00:00:00", tz = "UTC")
  epochs <- tibble(timestamp = start + 60 * 0:4)

  expect_equal(get_epoch_length(epochs), 60)
  expect_error(
    get_epoch_length(tibble(axis1 = 1:5)),
    "Tibble has no timestamp column"
  )

  irregular <- tibble(timestamp = start + c(0, 60, 120, 240))
  expect_error(get_epoch_length(irregular), "Failed to determine epoch length")
})

test_that("mode and rleid helpers return the expected values", {
  expect_equal(actigraph.sleepr:::mode(c(1, 2, 2, 3)), 2)
  expect_equal(actigraph.sleepr:::mode(c(1, 1, 2, 2)), 1)
  expect_equal(
    actigraph.sleepr:::rleid(c(TRUE, TRUE, FALSE, FALSE, TRUE)),
    c(1, 1, 2, 2, 3)
  )
})
