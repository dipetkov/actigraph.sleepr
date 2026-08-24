library("actigraph.sleepr")
library("readr")
library("dplyr")

context("Non-wear detection algorithm")
test_that("apply_troiano returns a tibble", {
  file <- system.file("extdata", "GT3XPlus-RawData-Day01.agd",
    package = "actigraph.sleepr"
  )
  agdb_10s <- read_agd(file)
  agdb_60s <- collapse_epochs(agdb_10s, 60)
  agdb_nonwear <- apply_troiano(agdb_60s)
  expect_s3_class(agdb_nonwear, "tbl")
})
test_that("apply_troiano uses vector magnitude when requested", {
  epochs <- tbl_agd(
    tibble::tibble(
      timestamp = as.POSIXct("2020-01-01 00:00:00", tz = "UTC") + 60 * 0:8,
      axis1 = 0,
      axis2 = c(rep(0, 3), rep(1, 3), rep(0, 3)),
      axis3 = 0
    ),
    tibble::tibble(epochlength = 60)
  )

  nonwear <- apply_troiano(
    epochs,
    min_period_len = 3,
    use_magnitude = TRUE
  )

  expect_equal(nonwear$period_start, epochs$timestamp[c(1, 7)])
  expect_equal(nonwear$length, c(3L, 3L))
  expect_true(attr(nonwear, "use_magnitude"))
})
test_that("apply_troiano return same result as ActiLife 6", {
  agd_file <-
    system.file("extdata", "GT3XPlus-RawData-Day01.agd",
      package = "actigraph.sleepr"
    )
  csv_file <-
    system.file("extdata", "GT3XPlus-RawData-Day01-Troiano-periods.csv",
      package = "actigraph.sleepr"
    )
  join_vars <- c("period_start", "period_end", "length")
  actilife <- read_csv(csv_file)
  epochs <- read_agd(agd_file) %>% collapse_epochs(60)
  params <- actilife %>%
    select(
      min_period_len, max_nonzero_count,
      spike_tolerance, spike_stoplevel,
      activity_threshold, endat_nnz_seq
    ) %>%
    distinct()
  troiano <-
    params %>%
    rowwise() %>%
    do({
      min_len <- .$min_period_len
      spike_tol <- .$spike_tolerance
      spike_stop <- .$spike_stoplevel
      seq <- .$endat_nnz_seq

      actisleepr_periods <- epochs %>%
        apply_troiano(
          min_period_len = min_len,
          spike_stoplevel = spike_stop,
          spike_tolerance = spike_tol,
          endat_nnz_seq = seq
        )

      actilife_periods <- actilife %>%
        filter(
          min_period_len == min_len,
          spike_stoplevel == spike_stop,
          spike_tolerance == spike_tol,
          endat_nnz_seq == seq,
          wear == FALSE
        )

      actilife_anti_actsleepr <-
        actilife_periods %>%
        anti_join(actisleepr_periods, by = join_vars)
      actsleepr_anti_actilife <-
        actisleepr_periods %>%
        anti_join(actilife_periods, by = join_vars)

      expect_equal(actilife_anti_actsleepr %>% nrow(), 0)
      expect_equal(actsleepr_anti_actilife %>% nrow(), 0)

      actisleepr_periods
    })
})
test_that("apply_choi uses vector magnitude when requested", {
  epochs <- tbl_agd(
    tibble::tibble(
      timestamp = as.POSIXct("2020-01-01 00:00:00", tz = "UTC") + 60 * 0:8,
      axis1 = 0,
      axis2 = c(rep(0, 3), rep(1, 3), rep(0, 3)),
      axis3 = 0
    ),
    tibble::tibble(epochlength = 60)
  )

  nonwear <- apply_choi(
    epochs,
    min_period_len = 3,
    min_window_len = 3,
    use_magnitude = TRUE
  )

  expect_equal(nonwear$period_start, epochs$timestamp[c(1, 7)])
  expect_equal(nonwear$length, c(3L, 3L))
  expect_true(attr(nonwear, "use_magnitude"))
})
test_that("apply_choi return same result as ActiLife 6", {
  agd_file <- system.file("extdata", "GT3XPlus-RawData-Day01.agd",
    package = "actigraph.sleepr"
  )
  csv_file <- system.file("extdata", "GT3XPlus-RawData-Day01-Choi-periods.csv",
    package = "actigraph.sleepr"
  )
  join_vars <- c("period_start", "period_end", "length")
  actilife <- read_csv(csv_file)
  epochs <- read_agd(agd_file) %>% collapse_epochs(60)
  params <- actilife %>%
    select(
      min_period_len, min_window_len,
      spike_tolerance, use_magnitude
    ) %>%
    distinct()
  choi <-
    params %>%
    rowwise() %>%
    do({
      min_len <- .$min_period_len
      window_len <- .$min_window_len
      spike_tol <- .$spike_tolerance

      actisleepr_periods <-
        apply_choi(epochs,
          min_period_len = min_len,
          min_window_len = window_len,
          spike_tolerance = spike_tol
        )

      actilife_periods <- actilife %>%
        filter(
          min_period_len == min_len,
          min_window_len == window_len,
          spike_tolerance == spike_tol,
          wear == FALSE
        )

      actilife_anti_actsleepr <-
        actilife_periods %>%
        anti_join(actisleepr_periods, by = join_vars)

      actsleepr_anti_actilife <-
        actisleepr_periods %>%
        anti_join(actilife_periods, by = join_vars)

      expect_equal(actilife_anti_actsleepr %>% nrow(), 0)
      expect_equal(actsleepr_anti_actilife %>% nrow(), 0)

      actisleepr_periods
    })
})
