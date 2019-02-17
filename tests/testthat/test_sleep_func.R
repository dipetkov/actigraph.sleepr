
library("actigraph.sleepr")
library("readr")
library("dplyr")

context("Sleep scoring algorithms")
test_that("apply_sadeh/apply_cole_kripke return same result as ActiLife 6", {
  agd_file <- system.file("extdata", "GT3XPlus-RawData-Day01.agd",
                          package = "actigraph.sleepr")
  csv_file <- system.file("extdata", "GT3XPlus-RawData-Day01-sleep-awake.csv",
                          package = "actigraph.sleepr")
  actilife <- read_csv(csv_file)
  agdb_60s <- read_agd(agd_file) %>% collapse_epochs(60)
  agdb_sadeh <- agdb_60s %>% apply_sadeh()
  expect_identical(agdb_sadeh$sleep, actilife$sadeh)
  agdb_colkrip <- agdb_60s %>% apply_cole_kripke()
  expect_identical(agdb_colkrip$sleep, actilife$`cole-kripke`)
})

context("Period detection algorithm")
test_that("apply_tudor_locke returns a tibble", {
  file <- system.file("extdata", "GT3XPlus-RawData-Day01.agd",
                      package = "actigraph.sleepr")
  periods <- read_agd(file) %>%
    collapse_epochs(60) %>%
    apply_sadeh() %>%
    apply_tudor_locke()
  expect_s3_class(periods, "tibble")
})
test_that("apply_tudor_locke return same result as ActiLife 6", {
  agd_file <- system.file("extdata", "GT3XPlus-RawData-Day01.agd",
                          package = "actigraph.sleepr")
  csv_file <- system.file("extdata", "GT3XPlus-RawData-Day01-sleep-periods.csv",
                          package = "actigraph.sleepr")
  join_vars <- c("in_bed_time", "out_bed_time", "efficiency", "duration",
                 "wake_after_onset", "nb_awakenings", "ave_awakening",
                 "movement_index", "fragmentation_index",
                 "sleep_fragmentation_index")
  actilife <- read_csv(csv_file)
  epochs <- read_agd(agd_file) %>% collapse_epochs(60) %>% apply_sadeh()
  params <- actilife %>% select(min_sleep_period, n_bedtime_start,
                                max_sleep_period, n_wake_time_end,
                                min_nonzero_epochs) %>%
    distinct()
  tudor_locke <-
    params %>%
    rowwise() %>%
    do({

      min_sleep <- .$min_sleep_period
      n_start <- .$n_bedtime_start
      n_end <- .$n_wake_time_end
      min_nnz <- .$min_nonzero_epochs

      last_record <- last(epochs$timestamp)
      actisleepr_periods <-
        apply_tudor_locke(epochs,
                          n_bedtime_start = n_start,
                          n_wake_time_end = n_end,
                          min_sleep_period = min_sleep,
                          min_nonzero_epochs = min_nnz) %>%
        mutate(last_record = last_record) %>%
        mutate_if(is.numeric, as.integer)

      actilife_periods <- actilife %>%
        filter(n_bedtime_start == n_start,
               n_wake_time_end == n_end,
               min_sleep_period == min_sleep,
               min_nonzero_epochs == min_nnz) %>%
        mutate_if(is.numeric, as.integer)

      actilife_anti_actsleepr <-
        actilife_periods %>%
        anti_join(actisleepr_periods, by = join_vars)
      actsleepr_anti_actilife <-
        actisleepr_periods %>%
        # Case 1: ActiLife filters out sleep periods that end
        # when the activity data ends
        filter(out_bed_time < last_record) %>%
        # Case 2: ActiLife filters out some sleep periods with
        filter(nonzero_epochs != min_nnz) %>%
        anti_join(actilife_periods, by = join_vars)

      expect_equal(actilife_anti_actsleepr %>% nrow(), 0)
      expect_equal(actsleepr_anti_actilife %>% nrow(), 0)

      actisleepr_periods
    })
})
