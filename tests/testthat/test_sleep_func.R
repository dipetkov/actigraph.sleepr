
library("actigraph.sleepr")
library("dplyr")

context("Sleep scoring algorithms")
test_that("apply_sadeh/apply_cole_kripke return same result as ActiLife 6", {
  agd_file <- system.file("extdata", "GT3XPlus-RawData-Day01-10sec60sec.agd",
                          package = "actigraph.sleepr")
  csv_file <- system.file("extdata", "GT3XPlus-RawData-Day01-sleep-awake.csv",
                          package = "actigraph.sleepr")
  agdb_60s <- read_agd(agd_file)
  actilife <- read_csv(csv_file)

  agdb_sadeh <- apply_sadeh(agdb_60s)
  expect_identical(agdb_sadeh$state, actilife$sadeh)
  agdb_colkrip <- apply_cole_kripke(agdb_60s)
  expect_identical(agdb_colkrip$state, actilife$`cole-kripke`)
})

context("Period detection algorithm")
test_that("apply_tudor_locke returns a tbl_period", {
  file <- system.file("extdata", "GT3XPlus-RawData-Day01-10sec.agd",
                      package = "actigraph.sleepr")
  agdb_10s <- read_agd(file)
  agdb_60s <- collapse_epochs(agdb_10s, 60)
  agdb_sadeh <- apply_sadeh(agdb_60s)
  agdb_sleep <- apply_tudor_locke(agdb_sadeh)
  expect_s3_class(agdb_sleep, "tbl_period")
})
test_that("apply_tudor_locke return same result as ActiLife 6", {
  agd_file <- system.file("extdata", "GT3XPlus-RawData-Day01-10sec60sec.agd",
                          package = "actigraph.sleepr")
  csv_file <- system.file("extdata", "GT3XPlus-RawData-Day01-sleep-periods.csv",
                          package = "actigraph.sleepr")
  agdb_state <- apply_sadeh(read_agd(agd_file))
  for (algorithm in c("Tudor-Locke Default",
                      "Tudor-Locke Custom1",
                      "Tudor-Locke Custom2")) {
    actilife <- read_csv(csv_file) %>%
      filter(period_algorithm == algorithm)
    params <- actilife %>% filter(row_number() == 1)
    agdb_sleep <-
      apply_tudor_locke(agdb_state,
                        n_bedtime_start = params$n_bedtime_start,
                        n_wake_time_end = params$n_wake_time_end,
                        min_sleep_period = params$min_sleep_period,
                        max_sleep_period = params$max_sleep_period,
                        min_nonzero_epochs = params$min_nonzero_epochs)
    common_vars <- intersect(colnames(agdb_sleep),
                             colnames(actilife))
    expect_equal(common_vars, c("in_bed_timestamp", "out_bed_timestamp",
                                "onset_timestamp", "latency", "total_counts",
                                "efficiency", "time_in_bed", "time_asleep",
                                "time_awake", "awakenings", "ave_awakening",
                                "movement_index", "fragmentation_index",
                                "sleep_fragmentation_index"))
    x <- agdb_sleep %>% mutate_if(is.numeric, round)
    y <- actilife %>% mutate_if(is.numeric, round)
    x <- x %>%
      inner_join(y %>% select(in_bed_timestamp, out_bed_timestamp),
                 by = c("in_bed_timestamp", "out_bed_timestamp"))
    for (var in common_vars) expect_identical(x[[var]], y[[var]])
  }
})
