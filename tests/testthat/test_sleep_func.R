
library("actigraph.sleepr")
library("dplyr")

context("Sleep detection algorithms")
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
test_that("apply_tudor_locke returns a tbl_sleep", {
  file <- system.file("extdata", "GT3XPlus-RawData-Day01-10sec.agd",
                      package = "actigraph.sleepr")
  agdb_10s <- read_agd(file)
  agdb_60s <- collapse_epochs(agdb_10s, 60)
  agdb_sadeh <- apply_sadeh(agdb_60s)
  agdb_sleep <- apply_tudor_locke(agdb_sadeh)
  expect_s3_class(agdb_sleep, "tbl_sleep")
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

# Sometimes `apply_tudor_locke` detects more sleep periods than ActiLife 6
# But it should detect all periods ActiLife 6 detects, with the same
# values for the sleep quality metrics (up to rounding errors)
test_that("apply_tudor_locke to a batch of 200 agd files", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  agd_path <- path.expand(file.path("~",
                                    "Documents", "GitHub", "PG-Sensors",
                                    "BabySleepStudy", "data", "raw"))
  load(system.file("extdata", "test-sleep-periods.rda",
                   package = "actigraph.sleepr"))

  # Sometimes I might want to do just a few tests....
  for (filebase in subject_n200$file[1:10]) {

    subject <- strsplit(filebase, " ")[[1]][1]
    agd_file <- file.path(agd_path, paste0(filebase, "10sec.agd"))
    agdb_epoch <- collapse_epochs(read_agd(agd_file), 60,
                                  use_incomplete = TRUE)
    agdb_state <- apply_sadeh(agdb_epoch)

    for (algorithm in c("Tudor-Locke Default",
                        "Tudor-Locke Custom1",
                        "Tudor-Locke Custom2")) {
      params <- sleep_periods %>%
        filter(period_algorithm == algorithm) %>%
        filter(row_number() == 1)
      agdb_sleep <-
        apply_tudor_locke(agdb_state,
                          n_bedtime_start = params$n_bedtime_start,
                          n_wake_time_end = params$n_wake_time_end,
                          min_sleep_period = params$min_sleep_period,
                          max_sleep_period = params$max_sleep_period,
                          min_nonzero_epochs = params$min_nonzero_epochs)
      actilife_sleep <- sleep_periods %>%
        filter(period_algorithm == algorithm, subjectname == subject)

      if (nrow(actilife_sleep) == 0) next

      agdb_sleep <- agdb_sleep %>%
        inner_join(actilife_sleep %>% select(in_bed_timestamp,
                                             out_bed_timestamp),
                   by = c("in_bed_timestamp", "out_bed_timestamp"))

      for (var in c("in_bed_timestamp", "out_bed_timestamp",
                    "total_counts", "awakenings",
                    "time_in_bed", "time_awake")) {
        expect_equal(agdb_sleep[[var]], actilife_sleep[[var]])
      }
      for (var in c("efficiency", "ave_awakening",
                    "movement_index", "fragmentation_index")) {
        expect_lt(max(abs(agdb_sleep[[var]] - actilife_sleep[[var]])), 0.01)
      }
    }
  }
})
test_that("apply_tudor_locke on a grouped tibble of 200 agd datasets", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  agd_path <- path.expand(file.path("~",
                                    "Documents", "GitHub", "PG-Sensors",
                                    "BabySleepStudy", "data", "raw"))
  load(system.file("extdata", "test-sleep-periods.rda",
                   package = "actigraph.sleepr"))

  batch_agdb_epoch <- NULL
  batch_agdb_sleep <- NULL
  n_bedtime_start <- 10
  n_wake_time_end <- 10
  min_sleep_period <- 45
  max_sleep_period <- 1440
  min_nonzero_epochs <- 15

  # Sometimes I might want to do just a few tests....
  for (filebase in subject_n200$file[1:10]) {
    subject <- strsplit(filebase, " ")[[1]][1]
    agdb_epoch <- read_agd(file.path(agd_path, paste0(filebase, "10sec.agd")))
    batch_agdb_epoch <- bind_rows(batch_agdb_epoch,
                                  agdb_epoch %>% mutate(subject = subject))
    agdb_epoch <- collapse_epochs(agdb_epoch, 60)
    agdb_sleep <- apply_tudor_locke(apply_sadeh(agdb_epoch),
                                    n_bedtime_start = n_bedtime_start,
                                    n_wake_time_end = n_wake_time_end,
                                    min_sleep_period = min_sleep_period,
                                    max_sleep_period = max_sleep_period,
                                    min_nonzero_epochs = min_nonzero_epochs)
    batch_agdb_sleep <- bind_rows(batch_agdb_sleep,
                                  agdb_sleep %>% mutate(subject = subject))
  }
  attr(batch_agdb_epoch, "epochlength") <- 10
  batch_agdb_epoch <- batch_agdb_epoch %>% group_by(subject)
  batch_agdb_epoch <- collapse_epochs(batch_agdb_epoch, 60)
  expect_equivalent(batch_agdb_sleep,
                    apply_tudor_locke(apply_sadeh(batch_agdb_epoch),
                                      n_bedtime_start = n_bedtime_start,
                                      n_wake_time_end = n_wake_time_end,
                                      min_sleep_period = min_sleep_period,
                                      max_sleep_period = max_sleep_period,
                                      min_nonzero_epochs = min_nonzero_epochs))
})
