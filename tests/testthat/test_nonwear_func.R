
library("actigraph.sleepr")
library("dplyr")

context("Non-wear detection algorithm")
test_that("apply_troiano returns a tbl_period", {
  file <- system.file("extdata", "GT3XPlus-RawData-Day01-10sec.agd",
                      package = "actigraph.sleepr")
  agdb_10s <- read_agd(file)
  agdb_60s <- collapse_epochs(agdb_10s, 60)
  agdb_nonwear <- apply_troiano(agdb_60s)
  expect_s3_class(agdb_nonwear, "tbl_period")
})
test_that("apply_troiano return same result as ActiLife 6", {
  dataset <- "GT3XPlus-RawData-Day01"
  for (method in c("seq", "nonseq")) {
    agd_file <- system.file("extdata", paste0(dataset, "-10sec60sec.agd"),
                            package = "actigraph.sleepr")
    csv_file <- system.file("extdata", paste0(dataset, "-Troiano-", method,
                                             "-periods.csv"),
                            package = "actigraph.sleepr")
    for (algorithm in c("Troiano Default",
                        "Troiano Custom1",
                        "Troiano Custom2")) {
      actilife <- read_csv(csv_file) %>%
        filter(nonwear_algorithm == algorithm)
      params <- actilife %>% filter(row_number() == 1)
      actilife <- actilife %>% filter( wear == FALSE)
      agdb_10s <- read_agd(agd_file)
      agdb_60s <- collapse_epochs(agdb_10s, 60)
      agdb_nonwear <-
        apply_troiano(agdb_60s,
                      min_period_len = params$min_period_len,
                      spike_tolerance = params$spike_tolerance,
                      spike_stoplevel = params$spike_stoplevel,
                      endat_nnz_seq = params$endat_nnz_seq)
      common_vars <- intersect(colnames(agdb_nonwear),
                               colnames(actilife))
      expect_equal(common_vars, c("period_start", "period_end", "length"))
      for (var in common_vars)
        expect_equal(agdb_nonwear[[var]], actilife[[var]])
    }
  }
})
test_that("apply_choi return same result as ActiLife 6", {
  for (dataset in c("GT3XPlus-RawData-Day01",
                    "ActiSleepPlus-RawData-Day01")) {
    agd_file <- system.file("extdata", paste0(dataset, "-10sec60sec.agd"),
                            package = "actigraph.sleepr")
    csv_file <- system.file("extdata", paste0(dataset, "-Choi-periods.csv"),
                            package = "actigraph.sleepr")
    for (algorithm in c("Choi Default",
                        "Choi Custom1",
                        "Choi Custom2")) {
      actilife <- read_csv(csv_file) %>%
        filter(nonwear_algorithm == algorithm)
      params <- actilife %>% filter(row_number() == 1)
      actilife <- actilife %>% filter( wear == FALSE)
      agdb_10s <- read_agd(agd_file)
      agdb_60s <- collapse_epochs(agdb_10s, 60)
      agdb_nonwear <-
        apply_choi(agdb_60s,
                   min_period_len = params$min_period_len,
                   min_window_len = params$min_window_len,
                   spike_tolerance = params$spike_tolerance)
      common_vars <- intersect(colnames(agdb_nonwear),
                               colnames(actilife))
      expect_equal(common_vars, c("period_start", "period_end", "length"))
      for (var in common_vars)
        expect_equal(agdb_nonwear[[var]], actilife[[var]])
    }
  }
})
