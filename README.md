[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/dipetkov/actigraph.sleepr?branch=master&svg=true)](https://ci.appveyor.com/project/dipetkov/actigraph.sleepr) [![Travis-CI Build Status](https://travis-ci.org/dipetkov/actigraph.sleepr.svg?branch=master)](https://travis-ci.org/dipetkov/actigraph.sleepr) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/actigraph.sleepr)](https://cran.r-project.org/package=actigraph.sleepr)

[![minimal R version](https://img.shields.io/badge/R%3E%3D-3.2.4-6666ff.svg)](https://cran.r-project.org/) [![packageversion](https://img.shields.io/badge/Package%20version-0.1.0-orange.svg?style=flat-square)](commits/master) [![Last-changedate](https://img.shields.io/badge/last%20change-2017--09--08-yellowgreen.svg)](/commits/master) [![codecov](https://codecov.io/gh/dipetkov/actigraph.sleepr/branch/master/graph/badge.svg)](https://codecov.io/gh/dipetkov/actigraph.sleepr)

<!-- README.md is generated from README.Rmd. Please edit that file -->
### actigraph.sleepr: Sleep and non-wear detection from ActiGraph data

The `actigraph.sleepr` package implements three sleep scoring/detection algorithms: Sadeh (Sadeh, Sharkey, and Carskadon 1994), Cole-Kripke (Cole et al. 1992) and Tudor-Locke (Tudor-Locke et al. 2014) as well as two non-wear detection algorithms: Troiano (Troiano et al. 2008) and Choi (Choi et al. 2011).

### Installation

``` r
library("devtools")
install_github("dipetkov/actigraph.sleepr")
```

### Read AGD file(s)

An AGD file is an SQLite database file exported by an ActiGraph device. See the [ActiLife 6 User manual](http://actigraphcorp.com/support/manuals/actilife-6-manual/). For illustration let's use GT3X+ sample data taken from ActiGraph's [online documentation](https://actigraph.desk.com).

``` r
library("actigraph.sleepr")
file_10s <- system.file("extdata", "GT3XPlus-RawData-Day01.agd",
                        package = "actigraph.sleepr")
agdb_10s <- read_agd(file_10s)
```

The `read_agd` function loads the raw activity measurements into a convenient format: a tibble (data frame) of timestamped activity counts, whose attributes are the device settings.

``` r
str(agdb_10s)
#> Classes 'tbl_agd', 'tbl_df', 'tbl' and 'data.frame': 8999 obs. of  10 variables:
#>  $ timestamp      : POSIXct, format: "2012-06-27 10:54:00" "2012-06-27 10:54:10" ...
#>  $ axis1          : int  377 465 505 73 45 0 0 207 0 0 ...
#>  $ axis2          : int  397 816 444 91 43 0 0 218 0 0 ...
#>  $ axis3          : int  413 1225 713 106 115 0 0 270 0 0 ...
#>  $ steps          : int  2 4 6 1 0 0 0 1 0 0 ...
#>  $ lux            : int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ inclineoff     : int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ inclinestanding: int  10 10 10 10 0 0 0 10 0 0 ...
#>  $ inclinesitting : int  0 0 0 0 10 10 10 0 10 10 ...
#>  $ inclinelying   : int  0 0 0 0 0 0 0 0 0 0 ...
#>  - attr(*, "age")= chr "43"
#>  - attr(*, "batteryvoltage")= chr "4.22"
#>  - attr(*, "culturename")= chr "English (United States)"
#>  - attr(*, "dateOfBirth")= POSIXct, format: "1969-04-17"
#>  - attr(*, "datetimeformat")= chr "M/d/yyyy"
#>  - attr(*, "decimal")= chr "."
#>  - attr(*, "devicename")= chr "GT3XPlus"
#>  - attr(*, "deviceserial")= chr "NEO1DXXXXXXXX"
#>  - attr(*, "deviceversion")= chr "2.5.0"
#>  - attr(*, "dominance")= chr "Non-Dominant"
#>  - attr(*, "downloaddatetime")= POSIXct, format: "2012-06-28 16:25:52"
#>  - attr(*, "epochcount")= int 8999
#>  - attr(*, "epochlength")= int 10
#>  - attr(*, "filter")= chr "Normal"
#>  - attr(*, "finished")= chr "true"
#>  - attr(*, "grouping")= chr ","
#>  - attr(*, "height")= chr "172.72"
#>  - attr(*, "limb")= chr "Ankle"
#>  - attr(*, "machinename")= chr "DESKTOP-51642G4"
#>  - attr(*, "mass")= chr "69.8532249799612"
#>  - attr(*, "modenumber")= chr "61"
#>  - attr(*, "original sample rate")= chr "30"
#>  - attr(*, "osversion")= chr "Microsoft Windows NT 10.0.14393.0"
#>  - attr(*, "proximityIntervalInSeconds")= chr "0"
#>  - attr(*, "race")= chr "White / Caucasian"
#>  - attr(*, "sex")= chr "Male"
#>  - attr(*, "side")= chr "Left"
#>  - attr(*, "softwarename")= chr "ActiLife"
#>  - attr(*, "softwareversion")= chr "6.13.3"
#>  - attr(*, "startdatetime")= POSIXct, format: "2012-06-27 10:54:00"
#>  - attr(*, "stopdatetime")= POSIXct, format: "2012-06-28 11:53:58"
#>  - attr(*, "subjectname")= chr "GT3XPlus"
#>  - attr(*, "unexpectedResets")= chr "0"
```

Since the data is stored in a tibble, we can use the dplyr verbs (mutate, select, filter, summarise, group\_by, arrange) to manipulate the data. For example, let's compute the magnitude of the three-axis counts (axis1 - vertical, axis2 - horizontal, axis3 - lateral).

``` r
suppressMessages(library("dplyr"))
agdb_10s <- agdb_10s %>% select(timestamp, starts_with("axis"))
agdb_10s %>%
  mutate(magnitude = sqrt(axis1 ^ 2 + axis2 ^ 2 + axis3 ^ 2))
#> # A tibble: 8,999 x 5
#>              timestamp axis1 axis2 axis3 magnitude
#>                 <dttm> <int> <int> <int>     <dbl>
#>  1 2012-06-27 10:54:00   377   397   413     685.8
#>  2 2012-06-27 10:54:10   465   816  1225    1543.6
#>  3 2012-06-27 10:54:20   505   444   713     980.1
#>  4 2012-06-27 10:54:30    73    91   106     157.6
#>  5 2012-06-27 10:54:40    45    43   115     130.8
#>  6 2012-06-27 10:54:50     0     0     0       0.0
#>  7 2012-06-27 10:55:00     0     0     0       0.0
#>  8 2012-06-27 10:55:10   207   218   270     404.1
#>  9 2012-06-27 10:55:20     0     0     0       0.0
#> 10 2012-06-27 10:55:30     0     0     0       0.0
#> # ... with 8,989 more rows
```

### Reintegrate from 10s to 60s epochs

The Sadeh and Cole-Kripke algorithms for converting activity measurements into asleep/awake indicators were developed for 60s epochs. If the data is in smaller epochs, we need to collapse or aggregate the epochs. The example data is in 10s epochs. So we aggregate the epochs from 10s to 60s by adding the counts for the six consecutive 10s epochs that fall into the same 60s epoch.

``` r
# Collapse epochs from 10 sec to 60 sec by summing
agdb_60s <- agdb_10s %>% collapse_epochs(60)
agdb_60s
#> # A tibble: 1,500 x 4
#>              timestamp axis1 axis2 axis3
#>                 <dttm> <int> <int> <int>
#>  1 2012-06-27 10:54:00  1465  1791  2572
#>  2 2012-06-27 10:55:00   207   218   270
#>  3 2012-06-27 10:56:00   169   257   270
#>  4 2012-06-27 10:57:00     0     0     0
#>  5 2012-06-27 10:58:00   157   174   248
#>  6 2012-06-27 10:59:00    23    23   279
#>  7 2012-06-27 11:00:00     0     0     0
#>  8 2012-06-27 11:01:00     0     0     0
#>  9 2012-06-27 11:02:00     0     0     0
#> 10 2012-06-27 11:03:00     0     0     0
#> # ... with 1,490 more rows
```

### Sleep scoring with the Sadeh algorithm

The Sadeh algorithm is primarily used for younger adolescents as the supporting research was performed on children and young adults. It requires 60s epochs and uses an 11-minute window that includes the five previous and five future epochs. The `apply_sadeh` function implements the algorithm as described in the ActiGraph user manual.

``` r
agdb_60s %>% apply_sadeh()
#> # A tibble: 1,500 x 6
#>              timestamp axis1 axis2 axis3 count sleep
#>                 <dttm> <int> <int> <int> <dbl> <chr>
#>  1 2012-06-27 10:54:00  1465  1791  2572   300     W
#>  2 2012-06-27 10:55:00   207   218   270   207     W
#>  3 2012-06-27 10:56:00   169   257   270   169     W
#>  4 2012-06-27 10:57:00     0     0     0     0     W
#>  5 2012-06-27 10:58:00   157   174   248   157     W
#>  6 2012-06-27 10:59:00    23    23   279    23     W
#>  7 2012-06-27 11:00:00     0     0     0     0     S
#>  8 2012-06-27 11:01:00     0     0     0     0     S
#>  9 2012-06-27 11:02:00     0     0     0     0     S
#> 10 2012-06-27 11:03:00     0     0     0     0     S
#> # ... with 1,490 more rows
```

### Sleep scoring with the Cole-Kripke algorithm

The Cole-Kripke algorithm is primarily used for adult populations as the supporting research was performed on subjects ranging from 35 to 65 years of age. Like the Sadeh algorithm, it requires 60s epochs and uses a 7-minute window that includes the four previous and two future epochs. The `apply_cole` function implements the algorithm as described in the ActiGraph user manual.

``` r
agdb_60s %>% apply_cole_kripke()
#> # A tibble: 1,500 x 6
#>              timestamp axis1 axis2 axis3 count sleep
#>                 <dttm> <int> <int> <int> <dbl> <chr>
#>  1 2012-06-27 10:54:00  1465  1791  2572 14.65     W
#>  2 2012-06-27 10:55:00   207   218   270  2.07     W
#>  3 2012-06-27 10:56:00   169   257   270  1.69     W
#>  4 2012-06-27 10:57:00     0     0     0  0.00     W
#>  5 2012-06-27 10:58:00   157   174   248  1.57     W
#>  6 2012-06-27 10:59:00    23    23   279  0.23     S
#>  7 2012-06-27 11:00:00     0     0     0  0.00     S
#>  8 2012-06-27 11:01:00     0     0     0  0.00     S
#>  9 2012-06-27 11:02:00     0     0     0  0.00     S
#> 10 2012-06-27 11:03:00     0     0     0  0.00     S
#> # ... with 1,490 more rows
```

### Sleep period detection with the Tudor-Locke algorithm

Once each one-minute epoch is labeled as asleep (S) or awake (W), we can use the Tudor-Locke algorithm to detect periods of time in bed and, for each period, to compute sleep quality metrics such as total minutes in bed, total sleep time, number and average length of awakenings, movement and fragmentation index.

``` r
agdb_60s %>% apply_sadeh() %>% apply_tudor_locke()
#> # A tibble: 1 x 15
#>           in_bed_time        out_bed_time               onset latency
#> *              <dttm>              <dttm>              <dttm>   <int>
#> 1 2012-06-28 00:03:00 2012-06-28 07:38:00 2012-06-28 00:03:00       0
#> # ... with 11 more variables: efficiency <dbl>, duration <int>,
#> #   activity_counts <int>, nonzero_epochs <int>, total_sleep_time <int>,
#> #   wake_after_onset <int>, nb_awakenings <int>, ave_awakening <dbl>,
#> #   movement_index <dbl>, fragmentation_index <dbl>,
#> #   sleep_fragmentation_index <dbl>
```

### Non-wear period detection with the Troiano and Choi algorithms

Long stretches that consist almost entirely of zero counts (zero epochs) suggest that the device wasn't worn at all and therefore should be excluded from downstream analysis. The Troiano algorithm for detecting periods of non-wear formalizes a technique used to analyze the 2003-2004 NHANES data, which allows a non-wear period to contain a few nonzero epochs of artifactual movement (spikes). The Choi algorithm extends the Troiano algorithm by requiring that short spikes of artifactual movement during a non-wear period are preceded and followed by a fixed number of consecutive zero epochs.

``` r
agdb_60s %>% apply_troiano()
#> # A tibble: 3 x 3
#>          period_start          period_end length
#> *              <dttm>              <dttm>  <int>
#> 1 2012-06-28 00:00:00 2012-06-28 02:37:00    157
#> 2 2012-06-28 02:46:00 2012-06-28 03:59:00     73
#> 3 2012-06-28 05:50:00 2012-06-28 07:25:00     95
agdb_60s %>% apply_choi()
#> # A tibble: 1 x 3
#>   period_start          period_end length
#> *       <dttm>              <dttm>  <int>
#> 1   2012-06-28 2012-06-28 02:37:00    157
```

### References

Choi, Leena, Zhouwen Liu, Charles E. Matthews, and Maciej S. Buchowski. 2011. “Validation of Accelerometer Wear and Nonwear Time Classification Algorithm.” *Medicine & Science in Sports & Exercise* 43 (2): 357–64.

Cole, Roger J, Daniel F Kripke, William Gruen, Daniel J Mullaney, and J Christian Gillin. 1992. “Automatic Sleep/Wake Identification from Wrist Activity.” *Sleep* 15 (5): 461–69.

Sadeh, Avi, Katherine M Sharkey, and Mary A Carskadon. 1994. “Activity Based Sleep-Wake Identification: An Empirical Test of Methodological Issues.” *Sleep* 17 (3): 201–7.

Troiano, Richard P, David Berrigan, Kevin W Dodd, Louise C Mâsse, Timothy Tilert, and Margaret McDowell. 2008. “Physical Activity in the United States Measured by Accelerometer.” *Medicine & Science in Sports & Exercise* 40 (1): 181–88.

Tudor-Locke, Catrine, Tiago V. Barreira, John M. Schuna, Emily F. Mire, and Peter T. Katzmarzyk. 2014. “Fully Automated Waist-Worn Accelerometer Algorithm for Detecting Children’s Sleep-Period Time Separate from 24-H Physical Activity or Sedentary Behaviors.” *Applied Physiology, Nutrition, and Metabolism* 39 (1): 53–57.
