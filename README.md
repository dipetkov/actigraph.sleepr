[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/dipetkov/actigraph.sleepr?branch=master&svg=true)](https://ci.appveyor.com/project/dipetkov/actigraph.sleepr) [![Travis-CI Build Status](https://travis-ci.org/dipetkov/actigraph.sleepr.svg?branch=master)](https://travis-ci.org/dipetkov/actigraph.sleepr) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/actigraph.sleepr)](https://cran.r-project.org/package=actigraph.sleepr)

[![minimal R version](https://img.shields.io/badge/R%3E%3D-3.2.4-6666ff.svg)](https://cran.r-project.org/) [![packageversion](https://img.shields.io/badge/Package%20version-0.1.0-orange.svg?style=flat-square)](commits/master) [![Last-changedate](https://img.shields.io/badge/last%20change-2019--02--17-yellowgreen.svg)](/commits/master) [![Coverage status](https://codecov.io/gh/dipetkov/actigraph.sleepr/branch/master/graph/badge.svg)](https://codecov.io/github/dipetkov/actigraph.sleepr?branch=master)

<!-- README.md is generated from README.Rmd. Please edit that file -->
### actigraph.sleepr: Sleep and non-wear detection from ActiGraph data

The `actigraph.sleepr` package implements three sleep scoring/detection algorithms: Sadeh (Sadeh, Sharkey, and Carskadon 1994), Cole-Kripke (Cole et al. 1992) and Tudor-Locke (Tudor-Locke et al. 2014) as well as two non-wear detection algorithms: Troiano (Troiano et al. 2008) and Choi (Choi et al. 2011).

### Installation

``` r
library("devtools")
install_github("dipetkov/actigraph.sleepr")
```

### Read AGD file(s)

An AGD file is an SQLite database file exported by an ActiGraph device. See the [ActiLife 6 User manual](https://www.actigraphcorp.com/support/manuals/actilife-6-manual/). For illustration let's use GT3X+ sample data taken from ActiGraph's online documentation. \[The link I downloaded the data from, <https://actigraph.desk.com>, seems to be currently unavailable.\]

``` r
library("actigraph.sleepr")
#> Loading required package: dplyr
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
#> Loading required package: tidyr
file_10s <- system.file("extdata", "GT3XPlus-RawData-Day01.agd",
                        package = "actigraph.sleepr")
agdb_10s <- read_agd(file_10s)
```

The `read_agd` function loads the raw activity measurements into a convenient format: a tibble (data frame) of timestamped activity counts, whose attributes are the device settings. However, further manipulations of the tibble using the `dplyr` verbs (e.g., `mutate`, `inner_join`) might drop these non-standard attributes, i.e., the attributes are not always inherited.

``` r
attributes(agdb_10s)[-3]
#> $names
#>  [1] "timestamp"       "axis1"           "axis2"          
#>  [4] "axis3"           "steps"           "lux"            
#>  [7] "inclineoff"      "inclinestanding" "inclinesitting" 
#> [10] "inclinelying"   
#> 
#> $class
#> [1] "tibble"     "tbl_df"     "data.frame"
#> 
#> $age
#> [1] "43"
#> 
#> $batteryvoltage
#> [1] "4.22"
#> 
#> $culturename
#> [1] "English (United States)"
#> 
#> $dateOfBirth
#> [1] "1969-04-17 UTC"
#> 
#> $datetimeformat
#> [1] "M/d/yyyy"
#> 
#> $decimal
#> [1] "."
#> 
#> $devicename
#> [1] "GT3XPlus"
#> 
#> $deviceserial
#> [1] "NEO1DXXXXXXXX"
#> 
#> $deviceversion
#> [1] "2.5.0"
#> 
#> $dominance
#> [1] "Non-Dominant"
#> 
#> $downloaddatetime
#> [1] "2012-06-28 16:25:52 UTC"
#> 
#> $epochcount
#> [1] 8999
#> 
#> $epochlength
#> [1] 10
#> 
#> $filter
#> [1] "Normal"
#> 
#> $finished
#> [1] "true"
#> 
#> $grouping
#> [1] ","
#> 
#> $height
#> [1] "172.72"
#> 
#> $limb
#> [1] "Ankle"
#> 
#> $machinename
#> [1] "DESKTOP-51642G4"
#> 
#> $mass
#> [1] "69.8532249799612"
#> 
#> $modenumber
#> [1] "61"
#> 
#> $`original sample rate`
#> [1] "30"
#> 
#> $osversion
#> [1] "Microsoft Windows NT 10.0.14393.0"
#> 
#> $proximityIntervalInSeconds
#> [1] "0"
#> 
#> $race
#> [1] "White / Caucasian"
#> 
#> $sex
#> [1] "Male"
#> 
#> $side
#> [1] "Left"
#> 
#> $softwarename
#> [1] "ActiLife"
#> 
#> $softwareversion
#> [1] "6.13.3"
#> 
#> $startdatetime
#> [1] "2012-06-27 10:54:00 UTC"
#> 
#> $stopdatetime
#> [1] "2012-06-28 11:53:58 UTC"
#> 
#> $subjectname
#> [1] "GT3XPlus"
#> 
#> $unexpectedResets
#> [1] "0"
```

Since the data is stored in a tibble, we can use the dplyr verbs (mutate, select, filter, summarise, group\_by, arrange) to manipulate the data. For example, let's compute the magnitude of the three-axis counts (axis1 - vertical, axis2 - horizontal, axis3 - lateral).

``` r
suppressMessages(library("dplyr"))
agdb_10s <- agdb_10s %>% select(timestamp, starts_with("axis"))
agdb_10s %>%
  mutate(magnitude = sqrt(axis1 ^ 2 + axis2 ^ 2 + axis3 ^ 2)) %>%
  head()
#>             timestamp axis1 axis2 axis3 magnitude
#> 1 2012-06-27 10:54:00   377   397   413     685.8
#> 2 2012-06-27 10:54:10   465   816  1225    1543.6
#> 3 2012-06-27 10:54:20   505   444   713     980.1
#> 4 2012-06-27 10:54:30    73    91   106     157.6
#> 5 2012-06-27 10:54:40    45    43   115     130.8
#> 6 2012-06-27 10:54:50     0     0     0       0.0
```

### Reintegrate from 10s to 60s epochs

The Sadeh and Cole-Kripke algorithms for converting activity measurements into asleep/awake indicators were developed for 60s epochs. If the data is in smaller epochs, we need to collapse or aggregate the epochs. The example data is in 10s epochs. So we aggregate the epochs from 10s to 60s by adding the counts for the six consecutive 10s epochs that fall into the same 60s epoch.

``` r
# Collapse epochs from 10 sec to 60 sec by summing
agdb_60s <- agdb_10s %>% collapse_epochs(60)
agdb_60s
#> # A tibble: 1,500 x 4
#>    timestamp           axis1 axis2 axis3
#>    <dttm>              <int> <int> <int>
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
#> # … with 1,490 more rows
```

### Sleep scoring with the Sadeh algorithm

The Sadeh algorithm is primarily used for younger adolescents as the supporting research was performed on children and young adults. It requires 60s epochs and uses an 11-minute window that includes the five previous and five future epochs. The `apply_sadeh` function implements the algorithm as described in the ActiGraph user manual.

``` r
agdb_60s %>% apply_sadeh()
#> # A tibble: 1,500 x 6
#>    timestamp           axis1 axis2 axis3 count sleep
#>    <dttm>              <int> <int> <int> <dbl> <chr>
#>  1 2012-06-27 10:54:00  1465  1791  2572   300 W    
#>  2 2012-06-27 10:55:00   207   218   270   207 W    
#>  3 2012-06-27 10:56:00   169   257   270   169 W    
#>  4 2012-06-27 10:57:00     0     0     0     0 W    
#>  5 2012-06-27 10:58:00   157   174   248   157 W    
#>  6 2012-06-27 10:59:00    23    23   279    23 W    
#>  7 2012-06-27 11:00:00     0     0     0     0 S    
#>  8 2012-06-27 11:01:00     0     0     0     0 S    
#>  9 2012-06-27 11:02:00     0     0     0     0 S    
#> 10 2012-06-27 11:03:00     0     0     0     0 S    
#> # … with 1,490 more rows
```

### Sleep scoring with the Cole-Kripke algorithm

The Cole-Kripke algorithm is primarily used for adult populations as the supporting research was performed on subjects ranging from 35 to 65 years of age. Like the Sadeh algorithm, it requires 60s epochs and uses a 7-minute window that includes the four previous and two future epochs. The `apply_cole` function implements the algorithm as described in the ActiGraph user manual.

``` r
agdb_60s %>% apply_cole_kripke()
#> # A tibble: 1,500 x 6
#>    timestamp           axis1 axis2 axis3 count sleep
#>    <dttm>              <int> <int> <int> <dbl> <chr>
#>  1 2012-06-27 10:54:00  1465  1791  2572 14.6  W    
#>  2 2012-06-27 10:55:00   207   218   270  2.07 W    
#>  3 2012-06-27 10:56:00   169   257   270  1.69 W    
#>  4 2012-06-27 10:57:00     0     0     0  0    W    
#>  5 2012-06-27 10:58:00   157   174   248  1.57 W    
#>  6 2012-06-27 10:59:00    23    23   279  0.23 S    
#>  7 2012-06-27 11:00:00     0     0     0  0    S    
#>  8 2012-06-27 11:01:00     0     0     0  0    S    
#>  9 2012-06-27 11:02:00     0     0     0  0    S    
#> 10 2012-06-27 11:03:00     0     0     0  0    S    
#> # … with 1,490 more rows
```

### Sleep period detection with the Tudor-Locke algorithm

Once each one-minute epoch is labeled as asleep (S) or awake (W), we can use the Tudor-Locke algorithm to detect periods of time in bed and, for each period, to compute sleep quality metrics such as total minutes in bed, total sleep time, number and average length of awakenings, movement and fragmentation index.

``` r
agdb_60s %>% apply_sadeh() %>% apply_tudor_locke()
#>           in_bed_time        out_bed_time               onset latency
#> 1 2012-06-28 00:03:00 2012-06-28 07:38:00 2012-06-28 00:03:00       0
#>   efficiency duration activity_counts nonzero_epochs total_sleep_time
#> 1      97.14      455            9126             27              442
#>   wake_after_onset nb_awakenings ave_awakening movement_index
#> 1               13             4          3.25          5.934
#>   fragmentation_index sleep_fragmentation_index
#> 1                  40                     45.93
```

### Non-wear period detection with the Troiano and Choi algorithms

Long stretches that consist almost entirely of zero counts (zero epochs) suggest that the device wasn't worn at all and therefore should be excluded from downstream analysis. The Troiano algorithm for detecting periods of non-wear formalizes a technique used to analyze the 2003-2004 NHANES data, which allows a non-wear period to contain a few nonzero epochs of artifactual movement (spikes). The Choi algorithm extends the Troiano algorithm by requiring that short spikes of artifactual movement during a non-wear period are preceded and followed by a fixed number of consecutive zero epochs.

``` r
agdb_60s %>% apply_troiano()
#>          period_start          period_end length
#> 1 2012-06-28 00:00:00 2012-06-28 02:37:00    157
#> 2 2012-06-28 02:46:00 2012-06-28 03:59:00     73
#> 3 2012-06-28 05:50:00 2012-06-28 07:25:00     95
agdb_60s %>% apply_choi()
#>   period_start          period_end length
#> 1   2012-06-28 2012-06-28 02:37:00    157
```

### References

Choi, Leena, Zhouwen Liu, Charles E. Matthews, and Maciej S. Buchowski. 2011. “Validation of Accelerometer Wear and Nonwear Time Classification Algorithm.” *Medicine & Science in Sports & Exercise* 43 (2): 357–64.

Cole, Roger J, Daniel F Kripke, William Gruen, Daniel J Mullaney, and J Christian Gillin. 1992. “Automatic Sleep/Wake Identification from Wrist Activity.” *Sleep* 15 (5): 461–69.

Sadeh, Avi, Katherine M Sharkey, and Mary A Carskadon. 1994. “Activity Based Sleep-Wake Identification: An Empirical Test of Methodological Issues.” *Sleep* 17 (3): 201–7.

Troiano, Richard P, David Berrigan, Kevin W Dodd, Louise C Mâsse, Timothy Tilert, and Margaret McDowell. 2008. “Physical Activity in the United States Measured by Accelerometer.” *Medicine & Science in Sports & Exercise* 40 (1): 181–88.

Tudor-Locke, Catrine, Tiago V. Barreira, John M. Schuna, Emily F. Mire, and Peter T. Katzmarzyk. 2014. “Fully Automated Waist-Worn Accelerometer Algorithm for Detecting Children’s Sleep-Period Time Separate from 24-H Physical Activity or Sedentary Behaviors.” *Applied Physiology, Nutrition, and Metabolism* 39 (1): 53–57.
