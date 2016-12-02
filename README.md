<!-- README.md is generated from README.Rmd. Please edit that file -->

### actigraph.sleepr: Sleep detection from ActiGraph data using standard algorithms

---------------

[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/dipetkov/actigraph.sleepr?branch=master&svg=true)](https://ci.appveyor.com/project/dipetkov/actigraph.sleepr)
[![Travis-CI Build Status](https://travis-ci.org/dipetkov/actigraph.sleepr.svg?branch=master)](https://travis-ci.org/dipetkov/actigraph.sleepr)
[![codecov](https://codecov.io/gh/dipetkov/actigraph.sleepr/branch/master/graph/badge.svg)](https://codecov.io/gh/dipetkov/actigraph.sleepr)



The `actigraph.sleepr` package implements functions to read AGD files and to apply three standard sleep algorithms: Sadeh, Cole-Kripke and Tudor-Locke.

### Installation


```r
library("devtools")
install_github("dipetkov/actigraph.sleepr")
```

### Read AGD file(s)

An AGD file is an SQLite database file exported by an ActiGraph device. See the [ActiLife 6 User manual](http://actigraphcorp.com/support/manuals/actilife-6-manual/). For illustration let's use GT3X+ sample data taken from [ActiGraph's online documentation](https://actigraph.desk.com).


```r
library("actigraph.sleepr")
file_10s <- system.file("extdata", "GT3XPlus-RawData-Day01-10sec.agd",
                        package = "actigraph.sleepr")
agdb_10s <- read_agd(file_10s)
```

The `read_agd` function loads the raw activity measurements into a convenient format: a `dplyr` data frame (a tibble) of timestamped activity counts, whose attributes are the device settings.


```r
str(agdb_10s)
#> Classes 'tbl_agd', 'tbl_df', 'tbl' and 'data.frame':	8999 obs. of  10 variables:
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
#>  - attr(*, "dateOfBirth")= chr "621132192000000000"
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

Since the data is stored in a tibble, we can use the `dplyr` verbs to manipulate the data. For example, let's compute the vector magnitude from the axis variables. The formula for vector magnitude (norm) is straightforward. Let $x$, $y$ and $z$ be the three axis measurements, `axis1`, `axis2` and `axis3`, respectively. Then the magnitude of the movement vector is $$\sqrt{x^2 + y^2 + z^2}$$.


```r
library("dplyr")
agdb_10s %>%
  rename(y = axis1, x = axis2, z = axis3) %>%
  mutate(magnitude = sqrt(x^2 + y^2 + z^2))
#> # A tibble: 8,999 × 11
#>              timestamp     y     x     z steps   lux inclineoff
#> *               <dttm> <int> <int> <int> <int> <int>      <int>
#> 1  2012-06-27 10:54:00   377   397   413     2     0          0
#> 2  2012-06-27 10:54:10   465   816  1225     4     0          0
#> 3  2012-06-27 10:54:20   505   444   713     6     0          0
#> 4  2012-06-27 10:54:30    73    91   106     1     0          0
#> 5  2012-06-27 10:54:40    45    43   115     0     0          0
#> 6  2012-06-27 10:54:50     0     0     0     0     0          0
#> 7  2012-06-27 10:55:00     0     0     0     0     0          0
#> 8  2012-06-27 10:55:10   207   218   270     1     0          0
#> 9  2012-06-27 10:55:20     0     0     0     0     0          0
#> 10 2012-06-27 10:55:30     0     0     0     0     0          0
#> # ... with 8,989 more rows, and 4 more variables: inclinestanding <int>,
#> #   inclinesitting <int>, inclinelying <int>, magnitude <dbl>
```

### Reintegrate from 10s to 60s epochs

The standard algorithms for converting activity measurements into asleep/awake indicators -- Sadeh and Cole-Kripke -- were developed for 60s epochs. If the data is in smaller epochs, we need to collapse or aggregate the epochs. The example data is in 10s epochs. So we aggregate the epochs from 10 sec to 60 sec by adding the counts for the six consecutive 10s epochs that fall in the same 60s epoch.


```r
# Collapse epochs from 10 sec to 60 sec by summing
agdb_60s <- collapse_epochs(agdb_10s, 60)
agdb_60s
#> # A tibble: 1,500 × 9
#>              timestamp axis1 axis2 axis3 steps inclineoff inclinestanding
#>                 <dttm> <int> <int> <int> <int>      <int>           <int>
#> 1  2012-06-27 10:54:00  1465  1791  2572    13          0              40
#> 2  2012-06-27 10:55:00   207   218   270     1          0              10
#> 3  2012-06-27 10:56:00   169   257   270     3          0              11
#> 4  2012-06-27 10:57:00     0     0     0     0          0               0
#> 5  2012-06-27 10:58:00   157   174   248     1          0              10
#> 6  2012-06-27 10:59:00    23    23   279     1          0               0
#> 7  2012-06-27 11:00:00     0     0     0     0          0               0
#> 8  2012-06-27 11:01:00     0     0     0     0          0               0
#> 9  2012-06-27 11:02:00     0     0     0     0          0               0
#> 10 2012-06-27 11:03:00     0     0     0     0          0               0
#> # ... with 1,490 more rows, and 2 more variables: inclinesitting <int>,
#> #   inclinelying <int>
```

### Apply the Sadeh algorithm

The Sadeh sleep scoring algorithm is primarily used for younger adolescents as the supporting research was performed on children and young adults. It requires 60s epochs and uses an 11-minute window that includes the five previous and five future epochs. The `apply_sadeh` function in the `actigraph.sleepr` package implements the algorithm as described in the ActiGraph user manual.


```r
agdb_sadeh <- apply_sadeh(agdb_60s)
agdb_sadeh
#> # A tibble: 1,500 × 11
#>              timestamp axis1 axis2 axis3 steps inclineoff inclinestanding
#>                 <dttm> <int> <int> <int> <int>      <int>           <int>
#> 1  2012-06-27 10:54:00  1465  1791  2572    13          0              40
#> 2  2012-06-27 10:55:00   207   218   270     1          0              10
#> 3  2012-06-27 10:56:00   169   257   270     3          0              11
#> 4  2012-06-27 10:57:00     0     0     0     0          0               0
#> 5  2012-06-27 10:58:00   157   174   248     1          0              10
#> 6  2012-06-27 10:59:00    23    23   279     1          0               0
#> 7  2012-06-27 11:00:00     0     0     0     0          0               0
#> 8  2012-06-27 11:01:00     0     0     0     0          0               0
#> 9  2012-06-27 11:02:00     0     0     0     0          0               0
#> 10 2012-06-27 11:03:00     0     0     0     0          0               0
#> # ... with 1,490 more rows, and 4 more variables: inclinesitting <int>,
#> #   inclinelying <int>, count <dbl>, state <chr>
```

### Apply the Cole-Kripke algorithm

The Cole-Kripke sleep scoring algorithm is primarily used for adult populations as the supporting research was performed on subjects ranging from 35 to 65 years of age. Like the Sadeh algorithm, it requires 60s epochs and uses a 7-minute window that includes the four previous and two future epochs. The `apply_cole` function in the `actigraph.sleepr` package implements the algorithm as described in the ActiGraph user manual.


```r
agdb_colekripke <- apply_cole_kripke(agdb_60s)
agdb_colekripke
#> # A tibble: 1,500 × 11
#>              timestamp axis1 axis2 axis3 steps inclineoff inclinestanding
#>                 <dttm> <int> <int> <int> <int>      <int>           <int>
#> 1  2012-06-27 10:54:00  1465  1791  2572    13          0              40
#> 2  2012-06-27 10:55:00   207   218   270     1          0              10
#> 3  2012-06-27 10:56:00   169   257   270     3          0              11
#> 4  2012-06-27 10:57:00     0     0     0     0          0               0
#> 5  2012-06-27 10:58:00   157   174   248     1          0              10
#> 6  2012-06-27 10:59:00    23    23   279     1          0               0
#> 7  2012-06-27 11:00:00     0     0     0     0          0               0
#> 8  2012-06-27 11:01:00     0     0     0     0          0               0
#> 9  2012-06-27 11:02:00     0     0     0     0          0               0
#> 10 2012-06-27 11:03:00     0     0     0     0          0               0
#> # ... with 1,490 more rows, and 4 more variables: inclinesitting <int>,
#> #   inclinelying <int>, count <dbl>, state <chr>
```

What is the agreement between the Sadeh and Cole-Kripke asleep/awake algorithms, on the example dataset?


```r
table(agdb_sadeh$state, agdb_colekripke$state)
#>    
#>       S   W
#>   S 881  56
#>   W 114 449
```

### Apply the Tudor-Locke algorithm

Once each one-minute epoch is labeled as asleep (S) or awake (W), we can use the Tudor-Locke algorithm to detect periods of time in bed and, for each period, to compute sleep quality metrics such as total minutes in bed, total sleep time, number and average length of awakenings, movement and fragmentation index.


```r
agdb_sleep <- apply_tudor_locke(agdb_sadeh)
agdb_sleep
#> # A tibble: 1 × 14
#>      in_bed_timestamp   out_bed_timestamp     onset_timestamp latency
#> *              <dttm>              <dttm>              <dttm>   <int>
#> 1 2012-06-28 00:03:00 2012-06-28 07:38:00 2012-06-28 00:03:00       0
#> # ... with 10 more variables: total_counts <int>, efficiency <dbl>,
#> #   time_in_bed <int>, time_asleep <int>, time_awake <int>,
#> #   awakenings <int>, ave_awakening <dbl>, movement_index <dbl>,
#> #   fragmentation_index <dbl>, sleep_fragmentation_index <dbl>
```
