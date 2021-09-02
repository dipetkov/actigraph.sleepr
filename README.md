<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/dipetkov/actigraph.sleepr?branch=master&svg=true)](https://ci.appveyor.com/project/dipetkov/actigraph.sleepr)
[![Build
Status](https://travis-ci.org/dipetkov/actigraph.sleepr.svg?branch=master)](https://travis-ci.org/dipetkov/actigraph.sleepr)
[![codecov](https://codecov.io/gh/dipetkov/actigraph.sleepr/branch/master/graph/badge.svg)](https://codecov.io/gh/dipetkov/actigraph.sleepr)
<!-- badges: end -->

<!-- README.md is generated from README.Rmd. Please edit that file -->

### `actigraph.sleepr`: Sleep and non-wear detection from ActiGraph data

The `actigraph.sleepr` package implements three sleep scoring/detection
algorithms: Sadeh (Sadeh, Sharkey, and Carskadon 1994), Cole-Kripke
(Cole et al. 1992) and Tudor-Locke (Tudor-Locke et al. 2014) as well as
two non-wear detection algorithms: Troiano (Troiano et al. 2008) and
Choi (Choi et al. 2011).

### Installation

    # install.packages("remotes")
    remotes::install_github("dipetkov/actigraph.sleepr")

### Read AGD file(s)

An AGD file is an SQLite database file exported by an ActiGraph device.
See the [ActiLife 6 User
manual](https://www.actigraphcorp.com/support/manuals/actilife-6-manual/).
For illustration let’s use one day of sample data recorded by a GT3X+
monitor.

    library("actigraph.sleepr")
    file_10s <- system.file("extdata", "GT3XPlus-RawData-Day01.agd",
      package = "actigraph.sleepr"
    )
    agdb_10s <- read_agd(file_10s)

The `read_agd` function loads the raw activity measurements into a
convenient format: a tibble (data frame) of timestamped activity counts,
whose attributes are the device settings. However, further manipulations
of the tibble using the `dplyr` verbs (e.g., `mutate`, `inner_join`)
might drop these non-standard attributes, i.e., the attributes are not
always inherited.

    attributes(agdb_10s)[10:12]
    #> $devicename
    #> [1] "GT3XPlus"
    #> 
    #> $deviceserial
    #> [1] "NEO1DXXXXXXXX"
    #> 
    #> $deviceversion
    #> [1] "2.5.0"

Since the data is stored in a tibble, we can use the dplyr verbs
(mutate, select, filter, summarise, group\_by, arrange) to manipulate
the data. For example, let’s compute the vector magnitude of the
three-axis counts (axis1 - vertical, axis2 - horizontal, axis3 -
lateral).

    suppressMessages(library("dplyr"))
    agdb_10s <- agdb_10s %>% select(timestamp, starts_with("axis"))
    agdb_10s %>%
      mutate(magnitude = sqrt(axis1^2 + axis2^2 + axis3^2))
    #> # A tibble: 8,999 x 5
    #>    timestamp           axis1 axis2 axis3 magnitude
    #>    <dttm>              <int> <int> <int>     <dbl>
    #>  1 2012-06-27 10:54:00   377   397   413      686.
    #>  2 2012-06-27 10:54:10   465   816  1225     1544.
    #>  3 2012-06-27 10:54:20   505   444   713      980.
    #>  4 2012-06-27 10:54:30    73    91   106      158.
    #>  5 2012-06-27 10:54:40    45    43   115      131.
    #>  6 2012-06-27 10:54:50     0     0     0        0 
    #>  7 2012-06-27 10:55:00     0     0     0        0 
    #>  8 2012-06-27 10:55:10   207   218   270      404.
    #>  9 2012-06-27 10:55:20     0     0     0        0 
    #> 10 2012-06-27 10:55:30     0     0     0        0 
    #> # … with 8,989 more rows

### Reintegrate from 10s to 60s epochs

The Sadeh and Cole-Kripke algorithms for converting activity
measurements into asleep/awake indicators were developed for 60s epochs.
If the data is in smaller epochs, we need to collapse or aggregate the
epochs. The example data is in 10s epochs. So we aggregate the epochs
from 10s to 60s by adding the counts for the six consecutive 10s epochs
that fall into the same 60s epoch.

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

### Sleep scoring with the Sadeh algorithm

The Sadeh algorithm is primarily used for younger adolescents as the
supporting research was performed on children and young adults. It takes
60s epochs and uses an 11-minute window that includes the five previous
and five future epochs. The `apply_sadeh` function implements the
algorithm as described in the ActiGraph user manual.

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

### Sleep scoring with the Cole-Kripke algorithm

The Cole-Kripke algorithm is primarily used for adult populations as the
supporting research was performed on subjects ranging from 35 to 65
years of age. Like the Sadeh algorithm, it takes 60s epochs and uses a
7-minute window that includes the four previous and two future epochs.
The `apply_cole` function implements the algorithm as described in the
ActiGraph user manual.

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

### Sleep period detection with the Tudor-Locke algorithm

Once each one-minute epoch is labeled as asleep (S) or awake (W), we can
use the Tudor-Locke algorithm to detect periods of time in bed and, for
each period, to compute sleep quality metrics such as total minutes in
bed, total sleep time, number and average length of awakenings, movement
and fragmentation index.

    agdb_60s %>%
      apply_sadeh() %>%
      apply_tudor_locke()
    #>           in_bed_time        out_bed_time               onset latency
    #> 1 2012-06-28 00:03:00 2012-06-28 07:38:00 2012-06-28 00:03:00       0
    #>   efficiency duration activity_counts nonzero_epochs total_sleep_time
    #> 1   97.14286      455            9126             27              442
    #>   wake_after_onset nb_awakenings ave_awakening movement_index
    #> 1               13             4          3.25       5.934066
    #>   fragmentation_index sleep_fragmentation_index
    #> 1                  40                  45.93407

### Non-wear period detection with the Troiano and Choi algorithms

Long stretches that consist almost entirely of zero counts (zero epochs)
suggest that the device wasn’t worn at all and therefore should be
excluded from downstream analysis. The Troiano algorithm for detecting
periods of non-wear formalizes a technique used to analyze the 2003-2004
NHANES data, which allows a non-wear period to contain a few nonzero
epochs of artifactual movement (spikes). The Choi algorithm extends the
Troiano algorithm by requiring that short spikes of artifactual movement
during a non-wear period are preceded and followed by a fixed number of
consecutive zero epochs.

    agdb_60s %>% apply_troiano()
    #>          period_start          period_end length
    #> 1 2012-06-28 00:00:00 2012-06-28 02:37:00    157
    #> 2 2012-06-28 02:46:00 2012-06-28 03:59:00     73
    #> 3 2012-06-28 05:50:00 2012-06-28 07:25:00     95
    agdb_60s %>% apply_choi()
    #>   period_start          period_end length
    #> 1   2012-06-28 2012-06-28 02:37:00    157

### References

<div id="refs" class="references hanging-indent">

<div id="ref-Choi:2011aa">

Choi, Leena, Zhouwen Liu, Charles E. Matthews, and Maciej S. Buchowski.
2011. “Validation of Accelerometer Wear and Nonwear Time Classification
Algorithm.” *Medicine & Science in Sports & Exercise* 43 (2): 357–64.

</div>

<div id="ref-Cole:1992aa">

Cole, Roger J, Daniel F Kripke, William Gruen, Daniel J Mullaney, and J
Christian Gillin. 1992. “Automatic Sleep/Wake Identification from Wrist
Activity.” *Sleep* 15 (5): 461–69.

</div>

<div id="ref-Sadeh:1994aa">

Sadeh, Avi, Katherine M Sharkey, and Mary A Carskadon. 1994. “Activity
Based Sleep-Wake Identification: An Empirical Test of Methodological
Issues.” *Sleep* 17 (3): 201–7.

</div>

<div id="ref-Troiano:2008aa">

Troiano, Richard P, David Berrigan, Kevin W Dodd, Louise C Mâsse,
Timothy Tilert, and Margaret McDowell. 2008. “Physical Activity in the
United States Measured by Accelerometer.” *Medicine & Science in Sports
& Exercise* 40 (1): 181–88.

</div>

<div id="ref-Tudor-Locke:2014aa">

Tudor-Locke, Catrine, Tiago V. Barreira, John M. Schuna, Emily F. Mire,
and Peter T. Katzmarzyk. 2014. “Fully Automated Waist-Worn Accelerometer
Algorithm for Detecting Children’s Sleep-Period Time Separate from 24-H
Physical Activity or Sedentary Behaviors.” *Applied Physiology,
Nutrition, and Metabolism* 39 (1): 53–57.

</div>

</div>
