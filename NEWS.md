# actigraph.sleepr 0.5.0

## Breaking changes

* The package no longer re-exports the magrittr pipe `%>%`. All code,
  examples and vignettes now use the native pipe `|>`. Load `dplyr` or
  `magrittr` to keep using `%>%`.
* Now requires R >= 4.1.0 (for the native pipe).

## Dependencies

* Reduced hard dependencies from 43 to 28 recursive packages:
  * `tidyr`, `purrr` and `magrittr` are no longer used.
  * `ggplot2` moved to Suggests; `plot_activity()` and
    `plot_activity_period()` ask to install it when needed.
  * `zoo` (`na.locf()`, `na.spline()`, `na.trim()`) replaced by internal
    helpers; `zoo` is in Suggests only to test them for identical results.
  * `RcppRoll` (`roll_mean()`, `roll_sd()`, `roll_sum()`, used by
    `apply_sadeh()`) replaced by internal C++ functions; `RcppRoll` is in
    Suggests only to test them against it.
* `impute_epochs()` uses `tidyselect::eval_select()` instead of the
  superseded `tidyselect::vars_select()`.

# actigraph.sleepr 0.4.0

* Added Webster rescoring for Cole-Kripke.
* Added Oakley sleep algorithm.

# actigraph.sleepr 0.3.1

* Added new tests and fixes for spelling.
  
# actigraph.sleepr 0.3.0

* First CRAN submission.
* Read `*.agd` files directly via `DBI` and `RSQLite`, removing the
  dependency on `dbplyr` (#16, @muschellij2).

# actigraph.sleepr 0.2.0

* Fixes bugs and updates to `dplyr` and `tidyr`.

# actigraph.sleepr 0.1.0

* Initial release for sleep detection using standard algorithms.
