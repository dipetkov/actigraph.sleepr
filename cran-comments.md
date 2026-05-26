## Test environments
* local: macOS 15.7 (aarch64), R 4.6.0
* GitHub Actions:
  * macos-latest, R release
  * windows-latest, R release
  * ubuntu-latest, R devel
  * ubuntu-latest, R release
  * ubuntu-latest, R oldrel-1
* win-builder: R devel, R release

## R CMD check results

0 errors | 0 warnings | 0 notes

## Resubmission

This is a resubmission of `actigraph.sleepr` in response to the reviewer
comments on version 0.3.0. Changes:

* Removed single quotes around `ActiGraph` in the Title and Description fields.
* Added `\value{}` to the .Rd files of the following functions: `expand_periods`,
  `expand_timestamp`, `get_epoch_length`, `plot_activity`, `plot_activity_period`,
  `tbl_agd` and `pipe.Rd`.
* References in the Description field are in `authors (year) <doi:...>` form.

## Notes for CRAN

The "Possibly misspelled words in DESCRIPTION" check flags the following terms,
which are intentional:

* Choi, Kripke, Sadeh, Troiano — author surnames of the cited algorithms
  (Choi 2011, Cole-Kripke 1992, Sadeh 1994, Troiano 2008).
* agd — the file extension used by ActiGraph devices for activity count exports.
