## Resubmission

This release fixes the test failure reported for version 0.3.1 on the r-devel
and Windows check flavors ("function 'enterRNGScope' not provided by package
'Rcpp'"). The package now imports Rcpp, so the Rcpp shared library is loaded at
runtime before the package's compiled routines are called.

## Test environments
* local: macOS (aarch64), R 4.6.1
* GitHub Actions:
  * macos-latest, R release
  * windows-latest, R release
  * ubuntu-latest, R devel
  * ubuntu-latest, R release
  * ubuntu-latest, R oldrel-1
* R-hub v2, all R devel:
  * linux, windows, macos, macos-arm64, m1-san
* win-builder: R devel, R release

## R CMD check results

0 errors | 0 warnings | 1 note

The note is the standard CRAN incoming feasibility note identifying the
maintainer.

---
The Oakley reference has no DOI; it is cited by its FCC ID document URL.
