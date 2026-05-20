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

## Notes for CRAN

This is the first CRAN submission of `actigraph.sleepr`.

The "Possibly misspelled words in DESCRIPTION" check flags the following terms,
which are intentional:

* Choi, Kripke, Sadeh, Troiano — author surnames of the cited algorithms
  (Choi 2011, Cole-Kripke 1992, Sadeh 1994, Troiano 2008).
* agd — the file extension used by ActiGraph devices for activity count exports.
