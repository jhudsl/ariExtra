## Test environments
* local R installation, R 4.0.0
* ubuntu 16.04 (on travis-ci), R 4.0.0
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

---
Fixes the "The browser is not executable:" error on MacOS for `pagedown::chrome_print` functionality by adding in this use case for the regex.
