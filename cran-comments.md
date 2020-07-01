## Test environments
* local R installation, R 4.0.0
* ubuntu 16.04 (on travis-ci), R 4.0.0
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

--- 
Fixed `gs_to_ari` example to make shorter and wrapped some in \donttest vs \dontrun previously.  Also, moved examples to tests and made sure example did not call external packages not included in Suggests here.
