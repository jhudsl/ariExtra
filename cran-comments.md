## Test environments
* local R installation, R 4.0.0
* ubuntu 16.04 (on travis-ci), R 4.0.0
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

--- 
Hotfix for `libreoffice` and `soffice` issues with `LD_LIBRARY_PATH` on r-devel-linux-x86_64-fedora-gcc	and r-devel-linux-x86_64-fedora-clang, which I belive are related to:
https://codeyarns.github.io/tech/2019-09-05-libregloso-cannot-open-shared-object-file.html.  I can't debug the configuration on CRAN, so I added `fix_soffice_library_path` to try adapt the path, but any fails due to this now give a message and don't run the examples.
