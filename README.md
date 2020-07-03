
[![Travis build
status](https://travis-ci.com/jhudsl/ariExtra.svg?branch=master)](https://travis-ci.com/jhudsl/ariExtra)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/jhudsl/ariExtra?branch=master&svg=true)](https://ci.appveyor.com/project/jhudsl/ariExtra)
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ariExtra Package:

The goal of `ariExtra` is to provide leverages the `ari`’ package and
other tools to create automated courses from slides and a script.

## Installation

You can install `ariExtra` from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("jhudsl/ariExtra")
```

## Example

``` r
library(ariExtra)
id = "1Opt6lv7rRi7Kzb9bI0u3SWX1pSz1k7botaphTuFYgNs"
res = gs_to_ari(id, verbose = FALSE, voice = "Joanna", service = "amazon", open = FALSE) 
```

``` r
head(readLines(res$output_file), 20)
#>  [1] "---"                                                                                                                                               
#>  [2] "output:"                                                                                                                                           
#>  [3] "  ariExtra::ari_document:"                                                                                                                         
#>  [4] "    voice: Joanna"                                                                                                                                 
#>  [5] "    service: amazon"                                                                                                                               
#>  [6] "    verbose: no"                                                                                                                                   
#>  [7] "---"                                                                                                                                               
#>  [8] ""                                                                                                                                                  
#>  [9] ""                                                                                                                                                  
#> [10] "----------"                                                                                                                                        
#> [11] ""                                                                                                                                                  
#> [12] "<!--Lean pub created a mook platform.  We want to discuss some options for creating courses with Lean pub at Johns Hopkins.-->"                    
#> [13] "![](/private/var/folders/1s/wrtqcpxn685_zk570bnx9_rr0000gr/T/RtmpsE0Sv2/filecef632678c61.png)"                                                     
#> [14] ""                                                                                                                                                  
#> [15] ""                                                                                                                                                  
#> [16] "----------"                                                                                                                                        
#> [17] ""                                                                                                                                                  
#> [18] "<!--Here is an example of the way Lean pub turns text into an output course.  The left hand side is written in a markdown format called Markua.-->"
#> [19] "![](/private/var/folders/1s/wrtqcpxn685_zk570bnx9_rr0000gr/T/RtmpsE0Sv2/filecef62c314e21.png)"                                                     
#> [20] ""
```
