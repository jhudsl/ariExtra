
[![Travis build
status](https://travis-ci.com/muschellij2/ariExtra.svg?branch=master)](https://travis-ci.com/muschellij2/ariExtra)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/muschellij2/ariExtra?branch=master&svg=true)](https://ci.appveyor.com/project/muschellij2/ariExtra)
[![Coverage
status](https://codecov.io/gh/muschellij2/ariExtra/branch/master/graph/badge.svg)](https://codecov.io/github/muschellij2/ariExtra?branch=master)
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ariExtra Package:

The goal of `ariExtra` is to provide leverages the `ari`’ package and
other tools to create automated courses from slides and a script.

## Installation

You can install `ariExtra` from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("muschellij2/ariExtra")
```

## Example

``` r
library(ariExtra)
id = "1Opt6lv7rRi7Kzb9bI0u3SWX1pSz1k7botaphTuFYgNs"
res = gs_to_ari(id, verbose = FALSE, voice = "Joanna", service = "amazon") 
readLines(res)
#>  [1] "---"                                                                                                                                                                                                          
#>  [2] "output:"                                                                                                                                                                                                      
#>  [3] "  ari_video:"                                                                                                                                                                                                 
#>  [4] "    voice: Joanna"                                                                                                                                                                                            
#>  [5] "    service: amazon"                                                                                                                                                                                          
#>  [6] "    verbose: no"                                                                                                                                                                                              
#>  [7] "---"                                                                                                                                                                                                          
#>  [8] ""                                                                                                                                                                                                             
#>  [9] "<!--Lean pub created a mook platform.  We want to discuss some options for creating courses with Lean pub at Johns Hopkins.-->"                                                                               
#> [10] "![](/private/var/folders/1s/wrtqcpxn685_zk570bnx9_rr0000gr/T/Rtmp899nRu/filec81076c58194.png)"                                                                                                                
#> [11] "<!--Here is an example of the way Lean pub turns text into an output course.  The left hand side is written in a markdown format called Markua.-->"                                                           
#> [12] "![](/private/var/folders/1s/wrtqcpxn685_zk570bnx9_rr0000gr/T/Rtmp899nRu/filec81020be5877.png)"                                                                                                                
#> [13] "<!--The big difference between this and regular markdown is that you can have quizzes and exercises.  The questions and answers can both be randomized.  Multiple wrong and correct answers can be given.-->" 
#> [14] "![](/private/var/folders/1s/wrtqcpxn685_zk570bnx9_rr0000gr/T/Rtmp899nRu/filec81030a1fd6c.png)"                                                                                                                
#> [15] "<!--;-->"                                                                                                                                                                                                     
#> [16] "![](/private/var/folders/1s/wrtqcpxn685_zk570bnx9_rr0000gr/T/Rtmp899nRu/filec8105b04d665.png)"                                                                                                                
#> [17] "<!--The first option is go through the Center for Teaching and Learning.  You must build the course yourself and Ira’s team allows you to have the Johns Hopkins designation.-->"                             
#> [18] "![](/private/var/folders/1s/wrtqcpxn685_zk570bnx9_rr0000gr/T/Rtmp899nRu/filec8101a97af92.png)"                                                                                                                
#> [19] "<!--Another option is to start with a google slide deck.  From this, we can help create the structure and the format necessary for courses.  We can make videos and upload them to youtube, like this one.-->"
#> [20] "![](/private/var/folders/1s/wrtqcpxn685_zk570bnx9_rr0000gr/T/Rtmp899nRu/filec8105c87a7e1.png)"                                                                                                                
#> [21] "<!--If you are interested, please email us.-->"                                                                                                                                                               
#> [22] "![](/private/var/folders/1s/wrtqcpxn685_zk570bnx9_rr0000gr/T/Rtmp899nRu/filec8104e1ed43c.png)"
```
