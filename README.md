
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
res = gs_to_ari(id, verbose = FALSE, voice = "Joanna", service = "amazon", open = FALSE) 
readLines(res$output_file)
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
#> [13] "![](/private/var/folders/1s/wrtqcpxn685_zk570bnx9_rr0000gr/T/RtmpXm4ClX/file7b771dad0f65.png)"                                                                                                                
#> [14] ""                                                                                                                                                                                                             
#> [15] ""                                                                                                                                                                                                             
#> [16] "----------"                                                                                                                                                                                                   
#> [17] ""                                                                                                                                                                                                             
#> [18] "<!--Here is an example of the way Lean pub turns text into an output course.  The left hand side is written in a markdown format called Markua.-->"                                                           
#> [19] "![](/private/var/folders/1s/wrtqcpxn685_zk570bnx9_rr0000gr/T/RtmpXm4ClX/file7b774ccdbf1b.png)"                                                                                                                
#> [20] ""                                                                                                                                                                                                             
#> [21] ""                                                                                                                                                                                                             
#> [22] "----------"                                                                                                                                                                                                   
#> [23] ""                                                                                                                                                                                                             
#> [24] "<!--The big difference between this and regular markdown is that you can have quizzes and exercises.  The questions and answers can both be randomized.  Multiple wrong and correct answers can be given.-->" 
#> [25] "![](/private/var/folders/1s/wrtqcpxn685_zk570bnx9_rr0000gr/T/RtmpXm4ClX/file7b7757bdad01.png)"                                                                                                                
#> [26] ""                                                                                                                                                                                                             
#> [27] ""                                                                                                                                                                                                             
#> [28] "----------"                                                                                                                                                                                                   
#> [29] ""                                                                                                                                                                                                             
#> [30] "<!---->"                                                                                                                                                                                                      
#> [31] "![](/private/var/folders/1s/wrtqcpxn685_zk570bnx9_rr0000gr/T/RtmpXm4ClX/file7b7765a949a7.png)"                                                                                                                
#> [32] ""                                                                                                                                                                                                             
#> [33] ""                                                                                                                                                                                                             
#> [34] "----------"                                                                                                                                                                                                   
#> [35] ""                                                                                                                                                                                                             
#> [36] "<!--The first option is go through the Center for Teaching and Learning.  You must build the course yourself and Ira’s team allows you to have the Johns Hopkins designation.-->"                             
#> [37] "![](/private/var/folders/1s/wrtqcpxn685_zk570bnx9_rr0000gr/T/RtmpXm4ClX/file7b774d22a715.png)"                                                                                                                
#> [38] ""                                                                                                                                                                                                             
#> [39] ""                                                                                                                                                                                                             
#> [40] "----------"                                                                                                                                                                                                   
#> [41] ""                                                                                                                                                                                                             
#> [42] "<!--Another option is to start with a google slide deck.  From this, we can help create the structure and the format necessary for courses.  We can make videos and upload them to youtube, like this one.-->"
#> [43] "![](/private/var/folders/1s/wrtqcpxn685_zk570bnx9_rr0000gr/T/RtmpXm4ClX/file7b771e077b43.png)"                                                                                                                
#> [44] ""                                                                                                                                                                                                             
#> [45] ""                                                                                                                                                                                                             
#> [46] "----------"                                                                                                                                                                                                   
#> [47] ""                                                                                                                                                                                                             
#> [48] "<!--If you are interested, please email us.-->"                                                                                                                                                               
#> [49] "![](/private/var/folders/1s/wrtqcpxn685_zk570bnx9_rr0000gr/T/RtmpXm4ClX/file7b777d2d7b1b.png)"                                                                                                                
#> [50] ""
```
