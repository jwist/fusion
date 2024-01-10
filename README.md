
# fusion

<!-- badges: start -->
[![Codecov test coverage](https://codecov.io/gh/jwist/fusion/branch/master/graph/badge.svg)](https://codecov.io/gh/jwist/fusion?branch=master)
[![Travis build status](https://travis-ci.com/jwist/fusion.svg?branch=master)](https://travis-ci.com/jwist/fusion)
<!-- badges: end -->

Fusion aim at providing a structure to data produce for metabolic profiling or phenotyping using nuclear magnetic resonance and mass spectrometry. 

## Pre-requisites

You will need to manually install two dependencies:

```r
remotes::install_url("https://anpc.mylims.org/gitea/jul/rldx/archive/main.tar.gz")

remotes::install_github("phenological/nmr-parser@0.1.9")
```
## Installation

You can install the released version of fusion from github:

``` r
devtools::install_github("jwist/fusion")
```

### post-installation 

ANPC members can perform some environment configuration by running the startup script provided here. Before, it is important to know the path of the institutional OneDrive Folder and to check that the datasets folder exists and is in sync. 

It is also important to be able to locate the .Rprofile that allows users to perform task during R startup and the .Renviron file that allows to define variable to be used while starting R. Both files are usually located into the home directory of the user that runs the R instance.

``` r
homePath <- Sys.getenv("HOME") # locate your home directory
file.path(homePath, ".Rprofile") # typical location of .Rprofile
file.path(homePath, ".Renviron") # typical location of .Renviron

fusion::startup()
```

### Troubleshooting
 - Both files are usually hidden by the windows manager of major OS. Check how to toggle the display of hidden files in your file browser.
 - Make sure that you have the permission to read in the datasets folder.

## Introduction


