# TTI-branching-process

1. System requirements

All results were run using RStudio 1.4.1106 and R version 4.0.3 (2020-10-10) on macOS Big Sur Version 11.2.3

2. Installation guide

Install packages using the following file: 
  TTI-branching-process/inst/scripts/installPackages.r
  
Load ringbp package / source code run the following code chunk (typical install time <1min): 
  
  library(data.table)
  library(tidyverse)
  library(git2r)
  library(tictoc)
  library(ggplot2)
  library(patchwork)
  library(cowplot)
  library(latex2exp)
  library(furrr)
  library(sn)
  library(ggrepel)
  library(testthat)
  library(svglite)

  devtools::load_all() #load in ringbp package manually

3. Demo

A demo can be found at:
  TTI-branching-process/inst/scripts/demo.R

The expected run time for this demo is ~5mins

Expected output is a 2x3 Figure with 
x-axis 'Contact tracing coverage' - the proportion of true contacts that are identified and traced.
y-axis 'Prob. large outbreak' - the proportion of simulations that reached 2000 cases.
There should be no discernible trend with changing coverage, due to the low numbers of runs in this demo.
The probability of a large outbreak should roughly decrease as 'Rs' decreases (labels vertically down the RHS).

4. Instructions for use

To recreate the results from the paper run the following files:
  a. TTI-branching-process/inst/scripts/generateResults.R
  b. TTI-branching-process/inst/scripts/generatePlots.R

Running the full results is likely to take >48 hours on a standard PC.
