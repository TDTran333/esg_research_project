# Required libraries and functions
shhh <- suppressPackageStartupMessages # It's a library, so shhh!

shhh(require(here))                 # Relative paths
shhh(require(readxl))               # Read excel file
shhh(require(tidyverse))            # Data manipulation
shhh(require(lubridate))            # Date management
shhh(require(janitor))              # Data cleaning
shhh(require(summarytools))         # Summary stats
shhh(require(markovchain))          # Transition matrix
shhh(require(PeerPerformance))      # Alpha screening
shhh(require(scales))               # For ggplot2 charts
shhh(require(widyr))                # Pairwise correlation
shhh(require(knitr))                # Generate tables
shhh(require(zoo))                  # For rolling window
shhh(require(xts))                  # Time series
shhh(require(quantmod))             # Stock returns
shhh(require(PerformanceAnalytics)) # Portfolio Performance

options(dplyr.summarise.inform = FALSE)

source(here("function", "analysis_funs.R"))