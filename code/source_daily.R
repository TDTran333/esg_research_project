# Required libraries and functions

shhh <- suppressPackageStartupMessages # It's a library, so shhh!

shhh(require(here))             # Relative paths
shhh(require(tidyverse))        # Data manipulation
shhh(require(lubridate))        # Date management
shhh(require(janitor))          # Data cleaning
shhh(require(summarytools))     # Summary stats
shhh(require(markovchain))      # Transition matrix
shhh(require(PeerPerformance))  # Alpha screening
shhh(require(scales))           # For ggplot2
shhh(require(zoo))              # For rolling window
shhh(require(stringr))          # String manipulation

options(digits = 4, max.print = 1000, prompt = "> ", warn = -1)

source(here("function", "screening_funs_daily.R"))