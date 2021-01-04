# Required libraries and functions
shhh <- suppressPackageStartupMessages # It's a library, so shhh!

shhh(require(here))             # Relative paths
shhh(require(tidyverse))        # Data manipulation
shhh(require(lubridate))        # Date management
shhh(require(purrr))            # Functional programming
shhh(require(janitor))          # Data cleaning
shhh(require(summarytools))     # Summary stats
shhh(require(markovchain))      # Transition matrix
shhh(require(PeerPerformance))  # Alpha screening
shhh(require(scales))           # For ggplot2
shhh(require(zoo))              # For rolling window
shhh(require(stringr))          # String manipulation
shhh(require(widyr))            # Pairwise correlation

source(here("function", "screening_funs.R"))