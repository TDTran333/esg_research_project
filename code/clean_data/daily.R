
library(tidyverse)
library(lubridate)
library(here)

# Options -----------------------------------------------------------------

start <- "2009"
end   <- "2017"

mainDir <- here("data")
subDir <- paste0(start, "-", end)

ifelse(!dir.exists(file.path(mainDir, subDir)), dir.create(file.path(mainDir, subDir)), FALSE)

# Load Data ---------------------------------------------------------------

load(here("data", "raw_data", "daily_data.rda"))

data_daily <- data_daily %>%
  janitor::clean_names() %>% 
  as_tibble()

# Factors Data ------------------------------------------------------------

factor_data <- data_daily %>%
  select(date, mkt_rf:umd) %>% 
  mutate(mom = umd) %>%
  select(-rf, -umd) %>% 
  group_by(date) %>% 
  summarize_all(function(x)(mean(x) / 100))

factor_data %>% 
  saveRDS(file = here("data", "all", "daily_factor.rds"))

factor_data %>% 
  filter(year(date) >= start,
         year(date) <= end) %>% 
  saveRDS(file = here("data", subDir, "daily_factor.rds"))

# Firm Data ---------------------------------------------------------------

dly_data <- data_daily %>%
  mutate(ghg = ifelse(!is.na(enerdp023) | !is.na(enerdp096), 
               (replace_na(enerdp023, 0) + replace_na(enerdp023, 0)) / revt_std, NA),
         ret_rf = (ret - rf) / 100,
         permno = as.numeric(permno)) %>%
  mutate_at(vars(ghg), ~replace(., is.infinite(.), NA)) %>%
  mutate(ghg = DescTools::Winsorize(ghg, probs = c(0.005, 0.995), na.rm = TRUE)) %>%
  select(date, permno, ret_rf, ghg, envscore)

dly_data %>% 
  saveRDS(file = here("data", "all", "daily_data.rds"))

dly_data %>% 
  filter(year(date) >= start,
         year(date) <= end) %>% 
  saveRDS(file = here("data", subDir, "daily_data.rds"))

# SP Firm Data ------------------------------------------------------------

dly_data_sp <- data_daily %>%
  filter(!is.na(spmim)) %>%
  mutate(ghg = ifelse(!is.na(enerdp023) | !is.na(enerdp096), 
                      (replace_na(enerdp023, 0) + replace_na(enerdp023, 0)) / revt_std, NA),
         ret_rf = (ret - rf) / 100,
         permno = as.numeric(permno)) %>%
  mutate_at(vars(ghg), ~replace(., is.infinite(.), NA)) %>%
  mutate(ghg = DescTools::Winsorize(ghg, probs = c(0.005, 0.995), na.rm = TRUE)) %>%
  select(date, permno, ret_rf, ghg, envscore, spmim)

dly_data_sp %>% 
  saveRDS(file = here("data", "all", "daily_data_sp.rds"))

dly_data_sp %>% 
  filter(year(date) >= start,
         year(date) <= end) %>% 
  saveRDS(file = here("data", subDir, "daily_data_sp.rds"))