
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
  saveRDS(file = here("data", "all", "dly_factor.rds"))

factor_data %>% 
  filter(year(date) >= start,
         year(date) <= end) %>% 
  saveRDS(file = here("data", subDir, "dly_factor.rds"))

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
  saveRDS(file = here("data", "all", "dly_data.rds"))

dly_data %>% 
  filter(year(date) >= start,
         year(date) <= end) %>% 
  saveRDS(file = here("data", subDir, "dly_data.rds"))

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
  saveRDS(file = here("data", "all", "dly_data_sp.rds"))

dly_data_sp %>% 
  filter(year(date) >= start,
         year(date) <= end) %>% 
  saveRDS(file = here("data", subDir, "dly_data_sp.rds"))

# Counting observations ---------------------------------------------------
# 
# tbl <- matrix(data = NA, nrow = 6, ncol = 5)
# colnames(tbl) <- c("", "All data", "All ghg data", "Sample data", "Sample ghg data")
# tbl[, 1] <- c("Total obs", "Obs scope 1&2", "Obs scope3", 
#               "Total firms", "Firms scope 1&2", "Firms scope 3") 
# 
# df <- data_daily
# df_sp <- data_daily %>% filter(year(date) >= start, year(date) <= end)
# 
# cnt_mat <- function(data) {
#   data %>% count() %>% as.matrix()
# }
# 
# dstnct_cnt_mat <- function(data) {
#   data %>% distinct() %>% count() %>% as.matrix()
# }
# 
# tbl[1,2] <- df %>% cnt_mat()
# tbl[2,2] <- df %>% filter(!is.na(enerdp023)) %>% cnt_mat()
# tbl[3,2] <- df %>% filter(!is.na(enerdp096)) %>% cnt_mat()
# tbl[4,2] <- df %>% select(permno) %>% dstnct_cnt_mat()
# tbl[5,2] <- df %>% filter(!is.na(enerdp023)) %>% select(permno) %>% dstnct_cnt_mat()
# tbl[6,2] <- df %>% filter(!is.na(enerdp096)) %>% select(permno) %>% dstnct_cnt_mat()
# 
# tbl[1,3] <- df %>% filter(!is.na(spmim)) %>% cnt_mat()
# tbl[2,3] <- df %>% filter(!is.na(spmim), !is.na(enerdp023)) %>% cnt_mat()
# tbl[3,3] <- df %>% filter(!is.na(spmim), !is.na(enerdp096)) %>% cnt_mat()
# tbl[4,3] <- df %>% filter(!is.na(spmim)) %>% select(permno) %>% dstnct_cnt_mat()
# tbl[5,3] <- df %>% filter(!is.na(spmim), !is.na(enerdp023)) %>% select(permno) %>% dstnct_cnt_mat()
# tbl[6,3] <- df %>% filter(!is.na(spmim), !is.na(enerdp096)) %>% select(permno) %>% dstnct_cnt_mat()
# 
# tbl[1,4] <- df_sp %>% cnt_mat()
# tbl[2,4] <- df_sp %>% filter(!is.na(enerdp023)) %>% cnt_mat()
# tbl[3,4] <- df_sp %>% filter(!is.na(enerdp096)) %>% cnt_mat()
# tbl[4,4] <- df_sp %>% select(permno) %>% dstnct_cnt_mat()
# tbl[5,4] <- df_sp %>% filter(!is.na(enerdp023)) %>% select(permno) %>% dstnct_cnt_mat()
# tbl[6,4] <- df_sp %>% filter(!is.na(enerdp096)) %>% select(permno) %>% dstnct_cnt_mat()
# 
# tbl[1,5] <- df_sp %>% filter(!is.na(spmim)) %>% cnt_mat()
# tbl[2,5] <- df_sp %>% filter(!is.na(spmim), !is.na(enerdp023)) %>% cnt_mat()
# tbl[3,5] <- df_sp %>% filter(!is.na(spmim), !is.na(enerdp096)) %>% cnt_mat()
# tbl[4,5] <- df_sp %>% filter(!is.na(spmim)) %>% select(permno) %>% dstnct_cnt_mat()
# tbl[5,5] <- df_sp %>% filter(!is.na(spmim), !is.na(enerdp023)) %>% select(permno) %>% dstnct_cnt_mat()
# tbl[6,5] <- df_sp %>% filter(!is.na(spmim), !is.na(enerdp096)) %>% select(permno) %>% dstnct_cnt_mat()
# 
# write.csv(tbl, file = here("code", "clean_data", "dly_summ_tbl.csv"))
