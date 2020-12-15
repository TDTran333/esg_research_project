
# Options -----------------------------------------------------------------

start <- "2009"
end   <- "2017"

mainDir <- here::here("data")
subDir <- paste0(start, "-", end)

ifelse(!dir.exists(file.path(mainDir, subDir)), dir.create(file.path(mainDir, subDir)), FALSE)

# Load Data ---------------------------------------------------------------

library(tidyverse)
library(lubridate)

load(here::here("data", "raw_data", "monthly_data.rda"))
data_monthly <- data_monthly %>%
  janitor::clean_names() %>% 
  as_tibble()

# Factors Data ------------------------------------------------------------

factor_data <- data_monthly %>%
  select(date, mkt_rf:umd) %>% 
  mutate(mom = umd) %>%
  select(-rf, -umd) %>% 
  group_by(date) %>% 
  summarize_all(function(x)(mean(x) / 100))

factor_data %>% 
  as.matrix() %>% 
  saveRDS(file = here::here("data", "all", "mthly_factor_mat.Rds"))

factor_data %>% 
  filter(year(date) >= start,
         year(date) <= end) %>% 
  as.matrix() %>% 
  saveRDS(file = here::here("data", subDir, "mthly_factor_mat.Rds"))

# Firm Data ---------------------------------------------------------------

mthly_data <- data_monthly %>%
  mutate(ghg = ifelse(!is.na(enerdp023) | !is.na(enerdp096), 
               (replace_na(enerdp023, 0) + replace_na(enerdp023, 0)) / revt_std, NA),
         ret_rf = (ret - rf) / 100,
         permno = as.numeric(permno)) %>%
  mutate_at(vars(ghg), ~replace(., is.infinite(.), NA)) %>%
  mutate(ghg = DescTools::Winsorize(ghg, probs = c(0.005, 0.995), na.rm = TRUE)) %>%
  select(date, permno, ret_rf, ghg, envscore)

mthly_data %>% 
  saveRDS(file = here::here("data", "all", "mthly_data.Rds"))

mthly_data %>% 
  filter(year(date) >= start,
         year(date) <= end) %>% 
  saveRDS(file = here::here("data", subDir, "mthly_data.Rds"))

# SP Firm Data ------------------------------------------------------------

mthly_data_sp <- data_monthly %>%
  filter(!is.na(spmim)) %>%
  mutate(ghg = ifelse(!is.na(enerdp023) | !is.na(enerdp096), 
                      (replace_na(enerdp023, 0) + replace_na(enerdp023, 0)) / revt_std, NA),
         ret_rf = (ret - rf) / 100,
         permno = as.numeric(permno)) %>%
  mutate_at(vars(ghg), ~replace(., is.infinite(.), NA)) %>%
  mutate(ghg = DescTools::Winsorize(ghg, probs = c(0.005, 0.995), na.rm = TRUE)) %>%
  select(date, permno, ret_rf, ghg, envscore, spmim)

mthly_data_sp %>% 
  saveRDS(file = here::here("data", "all", "mthly_data_sp.Rds"))

mthly_data_sp %>% 
  filter(year(date) >= start,
         year(date) <= end) %>% 
  saveRDS(file = here::here("data", subDir, "mthly_data_sp.Rds"))

# Counting observations ---------------------------------------------------

tbl <- matrix(data = NA, nrow = 6, ncol = 5)
colnames(tbl) <- c("", "All data", "All ghg data", "Sample data", "Sample ghg data")
tbl[, 1] <- c("Total obs", "Obs scope 1&2", "Obs scope3", 
              "Total firms", "Firms scope 1&2", "Firms scope 3") 

df <- data_monthly
df_sp <- data_monthly %>% filter(year(date) >= start, year(date) <= end)

cnt_mat <- function(data) {
  data %>% count() %>% as.matrix()
}

dstnct_cnt_mat <- function(data) {
  data %>% distinct() %>% count() %>% as.matrix()
}

tbl[1,2] <- df %>% cnt_mat()
tbl[2,2] <- df %>% filter(!is.na(enerdp023)) %>% cnt_mat()
tbl[3,2] <- df %>% filter(!is.na(enerdp096)) %>% cnt_mat()
tbl[4,2] <- df %>% select(permno) %>% dstnct_cnt_mat()
tbl[5,2] <- df %>% filter(!is.na(enerdp023)) %>% select(permno) %>% dstnct_cnt_mat()
tbl[6,2] <- df %>% filter(!is.na(enerdp096)) %>% select(permno) %>% dstnct_cnt_mat()

tbl[1,3] <- df %>% filter(!is.na(spmim)) %>% cnt_mat()
tbl[2,3] <- df %>% filter(!is.na(spmim), !is.na(enerdp023)) %>% cnt_mat()
tbl[3,3] <- df %>% filter(!is.na(spmim), !is.na(enerdp096)) %>% cnt_mat()
tbl[4,3] <- df %>% filter(!is.na(spmim)) %>% select(permno) %>% dstnct_cnt_mat()
tbl[5,3] <- df %>% filter(!is.na(spmim), !is.na(enerdp023)) %>% select(permno) %>% dstnct_cnt_mat()
tbl[6,3] <- df %>% filter(!is.na(spmim), !is.na(enerdp096)) %>% select(permno) %>% dstnct_cnt_mat()

tbl[1,4] <- df_sp %>% cnt_mat()
tbl[2,4] <- df_sp %>% filter(!is.na(enerdp023)) %>% cnt_mat()
tbl[3,4] <- df_sp %>% filter(!is.na(enerdp096)) %>% cnt_mat()
tbl[4,4] <- df_sp %>% select(permno) %>% dstnct_cnt_mat()
tbl[5,4] <- df_sp %>% filter(!is.na(enerdp023)) %>% select(permno) %>% dstnct_cnt_mat()
tbl[6,4] <- df_sp %>% filter(!is.na(enerdp096)) %>% select(permno) %>% dstnct_cnt_mat()

tbl[1,5] <- df_sp %>% filter(!is.na(spmim)) %>% cnt_mat()
tbl[2,5] <- df_sp %>% filter(!is.na(spmim), !is.na(enerdp023)) %>% cnt_mat()
tbl[3,5] <- df_sp %>% filter(!is.na(spmim), !is.na(enerdp096)) %>% cnt_mat()
tbl[4,5] <- df_sp %>% filter(!is.na(spmim)) %>% select(permno) %>% dstnct_cnt_mat()
tbl[5,5] <- df_sp %>% filter(!is.na(spmim), !is.na(enerdp023)) %>% select(permno) %>% dstnct_cnt_mat()
tbl[6,5] <- df_sp %>% filter(!is.na(spmim), !is.na(enerdp096)) %>% select(permno) %>% dstnct_cnt_mat()

write.csv(tbl, file = here::here("code", "clean_data", "mthly_summ_tbl.csv"))
