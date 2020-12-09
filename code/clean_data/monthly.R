library(tidyverse)

load(here::here("data", "monthly_data.rda"))
data_monthly <- data_monthly %>%
  janitor::clean_names()

# Factors Data ------------------------------------------------------------

mthly_factor <- data_monthly %>%
  select(date, mkt_rf:umd) %>% 
  mutate(mom = umd) %>%
  select(-rf, -umd) %>% 
  group_by(date) %>% 
  summarize(across(everything(), mean))

save(mthly_factor, file = here::here("data", "all", "mthly_factor.Rda"))

# Firm Data ---------------------------------------------------------------

mthly_data <- data_monthly %>%
  mutate(ghg = ifelse(!is.na(enerdp023) | !is.na(enerdp096), 
               (replace_na(enerdp023, 0) + replace_na(enerdp023, 0)) / revt_std, NA),
           ret_rf = ret - rf,
         permno = as.numeric(permno)) %>%
  mutate_at(vars(ghg), ~replace(., is.infinite(.), NA)) %>%
  select(date, permno, ret_rf, ghg, envscore)

save(mthly_data, file = here::here("data", "all", "mthly_data.Rda"))

# SP Firm Data ------------------------------------------------------------

mthly_data_sp <- data_monthly %>%
  filter(!is.na(spmim)) %>%
  mutate(ghg = ifelse(!is.na(enerdp023) | !is.na(enerdp096), 
                      (replace_na(enerdp023, 0) + replace_na(enerdp023, 0)) / revt_std, NA),
         ret_rf = ret - rf,
         permno = as.numeric(permno)) %>%
  mutate_at(vars(ghg), ~replace(., is.infinite(.), NA)) %>%
  select(date, permno, ret_rf, ghg, envscore, spmim)

save(mthly_data_sp, file = here::here("data", "all", "mthly_data_sp.Rda"))

# Counting observations ---------------------------------------------------

tbl <- matrix(data = NA, nrow = 6, ncol = 3)

data_monthly %>% 
  count() %>%
  as.matrix() -> tbl[1,1]

data_monthly %>% 
  filter(!is.na(spmim)) %>%
  count() %>%
  as.matrix() -> tbl[1,2]

data_monthly %>% 
  select(permno) %>% 
  distinct() %>% 
  count() %>%
  as.matrix() -> tbl[2,1]

data_monthly %>% 
  filter(!is.na(spmim)) %>%
  select(permno) %>%
  distinct() %>%
  count() %>%
  as.matrix() -> tbl[2,2]

data_monthly %>% 
  filter(!is.na(enerdp023)) %>%
  count() %>%
  as.matrix() -> tbl[3,1]

data_monthly %>% 
  filter(!is.na(spmim)) %>%
  filter(!is.na(enerdp023)) %>%
  count() %>%
  as.matrix() -> tbl[3,2]

data_monthly %>% 
  filter(!is.na(enerdp096)) %>%
  count() %>%
  as.matrix() -> tbl[4,1]

data_monthly %>% 
  filter(!is.na(spmim)) %>%
  filter(!is.na(enerdp096)) %>%
  count() %>%
  as.matrix() -> tbl[4,2]

data_monthly %>% 
  filter(!is.na(enerdp023) | !is.na(enerdp096)) %>%
  count() %>%
  as.matrix() -> tbl[5,1]

data_monthly %>% 
  filter(!is.na(spmim)) %>%
  filter(!is.na(enerdp023) | !is.na(enerdp096)) %>%
  count() %>%
  as.matrix() -> tbl[5,2]

data_monthly %>% 
  filter(!is.na(enerdp023) & !is.na(enerdp096)) %>%
  count() %>%
  as.matrix() -> tbl[6,1]

data_monthly %>% 
  filter(!is.na(spmim)) %>%
  filter(!is.na(enerdp023) & !is.na(enerdp096)) %>%
  count() %>%
  as.matrix() -> tbl[6,2]

tbl[, 3] <- tbl[, 2] / tbl[, 1]

clipr::write_clip(tbl)