library(tidyverse)

# load(here::here("data", "daily_data.rda"))
# data_daily <- data_daily %>%
#   janitor::clean_names()
# 
# # Factors Data ------------------------------------------------------------
# 
# dly_factor <- data_daily %>%
#   select(date, mkt_rf:umd) %>% 
#   mutate(mom = umd) %>%
#   select(-rf, -umd) %>% 
#   group_by(date) %>% 
#   summarize(across(everything(), mean))
# 
# save(dly_factor, file = here::here("data", "all", "dly_factor.Rda"))
# 
# # Firm Data ---------------------------------------------------------------
# 
# daily_data_2 <- data_daily %>% 
#   select(-mkt_rf, -(hml:umd))
# 
# save(daily_data_2, file = here::here("data", "all", "daily_data_2.Rda"))

---

load(here::here("data", "all", "daily_data_2.rda"))

dly_data <- daily_data_2 %>%
  mutate(ghg = ifelse(!is.na(enerdp023) | !is.na(enerdp096), 
               (replace_na(enerdp023, 0) + replace_na(enerdp023, 0)) / revt_std, NA),
           ret_rf = ret - rf,
         permno = as.numeric(permno)) %>%
  mutate_at(vars(ghg), ~replace(., is.infinite(.), NA)) %>%
  select(date, permno, ret_rf, ghg, envscore)

save(dly_data, file = here::here("data", "all", "dly_data.Rda"))

# SP Firm Data ------------------------------------------------------------

dly_data_sp <- daily_data_2 %>%
  filter(!is.na(spmim)) %>%
  mutate(ghg = ifelse(!is.na(enerdp023) | !is.na(enerdp096), 
                      (replace_na(enerdp023, 0) + replace_na(enerdp023, 0)) / revt_std, NA),
         ret_rf = ret - rf,
         permno = as.numeric(permno)) %>%
  mutate_at(vars(ghg), ~replace(., is.infinite(.), NA)) %>%
  select(date, permno, ret_rf, ghg, envscore, spmim)

save(dly_data_sp, file = here::here("data", "all", "dly_data_sp.Rda"))

# Counting observations ---------------------------------------------------

tbl <- matrix(data = NA, nrow = 6, ncol = 3)

daily_data_2 %>% 
  count() %>%
  as.matrix() -> tbl[1,1]

daily_data_2 %>% 
  filter(!is.na(spmim)) %>%
  count() %>%
  as.matrix() -> tbl[1,2]

daily_data_2 %>% 
  select(permno) %>% 
  distinct() %>% 
  count() %>%
  as.matrix() -> tbl[2,1]

daily_data_2 %>% 
  filter(!is.na(spmim)) %>%
  select(permno) %>%
  distinct() %>%
  count() %>%
  as.matrix() -> tbl[2,2]

daily_data_2 %>% 
  filter(!is.na(enerdp023)) %>%
  count() %>%
  as.matrix() -> tbl[3,1]

daily_data_2 %>% 
  filter(!is.na(spmim)) %>%
  filter(!is.na(enerdp023)) %>%
  count() %>%
  as.matrix() -> tbl[3,2]

daily_data_2 %>% 
  filter(!is.na(enerdp096)) %>%
  count() %>%
  as.matrix() -> tbl[4,1]

daily_data_2 %>% 
  filter(!is.na(spmim)) %>%
  filter(!is.na(enerdp096)) %>%
  count() %>%
  as.matrix() -> tbl[4,2]

daily_data_2 %>% 
  filter(!is.na(enerdp023) | !is.na(enerdp096)) %>%
  count() %>%
  as.matrix() -> tbl[5,1]

daily_data_2 %>% 
  filter(!is.na(spmim)) %>%
  filter(!is.na(enerdp023) | !is.na(enerdp096)) %>%
  count() %>%
  as.matrix() -> tbl[5,2]

daily_data_2 %>% 
  filter(!is.na(enerdp023) & !is.na(enerdp096)) %>%
  count() %>%
  as.matrix() -> tbl[6,1]

daily_data_2 %>% 
  filter(!is.na(spmim)) %>%
  filter(!is.na(enerdp023) & !is.na(enerdp096)) %>%
  count() %>%
  as.matrix() -> tbl[6,2]

tbl[, 3] <- tbl[, 2] / tbl[, 1]

clipr::write_clip(tbl)
