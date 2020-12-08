library(tidyverse)

load(here::here("data", "daily_data.rda"))
data_daily <- data_daily %>%
  janitor::clean_names()

# Factors Data ------------------------------------------------------------

dly_factor <- data_daily %>%
  select(date, mkt_rf:umd) %>% 
  mutate(mom = umd) %>%
  select(-rf, -umd) %>% 
  group_by(date) %>% 
  summarize(across(everything(), mean))

save(dly_factor, file = here::here("data","dly_factor.Rda"))

# Firm Data ---------------------------------------------------------------

data_daily_2 <- data_daily %>% 
  select(-mkt_rf, -(hml:umd))

save(data_daily_2, file = here::here("data","data_daily_2.Rda"))

---

load(here::here("data", "daily_data_2.rda"))

dly_data <- data_daily_2 %>%
  mutate(ghg = ifelse(!is.na(enerdp023) | !is.na(enerdp096), 
               (replace_na(enerdp023, 0) + replace_na(enerdp023, 0)) / revt_std, NA),
           ret_rf = ret - rf,
         permno = as.numeric(permno)) %>%
  mutate_at(vars(ghg), ~replace(., is.infinite(.), NA)) %>%
  select(date, permno, ret_rf, ghg, envscore)

save(dly_data, file = here::here("data","dly_data.Rda"))

# SP Firm Data ------------------------------------------------------------

dly_data_sp <- data_daily_2 %>%
  filter(!is.na(spmim)) %>%
  mutate(ghg = ifelse(!is.na(enerdp023) | !is.na(enerdp096), 
                      (replace_na(enerdp023, 0) + replace_na(enerdp023, 0)) / revt_std, NA),
         ret_rf = ret - rf,
         permno = as.numeric(permno)) %>%
  mutate_at(vars(ghg), ~replace(., is.infinite(.), NA)) %>%
  select(date, permno, ret_rf, ghg, envscore, spmim)

save(dly_data_sp, file = here::here("data","mthly_data_sp.Rda"))

# Counting observations ---------------------------------------------------

table <- matrix(data = NA, nrow = 6, ncol = 3)

data_daily_2 %>% 
  count() %>%
  as.matrix() -> table[1,1]

data_daily_2 %>% 
  filter(!is.na(spmim)) %>%
  count() %>%
  as.matrix() -> table[1,2]

data_daily_2 %>% 
  select(permno) %>% 
  distinct() %>% 
  count() %>%
  as.matrix() -> table[2,1]

data_daily_2 %>% 
  filter(!is.na(spmim)) %>%
  select(permno) %>%
  distinct() %>%
  count() %>%
  as.matrix() -> table[2,2]

data_daily_2 %>% 
  filter(!is.na(enerdp023)) %>%
  count() %>%
  as.matrix() -> table[3,1]

data_daily_2 %>% 
  filter(!is.na(spmim)) %>%
  filter(!is.na(enerdp023)) %>%
  count() %>%
  as.matrix() -> table[3,2]

data_daily_2 %>% 
  filter(!is.na(enerdp096)) %>%
  count() %>%
  as.matrix() -> table[4,1]

data_daily_2 %>% 
  filter(!is.na(spmim)) %>%
  filter(!is.na(enerdp096)) %>%
  count() %>%
  as.matrix() -> table[4,2]

data_daily_2 %>% 
  filter(!is.na(enerdp023) | !is.na(enerdp096)) %>%
  count() %>%
  as.matrix() -> table[5,1]

data_daily_2 %>% 
  filter(!is.na(spmim)) %>%
  filter(!is.na(enerdp023) | !is.na(enerdp096)) %>%
  count() %>%
  as.matrix() -> table[5,2]

data_daily_2 %>% 
  filter(!is.na(enerdp023) & !is.na(enerdp096)) %>%
  count() %>%
  as.matrix() -> table[6,1]

data_daily_2 %>% 
  filter(!is.na(spmim)) %>%
  filter(!is.na(enerdp023) & !is.na(enerdp096)) %>%
  count() %>%
  as.matrix() -> table[6,2]

table[, 3] <- table[, 2] / table[, 1]
