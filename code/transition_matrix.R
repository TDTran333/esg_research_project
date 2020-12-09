library(tidyverse)
library(zoo)
library(lubridate)

library(data.table)
library(RcppRoll)


load(here::here("data", "2009_2017", "mthly_data_sp.rda"))

avg_ghg <- mthly_data_sp %>%
  filter(spmim == 10) %>% 
  group_by(permno) %>% 
  summarize(avg_ghg = mean(ghg, na.rm = TRUE)) %>%
  filter(!is.nan(avg_ghg)) 

avg_ghg %>% 
  ggplot(aes(avg_ghg)) +
  geom_histogram()

test <- mthly_data_sp %>%
  select(date, permno, ghg) %>% 
  group_by(permno) %>% 
  mutate(avg_5y = rollapplyr(ghg, 
                             width = 60,
                             FUN = function(x) mean(x, na.rm = TRUE), 
                             fill = NA, 
                             partial = TRUE,
                             align = "right")) %>%
  group_by(year(date), permno) %>% 
  summarize(avg_5r_yearly = mean(avg_5y, na.rm = TRUE)) %>%
  mutate_at(vars(avg_5r_yearly), ~replace(., is.nan(.), NA)) %>% 
    mutate(status = case_when(is.na(avg_5r_yearly) ~ "U",
                            avg_5r_yearly >= 10000 ~ "B",
                            avg_5r_yearly <= 1000  ~ "G",
                            avg_5r_yearly < 1000 & avg_5r_yearly > 1000 ~ "N")) %>%
  arrange(permno) %>% 
  rename(c("year" = "year(date)"))
  
test2 <- mthly_data_sp %>%
  select(date, permno, ghg) %>% 
  group_by(year(date), permno) %>%
  summarize(avg_ghg_y = mean(ghg, na.rm = TRUE)) %>%
  group_by(permno) %>%
  mutate(avg_ghg_5y = rollapplyr(avg_ghg_y, 
                             width = 5,
                             FUN = function(x) mean(x, na.rm = TRUE), 
                             fill = NA, 
                             partial = TRUE,
                             align = "right")) %>%
  mutate_at(vars(avg_ghg_5y), ~replace(., is.nan(.), NA)) %>% 
  rename(c("year" = "year(date)")) %>% 
  group_by(year) %>% 
  mutate(q_25 = quantile(avg_ghg_5y, probs = 0.25, na.rm = TRUE),
         q_75 = quantile(avg_ghg_5y, probs = 0.75, na.rm = TRUE)) %>%
  filter(year>=2012) %>% 
  mutate(status = case_when(is.na(avg_ghg_5y) ~ "U",
                            avg_ghg_5y <= q_25~ "G",
                            avg_ghg_5y >= q_75 ~ "B",                           
                            avg_ghg_5y < q_75 & avg_ghg_5y > q_25 ~ "N")) %>%
  arrange(permno)

test2 %>% 
  group_by(year, status) %>% 
  count()
