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
  summarize(avg_5r_yearly = mean(avg_5y)) %>% 
  arrange(permno)
  

