# The purpose of this script is to test the functions used in the main analysis file.
# There is a lot of redundancy but it is normal.

# For best practice, please restart R session before running script. Ctrl/Cmd + Shift + F10.

source(here::here("code", "source.R"))

# Parameters --------------------------------------------------------------

params            <- list()         
params$control    <- list(nCore = 3)           # Number of core and other controls
params$sample_per <- 2009:2018                 # Sample period
params$subDir     <- paste0(first(params$sample), "-", last(params$sample))
params$window     <- 36                        # Rolling window in # of months
params$nblock     <- 20                        # Number of blocks in screenplots

# Formatting
st_options(round.digits = 2, style = "rmarkdown", plain.ascii = FALSE)
kable <- function(data, digits = 4) {knitr::kable(data, booktabs = TRUE, digits = digits)}

# Load Data ---------------------------------------------------------------

sp_df_monthly <- readRDS(here("data", params$subDir, "monthly_data_sp.rds"))
factor_df_monthly <- readRDS(here("data", params$subDir, "monthly_factor.rds"))

sp_df_daily <- readRDS(here("data", params$subDir, "daily_data_sp.rds"))
factor_df_daily <- readRDS(here("data", params$subDir, "daily_factor.rds"))

# 1. f_create_esg_type ----------------------------------------------------

# f_create_esg_type(.df, .esg_var, .datafreq, .window) 

ghg_df_monthly <- sp_df_monthly %>% f_create_esg_type(.esg_var = "ghg", "monthly", params$window)
env_df_monthly <- sp_df_monthly %>% f_create_esg_type(.esg_var = "envscore", "monthly", params$window)

plot_ghg <- ghg_df_monthly %>%
  group_by(date) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = q_50), color = "blue", lty = 2, size = 1.2) +
  geom_ribbon(aes(ymin = q_25, ymax = q_75), color = "black", size = 1.2, alpha = 0.25) +
  scale_x_yearqtr(format = "%Y-Q%q") +
  labs(x = "Date",
       y = "GHG emissions intensity",
       title = "Greenhouse gas emissions intensity over time.",
       subtitle = paste("Ribbon: 25th-75th percentile."))

plot_ghg %>% 
  ggsave(filename = here("Output", "figures", "firms", "ghg_over_time.png"), width = 8, height = 6, dpi = 150)

plot_env <- env_df_monthly %>%
  group_by(date) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = q_50), color = "blue", lty = 2, size = 1.2) +
  geom_ribbon(aes(ymin = q_25, ymax = q_75), color = "black", size = 1.2, alpha = 0.25) +
  scale_x_yearqtr(format = "%Y-Q%q") +
  labs(x = "Date",
       y = "Environment Score",
       title = "Environment score over time.",
       subtitle = paste("Ribbon: 25th-75th percentile."))

plot_env %>% 
  ggsave(filename = here("Output", "figures", "firms", "env_over_time.png"), width = 8, height = 6, dpi = 150)

# ghg_df_monthly %>% descr() %>% write.csv(file = here("output", "firms", "ghg_monthly_stats.csv"))
# env_df_monthly %>% descr() %>% write.csv(file = here("output", "firms", "env_monthly_stats.csv"))

ghg_df_daily <- sp_df_daily %>% f_create_esg_type(.esg_var = "ghg", "daily", params$window)
env_df_daily <- sp_df_daily %>% f_create_esg_type(.esg_var = "envscore", "daily", params$window)

# ghg_df_daily %>% descr() %>% write.csv(file = here("output", "firms", "ghg_daily_stats.csv"))
# env_df_daily %>% descr() %>% write.csv(file = here("output", "firms", "env_daily_stats.csv"))

# 2. f_plot_firms ---------------------------------------------------------

# f_plot_firms(.df, .esg_var). Note: filename based on first argument. Do not use pipe.

f_plot_firms(ghg_df_monthly, "GHG")
f_plot_firms(env_df_monthly, "Env Score")

f_plot_firms(ghg_df_daily, "GHG")
f_plot_firms(env_df_daily, "Env Score")

# 3. f_firms_count --------------------------------------------------------

# f_firms_count(.df). Note: filename based on first argument. Do not use pipe.
  
ghg_firms_monthly <- f_firms_count(ghg_df_monthly)
env_firms_monthly <- f_firms_count(env_df_monthly)

ghg_firms_monthly %>% kable()
env_firms_monthly %>% kable()

ghg_firms_daily <- f_firms_count(ghg_df_daily)
env_firms_daily <- f_firms_count(env_df_daily)

ghg_firms_daily %>% kable()
env_firms_daily %>% kable()

# 4. f_create_trans_mat ---------------------------------------------------

# f_create_trans_mat(.df). Note: filename based on first argument. Do not use pipe.

ghg_tm_monthly <- f_create_trans_mat(ghg_df_monthly)
env_tm_monthly <- f_create_trans_mat(env_df_monthly)

ghg_tm_monthly %>% kable()
env_tm_monthly %>% kable()

ghg_tm_daily <- f_create_trans_mat(ghg_df_daily)
env_tm_daily <- f_create_trans_mat(env_df_daily)

ghg_tm_daily %>% kable()
env_tm_daily %>% kable()

# 5. f_create_id ----------------------------------------------------------

# f_create_id(.df, .esg_type) 

esg_group <- c("green", "brown", "neutral")

ghg_id_monthly <- env_id_monthly <- list()

ghg_id_monthly[esg_group] <- map(c("G", "B", "N"), ~f_create_id(ghg_df_monthly, .x))
env_id_monthly[esg_group] <- map(c("G", "B", "N"), ~f_create_id(env_df_monthly, .x))

ghg_id_monthly %>% glimpse()
env_id_monthly %>% glimpse()

ghg_id_daily <- env_id_daily <- list()

ghg_id_daily[esg_group] <- map(c("G", "B", "N"), ~f_create_id(ghg_df_daily, .x))
env_id_daily[esg_group] <- map(c("G", "B", "N"), ~f_create_id(env_df_daily, .x))

ghg_id_daily %>% glimpse()
env_id_daily %>% glimpse()

# Unequal length check
bind_rows(map_df(ghg_id_monthly$green, ~sum(is.na(.x))), 
          map_df(ghg_id_monthly$brown, ~sum(is.na(.x))),
          map_df(ghg_id_monthly$neutral, ~sum(is.na(.x)))) %>%
  mutate(esg_group = esg_group)

bind_rows(map_df(env_id_monthly$green, ~sum(is.na(.x))), 
          map_df(env_id_monthly$brown, ~sum(is.na(.x))),
          map_df(env_id_monthly$neutral, ~sum(is.na(.x)))) %>%
  mutate(esg_group = esg_group)

bind_rows(map_df(ghg_id_daily$green, ~sum(is.na(.x))), 
          map_df(ghg_id_daily$brown, ~sum(is.na(.x))),
          map_df(ghg_id_daily$neutral, ~sum(is.na(.x)))) %>%
  mutate(esg_group = esg_group)

bind_rows(map_df(env_id_daily$green, ~sum(is.na(.x))), 
          map_df(env_id_daily$brown, ~sum(is.na(.x))),
          map_df(env_id_daily$neutral, ~sum(is.na(.x)))) %>%
  mutate(esg_group = esg_group)

# 6. f_create_dates -------------------------------------------------------

# f_create_dates(.date, .window, .model, .datafreq)
  
date_monthly <- ghg_df_monthly %>% select(date) %>% distinct() %>% pluck()

bw_monthly <- f_create_dates(date_monthly, params$window, "bw", "monthly")
fw_monthly <- f_create_dates(date_monthly, params$window, "fw", "monthly")

bw_monthly %>% tail()
fw_monthly %>% tail()

date_daily <- ghg_df_daily %>% select(date) %>% distinct() %>% pluck()

bw_daily <- f_create_dates(date_daily, params$window, "bw", "daily")
fw_daily <- f_create_dates(date_daily, params$window, "fw", "daily")

bw_daily %>% tail()
fw_daily %>% tail()

# 7. f_ret_mat ------------------------------------------------------------

# f_ret_mat(.df, .id, .id_date, .start_date, .end_date). 
# Note: Using a sample date to test the other functions. Also, ret_mat differs between bw and fw because they use a different id_date.
  
id_date_bw_monthly    <- head(tail(bw_monthly[, 1], n = 2), n = 1)
start_date_bw_monthly <- head(tail(bw_monthly[, 2], n = 2), n = 1)
end_date_bw_monthly   <- head(tail(bw_monthly[, 3], n = 2), n = 1)

ret_mat_green_bw_monthly <- f_ret_mat(sp_df_monthly, ghg_id_monthly$green, id_date_bw_monthly, start_date_bw_monthly, end_date_bw_monthly)

id_date_fw_monthly    <- head(tail(fw_monthly[, 1], n = 2), n = 1)
start_date_fw_monthly <- head(tail(fw_monthly[, 2], n = 2), n = 1)
end_date_fw_monthly   <- head(tail(fw_monthly[, 3], n = 2), n = 1)

ret_mat_green_fw_monthly <- f_ret_mat(sp_df_monthly, ghg_id_monthly$green, id_date_fw_monthly, start_date_fw_monthly, end_date_fw_monthly)

ret_mat_green_bw_monthly %>% glimpse()
ret_mat_green_fw_monthly %>% glimpse()

id_date_bw_daily    <- head(tail(bw_daily[, 1]), n = 1)
start_date_bw_daily <- head(tail(bw_daily[, 2]), n = 1)
end_date_bw_daily   <- head(tail(bw_daily[, 3]), n = 1)

ret_mat_green_bw_daily <- f_ret_mat(sp_df_daily, ghg_id_daily$green, id_date_bw_daily, start_date_bw_daily, end_date_bw_daily)

id_date_fw_daily    <- head(tail(fw_daily[, 1]), n = 1)
start_date_fw_daily <- head(tail(fw_daily[, 2]), n = 1)
end_date_fw_daily   <- head(tail(fw_daily[, 3]), n = 1)

ret_mat_green_fw_daily <- f_ret_mat(sp_df_daily, ghg_id_daily$green, id_date_fw_daily, start_date_fw_daily, end_date_fw_daily)

ret_mat_green_bw_daily %>% glimpse()
ret_mat_green_fw_daily %>% glimpse()

# 8. f_factor_mat ---------------------------------------------------------

# f_factor_mat(.factor, .start_date, .end_date) 

factor_mat_bw_monthly <- f_factor_mat(factor_df_monthly, start_date_bw_monthly, end_date_bw_monthly)
factor_mat_fw_monthly <- f_factor_mat(factor_df_monthly, start_date_fw_monthly, end_date_fw_monthly)

factor_mat_bw_monthly %>% glimpse()
factor_mat_fw_monthly %>% glimpse()

factor_mat_bw_daily <- f_factor_mat(factor_df_daily, start_date_bw_daily, end_date_bw_daily)
factor_mat_fw_daily <- f_factor_mat(factor_df_daily, start_date_fw_daily, end_date_fw_daily)

factor_mat_bw_daily %>% glimpse()
factor_mat_fw_daily %>% glimpse()

# 9. alpha screen ---------------------------------------------------------

# PeerPerformance::alphaScreening(ret_mat, factor_mat, control)

alpha_screen_green_bw_monthly <- PeerPerformance::alphaScreening(ret_mat_green_bw_monthly, factor_mat_bw_monthly, params$control)
alpha_screen_green_fw_monthly <- PeerPerformance::alphaScreening(ret_mat_green_fw_monthly, factor_mat_fw_monthly, params$control)

alpha_screen_green_bw_monthly %>% glimpse()
alpha_screen_green_fw_monthly %>% glimpse()

alpha_screen_green_bw_daily <- PeerPerformance::alphaScreening(ret_mat_green_bw_daily, factor_mat_bw_daily, params$control)
alpha_screen_green_fw_daily <- PeerPerformance::alphaScreening(ret_mat_green_fw_daily, factor_mat_fw_daily, params$control)

alpha_screen_green_bw_daily %>% glimpse()
alpha_screen_green_fw_daily %>% glimpse()

# 10. f_alpha_cor -------------------------------------------------------

# f_alpha_cor(.ret, .factor, .date, .method)

alpha_cor_green_bw_monthly <- f_alpha_cor(ret_mat_green_bw_monthly, factor_mat_bw_monthly, id_date_bw_monthly, "pearson")
alpha_cor_green_fw_monthly <- f_alpha_cor(ret_mat_green_fw_monthly, factor_mat_fw_monthly, id_date_fw_monthly, "pearson")

alpha_cor_green_bw_monthly %>% glimpse()
alpha_cor_green_fw_monthly %>% glimpse()

alpha_cor_green_bw_daily <- f_alpha_cor(ret_mat_green_bw_daily, factor_mat_bw_daily, id_date_bw_daily, "pearson")
alpha_cor_green_fw_daily <- f_alpha_cor(ret_mat_green_fw_daily, factor_mat_fw_daily, id_date_fw_daily, "pearson")

alpha_cor_green_bw_daily %>% glimpse()
alpha_cor_green_fw_daily %>% glimpse()

# 11. f_tbl_screening ---------------------------------------------------

# f_tbl_screening(.df, .nblock, do_norm = TRUE)

tbl_screening_green_bw_monthly <- f_tbl_screening(alpha_screen_green_bw_monthly, params$nblock)
tbl_screening_green_fw_monthly <- f_tbl_screening(alpha_screen_green_fw_monthly, params$nblock)

tbl_screening_green_bw_monthly %>% glimpse()
tbl_screening_green_fw_monthly %>% glimpse()

tbl_screening_green_bw_daily <- f_tbl_screening(alpha_screen_green_bw_daily, params$nblock)
tbl_screening_green_fw_daily <- f_tbl_screening(alpha_screen_green_fw_daily, params$nblock)

tbl_screening_green_bw_daily %>% glimpse()
tbl_screening_green_fw_daily %>% glimpse()

# 12. f_screenplot ------------------------------------------------------

# f_screenplot(.df, .fig_title, .datafreq)

f_screenplot(tbl_screening_green_bw_monthly, "tbl_screening_green_bw_monthly", "monthly")
f_screenplot(tbl_screening_green_fw_monthly, "tbl_screening_green_fw_monthly", "monthly")
f_screenplot(tbl_screening_green_bw_daily, "tbl_screening_green_bw_daily", "daily")
f_screenplot(tbl_screening_green_fw_daily, "tbl_screening_green_fw_daily", "daily")

# 13. f_tidy_screening --------------------------------------------------

# f_tidy_screening(.tbl_screening, .id_date)

tidy_screening_green_bw_monthly <- f_tidy_screening(tbl_screening_green_bw_monthly %>% list(), id_date_bw_monthly)
tidy_screening_green_fw_monthly <- f_tidy_screening(tbl_screening_green_fw_monthly %>% list(), id_date_fw_monthly)

tidy_screening_green_bw_monthly %>% glimpse()
tidy_screening_green_fw_monthly %>% glimpse()

tidy_screening_green_bw_daily <- f_tidy_screening(tbl_screening_green_bw_daily %>% list(), id_date_bw_daily)
tidy_screening_green_fw_daily <- f_tidy_screening(tbl_screening_green_fw_daily %>% list(), id_date_fw_daily)

tidy_screening_green_bw_daily %>% glimpse()
tidy_screening_green_fw_daily %>% glimpse()

# 14. f_tidy_n_obs ------------------------------------------------------

# f_tidy_n_obs(.alphascreen, .id_date)

tidy_obs_green_bw_monthly <- f_tidy_n_obs(alpha_screen_green_bw_monthly %>% list(), id_date_bw_monthly)
tidy_obs_green_fw_monthly <- f_tidy_n_obs(alpha_screen_green_fw_monthly %>% list(), id_date_fw_monthly)

tidy_obs_green_bw_monthly %>% glimpse()
tidy_obs_green_fw_monthly %>% glimpse()

tidy_obs_green_bw_daily <- f_tidy_n_obs(alpha_screen_green_bw_daily %>% list(), id_date_bw_daily)
tidy_obs_green_fw_daily <- f_tidy_n_obs(alpha_screen_green_fw_daily %>% list(), id_date_fw_daily)

tidy_obs_green_bw_daily %>% glimpse()
tidy_obs_green_fw_daily %>% glimpse()

# 15. f_plot_obs --------------------------------------------------------

# f_plot_obs(.obs_df)

tidy_obs_green_bw_monthly <- tidy_obs_green_bw_monthly %>% mutate(model_name = "green")
tidy_obs_green_fw_monthly <- tidy_obs_green_fw_monthly %>% mutate(model_name = "green")
f_plot_obs(tidy_obs_green_bw_monthly)
f_plot_obs(tidy_obs_green_fw_monthly)

tidy_obs_green_bw_daily <- tidy_obs_green_bw_daily %>% mutate(model_name = "green")
tidy_obs_green_fw_daily <- tidy_obs_green_fw_daily %>% mutate(model_name = "green")
f_plot_obs(tidy_obs_green_bw_daily)
f_plot_obs(tidy_obs_green_fw_daily)

# 16. f_missing_obs -----------------------------------------------------

# f_missing_obs(.obs_df)

f_missing_obs(tidy_obs_green_bw_monthly)
f_missing_obs(tidy_obs_green_fw_monthly)

f_missing_obs(tidy_obs_green_bw_daily)
f_missing_obs(tidy_obs_green_fw_daily)

# 17. f_plot_hist_cor ---------------------------------------------------

# f_plot_hist_cor(.alpha_cor, .model_name, .esg_name)

alpha_cor_green_bw_monthly <- alpha_cor_green_bw_monthly %>% mutate(model_name = "green")
alpha_cor_green_fw_monthly <- alpha_cor_green_fw_monthly %>% mutate(model_name = "green")

f_plot_hist_cor(alpha_cor_green_bw_monthly, "green", "GHG")
f_plot_hist_cor(alpha_cor_green_fw_monthly, "green", "ENV")

alpha_cor_green_bw_daily <- alpha_cor_green_bw_daily %>% mutate(model_name = "green")
alpha_cor_green_fw_daily <- alpha_cor_green_fw_daily %>% mutate(model_name = "green")

f_plot_hist_cor(alpha_cor_green_bw_daily, "green", "GHG")
f_plot_hist_cor(alpha_cor_green_fw_daily, "green", "ENV")

# 18. f_significant_cor -------------------------------------------------

# f_significant_cor(.alpha_cor)

f_significant_cor(alpha_cor_green_bw_monthly)
f_significant_cor(alpha_cor_green_fw_monthly)

f_significant_cor(alpha_cor_green_bw_daily)
f_significant_cor(alpha_cor_green_fw_daily)

# 19. f_plot_ratios -----------------------------------------------------

# f_plot_ratios(.screening_df)

tidy_screening_green_bw_monthly <- tidy_screening_green_bw_monthly %>% mutate(model_name = "green")
tidy_screening_green_fw_monthly <- tidy_screening_green_fw_monthly %>% mutate(model_name = "green")

f_plot_ratios(tidy_screening_green_bw_monthly)
f_plot_ratios(tidy_screening_green_fw_monthly)

tidy_screening_green_bw_daily <- tidy_screening_green_bw_daily %>% mutate(model_name = "green")
tidy_screening_green_fw_daily <- tidy_screening_green_fw_daily %>% mutate(model_name = "green")

f_plot_ratios(tidy_screening_green_bw_daily)
f_plot_ratios(tidy_screening_green_fw_daily)

# 20. f_port_permno -----------------------------------------------------

# f_port_permno(.alpha_screen, .id, .id_date)

port_permno_green_bw_monthly <- f_port_permno(alpha_screen_green_bw_monthly, ghg_id_monthly$green, id_date_bw_monthly)
port_permno_green_fw_monthly <-f_port_permno(alpha_screen_green_fw_monthly, ghg_id_monthly$green, id_date_fw_monthly)

port_permno_green_bw_daily <- f_port_permno(alpha_screen_green_bw_daily, ghg_id_daily$green, id_date_bw_daily)
port_permno_green_fw_daily <-f_port_permno(alpha_screen_green_fw_daily, ghg_id_daily$green, id_date_fw_daily)

# 21. f_port_ret --------------------------------------------------------

# f_port_ret(.df, .port_permno, .id_date, .datafreq)

port_ret_green_bw_monthly <- f_port_ret(sp_df_monthly, port_permno_green_bw_monthly, id_date_bw_monthly, "monthly")
port_ret_green_fw_monthly <- f_port_ret(sp_df_monthly, port_permno_green_fw_monthly, id_date_fw_monthly, "monthly")

port_ret_green_bw_daily <- f_port_ret(sp_df_daily, port_permno_green_bw_daily, id_date_bw_daily, "daily")
port_ret_green_fw_daily <- f_port_ret(sp_df_daily, port_permno_green_fw_daily, id_date_fw_daily, "daily")

# 22. f_port_growth -----------------------------------------------------

# f_port_growth(.port, .model_name)

port_ret_green_bw_monthly <- port_ret_green_bw_monthly %>% 
  map_dfr(~bind_rows(.x)) %>%
  mutate(model_name = "green") %>% 
  pivot_wider(., names_from = port, values_from = ret)

port_growth_green_bw_monthly <- f_port_growth(port_ret_green_bw_monthly, "green")

port_growth_green_bw_monthly %>% arrange(name)

port_ret_green_bw_daily <- port_ret_green_bw_daily %>%
  map_dfr(~bind_rows(.x)) %>%
  mutate(model_name = "green") %>% 
  pivot_wider(., names_from = port, values_from = ret)

port_growth_green_bw_daily <- f_port_growth(port_ret_green_bw_daily, "green")

port_growth_green_bw_daily %>% arrange(name)

# 23. f_port_growth_chart -----------------------------------------------

# f_port_growth_chart(.port, .esg_name)

f_port_growth_chart(port_growth_green_bw_monthly, "GHG")
f_port_growth_chart(port_growth_green_bw_daily, "GHG")

# 24  f_port_stats ------------------------------------------------------

# f_port_stats(.port, .model_name)

f_port_stats(port_ret_green_bw_monthly, "green")
f_port_stats(port_ret_green_bw_daily, "green")
