# For best practice, please restart R session before running script. Ctrl/Cmd + Shift + F10.

source(here::here("code", "source.R"))

# Parameters --------------------------------------------------------------

params            <- list()         
params$control    <- list(nCore = 3)           # Number of core and other controls
params$sample_per <- 2009:2017                 # Sample period
params$subDir     <- paste0(first(params$sample), "-", last(params$sample))
params$datafreq   <- "daily"                 # Data frequency : "monthly" or "daily"       
params$window     <- 36                        # Rolling window in # of months
params$nblock     <- 20                        # Number of blocks in screenplots
params$factor     <- "six"                     # Factor model : "three" or "six"

# Formatting
st_options(round.digits = 2, style = "rmarkdown", plain.ascii = FALSE)
kable <- function(data) {knitr::kable(data, booktabs = TRUE, digits = 2)}

# Load Results ------------------------------------------------------------

# ghg_results <- readRDS(here("output", "results", "daily_ghg_3f_results.Rds"))
ghg_results <- readRDS(here("output", "results", "daily_ghg_6f_results.Rds"))
env_results <- readRDS(here("output", "results", "daily_env_6f_results.Rds"))

# ghg_results <- readRDS(here("output", "results", "monthly_ghg_3f_results.Rds")) 
# ghg_results <- readRDS(here("output", "results", "monthly_ghg_6f_results.Rds"))
# env_results <- readRDS(here("output", "results", "monthly_env_6f_results.Rds"))

ghg_results %>% glimpse()
env_results %>% glimpse()

ghg_results %>% object.size()
env_results %>% object.size()

# Plot Obs ----------------------------------------------------------------

model_names <- str_to_title(gsub("_", " ", names(ghg_results)))

ghg_obs <- map2_df(map(ghg_results, "n_obs"), model_names, ~mutate(.x, model_name = .y))

f_plot_obs(ghg_obs)
ghg_missing_obs <- f_missing_obs(ghg_obs)
ghg_missing_obs %>% unclass() %>% write.csv(file = here("output", paste0(params$datafreq, "_6f_ghg_missing_obs.csv")))

env_obs <- map2_df(map(env_results, "n_obs"), model_names, ~mutate(.x, model_name = .y))
f_plot_obs(env_obs)
env_missing_obs <- f_missing_obs(env_obs)
env_missing_obs %>% unclass() %>% write.csv(file = here("output", paste0(params$datafreq, "_6f_env_missing_obs.csv")))

# Plot Alpha Cor ----------------------------------------------------------

# ghg_results_cor <- readRDS(here("output", "results", "monthly_ghg_6f_results_cor.Rds"))
# ghg_alpha_cor <- map2_df(map(ghg_results_cor, "alpha_cor"), model_names, ~mutate(.x, model_name = .y))
# ghg_alpha_cor_plots <- map(model_names, ~f_plot_hist_cor(ghg_alpha_cor, .x, "ghg_alpha_cor"))
# ghg_significant_cor <- f_significant_cor(ghg_alpha_cor)
# ghg_significant_cor %>% write.csv(file = here("output", paste0(params$datafreq, "_6f_ghg_significant_cor.csv")))
# 
# env_results_cor <- readRDS(here("output", "results", "monthly_env_6f_results_cor.Rds"))
# env_alpha_cor <- map2_df(map(env_results_cor, "alpha_cor"), model_names, ~mutate(.x, model_name = .y))
# env_alpha_cor_plots <- map(model_names, ~f_plot_hist_cor(env_alpha_cor, .x, "env_alpha_cor"))
# env_significant_cor <- f_significant_cor(env_alpha_cor)
# env_significant_cor %>% write.csv(file = here("output", paste0(params$datafreq, "_6f_env_significant_cor.csv")))

# Plot Ratios -------------------------------------------------------------

ghg_screening <- map2_df(map(ghg_results, "screening"), model_names, ~mutate(.x, model_name = .y))

f_plot_ratios(ghg_screening)
ghg_screening_stats <- ghg_screening %>% group_by(model_name, name) %>% descr()
ghg_screening_stats %>% unclass() %>% write.csv(file = here("output", paste0(params$datafreq, "_6f_ghg_ratios_stats.csv")))

env_screening <- map2_df(map(env_results, "screening"), model_names, ~mutate(.x, model_name = .y))
f_plot_ratios(env_screening)
env_screening_stats <- env_screening %>% group_by(model_name, name) %>% descr()
env_screening_stats %>% unclass() %>% write.csv(file = here("output", paste0(params$datafreq, "_6f_env_ratios_stats.csv")))

# Portfolio Analysis ------------------------------------------------------

ghg_port <- map2_df(map(ghg_results[1:3], "port_ret"), model_names[1:3], ~mutate(.x, model_name = .y)) %>% 
  pivot_wider(., names_from = port, values_from = ret)

ghg_green_port_growth <- f_port_growth(ghg_port, "Green Bw")
ghg_brown_port_growth <- f_port_growth(ghg_port, "Brown Bw")
ghg_neutral_port_growth <- f_port_growth(ghg_port, "Neutral Bw")

f_port_growth_chart(ghg_green_port_growth, "Green")
f_port_growth_chart(ghg_brown_port_growth, "Brown")
f_port_growth_chart(ghg_neutral_port_growth, "Neutral")

ghg_green_port_stats <- f_port_stats(ghg_port, "Green Bw")
ghg_brown_port_stats <- f_port_stats(ghg_port, "Brown Bw")
ghg_neutral_port_stats <- f_port_stats(ghg_port, "Neutral Bw")

env_port <- map2_df(map(env_results[1:3], "port_ret"), model_names[1:3], ~mutate(.x, model_name = .y)) %>% 
  pivot_wider(., names_from = port, values_from = ret)

env_green_port_growth <- f_port_growth(env_port, "Green Bw")
env_brown_port_growth <- f_port_growth(env_port, "Brown Bw")
env_neutral_port_growth <- f_port_growth(env_port, "Neutral Bw")

f_port_growth_chart(env_green_port_growth, "Green")
f_port_growth_chart(env_brown_port_growth, "Brown")
f_port_growth_chart(env_neutral_port_growth, "Neutral")

env_green_port_stats <- f_port_stats(env_port, "Green Bw")
env_brown_port_stats <- f_port_stats(env_port, "Brown Bw")
env_neutral_port_stats <- f_port_stats(env_port, "Neutral Bw")




# Checking weird output ---------------------------------------------------
# 
# env_brown_port_growth %>%
#   filter(name == "benchmark_minus_20_pct_brown_bw") %>% View()
# 
# env_port %>%
#   filter(model_name == "Brown Bw") %>%
#   select(date, benchmark_minus_20_pct) %>%
#   View()
# 
# id_date <- "Jun 2013"
# start_date <- "2010-07-01"
# end_date <- "2013-06-30"
# 
# sp_df_daily <- readRDS(here("data", params$subDir, "daily_data_sp.rds"))
# factor_df_daily <- readRDS(here("data", params$subDir, "daily_factor.rds"))
# env_df_daily <- sp_df_daily %>% f_create_esg_type(.esg_var = "envscore", "daily", params$window)
# 
# esg_group <- c("green", "brown", "neutral")
# env_id_daily <- list()
# env_id_daily[esg_group] <- map(c("G", "B", "N"), ~f_create_id(env_df_daily, .x))
# 
# ret_mat_brown_bw_daily <- f_ret_mat(sp_df_daily, env_id_daily$brown, id_date, start_date, end_date)
# factor_mat_bw_daily <- f_factor_mat(factor_df_daily, start_date, end_date)
# 
# alpha_screen_brown_bw_daily <- PeerPerformance::alphaScreening(ret_mat_brown_bw_daily, factor_mat_bw_daily, params$control)
# 
# tbl_screening_brown_bw_daily <- f_tbl_screening(alpha_screen_brown_bw_daily, params$nblock)
# tidy_screening_brown_bw_daily <- f_tidy_screening(tbl_screening_brown_bw_daily %>% list(), id_date)
# 
# tidy_obs_brown_bw_daily <- f_tidy_n_obs(alpha_screen_brown_bw_daily %>% list(), id_date)
# tidy_obs_brown_bw_daily <- tidy_obs_brown_bw_daily %>% mutate(model_name = "brown")
# f_missing_obs(tidy_obs_brown_bw_daily)
# 
# port_permno_brown_bw_daily <- f_port_permno(alpha_screen_brown_bw_daily, ghg_id_daily$brown, id_date)
# port_ret_brown_bw_daily <- f_port_ret(sp_df_daily, port_permno_brown_bw_daily, id_date, "daily")
# 
# port_ret_brown_bw_daily %>% View()
