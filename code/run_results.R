source(here::here("function", "screening_funs_V2.R"))

# ghg_results <- readRDS(here("output", "monthly_ghg_6f_results.Rds"))
# env_results <- readRDS(here("output", "monthly_env_6f_results.Rds"))

# Plotting Results -----------------------------------------------------------------

st_options(round.digits = 4, style = "rmarkdown", plain.ascii = FALSE)

# Plot Obs ----------------------------------------------------------------

model_names <- str_to_title(gsub("_", " ", names(ghg_results)))

ghg_obs <- map2_df(map(ghg_results, "n_obs"), model_names, ~mutate(.x, model_name = .y))
env_obs <- map2_df(map(env_results, "n_obs"), model_names, ~mutate(.x, model_name = .y))

f_plot_obs(ghg_obs)
f_plot_obs(env_obs)

ghg_missing_obs <- f_missing_obs(ghg_obs) 
ghg_missing_obs %>% unclass() %>% write.csv(file = here("output", paste0(params$datafreq, "_ghg_missing_obs.csv")))
env_missing_obs <- f_missing_obs(env_obs) 
env_missing_obs %>% unclass() %>% write.csv(file = here("output", paste0(params$datafreq, "_env_missing_obs.csv")))

# Plot Alpha Cor ----------------------------------------------------------

ghg_alpha_cor <- map2_df(map(ghg_results, "alpha_cor"), model_names, ~mutate(.x, model_name = .y))
env_alpha_cor <- map2_df(map(env_results, "alpha_cor"), model_names, ~mutate(.x, model_name = .y))

map2(ghg_alpha_cor, model_names, ~f_plot_hist_cor(.x, .y, "ghg_alpha_cor"))
map2(env_alpha_cor, model_names, ~f_plot_hist_cor(.x, .y, "env_alpha_cor"))

ghg_significant_cor <- f_significant_cor(ghg_alpha_cor)
ghg_significant_cor %>% write.csv(file = here("output", "ghg_significant_cor.csv"))
env_significant_cor <- f_significant_cor(env_alpha_cor)
env_significant_cor %>% write.csv(file = here("output", "env_significant_cor.csv"))

# Plot Ratios -------------------------------------------------------------

ghg_screening <- map2_df(map(ghg_results, "screening"), model_names, ~mutate(.x, model_name = .y))
env_screening <- map2_df(map(env_results, "screening"), model_names, ~mutate(.x, model_name = .y))

f_plot_ratios(ghg_screening)
f_plot_ratios(env_screening)

ghg_screening_stats <- ghg_screening %>% group_by(model_name) %>% descr()
ghg_screening_stats %>% unclass() %>% write.csv(file = here("output", paste0(params$datafreq, "_ghg_ratios_stats.csv")))
env_screening_stats <- env_screening %>% group_by(model_name) %>% descr()
env_screening_stats %>% unclass() %>% write.csv(file = here("output", paste0(params$datafreq, "_env_ratios_stats.csv")))

# Portfolio Analysis ------------------------------------------------------

ghg_port <- map2_df(map(ghg_results[1:3], "port_ret"), model_names[1:3], ~mutate(.x, model_name = .y)) %>% 
  pivot_wider(., names_from = port, values_from = ret)
env_port <- map2_df(map(env_results[1:3], "port_ret"), model_names[1:3], ~mutate(.x, model_name = .y)) %>% 
  pivot_wider(., names_from = port, values_from = ret)

ghg_green_port_growth <- f_port_growth(ghg_port, "Green Bw")
ghg_brown_port_growth <- f_port_growth(ghg_port, "Brown Bw")
ghg_neutral_port_growth <- f_port_growth(ghg_port, "Neutral Bw")

f_port_growth_chart(ghg_green_port_growth, "Green")
f_port_growth_chart(ghg_brown_port_growth, "Brown")
f_port_growth_chart(ghg_neutral_port_growth, "Neutral")

env_green_port_growth <- f_port_growth(env_port, "Green Bw")
env_brown_port_growth <- f_port_growth(env_port, "Brown Bw")
env_neutral_port_growth <- f_port_growth(env_port, "Neutral Bw")

f_port_growth_chart(env_green_port_growth, "Green")
f_port_growth_chart(env_brown_port_growth, "Brown")
f_port_growth_chart(env_neutral_port_growth, "Neutral")

ghg_green_port_stats <- f_port_stats(ghg_port, "Green Bw")
ghg_brown_port_stats <- f_port_stats(ghg_port, "Brown Bw")
ghg_neutral_port_stats <- f_port_stats(ghg_port, "Neutral Bw")

env_green_port_stats <- f_port_stats(env_port, "Green Bw")
env_brown_port_stats <- f_port_stats(env_port, "Brown Bw")
env_neutral_port_stats <- f_port_stats(env_port, "Neutral Bw")
