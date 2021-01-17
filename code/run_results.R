source(here::here("function", "screening_funs_V2.R"))

# ghg_results <- readRDS(here("output", "monthly_ghg_6f_results.Rds"))
# env_results <- readRDS(here("output", "monthly_env_6f_results.Rds"))

# Plotting Results -----------------------------------------------------------------

model_names <- str_to_title(gsub("_", " ", names(ghg_results)))

ghg_screening <- map2_df(map(ghg_results, "screening"), model_names, ~mutate(.x, model_name = .y))
env_screening <- map2_df(map(env_results, "n_obs"), model_names, ~mutate(.x, model_name = .y))

test4 <- function(x){
  print(x)
  print(deparse(substitute(x)))
}

test4(head(ghg_screening))

f_plot_screening <- function(.screening_df){
  
  title <- paste0("Mean Out/Under-Performance Ratios Using ", 
                  params$window, "-Month Rolling Window based on ", 
                  deparse(substitute(.screening_df)), ".")
  
  p <- .screening_df %>%
    ggplot(aes(date, value, color = name)) +
    geom_line(size = 1.5) +
    facet_wrap(~ model_name, ncol = 2, dir = "h", scale = "free_x") +
    labs(title = title,
         subtitle = paste0("Backward-Looking and Forward-Looking for Green, Brown and Neutral Firms Using ",
                           params$factor, "-factor model."),
         x = "Date",
         y = "Percent",
         color = "Ratios") +
    scale_y_continuous(labels = percent) +
    scale_x_yearqtr(format = "%Y-Q%q")
  
  screening_name <- paste(deparse(substitute(.screening_df)), params$window, "m", params$datafreq, "data", 
                          params$factor, "factor_model.png", sep = "_")
  ggsave(p, filename = here("Output", "figures", screening_name), width = 10, height = 8, dpi = 150)
  
  return(p)
}

f_plot_screening(ghg_screening)

env_screening %>%
  mutate(model_name = fct_relevel(model_name, c(paste0(esg_group, "_bw"), paste0(esg_group, "_fw")))) %>% 
  ggplot(aes(date, value, color = name)) +
  geom_line(size = 1.5) +
  facet_wrap(~ model_name, ncol = 2, dir = "v", scale = "free_x") +
  labs(title = paste0("Mean Out/Under-Performance Ratios Using ",params$window, "-Month Rolling Window."),
       subtitle = paste0("Backward-looking and forward-looking for Green, Brown and Neutral firms using ",
                         params$factor, "-factor model."),
       x = "Date",
       y = "Percent",
       color = "Ratios") +
  scale_y_continuous(labels = percent) +
  scale_x_yearqtr(format = "%Y-Q%q")

screening_name <- paste(params$window, "m", params$datafreq, "data", params$factor, "factor_model_mean_pi.png", sep = "_")
ggsave(here("Output", "figures", screening_name), width = 10, height = 8, dpi = 150)

# add table to summarize screening

# -------------------------------------------------------------------------

ghg_obs <- map2_df(map(test, "n_obs"), model_names, ~mutate(.x, model_name = .y))
env_obs <- map2_df(map(test2, "n_obs"), model_names, ~mutate(.x, model_name = .y))

ghg_obs %>% 
  filter(!is.na(obs)) %>%
  group_by(model_name, date) %>%
  mutate(pct = obs / max(obs)) %>% 
  summarize(average = mean(pct)) %>%
  ggplot(aes(average)) +
  geom_histogram(bins = 10) +
  facet_wrap(~ model_name, scale = "free", ncol = 3, dir = "v") +
  labs(title = paste0("Percent Concordant Observations.")) + 
  scale_x_continuous(labels = percent)

obs_name <- paste(params$window, "m", params$datafreq, "data", params$factor, "factor_model_missing_obs.png", sep = "_")
ggsave(here("Output", "figures", obs_name), width = 10, height = 8, dpi = 150)

ghg_obs %>% 
  filter(!is.na(obs)) %>%
  group_by(model_name) %>%
  summarize(n_obs = n(),
            full_obs = max(obs),
            avg_obs = mean(obs),
            missing_obs = mean(obs < full_obs) * n_obs,
            pct_missing_obs = (missing_obs / n_obs) * 100) %>%
  knitr::kable(digits = 2)

# -------------------------------------------------------------------------

ghg_alpha_cor <- map(test, "alpha_cor")
env_alpha_cor <- map(test2, "alpha_cor")

map2(ghg_alpha_cor, model_names, ~f_plot_hist_cor(.x, .y))
map2(env_alpha_cor, model_names, ~f_plot_hist_cor(.x, .y))

.f_plot_hist_cor <- function(.tidy_alpha_cor, .model_name)  {
  p <- .tidy_alpha_cor %>% 
    ggplot(aes(value)) +
    geom_histogram(bins = 30) +
    facet_wrap(~ factor(date)) +
    geom_vline(xintercept = -0.3, lty = "dashed") +
    geom_vline(xintercept = 0.3, lty = "dashed") + 
    labs(title = paste0("Correlations of Alphas For ", .model_name, "."),
         subtitle = "Dashed lines correspond to -0.3 and +0.3")
  
  alpha_cor_name <- paste(.model_name, params$window, "m", params$datafreq, "data", 
                          params$factor, "factor_model_alpha_cor.png", sep = "_")
  ggsave(p, filename = here("Output", "figures", alpha_cor_name), width = 8, height = 8, dpi = 150)
  
  return(p)
}
f_plot_hist_cor <- compiler::cmpfun(.f_plot_hist_cor)

# Add pct of cor higher and lower than +- 0.3


# Portfolio Analysis ------------------------------------------------------

library(PerformanceAnalytics)

port2 <- map2_df(map(ghg_results, "port_ret"), model_names, ~mutate(.x, model_name = .y)) %>% 
  pivot_wider(., names_from = port, values_from = ret)

ghg_port_ret <- test$green_bw$port_ret %>% 
  map_dfr(~bind_rows(.x)) %>% 
  mutate(model_name = "green_bw") %>%
  spread(port, ret)

green_port <- port2 %>%
  filter(model_name == "Green Bw")

green_port_xts <- green_port %>% 
  select(-(date:model_name)) %>% 
  as.xts(order.by = green_port$date)

green_port_xts %>% table.Stats() %>% knitr::kable()
green_port_xts %>% SharpeRatio() %>% knitr::kable()
green_port_xts %>% table.DownsideRisk() %>%  knitr::kable()
green_port_xts %>% chart.Histogram()
green_port_xts %>% chart.CumReturns(wealth.index = TRUE, legend.loc = "topright")
green_port_xts %>% chart.QQPlot()
green_port_xts %>% charts.PerformanceSummary()
green_port_xts %>% charts.RollingPerformance()
green_port_xts %>% VaR()

