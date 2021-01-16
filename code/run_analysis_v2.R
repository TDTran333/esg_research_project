# For best practice, please restart R session before running script. Ctrl/Cmd + Shift + F10.

source(here::here("code", "source.R"))

# Parameters --------------------------------------------------------------

params          <- list()         
params$control  <- list(nCore = 3)           # Number of core and other controls
params$subDir   <- "2009-2017"               # Sample period
params$datafreq <- "monthly"                 # Data frequency : "monthly" or "daily"       
params$window   <- 36                        # Rolling window in # of months
params$nblock   <- 20                        # Number of blocks in screenplots
params$factor   <- "six"                     # Factor model : "three" or "six"

# Load Data ---------------------------------------------------------------

sp_df <- readRDS(here("data", params$subDir, paste0(params$datafreq, "_data_sp.rds")))
factor_df <- readRDS(here("data", params$subDir, paste0(params$datafreq, "_factor.rds")))

# Create ESG Groups -------------------------------------------------------

create_esg <- partial(f_create_esg_type, .datafreq = params$datafreq, .window = params$window)

ghg_df <- sp_df %>% create_esg(.esg_var = "ghg")
env_df <- sp_df %>% create_esg(.esg_var = "envscore")

# Transition Matrix -------------------------------------------------------

tm_ghg <- ghg_df %>% f_create_trans_mat()
tm_env <- env_df %>% f_create_trans_mat()

# Create id ---------------------------------------------------------------

ghg_id <- env_id <- list()
esg_group <- c("green", "brown", "neutral")
ghg_id[esg_group] <- map(c("G", "B", "N"), ~f_create_id(ghg_df, .x))
env_id[esg_group] <- map(c("G", "B", "N"), ~f_create_id(env_df, .x))

# Main function -----------------------------------------------------------

f_process_model <- function(.df, .factor_df, .factor_model, .id, .id_name, .date, .model,
                            .datafreq, .run_cor = TRUE, .method, .window, .nblock, 
                            .control, .verbose = TRUE) {

  .factor_model = match.arg(.factor_model, c("three", "six"))
  .model        = match.arg(.model, c("bw", "fw"))
  .datafreq     = match.arg(.datafreq, c("monthly", "daily"))
  .method       = match.arg(.method, c("pearson", "kendall", "spearman"))
  
  # Create Dates
  dates <- f_create_dates(.date, .window, .model, .datafreq)

  model_name <- paste(.model, .window, "m_window", .factor_model, 
                    "factor_model", .datafreq, "data", .id_name, sep = "_")
  
  if (isTRUE(.verbose)) {message(paste0("Model: ", model_name, "."))}
  
  tmp1 <- tmp2 <- tmp3 <- tmp4 <- tmp5 <- list()

  for (i in seq_along(dates$id_date)) {
    
    if (isTRUE(.verbose)) {
      message(paste("Analyzing", dates$id_date[i]), ". ",
                    "Completed: ", i, " out of ", length(dates$id_date), ".")
    }
    
    # Alpha Screen
    ret_mat <- f_ret_mat(.df, .id, dates$id_date[i], dates$start_date[i], dates$end_date[i])
    factor_mat <- f_factor_mat(.factor_df, dates$start_date[i], dates$end_date[i])
    alpha_screen <- PeerPerformance::alphaScreening(ret_mat, factors = factor_mat, control = .control)
    
    # Alpha Correlations
    if (isTRUE(.run_cor)) {alpha_cor <- f_alpha_cor(ret_mat, factor_mat, dates$id_date[i], .method = .method)}
    
    # Table for Screening Plot
    tbl_screening <- f_tbl_screening(alpha_screen, .nblock)
    
    # Create Screening Plot
    fig_title <- paste(model_name, dates$id_date[i], sep = "_")
    f_screenplot(tbl_screening, fig_title, .datafreq)
    
    # Port permno
    port_permno <- f_port_permno(alpha_screen, .id, dates$id_date[i])
    
    # Port return
    port_ret <- f_port_ret(.df, port_permno, dates$id_date[i], .datafreq)
    
    # Outputs
    tmp1 <- append(tmp1, list(alpha_screen))
    tmp2 <-  if (isTRUE(.run_cor)) {append(tmp2, list(alpha_cor))}
    tmp3 <- append(tmp3, list(tbl_screening))
    tmp4 <- append(tmp4, list(port_permno))
    tmp5 <- append(tmp5, list(port_ret))
  }
  
  if (isTRUE(.verbose)) {message("Completed!")}
  
  names(tmp1) <- names(tmp3) <- names(tmp4) <- names(tmp5) <- dates$id_date
  if (isTRUE(.run_cor)) {names(tmp2) <- dates$id_date}
  # results <- tibble(out1, out2, out3, out4, out5, dates$id_date) %>% 
  #   `colnames<-`(c("alpha_screen", "alpha_cor", "tbl_screening", "port_permno", "port_ret", "id_date"))

  tidy_n_obs     <- f_tidy_n_obs(tmp1, dates$id_date)
  tidy_alpha_cor <- if (isTRUE(.run_cor)) {f_tidy_alpha_cor(tmp2)}
  tidy_screening <- f_tidy_screening(tmp3, dates$id_date)
  tidy_port_ret  <- tmp5 %>% map_dfr(~bind_rows(.x)) %>% spread(port, ret)
  
  if (isTRUE(.run_cor)) {
    tidy_results   <- list(dates$id_date, tidy_n_obs, tidy_alpha_cor, tidy_screening, tidy_port_ret) %>% 
      `names<-`(c("date", "n_obs", "alpha_cor", "screening", "port_ret"))
  } else {
    tidy_results<- list(dates$id_date, tidy_n_obs, tidy_screening, tidy_port_ret) %>% 
      `names<-`(c("date", "n_obs", "screening", "port_ret"))
  }
}

date_col <- ghg_df %>% select(date) %>% distinct() %>% pluck()

f_run_model <- partial(f_process_model,
                       .df           = sp_df,
                       .factor       = factor_df,
                       .factor_model = params$factor,
                       .date         = date_col[1:20, ],
                       .datafreq     = params$datafreq,
                       .run_cor      = FALSE,
                       .method       = "pearson",
                       .window       = params$window, 
                       .nblock       = params$nblock, 
                       .control      = params$control)

ghg_results <- env_results <- list()
ghg_results2[paste0(esg_group, "_bw")] <- map(esg_group, ~f_run_model(.id = ghg_id[[.x]], 
                                                                      .id_name = paste0("ghg_id['", .x, "']"), 
                                                                      .model = "bw"))
ghg_results2[paste0(esg_group, "_fw")] <- map(esg_group, ~f_run_model(.id = ghg_id[[.x]], 
                                                                      .id_name = paste0("ghg_id['", .x, "']"), 
                                                                      .model = "fw"))

env_results[paste0(esg_group, "_bw")] <- map(esg_group, ~f_run_model(.id = ghg_id[[.x]], 
                                                                     .id_name = paste0("ghg_id['", .x, "']"), 
                                                                     .model = "bw"))
env_results[paste0(esg_group, "_fw")] <- map(esg_group, ~f_run_model(.id = ghg_id[[.x]], 
                                                                     .id_name = paste0("ghg_id['", .x, "']"), 
                                                                     .model = "fw"))

glimpse(ghg_results)
glimpse(env_results)

ghg_results %>% saveRDS(file = here("output", "monthly_ghg_results2.Rds"))
env_results %>% saveRDS(file = here("output", "monthly_env_results.Rds"))

test <- readRDS(here("output", "monthly_ghg_results.Rds"))
test2 <- readRDS(here("output", "monthly_env_results.Rds"))

ghg_results %>% object.size()
env_results$green_bw$port_ret %>% object.size()

source(here::here("function", "screening_funs_V2.R"))

# Plotting Results -----------------------------------------------------------------

model_names <- str_to_title(gsub("_", " ", names(test)))

ghg_screening <- map2_df(map(test, "screening"), model_names, ~mutate(.x, model_name = .y))
env_screening <- map2_df(map(test2, "n_obs"), model_names, ~mutate(.x, model_name = .y))

ghg_screening %>%
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
library(tidyquant)

# Maybe Add sp500 or portfolio based on alpha

ghg_port_ret <- test$green_bw$port_ret %>% 
  map_dfr(~bind_rows(.x)) %>% 
  spread(port, ret)

ghg_port_ret %>% tq_portfolio()

ghg_port_ret_xts <- ghg_port_ret %>% 
  select(-period) %>% 
  as.matrix() %>% 
  xts(order.by = ghg_port_ret$date) %>% 
  table.DownsideRisk()

ghg_port_ret_xts$top_10 %>% class
  table.DownsideRisk(ghg_port_ret_xts$top_10)


id_benchmark_ret %>%
  select(date, ret) %>% 
  as.xts(order.by = .$date) %>%
  select(ret)

xts(ghg_port_ret$top_10, ghg_port_ret$date) %>% 
  table.DownsideRisk()

xts(id_benchmark_ret$ret, id_benchmark_ret$date) %>% 
  SharpeRatio()

xts(id_benchmark_ret$ret, id_benchmark_ret$date) %>% 
  table.Stats()

xts(id_benchmark_ret$ret, id_benchmark_ret$date) %>% 
  chart.Histogram()

xts(id_benchmark_ret$ret, id_benchmark_ret$date) %>% 
  chart.CumReturns()

xts(id_benchmark_ret$ret, id_benchmark_ret$date) %>% 
  chart.QQPlot()

xts(id_benchmark_ret$ret, id_benchmark_ret$date) %>% 
  charts.PerformanceSummary()

xts(id_benchmark_ret$ret, id_benchmark_ret$date) %>% 
  charts.RollingPerformance()

xts(id_top_10_ret$ret, id_benchmark_ret$date) %>% 
  VaR()

tidy_results$tidy_screening %>% 
ggplot(aes(date, value, color = name)) +
  geom_line() +
  scale_x_yearqtr(format = "%Y-Q%q") +
  labs(title = paste0("Mean Out/Under-Performance Ratios using ",params$window, "-months rolling window."),
       x = "Date",
       y = "Percent",
       color = "Pi Ratios") +
  scale_y_continuous(labels = percent)


tidy_results$tidy_n_obs %>% 
  filter(!is.na(obs)) %>%
  ggplot(aes(obs)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ factor(date)) +
  labs(title = paste0("Concordant observations."))

tidy_results$tidy_alpha_cor %>% 
  ggplot(aes(value)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ factor(date)) +
  geom_vline(xintercept = -0.3, lty = "dashed") +
  geom_vline(xintercept = 0.3, lty = "dashed") + 
  labs(title = paste0("Correlations of alphas"))
































