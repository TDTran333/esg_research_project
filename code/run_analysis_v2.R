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
                            .control, .run_port = TRUE, .verbose = TRUE) {

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
    
    if (isTRUE(.run_port)) {
      # Port permno
      port_permno <- f_port_permno(alpha_screen, .id, dates$id_date[i])
      
      # Port return
      port_ret <- f_port_ret(.df, port_permno, dates$id_date[i], .datafreq)
    }
    
    # Outputs
    tmp1 <- append(tmp1, list(alpha_screen))
    tmp2 <- if (isTRUE(.run_cor)) {append(tmp2, list(alpha_cor))}
    tmp3 <- append(tmp3, list(tbl_screening))
    tmp4 <- if (isTRUE(.run_port)) {append(tmp4, list(port_permno))}
    tmp5 <- if (isTRUE(.run_port)) {append(tmp5, list(port_ret))}
  }
  
  if (isTRUE(.verbose)) {message("Completed!")}
  
  names(tmp1) <- names(tmp3) <- dates$id_date
  if (isTRUE(.run_cor)) {names(tmp2) <- dates$id_date}
  if (isTRUE(.run_port)) {names(tmp4) <- names(tmp5) <- dates$id_date}
  
  tidy_n_obs     <- f_tidy_n_obs(tmp1, dates$id_date)
  tidy_alpha_cor <- if (isTRUE(.run_cor)) {f_tidy_alpha_cor(tmp2)}
  tidy_screening <- f_tidy_screening(tmp3, dates$id_date)
  tidy_port_ret  <- if (isTRUE(.run_port)) {tmp5 %>% map_dfr(~bind_rows(.x))}
  
  tidy_results <- list(dates$id_date, tidy_n_obs, tidy_screening) %>%
    `names<-`(c("date", "n_obs", "screening"))

  if (isTRUE(.run_cor)) {
    tidy_results   <- append(tidy_results, list(tidy_alpha_cor) %>% `names<-`(c("alpha_cor")))
  }
  
  if (isTRUE(.run_port)) {
    tidy_results   <- append(tidy_results, list(tidy_port_ret) %>% `names<-`(c("port_ret")))
  }
  
  return(tidy_results)
}

date_col <- ghg_df %>% select(date) %>% distinct() %>% pluck()

f_run_model <- partial(f_process_model,
                       .df           = sp_df,
                       .factor       = factor_df,
                       .factor_model = params$factor,
                       .date         = date_col,
                       .datafreq     = params$datafreq,
                       .run_cor      = if(params$datafreq == "daily") {FALSE} else {TRUE},
                       .method       = "pearson",
                       .window       = params$window, 
                       .nblock       = params$nblock, 
                       .control      = params$control)

ghg_results2 <- env_results2 <- list()
ghg_results[paste0(esg_group, "_bw")] <- map(esg_group, ~f_run_model(.id = ghg_id[[.x]], 
                                                                      .id_name = paste0("ghg_id['", .x, "']"), 
                                                                      .model = "bw"))

ghg_results[paste0(esg_group, "_fw")] <- map(esg_group, ~f_run_model(.id = ghg_id[[.x]], 
                                                                      .id_name = paste0("ghg_id['", .x, "']"), 
                                                                      .model = "fw",
                                                                      .run_port = FALSE))

env_results[paste0(esg_group, "_bw")] <- map(esg_group, ~f_run_model(.id = ghg_id[[.x]], 
                                                                     .id_name = paste0("ghg_id['", .x, "']"), 
                                                                     .model = "bw"))
env_results[paste0(esg_group, "_fw")] <- map(esg_group, ~f_run_model(.id = ghg_id[[.x]], 
                                                                     .id_name = paste0("ghg_id['", .x, "']"), 
                                                                     .model = "fw",
                                                                     .run_port = FALSE))

glimpse(ghg_results)
glimpse(env_results)

# ghg_results %>% saveRDS(file = here("output", "monthly_ghg_3f_results.Rds"))
# env_results %>% saveRDS(file = here("output", "monthly_env_3f_results.Rds"))
# ghg_results %>% saveRDS(file = here("output", "monthly_ghg_6f_results.Rds"))
# env_results %>% saveRDS(file = here("output", "monthly_env_6f_results.Rds"))
# ghg_results %>% saveRDS(file = here("output", "daily_ghg_3f_results.Rds"))
# env_results %>% saveRDS(file = here("output", "daily_env_3f_results.Rds"))
# ghg_results %>% saveRDS(file = here("output", "daily_ghg_6f_results.Rds"))
# env_results %>% saveRDS(file = here("output", "daily_env_6f_results.Rds"))

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
ghg_missing_obs %>% unclass() %>% write.csv(file = here("output", "ghg_missing_obs.csv"))
env_missing_obs <- f_missing_obs(env_obs) 
env_missing_obs %>% unclass() %>% write.csv(file = here("output", "env_missing_obs.csv"))

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
env_screening <- map2_df(map(env_results, "n_obs"), model_names, ~mutate(.x, model_name = .y))

f_plot_ratios(ghg_screening)
f_plot_ratios(env_screening)

ghg_screening_stats <- ghg_screening %>% group_by(model_name) %>% descr()
ghg_screening_stats %>% write.csv(file = here("output", "ghg_screening_stats.csv"))
env_screening_stats <- env_screening %>% group_by(model_name) %>% descr()
env_screening_stats %>% write.csv(file = here("output", "ghg_screening_stats.csv"))

# Portfolio Analysis ------------------------------------------------------

ghg_port <- map2_df(map(ghg_results[1:3], "port_ret"), model_names[1:3], ~mutate(.x, model_name = .y)) %>% 
  pivot_wider(., names_from = port, values_from = ret)
env_port <- map2_df(map(env_results[1:3], "port_ret"), model_names[1:3], ~mutate(.x, model_name = .y)) %>% 
  pivot_wider(., names_from = port, values_from = ret)

port_dates <- ghg_port %>% filter(model_name == "Green Bw") %>% select(date)

png(filename = here::here("output", "figures", "ghg_green_port_chart.png"), width = 700, height = 700, units = "px")
ghg_green_port <- ghg_port %>% 
  filter(model_name == "Green Bw") %>% 
  pivot_wider(names_from = model_name, values_from = top_10:benchmark_minus_20_pct) %>% select(-period) %>% clean_names()
ghg_green_port <- xts(ghg_green_port[, -1], order.by = port_dates$date)
ghg_green_port %>% chart.CumReturns(wealth.index = TRUE, legend.loc = "topleft")
dev.off()

ghg_brown_port <- ghg_port %>% 
  filter(model_name == "Brown Bw") %>% 
  pivot_wider(names_from = model_name, values_from = top_10:benchmark_minus_20_pct) %>% select(-period) %>% clean_names()
ghg_brown_port <- xts(ghg_brown_port[, -1], order.by = port_dates$date)
ghg_brown_port %>% chart.CumReturns(wealth.index = TRUE, legend.loc = "topleft")

ghg_neutral_port <- ghg_port %>% 
  filter(model_name == "Neutral Bw") %>% 
  pivot_wider(names_from = model_name, values_from = top_10:benchmark_minus_20_pct) %>% 
  select(-period) %>% 
  clean_names()
ghg_neutral_port <- xts(ghg_neutral_port[, -1], order.by = port_dates$date)
ghg_neutral_port %>% chart.CumReturns(wealth.index = TRUE, legend.loc = "topleft")

ghg_green_port %>% table.Stats()
ghg_green_port %>% SharpeRatio()
ghg_green_port %>% table.DownsideRisk()
ghg_green_port %>% chart.CumReturns(wealth.index = TRUE, legend.loc = "topright")

ghg_green_port %>% table.Stats()
ghg_green_port %>% SharpeRatio()
ghg_green_port %>% table.DownsideRisk()
ghg_green_port %>% chart.Histogram()
ghg_green_port %>% chart.CumReturns(wealth.index = TRUE, legend.loc = "topright")
ghg_green_port %>% chart.QQPlot()
ghg_green_port %>% charts.PerformanceSummary()
ghg_green_port %>% charts.RollingPerformance()
ghg_green_port %>% VaR()

ghg_green_port %>% table.Stats()
ghg_green_port %>% SharpeRatio()
ghg_green_port %>% table.DownsideRisk()
ghg_green_port %>% chart.Histogram()
ghg_green_port %>% chart.CumReturns(wealth.index = TRUE, legend.loc = "topright")
ghg_green_port %>% chart.QQPlot()
ghg_green_port %>% charts.PerformanceSummary()
ghg_green_port %>% charts.RollingPerformance()
ghg_green_port %>% VaR()













