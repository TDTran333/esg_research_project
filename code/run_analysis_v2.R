# For best practice, please restart R session before running script. Ctrl/Cmd + Shift + F10.

source(here::here("code", "source.R"))

# Parameters --------------------------------------------------------------

params          <- list()         
params$control  <- list(nCore = 3)           # Number of core and other controls
params$subDir   <- "2009-2017"               # Sample period
params$datafreq <- "monthly"                # Data frequency : "monthly" or "daily"       
params$window   <- 36                        # Rolling window in # of months
params$bucket   <- 5                         # Number of firms per bucket
params$factor   <- "three"                   # Factor model : "three" or "six"

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
ghg_id["green"]   <- ghg_df %>% f_create_id("G")
ghg_id["brown"]   <- ghg_df %>% f_create_id("B")
ghg_id["neutral"] <- ghg_df %>% f_create_id("N")

env_id["green"]   <- ghg_df %>% f_create_id("G")
env_id["brown"]   <- ghg_df %>% f_create_id("B")
env_id["neutral"] <- ghg_df %>% f_create_id("N")

# Main function -----------------------------------------------------------

f_process_model <- function(.df, .factor_df, .factor_model, .id, .date, .model,
                   .datafreq, .method, .window, .bucket, .control, verbose = TRUE) {
  
  .factor_model = match.arg(.factor_model, c("three", "six"))
  .model        = match.arg(.model, c("bw", "fw"))
  .datafreq     = match.arg(.datafreq, c("monthly", "daily"))
  .method       = match.arg(.method, c("pearson", "kendall", "spearman"))
  
  # Create Dates
  dates <- f_create_dates(.date, .window, .model, .datafreq)

  model_name <- paste(.model, .window, "m_window", .factor_model, 
                    "factor_model", .datafreq, "data", sep = "_")
  
  if (isTRUE(verbose)) {message(paste0("Model: ", model_name, "."))}
  
  out1 <- out2 <- out3 <- out4 <- out5 <- list()

  for (i in seq_along(dates$id_date)) {
    
    if (isTRUE(verbose)) {
      message(paste("Analyzing", dates$id_date[i]), ". ",
                    "Completed: ", i, " out of ", length(dates$id_date), ".")
    }
    
    # Alpha Screen
    ret_mat <- f_ret_mat(.df, .id, dates$id_date[i], dates$start_date[i], dates$end_date[i])
    factor_mat <- f_factor_mat(.factor_df, dates$start_date[i], dates$end_date[i])
    alpha_screen <- PeerPerformance::alphaScreening(ret_mat, factors = factor_mat, control = .control)
    
    # Alpha Correlations
    alpha_cor <- f_alpha_cor(ret_mat, factor_mat, dates$id_date[i], .method = .method)
    
    # Table for Screening Plot
    tbl_screening <- f_tbl_screening(alpha_screen, .bucket)
    
    # Create Screening Plot
    fig_title <- paste(model_name, deparse(substitute(.id)), dates$id_date[i], sep = "_")
    f_screenplot(tbl_screening, fig_title, .datafreq)
    
    # Port permno
    port_permno <- f_port_permno(alpha_screen, .id, dates$id_date[i])
    
    # Port return
    port_ret <- f_port_ret(.df, port_permno, dates$id_date[i], .datafreq)
    
    
    out1 <- append(out1, list(alpha_screen))
    out2 <- append(out2, list(alpha_cor))
    out3 <- append(out3, list(tbl_screening))
    out4 <- append(out4, list(port_permno))
    out5 <- append(out5, list(port_ret))
  }
  
  if (isTRUE(verbose)) {message("Completed!")}
  names(out1) <- names(out2) <- names(out3) <- names(out4) <- names(out5) <- dates$id_date
  results <- tibble(out1, out2, out3, out4, out5, dates$id_date) %>% 
    `colnames<-`(c("alpha_screen", "alpha_cor", "tbl_screening", "port_permno", "port_ret", "id_date"))

}

date_zoo <- ghg_df %>% select(date) %>% distinct() %>% pluck()

results <- f_process_model(sp_df, 
                           factor_df, 
                           .factor_model = "three", 
                           ghg_id$green,
                           date_zoo[1:20, ],
                           .model = "fw",
                           .datafreq = "monthly",
                           .method = "pearson",
                           params$window,
                           params$bucket,
                           params$control)

source(here::here("function", "screening_funs_V2.R"))

# Results -----------------------------------------------------------------

f_tidy_results <- function(.results) {
  tidy_n_obs     <- f_tidy_n_obs(.results$alpha_screen, .results$id_date)
  tidy_alpha_cor <- f_tidy_alpha_cor(.results$alpha_cor)
  tidy_screening <- f_tidy_screening(.results$tbl_screening, .results$id_date)
  tidy_results   <- list(tidy_n_obs, tidy_alpha_cor, tidy_screening) %>% 
    `names<-`(c("tidy_n_obs", "tidy_alpha_cor", "tidy_screening"))
}

tidy_results <- f_tidy_results(results)

# Plot results ------------------------------------------------------------

f_plot_results <- function(.tidy_results, .model_name) {
  p1 <- f_plot_line_pi(tidy_results$tidy_screening, .model_name)
  p2 <- f_plot_hist_obs(.tidy_results$tidy_n_obs, .model_name)
  p3 <- f_plot_hist_cor(.tidy_results$tidy_alpha_cor, .model_name)
  list(p1, p2, p3)
}

plots <- f_plot_results(tidy_results, "test")
walk(plots, print)

f_plot_line_pi(tidy_results$tidy_screening, "test")
f_plot_hist_obs(tidy_results$tidy_n_obs, "test")
f_plot_hist_cor(tidy_results$tidy_alpha_cor, "test")

# Portfolio Analysis ------------------------------------------------------

results$port_permno$`2009 Q4`$top_10

results$port_ret %>%  map("top_10") %>% bind_rows()
results$port_ret %>%  map("bottom_10") %>% bind_rows()
results$port_ret %>%  map("benchmark_10") %>% bind_rows()

results$port_ret %>% 
  map(bind_rows) %>% 
  bind_rows %>% 
  group_by(port) %>% 
  mutate(perf = cumprod(1 + ret)) %>% 
  ggplot(aes(date, perf, color = port)) +
  geom_line()





library(PerformanceAnalytics)

id_benchmark_ret %>%
  mutate(ret = (ret * 100)) %>% 
  select(ret) %>% 
  table.DownsideRisk


id_benchmark_ret %>%
  select(date, ret) %>% 
  as.xts(order.by = .$date) %>%
  select(ret)

xts(id_benchmark_ret$ret, id_benchmark_ret$date) %>% 
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
































