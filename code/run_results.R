# For best practice, please restart R session before running script. Ctrl/Cmd + Shift + F10.

source(here::here("code", "source.R"))

# Parameters --------------------------------------------------------------

params            <- list()         
params$control    <- list(nCore = 3)           # Number of core and other controls
params$sample_per <- 2009:2018                 # Sample period
params$subDir     <- paste0(first(params$sample), "-", last(params$sample))
params$datafreq   <- "monthly"                 # Data frequency : "monthly" or "daily"       
params$window     <- 36                        # Rolling window in # of months
params$nblock     <- 20                        # Number of blocks in screenplots
params$factor     <- "six"                     # Factor model : "three" or "six"

# Formatting
st_options(round.digits = 2, style = "rmarkdown", plain.ascii = FALSE)
kable <- function(data, digits = 4) {knitr::kable(data, booktabs = TRUE, digits = digits)}

# Load Results ------------------------------------------------------------

factor_str <- if (params$factor == "six") "_6f_" else "_3f_"

ghg_results <- readRDS(here("output", "results", paste0(params$datafreq, "_ghg", factor_str, "results.Rds")))
env_results <- readRDS(here("output", "results", paste0(params$datafreq, "_env", factor_str, "results.Rds")))

model_names <- str_to_title(gsub("_", " ", names(ghg_results)))

# Plot Obs ----------------------------------------------------------------

# 
# ghg_obs <- map2_df(map(ghg_results, "n_obs"), model_names, ~mutate(.x, model_name = .y))
# 
# f_plot_obs(ghg_obs)
# ghg_missing_obs <- f_missing_obs(ghg_obs)
# ghg_missing_obs %>% unclass() %>% write.csv(file = here("output", "obs", paste0(params$datafreq, factor_str, "ghg_missing_obs.csv")))
# 
# env_obs <- map2_df(map(env_results, "n_obs"), model_names, ~mutate(.x, model_name = .y))
# f_plot_obs(env_obs)
# env_missing_obs <- f_missing_obs(env_obs)
# env_missing_obs %>% unclass() %>% write.csv(file = here("output", "obs", paste0(params$datafreq, factor_str, "env_missing_obs.csv")))

# Plot Alpha Cor ----------------------------------------------------------

# ghg_results_cor <- readRDS(here("output", "results", paste0(params$datafreq, "_ghg", factor_str, "cor.Rds")))
# ghg_alpha_cor <- map2_df(map(ghg_results_cor, "alpha_cor"), model_names, ~mutate(.x, model_name = .y))
# ghg_alpha_cor_plots <- map(model_names, ~f_plot_hist_cor(ghg_alpha_cor, .x, "ghg_alpha_cor"))
# # ghg_alpha_cor_plots
# ghg_significant_cor <- f_significant_cor(ghg_alpha_cor)
# ghg_significant_cor %>% write.csv(file = here("output", "correlation", paste0(params$datafreq, factor_str, "ghg_significant_cor.csv")))
# 
# env_results_cor <- readRDS(here("output", "results", paste0(params$datafreq, "_env", factor_str, "cor.Rds")))
# env_alpha_cor <- map2_df(map(env_results_cor, "alpha_cor"), model_names, ~mutate(.x, model_name = .y))
# env_alpha_cor_plots <- map(model_names, ~f_plot_hist_cor(env_alpha_cor, .x, "env_alpha_cor"))
# # env_alpha_cor_plots
# env_significant_cor <- f_significant_cor(env_alpha_cor)
# env_significant_cor %>% write.csv(file = here("output", "correlation", paste0(params$datafreq, factor_str, "env_significant_cor.csv")))

# Plot Ratios -------------------------------------------------------------

# ghg_ratios <- map2_df(map(ghg_results, "avg_ratios"), model_names, ~mutate(.x, model_name = .y)) %>%
#   filter(name %in% c("pipos", "pineg"))
# 
# f_plot_ratios(ghg_ratios)
# ghg_ratios_stats <- ghg_ratios %>% group_by(model_name, name) %>% descr()
# ghg_ratios_stats %>% unclass() %>% write.csv(file = here("output", "ratios", paste0(params$datafreq, factor_str, "ghg_ratios_stats.csv")))
# 
# env_ratios <- map2_df(map(env_results, "avg_ratios"), model_names, ~mutate(.x, model_name = .y)) %>%
#   filter(name %in% c("pipos", "pineg"))
# 
# f_plot_ratios(env_ratios)
# env_ratios_stats <- env_ratios %>% group_by(model_name, name) %>% descr()
# env_ratios_stats %>% unclass() %>% write.csv(file = here("output", "ratios", paste0(params$datafreq, factor_str, "env_ratios_stats.csv")))

# Portfolio Analysis ------------------------------------------------------

ghg_port <- map2_df(map(ghg_results[1:3], "port_ret"), model_names[1:3], ~mutate(.x, model_name = .y)) %>% 
  pivot_wider(., names_from = port, values_from = ret)

ghg_green_port_growth <- f_port_growth(ghg_port, "Green Bw")
ghg_brown_port_growth <- f_port_growth(ghg_port, "Brown Bw")
ghg_neutral_port_growth <- f_port_growth(ghg_port, "Neutral Bw")

f_port_growth_chart(ghg_green_port_growth, "Green", "GHG")
f_port_growth_chart(ghg_brown_port_growth, "Brown", "GHG")
f_port_growth_chart(ghg_neutral_port_growth, "Neutral", "GHG")

ghg_green_port_stats <- f_port_stats(ghg_port, "Green Bw")
ghg_brown_port_stats <- f_port_stats(ghg_port, "Brown Bw")
ghg_neutral_port_stats <- f_port_stats(ghg_port, "Neutral Bw")

env_port <- map2_df(map(env_results[1:3], "port_ret"), model_names[1:3], ~mutate(.x, model_name = .y)) %>% 
  pivot_wider(., names_from = port, values_from = ret)

env_green_port_growth <- f_port_growth(env_port, "Green Bw")
env_brown_port_growth <- f_port_growth(env_port, "Brown Bw")
env_neutral_port_growth <- f_port_growth(env_port, "Neutral Bw")

f_port_growth_chart(env_green_port_growth, "Green", "Env Score")
f_port_growth_chart(env_brown_port_growth, "Brown", "Env Score")
f_port_growth_chart(env_neutral_port_growth, "Neutral", "Env Score")

env_green_port_stats <- f_port_stats(env_port, "Green Bw")
env_brown_port_stats <- f_port_stats(env_port, "Brown Bw")
env_neutral_port_stats <- f_port_stats(env_port, "Neutral Bw")
    
ghg_port

select_port <- ghg_port %>% filter(model_name == "Green Bw")

port_xts <- xts(select_port[, -(1:3)], order.by = select_port$date)
port_xts %>% table.Stats()
port_xts %>% SharpeRatio()
port_xts %>% table.CalendarReturns()
port_xts %>% Return.cumulative()
port_xts %>% Return.annualized()
port_xts %>% StdDev.annualized()
port_xts %>% SharpeRatio.annualized()
port_xts %>% skewness()
port_xts %>% kurtosis()
port_xts %>% VaR()
port_xts %>% ETL()
port_xts %>% maxDrawdown()
port_xts_2 %>% table.Arbitrary(metrics = c("Return.cumulative", 
                                         "Return.annualized", 
                                         "StdDev.annualized",
                                         "SharpeRatio.annualized",
                                         "skewness",
                                         "kurtosis",
                                         "VaR",
                                         "ETL",
                                         "maxDrawdown"), 
                             metricsNames = c("Cumulative return", 
                                              "Annualized return",
                                              "Annualized standard deviation", 
                                              "Annualized sharpe ratio",
                                              "Monthly sknewness",
                                              "Monthly excess kurtosis",
                                              "Value at risk",
                                              "Expected shortfall",
                                              "Max drawdown"))

port_xts %>% table.Distributions()
port_xts %>% table.DownsideRiskRatio()
port_xts %>% table.DrawdownsRatio()

library(quantmod)
getSymbols("^GSPC", from = '2011-12-01', to = "2018-06-30")
sp500 <- GSPC$GSPC.Adjusted[endpoints(GSPC, "months")] %>% Return.calculate()

benchmark <- as.xts(bind_cols(sp500[-1], select_port$date)[1], order.by = select_port$date)
names(benchmark) <- "sp500"

port_xts_2 <- merge(port_xts, benchmark)

port_xts %>% table.InformationRatio(Rb = benchmark)
port_xts %>% InformationRatio(Rb = benchmark)
