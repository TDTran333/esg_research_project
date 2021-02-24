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

ghg_obs <- map2_df(map(ghg_results, "n_obs"), model_names, ~mutate(.x, model_name = .y))

f_plot_obs(ghg_obs)
ghg_missing_obs <- f_missing_obs(ghg_obs)
ghg_missing_obs %>% unclass() %>% write.csv(file = here("output", "obs", paste0(params$datafreq, factor_str, "ghg_missing_obs.csv")))

env_obs <- map2_df(map(env_results, "n_obs"), model_names, ~mutate(.x, model_name = .y))
f_plot_obs(env_obs)
env_missing_obs <- f_missing_obs(env_obs)
env_missing_obs %>% unclass() %>% write.csv(file = here("output", "obs", paste0(params$datafreq, factor_str, "env_missing_obs.csv")))

# Plot Alpha Cor ----------------------------------------------------------

ghg_results_cor <- readRDS(here("output", "results", paste0(params$datafreq, "_ghg", factor_str, "cor.Rds")))
ghg_alpha_cor <- map2_df(map(ghg_results_cor, "alpha_cor"), model_names, ~mutate(.x, model_name = .y))
ghg_alpha_cor_plots <- map(model_names, ~f_plot_hist_cor(ghg_alpha_cor, .x, "ghg_alpha_cor"))
# ghg_alpha_cor_plots
ghg_significant_cor <- f_significant_cor(ghg_alpha_cor)
ghg_significant_cor %>% write.csv(file = here("output", "correlation", paste0(params$datafreq, factor_str, "ghg_significant_cor.csv")))

env_results_cor <- readRDS(here("output", "results", paste0(params$datafreq, "_env", factor_str, "cor.Rds")))
env_alpha_cor <- map2_df(map(env_results_cor, "alpha_cor"), model_names, ~mutate(.x, model_name = .y))
env_alpha_cor_plots <- map(model_names, ~f_plot_hist_cor(env_alpha_cor, .x, "env_alpha_cor"))
# env_alpha_cor_plots
env_significant_cor <- f_significant_cor(env_alpha_cor)
env_significant_cor %>% write.csv(file = here("output", "correlation", paste0(params$datafreq, factor_str, "env_significant_cor.csv")))

# Plot Ratios -------------------------------------------------------------

ghg_ratios <- map2_df(map(ghg_results, "avg_ratios"), model_names, ~mutate(.x, model_name = .y)) %>%
  filter(name %in% c("pipos", "pineg"))

f_plot_ratios(ghg_ratios)
ghg_ratios_stats <- ghg_ratios %>% group_by(model_name, name) %>% descr()
ghg_ratios_stats %>% unclass() %>% write.csv(file = here("output", "ratios", paste0(params$datafreq, factor_str, "ghg_ratios_stats.csv")))

env_ratios <- map2_df(map(env_results, "avg_ratios"), model_names, ~mutate(.x, model_name = .y)) %>%
  filter(name %in% c("pipos", "pineg"))

f_plot_ratios(env_ratios)
env_ratios_stats <- env_ratios %>% group_by(model_name, name) %>% descr()
env_ratios_stats %>% unclass() %>% write.csv(file = here("output", "ratios", paste0(params$datafreq, factor_str, "env_ratios_stats.csv")))

# Portfolio Analysis ------------------------------------------------------

sp500_tr <- getSymbols("^SP500TR", auto.assign = getOption('getSymbols.auto.assign',FALSE)) %>% suppressWarnings()
sp1500_tr <- read_xls(here("data", "raw_data", "sp1500_total_ret.xls"), 
                      skip = 7, 
                      col_names = c("date", "sp1500"), 
                      col_types = c("date", "numeric")) %>% 
  suppressWarnings() %>%
  filter(!is.na(sp1500))

sp1500_xts <- xts(sp1500_tr$sp1500, order.by = sp1500_tr$date)

if (params$datafreq == "monthly") {
  sp500_ret <- sp500_tr$SP500TR.Adjusted[endpoints(sp500_tr, "months")] %>% Return.calculate()
  sp1500_ret <- sp1500_xts[endpoints(sp1500_xts, "months")] %>% Return.calculate()
  bm_ret <- merge(sp500_ret, sp1500_ret) %>% fortify.zoo() %>% as_tibble() %>%  
    `colnames<-`(c("date", "sp500", "sp1500")) %>% 
    mutate(date = as.yearmon(date) %>% as.Date.yearmon())
} else {
  sp500_ret <- sp500_tr$SP500TR.Adjusted %>% Return.calculate()
  sp1500_ret <- sp1500_xts %>% Return.calculate()
  bm_ret <- merge(sp500_ret, sp1500_ret) %>% fortify.zoo() %>% as_tibble() %>%  
    `colnames<-`(c("date", "sp500", "sp1500"))
}

ghg_port <- map2_df(map(ghg_results[1:3], "port_ret"), model_names[1:3], ~mutate(.x, model_name = .y)) %>% 
  pivot_wider(., names_from = port, values_from = ret)

ghg_port_all <- ghg_port %>% 
  inner_join(bm_ret, by = "date") %>%
  mutate(top_minus_bottom = top_10 - bottom_10, 
         top_minus_bottom_pct = top_10_pct - bottom_10_pct,
         top_minus_bottom_alpha = top_10_alpha - bottom_10_alpha,
         top_minus_bottom_alpha_pct = top_10_alpha_pct - bottom_10_alpha_pct) %>%
  rename(top_10_ratio         = top_10,
         bot_10_ratio         = bottom_10,
         top_10_alpha         = top_10_alpha,
         bot_10_alpha         = bottom_10_alpha,
         top_10_ratio_pct     = top_10_pct,
         bot_10_ratio_pct     = bottom_10_pct,
         top_10_alpha_pct     = top_10_alpha_pct,
         bot_10_alpha_pct     = bottom_10_alpha_pct,
         long_short_ratio     = top_minus_bottom,
         long_short_ratio_pct = top_minus_bottom_pct,
         long_short_alpha     = top_minus_bottom_alpha,
         long_short_alpha_pct = top_minus_bottom_alpha_pct,
         bm_ex_ratio          = benchmark_minus_20,
         bm_ex_ratio_pct      = benchmark_minus_20_pct,
         sp500                = sp500,
         sp1500               = sp1500
  )

ghg_green_port_growth <- f_port_growth(ghg_port_all, "Green Bw")
ghg_brown_port_growth <- f_port_growth(ghg_port_all, "Brown Bw")
ghg_neutral_port_growth <- f_port_growth(ghg_port_all, "Neutral Bw")

f_port_growth_chart(ghg_green_port_growth, "Green", "GHG")
f_port_growth_chart(ghg_brown_port_growth, "Brown", "GHG")
f_port_growth_chart(ghg_neutral_port_growth, "Neutral", "GHG")

ghg_green_port_stats <- f_port_stats(ghg_port_all, "Green Bw")
ghg_brown_port_stats <- f_port_stats(ghg_port_all, "Brown Bw")
ghg_neutral_port_stats <- f_port_stats(ghg_port_all, "Neutral Bw")

env_port <- map2_df(map(env_results[1:3], "port_ret"), model_names[1:3], ~mutate(.x, model_name = .y)) %>% 
  pivot_wider(., names_from = port, values_from = ret)

env_port_all <- env_port %>% 
  inner_join(bm_ret, by = "date") %>%
  mutate(top_minus_bottom = top_10 - bottom_10, 
         top_minus_bottom_pct = top_10_pct - bottom_10_pct,
         top_minus_bottom_alpha = top_10_alpha - bottom_10_alpha,
         top_minus_bottom_alpha_pct = top_10_alpha_pct - bottom_10_alpha_pct) %>%
  rename(top_10_ratio         = top_10,
         bot_10_ratio         = bottom_10,
         top_10_alpha         = top_10_alpha,
         bot_10_alpha         = bottom_10_alpha,
         top_10_ratio_pct     = top_10_pct,
         bot_10_ratio_pct     = bottom_10_pct,
         top_10_alpha_pct     = top_10_alpha_pct,
         bot_10_alpha_pct     = bottom_10_alpha_pct,
         long_short_ratio     = top_minus_bottom,
         long_short_ratio_pct = top_minus_bottom_pct,
         long_short_alpha     = top_minus_bottom_alpha,
         long_short_alpha_pct = top_minus_bottom_alpha_pct,
         bm_ex_ratio          = benchmark_minus_20,
         bm_ex_ratio_pct      = benchmark_minus_20_pct,
         sp500                = sp500,
         sp1500               = sp1500
  )

env_green_port_growth <- f_port_growth(env_port_all, "Green Bw")
env_brown_port_growth <- f_port_growth(env_port_all, "Brown Bw")
env_neutral_port_growth <- f_port_growth(env_port_all, "Neutral Bw")

f_port_growth_chart(env_green_port_growth, "Green", "env")
f_port_growth_chart(env_brown_port_growth, "Brown", "env")
f_port_growth_chart(env_neutral_port_growth, "Neutral", "env")

env_green_port_stats <- f_port_stats(env_port_all, "Green Bw")
env_brown_port_stats <- f_port_stats(env_port_all, "Brown Bw")
env_neutral_port_stats <- f_port_stats(env_port_all, "Neutral Bw")
