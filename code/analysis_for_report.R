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

factor_str <- if (params$factor == "six") "_6f_" else "_3f_"

# Load Data ---------------------------------------------------------------

sp_df <- readRDS(here("data", params$subDir, paste0(params$datafreq, "_data_sp.rds"))) %>% 
  mutate(spmim = as.character(spmim))
factor_df <- readRDS(here("data", params$subDir, paste0(params$datafreq, "_factor.rds")))

# Data on firms -----------------------------------------------------------

data_all_sp <- readRDS(here("data", "all", "monthly_data_sp.Rds")) %>% 
  mutate(spmim = as.character(spmim))

mean_rm_na <- partial(mean, na.rm = TRUE)

esg_firms_table <- function(.df, .esg_var) {
  .df %>% 
    group_by(year = year(date), spmim) %>%
    mutate(n_obs_all = n(),
           n_firms_all = n_distinct(permno)) %>% 
    filter(!is.na(.data[[.esg_var]])) %>%
    mutate(n_obs_esg = n(),
           n_firms_esg = n_distinct(permno),
           pct_firms_esg = (n_firms_esg / n_firms_all) * 100) %>%
    summarize(n_obs_all     = mean_rm_na(n_obs_all),
              n_obs_ehg     = mean_rm_na(n_obs_esg),
              n_firms_all   = mean_rm_na(n_firms_all),
              n_firms_esg   = mean_rm_na(n_firms_esg),
              pct_firms_esg = mean_rm_na(pct_firms_esg)) %>%
    ungroup()
}

firms_ghg_all <- data_all_sp %>% mutate(spmim = "all") %>% esg_firms_table("ghg")
firms_ghg <- data_all_sp %>% esg_firms_table("ghg")

firms_ghg_table <- firms_ghg %>% 
  bind_rows(firms_ghg_all) %>% 
  select(year, spmim, pct_firms_esg) %>% 
  pivot_wider(names_from = spmim, values_from = pct_firms_esg) %>%
  filter(year >= 2009)

firms_ghg_table %>% 
  write.csv(file = here("output", "firms", paste0(params$datafreq, "_ghg_pct_firms_sp.csv")))

firms_env_all <- data_all_sp %>% mutate(spmim = "all") %>% esg_firms_table("envscore")
firms_env <- data_all_sp %>% esg_firms_table("envscore")
  
firms_env_table <- firms_env %>% 
  bind_rows(firms_env_all) %>% 
    select(year, spmim, pct_firms_esg) %>% 
    pivot_wider(names_from = spmim, values_from = pct_firms_esg) %>%
    filter(year >= 2009)

firms_env_table %>% 
  write.csv(file = here("output", "firms", paste0(params$datafreq, "_env_pct_firms_sp.csv")))

# Descriptive Stats on ESG Variable ---------------------------------------

esg_all <- sp_df %>% mutate(spmim = "all")

summary_stats <- sp_df %>% 
  bind_rows(esg_all) %>%
  group_by(year = year(date), permno, spmim) %>%
  summarize(ghg_yr = mean_rm_na(ghg),
            envscore_yr = mean_rm_na(envscore)) %>%
  ungroup() %>% 
  select(spmim, ghg_yr, envscore_yr) %>% 
  group_by(spmim) %>% 
  descr()

summary_stats

summary_stats %>% 
  unclass() %>% write.csv(file = here("output", "firms", paste0(params$datafreq, "_summary_stats.csv")))

# Number of firms by ESG Type ---------------------------------------------
# See run_testing_fun.R section 1 to section 3 

# Load Results ------------------------------------------------------------

ghg_results <- readRDS(here("output", "results", "monthly_ghg_6f_results.Rds"))
env_results <- readRDS(here("output", "results", "monthly_env_6f_results.Rds"))

ghg_results %>% glimpse()
env_results %>% glimpse()

# -------------------------------------------------------------------------

model_names <- str_to_title(gsub("_", " ", names(ghg_results)))
ghg_ratios <- map2_df(map(ghg_results, "avg_ratios"), model_names, ~mutate(.x, model_name = .y))
env_ratios <- map2_df(map(env_results, "avg_ratios"), model_names, ~mutate(.x, model_name = .y))

ghg_ratios_filtered <- ghg_ratios %>%
  filter(model_name %in% c("Green Bw", "Green Fw", "Brown Bw", "Brown Fw"),
         name %in% c("pipos", "pineg"))

ghg_ratios_filtered %>%
  ggplot(aes(date, value, color = name)) +
  geom_line(size = 1.5) +
  facet_wrap(~ model_name, ncol = 2, dir = "h", scale = "free_x") +
  labs(x = "Date",
       y = "Percent",
       color = "Ratios",
       title = "Evolution of performance ratios over time.") +
  scale_y_continuous(labels = percent) +
  scale_x_yearqtr(format = "%Y-Q%q") +
  geom_smooth(method = "lm", se = FALSE)

ghg_ratios_stats <- ghg_ratios_filtered %>% 
  pivot_wider(names_from = c(name, model_name), values_from = value) %>%
  descr()
  
ghg_ratios_stats%>% 
  unclass() %>% write.csv(file = here("output", "ratios", paste0(params$datafreq, "_ghg_ratios_stats.csv")))

env_ratios_filtered <- env_ratios %>%
  filter(model_name %in% c("Green Bw", "Green Fw", "Brown Bw", "Brown Fw"),
         name %in% c("pipos", "pineg"))

env_ratios_filtered %>%
  ggplot(aes(date, value, color = name)) +
  geom_line(size = 1.5) +
  facet_wrap(~ model_name, ncol = 2, dir = "h", scale = "free_x") +
  labs(x = "Date",
       y = "Percent",
       color = "Ratios",
       title = "Evolution of performance ratios over time.") +
  scale_y_continuous(labels = percent) +
  scale_x_yearqtr(format = "%Y-Q%q") +
  geom_smooth(method = "lm", se = FALSE)

env_ratios_stats <- env_ratios_filtered %>% 
  pivot_wider(names_from = c(name, model_name), values_from = value) %>%
  descr()

env_ratios_stats%>% 
  unclass() %>% write.csv(file = here("output", "ratios", paste0(params$datafreq, "_env_ratios_stats.csv")))









