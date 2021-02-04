# For best practice, please restart R session before running script. Ctrl/Cmd + Shift + F10.

source(here::here("code", "source.R"))

# Parameters --------------------------------------------------------------

params            <- list()         
params$control    <- list(nCore = 3)           # Number of core and other controls
params$sample_per <- 2009:2017                 # Sample period
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
firms_env <- data_all_sp %>% mutate(spmim = as.character(spmim)) %>% esg_firms_table("envscore")
  
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

summary_stats %>% unclass() %>% write.csv(file = here("output", "firms", paste0(params$datafreq, "_summary_stats.csv")))

# Load Results ------------------------------------------------------------

ghg_results <- readRDS(here("output", "results", "monthly_ghg_6f_results.Rds"))
env_results <- readRDS(here("output", "results", "monthly_env_6f_results.Rds"))

ghg_results %>% glimpse()
env_results %>% glimpse()

# -------------------------------------------------------------------------

model_names <- str_to_title(gsub("_", " ", names(ghg_results)))
ghg_screening <- map2_df(map(ghg_results, "avg_ratios"), model_names, ~mutate(.x, model_name = .y))
env_screening <- map2_df(map(env_results, "avg_ratios"), model_names, ~mutate(.x, model_name = .y))

ghg_screening %>% 
  ggplot(aes(date, value, color = name)) +
  geom_line(size = 1.5) +
  facet_wrap(~ model_name, ncol = 2, dir = "h", scale = "free_x") +
  labs(x = "Date",
       y = "Percent",
       color = "Ratios") +
  scale_y_continuous(labels = percent) +
  scale_x_yearqtr(format = "%Y-Q%q")

ghg_bw <- ghg_screening %>% 
  filter(model_name %in% c("Brown Bw", "Green Bw")) %>%
  pivot_wider(names_from = c(name, model_name), values_from = value)

ghg_bw %>%
  clean_names() %>% 
  filter(pipos_green_bw < 0.05) %>% 
  kable()

# 2015-Q1 to 2017-Q2 are problematic?

# -------------------------------------------------------------------------
sp_df <- readRDS(here("data", params$subDir, paste0(params$datafreq, "_data_sp.rds")))
factor_df <- readRDS(here("data", params$subDir, paste0(params$datafreq, "_factor.rds")))

create_esg <- partial(f_create_esg_type, .datafreq = params$datafreq, .window = params$window)
ghg_df <- sp_df %>% create_esg(.esg_var = "ghg")
env_df <- sp_df %>% create_esg(.esg_var = "env")

ghg_id <- env_id <-  list()
esg_group <- c("green", "brown")
ghg_id[esg_group] <- map(c("G", "B"), ~f_create_id(ghg_df, .x))
env_id[esg_group] <- map(c("G", "B"), ~f_create_id(env_df, .x))

f_process_model <- function(.df, .factor_df, .factor_model, .id, .id_name, .date, .model,
                            .datafreq, .window, .nblock, .control, .verbose = TRUE) {
  
  .factor_model = match.arg(.factor_model, c("three", "six"))
  .model        = match.arg(.model, c("bw", "fw"))
  .datafreq     = match.arg(.datafreq, c("monthly", "daily"))

  # Create Dates
  dates <- f_create_dates(.date, .window, .model, .datafreq)
  
  model_name <- paste(.model, .window, "m_window", .factor_model, 
                      "factor_model", .datafreq, "data", .id_name, sep = "_")
  
  if (isTRUE(.verbose)) {message(paste0("Model: ", model_name, "."))}
  
  tmp1 <- tmp2 <- list()
  
  for (i in seq_along(dates$id_date)) {
    
    if (isTRUE(.verbose)) {
      message(paste("Analyzing", dates$id_date[i]), ". ",
              "Completed: ", i, " out of ", length(dates$id_date), ".")
    }
    
    # Alpha Screen
    ret_mat <- f_ret_mat(.df, .id, dates$id_date[i], dates$start_date[i], dates$end_date[i])
    factor_mat <- f_factor_mat(.factor_df, dates$start_date[i], dates$end_date[i])
    alpha_screen <- PeerPerformance::alphaScreening(ret_mat, factors = factor_mat, control = .control)

    # Table for Screening Plot
    tbl_screening <- f_tbl_screening(alpha_screen, .nblock)
    avg_ratios <- f_avg_ratios(alpha_screen, dates$id_date[i])
    
    # Outputs
    tmp1 <- append(tmp1, list(alpha_screen))
    tmp2 <- append(tmp2, list(avg_ratios))
  }
  
  if (isTRUE(.verbose)) {message("Completed!")}
  
  names(tmp1) <- names(tmp2) <- dates$id_date
  binded_ratios <- tmp2 %>% map_dfr(~bind_rows(.x)) %>% pivot_longer(-date)
    
  results <- list(dates$id_date, tmp1, tmp2, binded_ratios) %>%
    `names<-`(c("date", "alpha_screening", "ratios_raw", "avg_ratios"))
  
  return(results)
}

date_input <- ghg_df %>% select(date) %>% distinct() %>% pluck()

f_run_model <- partial(f_process_model,
                       .df           = sp_df,
                       .factor       = if (params$factor == "six") {factor_df} else {factor_df[, 1:4]},
                       .factor_model = params$factor,
                       .date         = date_input[1:15, ],
                       .datafreq     = params$datafreq,
                       .window       = params$window, 
                       .nblock       = params$nblock, 
                       .control      = params$control)

ghg_results2 <- env_results2 <-  list()
ghg_results2[paste0(esg_group, "_bw")] <- map(esg_group, ~f_run_model(.id = ghg_id[[.x]], 
                                                                     .id_name = paste0("ghg_id['", .x, "']"), 
                                                                     .model = "bw"))

env_results2[paste0(esg_group, "_bw")] <- map(esg_group, ~f_run_model(.id = env_id[[.x]], 
                                                                      .id_name = paste0("env_id['", .x, "']"), 
                                                                      .model = "bw"))

ghg_results %>% glimpse()

green_bw_pipos <- map(ghg_results$green_bw$alpha_screening, "pipos")

ghg_results2$green_bw$ratios_raw

ghg_results2$green_bw$screening_raw$`2012 Q2` %>%
  summarize(mean(pipos, na.rm = TRUE),
            mean(pineg, na.rm = TRUE))

ghg_results2$green_bw$screening


ghg_results$green_bw$alpha_screening$`2011 Q4`$pineg %>% 
  mean()

ghg_results2$green_bw$screening_raw$`2011 Q4`


.df <- ghg_results$green_bw$alpha_screening$`2011 Q4` %>%
  as_tibble() %>%
  select(alpha, pizero, pipos, pineg) %>% 
  drop_na()

.df %>% 
  summarise(across(everything(), list(mean)))

.df %>% 
  arrange(desc(alpha))

tbl %>% 
  summarise(across(everything(), list(mean)))



pos    <- order(.df$alpha, decreasing = TRUE)
n      <- nrow(.df)
idx  <- split(pos, ceiling(seq_along(pos)/(n/params$nblock)))
tbl    <- matrix(data = NA, nrow = .nblock, ncol = 4)

for (i in 1:.nblock) {
  tbl[i, 1]     <- mean(.df$alpha[idx[[i]]])
  tbl[i, 2:4]   <- colMeans(.df[idx[[i]], c("pipos", "pizero", "pineg")])
  colnames(tbl) <- c("alpha", "pipos", "pizero", "pineg")
}

tbl <- tbl %>% as_tibble()


for (i in 1:.nblock) {
  idx <- pos[((i - 1) * ngroup + 1):(i * ngroup)]
  if (i == .nblock) {
    idx <- pos[((i - 1) * ngroup + 1):n]
  }
  
  idx           <- idx[!is.na(idx)]
  tbl[i, 1]     <- mean(.df$alpha[idx])
  tbl[i, 2:4]   <- colMeans(.df[idx, c("pipos", "pizero", "pineg")])
  colnames(tbl) <- c("alpha", "pipos", "pizero", "pineg")
}




length(ghg_results$green_bw$alpha_screening$`2011 Q4`$n)

ghg_results$green_bw$screening


mean_pi <- function(.tbl_screening) {
  .tbl_screening %>%
    summarize(mean_pipos = mean(pipos, na.rm = TRUE),
              mean_pineg = mean(pineg, na.rm = TRUE))
}

ghg_results2$green_bw$screening_raw %>%
  map(mean_pi) %>%
  transpose() %>%
  map(rbind) %>% 
  unlist() %>% 
  matrix(byrow=T, ncol = 2) %>% 
  `colnames<-`(c("pipos", "pineg")) %>%
  as_tibble() %>% 
  mutate(date = "test") %>%
  pivot_longer(-date)


ghg_results2$green_bw$screening_raw %>%
  map(mean_pi) %>%
  transpose() %>%
  bind_rows() %>% 
  t() %>% 
  as_tibble() %>% 
  rename(pipos = V1,
         pineg = V2) %>% 
  mutate(date = ghg_results2$green_bw$date) %>% 
  pivot_longer(-date)













# -------------------------------------------------------------------------

f_extract_ratios <- function(.df_screening, ratio_type) {
  
  df_ratio <- map(.df_screening, ratio_type)
  dates <- as.yearqtr(names(.df_screening))
  
  count <- df_ratio %>% 
    map_df(~sum(.x > 0, na.rm = TRUE)) %>% 
    t() %>% 
    as_tibble() %>% 
    rename(nb_pipos = V1) %>% 
    mutate(date = dates)
  
  avg <- df_ratio %>% 
    map_df(~mean(.x, na.rm = TRUE)) %>% 
    t() %>% 
    as_tibble() %>% 
    rename(avg_pipos = V1) %>% 
    mutate(date = dates)
  
  out <- count %>% 
    inner_join(avg, by = "date") %>% 
    select(date, everything()) %>% 
    mutate(type = ratio_type)
  
  return(out)
}

f_extract_ratios(ghg_results$brown_bw$alpha_screening, "pipos")

ratios <- c("pipos", "pineg", "pizero")

green_bw_ratios <- brown_bw_ratios <- list()
green_bw_ratios[ratios] <- map(ratios, ~f_extract_ratios(ghg_results$green_bw$alpha_screening, .x))
green_bw_ratios_tidy <- green_bw_ratios %>% bind_rows()

green_bw_ratios_tidy %>% 
  ggplot(aes(date, nb_pipos, fill = type)) +
  geom_col() +
  scale_x_yearqtr(format = "%Y-Q%q")

green_bw_ratios_tidy %>% 
  ggplot(aes(date, avg_pipos, fill = type)) +
  geom_col() +
  scale_x_yearqtr(format = "%Y-Q%q")

brown_bw_ratios[ratios] <- map(ratios, ~f_extract_ratios(ghg_results$brown_bw$alpha_screening, .x))
brown_bw_ratios_tidy <- brown_bw_ratios %>% bind_rows()

brown_bw_ratios_tidy %>% 
  ggplot(aes(date, nb_pipos, fill = type)) +
  geom_col() +
  scale_x_yearqtr(format = "%Y-Q%q")

brown_bw_ratios_tidy %>% 
  ggplot(aes(date, avg_pipos, fill = type)) +
  geom_col() +
  scale_x_yearqtr(format = "%Y-Q%q")

ghg_screening
green_bw_ratios_tidy

map(map(env_results2$green_bw$alpha_screening, "n"), ~length(.x))

