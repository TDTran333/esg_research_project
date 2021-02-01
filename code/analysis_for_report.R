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

# Data on firms -----------------------------------------------------------

data_all_sp <- readRDS(here("data", "all", "monthly_data_sp.Rds"))

firms_ghg <- data_all_sp %>%
  group_by(year = year(date)) %>%
  mutate(n_obs = n(),
         n_firms = n_distinct(permno)) %>% 
  ungroup() %>% 
  filter(!is.na(ghg)) %>%
  group_by(year) %>% 
  mutate(n_obs_ghg = n(),
         n_firms_ghg_data = n_distinct(permno),
         pct_firms_ghg_data = (n_firms_ghg_data / n_firms) * 100)

firms_ghg_table <- firms_ghg %>% 
  select(year, n_obs, n_obs_ghg, n_firms, n_firms_ghg_data, pct_firms_ghg_data) %>%
  group_by(year) %>% 
  summarize(n_obs         = mean(n_obs),
            n_obs_ghg     = mean(n_obs_ghg),
            n_firms       = mean(n_firms),
            n_firms_ghg   = mean(n_firms_ghg_data),
            pct_firms_ghg = mean(pct_firms_ghg_data))

firms_ghg_table %>% 
  kable()

firms_env <- data_all_sp %>%
  group_by(year = year(date)) %>%
  mutate(n_obs = n(),
         n_firms = n_distinct(permno)) %>% 
  ungroup() %>% 
  filter(!is.na(envscore)) %>%
  group_by(year) %>% 
  mutate(n_obs_env = n(),
         n_firms_env_data = n_distinct(permno),
         pct_firms_env_data = (n_firms_env_data / n_firms) * 100)

firms_env_table <- firms_env %>% 
  select(year, n_obs, n_obs_env, n_firms, n_firms_env_data, pct_firms_env_data) %>%
  group_by(year) %>% 
  summarize(n_obs         = mean(n_obs),
            n_obs_env     = mean(n_obs_env),
            n_firms       = mean(n_firms),
            n_firms_env   = mean(n_firms_env_data),
            pct_firms_env = mean(pct_firms_env_data))

firms_env_table %>% 
  kable()


# Load Results ------------------------------------------------------------

ghg_results <- readRDS(here("output", "results", "monthly_ghg_6f_results.Rds"))
env_results <- readRDS(here("output", "results", "monthly_env_6f_results.Rds"))

ghg_results %>% glimpse()
env_results %>% glimpse()

# -------------------------------------------------------------------------

model_names <- str_to_title(gsub("_", " ", names(ghg_results)))
ghg_screening <- map2_df(map(ghg_results, "screening"), model_names, ~mutate(.x, model_name = .y))
env_screening <- map2_df(map(env_results, "screening"), model_names, ~mutate(.x, model_name = .y))

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

