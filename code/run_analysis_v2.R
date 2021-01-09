# For best practice, please restart R session before running script. Ctrl/Cmd + Shift + F10.

source(here::here("code", "source.R"))

# Parameters --------------------------------------------------------------

params          <- list()         
params$control  <- list(nCore = 3)           # Number of core and other controls
params$date     <- c(2009, 2017)             # Sample period
params$subDir   <- paste(params$date[1], params$date[2], sep = "-")
params$datafreq <- "monthly"                   # Data frequency : "monthly" or "daily"       
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

f_main_funs <- function(.df, .factor_df, .factor_model, .id, .date, .model,
                   .datafreq, .method, .window, .bucket, .control) {
  
  .factor_model = match.arg(.factor_model, c("three", "six"))
  .model        = match.arg(.model, c("bw", "fw"))
  .datafreq     = match.arg(.datafreq, c("monthly", "daily"))
  .method       = match.arg(.method, c("pearson", "kendall", "spearman"))
  
  # Create Dates
  dates <- f_create_dates(.date, .window, .model, .datafreq)

  out1 <- out2 <- out3 <- list()

  for (i in seq_along(dates$id_date)) {
    print(dates$id_date[i])

    # Alpha screen
    ret_mat <- f_ret_mat(.df, .id, dates$id_date[i], dates$start_date[i], dates$end_date[i])
    factor_mat <- f_factor_mat(.factor_df, dates$start_date[i], dates$end_date[i])
    alpha_screen <- PeerPerformance::alphaScreening(ret_mat, factors = factor_mat, control = .control)

    # Alpha correlations
    alpha_cor <- f_alpha_cor(ret_mat, factor_mat, dates$id_date[i], .method = .method)
    
    # Table for screening plot
    tbl_screening <- f_tbl_screening(alpha_screen, .bucket)
    
    # Screening plot figures
    folder_name <- paste(.model, .window, "m_window", .factor_model, "factor_model", .datafreq, "data", sep = "_")
    fig_title <- toupper(paste(folder_name, deparse(substitute(.id)), dates$id_date[i], sep = "_"))
    f_screenplot(tbl_screening, fig_title, folder_name, .datafreq)
    
    out1 <- append(out1, list(alpha_screen))
    out2 <- append(out2, list(alpha_cor))
    out3 <- append(out3, list(tbl_screening))
    
  }
  
  
  
  names(out1) <- names(out2) <- names(out3) <- dates$id_date
  
  print("completed!")
  
  tibble(out1, out2, out3) %>% `colnames<-`(c("alpha_screen", "alpha_cor", "tbl_screening"))
}

results <- f_main_funs(sp_df, 
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







f_main_plot <- function(.result, .factor_model, .id, .id_date, .model, .datafreq, .window) {
  
  for (i in seq_along(.id_date)) {
      folder <- paste(.model, .window, "m_window", .factor_model, "factor_model", .datafreq, "data", sep = "_")
    fig_title <- toTitleCase(paste(folder_name, deparse(substitute(.id)), id_date[i], sep = "_"))
    f_screenplot(.result$tbl_screening[i], fig_title, folder, .datafreq)
  }
  
  
  f_plot_mean_pi()
  
}




f_main_port <- function() {
  
  
}



results$tbl_screening %>%
  map(mean_pi) %>%
  transpose() %>%
  do.call(rbind, .) %>% 
  unlist() %>% 
  matrix(byrow=T, ncol = 2) %>% 
  `colnames<-`(c("pipos", "pineg")) %>%
  as_tibble() %>% 
  mutate(date = names(results$tbl_screening)) %>%
  mutate(model = "model_name") %>% 
  pivot_longer(-c(date, model))

tidy_pi_result <- f_tidy_pi_result(results$tbl_screening, names(results$tbl_screening), "model_name")





tidy_pi_result %>% 
  mutate(date = as.yearqtr(date)) %>%
  filter(name == "pipos") %>% 
ggplot(aes(date, value)) +
  geom_line() +
  scale_x_yearqtr() +
  labs(title = paste0("Mean Out/Under-Performance Ratios using ",params$window, "-months rolling window."),
       subtitle = paste0("Backward-looking and forward-looking for Green and Brown firms using ", "fctr_flag"),
       x = "Date",
       y = "Percent") +
  scale_y_continuous(labels = percent)












  results$alpha_screen[1]

f_tidy_n_obs <- function(.alphascreen, .id_date) {
  .alphascreen %>%
    map("n") %>% 
    bind_rows() %>%
    mutate(date = .id_date) %>% 
    pivot_longer(-date, names_to = "permno", values_to = "obs") 
}

tidy_n_obs <- f_tidy_n_obs(results$alpha_screen, names(results$alpha_screen))

results$alpha_screen %>%
  map("n") %>% 
  bind_rows() %>%
  mutate(date = names(results$tbl_screening)) %>% 
  pivot_longer(-date, names_to = "permno", values_to = "obs") 

f_obs_hist <- function(.tidy_n_obs, .model_name){
  .tidy_n_obs %>% 
    filter(!is.na(obs)) %>%
    ggplot(aes(obs)) +
    geom_histogram(bins = 30) +
    facet_wrap(~date) +
    labs(title = paste0("Concordant observations for ", .model_name))
}

f_obs_hist(tidy_n_obs, "model_name")


f_tidy_alpha_cor <- function(.alpha_cor) {
  .alpha_cor %>%
    bind_rows() %>% 
    mutate(date = factor(date)) %>%
    select(date, correlation) %>% 
    pivot_longer(-date) %>% 
    filter(!is.na(value))
}

tidy_alpha_cor <- f_tidy_alpha_cor(results$alpha_cor)

results$alpha_cor %>%
  bind_rows() %>% 
  mutate(date = factor(date)) %>%
  select(date, correlation) %>% 
  pivot_longer(-date) %>% 
  filter(!is.na(value))
  
  
f_alpha_cor_hist <- function(.tidy_alpha_cor, .model_name)  {
  .tidy_alpha_cor %>% 
    ggplot(aes(value)) +
    geom_histogram(bins = 30) +
    facet_wrap(~date) +
    geom_vline(xintercept = -0.3, lty = "dashed") +
    geom_vline(xintercept = 0.3, lty = "dashed") + 
    labs(title = paste0("Correlations of alphas for ", .model_name))
}

f_alpha_cor_hist(tidy_alpha_cor, "model_name")






f_plot_hist_cor <- function(df, name) {
  {{ df }} %>% 
    bind_rows() %>% 
    mutate(date = as.Date.yearqtr(date)) %>%
    select(date, correlation) %>% 
    pivot_longer(-date) %>% 
    filter(!is.na(value)) %>%
    ggplot(aes(value)) +
    geom_histogram(bins = 30) +
    facet_wrap(~date) +
    labs(title = paste0("Correlations of alphas for ", {{ name }}))
}












date_zoo <- ghg_df %>% select(date) %>% distinct() %>% pluck()

dates_bw <- date_zoo %>% f_create_dates(.model = "bw", params$window, params$datafreq)

ret_mat2 <- sp_df %>%
  filter(permno %in% ghg_id$green$`2011 Q4`) %>%
  filter(date >= "2009-01-01",
         date <= "2011-12-01") %>%
  select(date, permno, ret_rf) %>%
  pivot_wider(names_from = permno, values_from = ret_rf) %>%
  janitor::remove_empty(which = "cols") %>%
  select(-date) %>%
  as.matrix()


  f_ret_mat(sp_df, ghg_id$green, "2011 q4", "2009-01-01", "2011-12-01")
factor_mat2 <- f_factor_mat(factor_df, "2009-01-01", "2011-12-01")


alpha_screen <- PeerPerformance::alphaScreening(ret_mat2, factors = factor_mat2, control = params$control)

test <- f_tbl_screening(alpha_screen, 5)

attr(test)
f_screenplot(test, 0, 0, "monthly")























