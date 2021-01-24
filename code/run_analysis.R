# For best practice, please restart R session before running script. Ctrl/Cmd + Shift + F10.

source(here::here("code", "source.R"))

# Parameters --------------------------------------------------------------

params            <- list()         
params$control    <- list(nCore = 3)           # Number of core and other controls
params$sample_per <- 2009:2017                 # Sample period
params$subDir     <- paste0(first(params$sample), "-", last(params$sample))
params$datafreq   <- "daily"                 # Data frequency : "monthly" or "daily"       
params$window     <- 36                        # Rolling window in # of months
params$nblock     <- 20                        # Number of blocks in screenplots
params$factor     <- "six"                     # Factor model : "three" or "six"

# Formatting
st_options(round.digits = 2, style = "rmarkdown", plain.ascii = FALSE)
kable <- function(data) {knitr::kable(data, booktabs = TRUE, digits = 2)}

# Load Data ---------------------------------------------------------------

sp_df <- readRDS(here("data", params$subDir, paste0(params$datafreq, "_data_sp.rds")))
factor_df <- readRDS(here("data", params$subDir, paste0(params$datafreq, "_factor.rds")))

# Create ESG Groups -------------------------------------------------------

create_esg <- partial(f_create_esg_type, .datafreq = params$datafreq, .window = params$window)

ghg_df <- sp_df %>% create_esg(.esg_var = "ghg")
env_df <- sp_df %>% create_esg(.esg_var = "envscore")

# Transition Matrix -------------------------------------------------------
# Moved over to run_testing_funs

# Create id ---------------------------------------------------------------

ghg_id <- env_id <- list()
esg_group <- c("green", "brown", "neutral")
ghg_id[esg_group] <- map(c("G", "B", "N"), ~f_create_id(ghg_df, .x))
env_id[esg_group] <- map(c("G", "B", "N"), ~f_create_id(env_df, .x))

# Main function -----------------------------------------------------------

f_process_model <- function(.df, .factor_df, .factor_model, .id, .id_name, .date, .model,
                            .datafreq, .run_cor = FALSE, .method, .window, .nblock, 
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
  tidy_screening <- f_tidy_screening(tmp3, dates$id_date)
  tidy_port_ret  <- if (isTRUE(.run_port)) {tmp5 %>% map_dfr(~bind_rows(.x))}
  
  tidy_results <- list(dates$id_date, tidy_n_obs, tidy_screening) %>%
    `names<-`(c("date", "n_obs", "screening"))

  if (isTRUE(.run_cor)) {
    tidy_results   <- append(tidy_results, list(tmp2) %>% `names<-`(c("alpha_cor")))
  }
  
  if (isTRUE(.run_port)) {
    tidy_results   <- append(tidy_results, list(tidy_port_ret) %>% `names<-`(c("port_ret")))
  }
  
  return(tidy_results)
}

date_input <- ghg_df %>% select(date) %>% distinct() %>% pluck()

f_run_model <- partial(f_process_model,
                       .df           = sp_df,
                       .factor       = if (params$factor == "six") {factor_df} else {factor_df[, 1:4]},
                       .factor_model = params$factor,
                       .date         = date_input,
                       .datafreq     = params$datafreq,
                       .method       = "pearson",
                       .window       = params$window, 
                       .nblock       = params$nblock, 
                       .control      = params$control)

ghg_results <- env_results <- list()
ghg_results[paste0(esg_group, "_bw")] <- map(esg_group, ~f_run_model(.id = ghg_id[[.x]], 
                                                                      .id_name = paste0("ghg_id['", .x, "']"), 
                                                                      .model = "bw",
                                                                      .run_cor = FALSE))

ghg_results[paste0(esg_group, "_fw")] <- map(esg_group, ~f_run_model(.id = ghg_id[[.x]], 
                                                                     .id_name = paste0("ghg_id['", .x, "']"), 
                                                                     .model = "fw",
                                                                     .run_port = FALSE))

env_results[paste0(esg_group, "_bw")] <- map(esg_group, ~f_run_model(.id = env_id[[.x]], 
                                                                     .id_name = paste0("env_id['", .x, "']"), 
                                                                     .model = "bw"))

env_results[paste0(esg_group, "_fw")] <- map(esg_group, ~f_run_model(.id = env_id[[.x]], 
                                                                     .id_name = paste0("env_id['", .x, "']"), 
                                                                     .model = "fw",
                                                                     .run_port = FALSE))

glimpse(ghg_results)
glimpse(env_results)

# -------------------------------------------------------------------------

# Note: Splitted out the correlation function because of file size.

# f_run_cor <- function(.df, .factor_df, .factor_model, .id, .id_name, .date, .model,
#                             .datafreq, .method, .window, .verbose = TRUE) {
#   
#   .factor_model = match.arg(.factor_model, c("three", "six"))
#   .model        = match.arg(.model, c("bw", "fw"))
#   .datafreq     = match.arg(.datafreq, c("monthly", "daily"))
#   .method       = match.arg(.method, c("pearson", "kendall", "spearman"))
#   
#   # Create Dates
#   dates <- f_create_dates(.date, .window, .model, .datafreq)
#   
#   model_name <- paste(.model, .window, "m_window", .factor_model, 
#                       "factor_model", .datafreq, "data", .id_name, sep = "_")
#   
#   if (isTRUE(.verbose)) {message(paste0("Model: ", model_name, "."))}
#   
#   tmp <- list()
#   
#   for (i in seq_along(dates$id_date)) {
#     
#     if (isTRUE(.verbose)) {
#       message(paste("Analyzing", dates$id_date[i]), ". ",
#               "Completed: ", i, " out of ", length(dates$id_date), ".")
#     }
#     
#     # Alpha Screen
#     ret_mat <- f_ret_mat(.df, .id, dates$id_date[i], dates$start_date[i], dates$end_date[i])
#     factor_mat <- f_factor_mat(.factor_df, dates$start_date[i], dates$end_date[i])
#     alpha_cor <- f_alpha_cor(ret_mat, factor_mat, dates$id_date[i], .method = .method)
# 
#     # Outputs
#     tmp <- append(tmp, list(alpha_cor))
#   }
#   
#   if (isTRUE(.verbose)) {message("Completed!")}
#   
#   names(tmp) <- dates$id_date
#   out <- list(tmp) %>% `names<-`(c("alpha_cor"))
#   
#   return(out)
# }
# 
# date_input <- ghg_df %>% select(date) %>% distinct() %>% pluck()
# 
# run_cor <- partial(f_run_cor,
#                    .df           = sp_df,
#                    .factor       = if (params$factor == "six") {factor_df} else {factor_df[, 1:4]},
#                    .factor_model = params$factor,
#                    .date         = date_input,
#                    .datafreq     = params$datafreq,
#                    .method       = "pearson",
#                    .window       = params$window)
# 
# ghg_results_cor <- env_results_cor <- list()
# ghg_results_cor[paste0(esg_group, "_bw")] <- map(esg_group, ~run_cor(.id = ghg_id[[.x]], 
#                                                                      .id_name = paste0("ghg_id['", .x, "']"), 
#                                                                      .model = "bw"                                                                      ))
# 
# ghg_results_cor[paste0(esg_group, "_fw")] <- map(esg_group, ~run_cor(.id = ghg_id[[.x]], 
#                                                                      .id_name = paste0("ghg_id['", .x, "']"), 
#                                                                      .model = "fw"))
# 
# env_results_cor[paste0(esg_group, "_bw")] <- map(esg_group, ~run_cor(.id = env_id[[.x]], 
#                                                                      .id_name = paste0("env_id['", .x, "']"), 
#                                                                      .model = "bw"))
# 
# env_results_cor[paste0(esg_group, "_fw")] <- map(esg_group, ~run_cor(.id = env_id[[.x]], 
#                                                                      .id_name = paste0("env_id['", .x, "']"), 
#                                                                      .model = "fw"))
# 
# glimpse(ghg_results_cor)
# glimpse(env_results_cor)
