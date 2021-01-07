# For best practice, please restart R session before running script. Ctrl/Cmd + Shift + F10.


# Load Libraries ----------------------------------------------------------

shhh <- suppressPackageStartupMessages # It's a library, so shhh!

shhh(require(here))             # Relative paths
shhh(require(tidyverse))        # Data manipulation
shhh(require(lubridate))        # Date management
shhh(require(purrr))            # Functional programming
shhh(require(janitor))          # Data cleaning
shhh(require(summarytools))     # Summary stats
shhh(require(markovchain))      # Transition matrix
shhh(require(PeerPerformance))  # Alpha screening
shhh(require(scales))           # For ggplot2
shhh(require(zoo))              # For rolling window
shhh(require(stringr))          # String manipulation
shhh(require(widyr))            # Pairwise correlation

# Parameters --------------------------------------------------------------

params          <- list()         
params$control  <- list(nCore = 3)           # Number of core and other controls
params$date     <- c(2009, 2017)             # Sample period
params$subDir   <- paste(params$date[1], params$date[2], sep = "-")
params$datafreq <- "dly"                   # Data frequency : "mthly" or "dly"       
params$window   <- 36                        # Rolling window in # of months
params$bucket   <- 5                         # Number of firms per bucket
params$factor   <- c("three","six")          # Three-factor or Six-Factor model

# Load Data ---------------------------------------------------------------

sp_df <- readRDS(here("data", params$subDir, paste0(params$datafreq, "_data_sp.rds")))
factor_df <- readRDS(here("data", params$subDir, paste0(params$datafreq, "_factor.rds")))

# Generate groups ---------------------------------------------------------
mean_na_rm <- partial(mean, na.rm = TRUE)
quantile_na_rm <- partial(quantile, na.rm = TRUE)

# 1. If the data is monthly, we use the quarterly mean of ghg, 
#    otherwise we use monthly mean of ghg for daily data
# 2. We compute rolling window of ghg with window step = quarter / month
# 3. We compute the q1 and q3 quantile for each rolling window
# 4. We define the groups as Undisclosed, Green, Brown and Neutral

ghg_df <- sp_df %>%
  select(date, permno, ghg) %>% 
  mutate(date = if(params$datafreq == "mthly") {as.yearqtr(date)}
         else {as.yearmon(date)}) %>%
  group_by(date, permno) %>%
  summarize(mean_ghg = mean_na_rm(ghg)) %>%
  group_by(permno) %>%
  mutate(rollwin = rollapplyr(mean_ghg, 
                              width = if(params$datafreq == "mthly") {
                                params$window / 3}
                              else {params$window}, 
                              FUN = function(x) mean_na_rm(x), 
                              fill = NA, 
                              partial = TRUE,
                              align = "right")) %>%
  mutate_at(vars(mean_ghg, rollwin), ~ replace(., is.nan(.), NA)) %>% 
  group_by(date) %>% 
  mutate(q_25 = quantile_na_rm(rollwin, probs = 0.25),
         q_75 = quantile_na_rm(rollwin, probs = 0.75)) %>%
  mutate(status = case_when(is.na(rollwin)  ~ "U",
                            rollwin <= q_25 ~ "G",
                            rollwin >= q_75 ~ "B",                           
                            rollwin < q_75 & rollwin > q_25 ~ "N")) %>%
  arrange(permno, date) %>% 
  ungroup()

# Transition Matrix -------------------------------------------------------
trans_mat <- ghg_df %>%
  select(date, permno, status) %>%
  pivot_wider(names_from = permno, values_from = status) %>%
  select(-date) %>% 
  as.matrix()

mcFit <- markovchainFit(data = trans_mat, byrow = FALSE)

mcFit$estimate@transitionMatrix %>% 
  rbind(mcFit$upperEndpointMatrix, mcFit$lowerEndpointMatrix) %>% 
  write.csv(here("output", 
                 paste0(params$window, 
                        "m_rolling_window_using_", 
                        params$datafreq, 
                        "_data_", 
                        "transmat.csv")))

# Alpha screening ---------------------------------------------------------

date_col <- ghg_df %>% select(date) %>% distinct() %>% pluck()

# Functions Descriptions --------------------------------------------------

# 0. f_main:            Main module
# 1. f_create_id:       Identify a list of permno for each quarter / month
# 2. f_return_date:     create start_date and end_date for loop
# 3. f_ret_mat:         Generate ret_mat from data_df
# 4. f_factor_mat:      Generate factor_mat from factor_df
# 5. alpha screen:      Alpha screening function
# 6. f_alpha_cor:       Alpha correlations
# 7. f_tbl_screening:   Summary table of probability ratios
# 8. f_screenplot:      Create screenplot

source(here::here("function", "screening_funs_V2.R"))

id <- list()
id["green"] <- ghg_df %>% f_create_id("G") %>% list()
id["brown"] <- ghg_df %>% f_create_id("B") %>% list()

# Main function -----------------------------------------------------------

f_main <- function(.df,
                   .factor,
                   .factor_model = c("three", "six"),
                   .id,
                   .date,
                   .model = c("bw", "fw"),
                   .datafreq = c("mthly", "dly"),
                   .method = c("pearson", "kendall", "spearman"),
                   .window,
                   .bucket,
                   .control) {

  .factor_model <- match.arg(.factor_model)
  .model        <- match.arg(.model)
  .datafreq     <- match.arg(.datafreq)
  .method       <- match.arg(.method)
  
  wind_adj <- if(.datafreq == "mthly") .window / 3 else .window
  fw_start <- if(.datafreq == "mthly") 4 else 12
  id_date  <- if(.model == "bw") {.date[-(1:(wind_adj - 1)), ]} 
              else {.date[fw_start:(nrow(.date) - wind_adj), ]}

  dates <- f_return_date(id_date, .window, .model, .datafreq)
  
  out1 <- out2 <- out3 <- list()

  for (i in seq_along(id_date$date)) {
    print(id_date$date[i])

    ret_mat <- f_ret_mat(.df, .id, id_date$date[i], dates$start_date[i], dates$end_date[i])
    factor_mat <- f_factor_mat(.factor, dates$start_date[i], dates$end_date[i])

    alpha_screen <- PeerPerformance::alphaScreening(ret_mat, factors = factor_mat, control = .control)
    alpha_cor <- f_alpha_cor(ret_mat, factor_mat, id_date$date[i], .method = .method)
    tbl_screening <- f_tbl_screening(alpha_screen, .bucket)
    
    folder_name <- paste(.model, .window, "m_window", .factor_model, "factor_model", .datafreq, "data", sep = "_")
    fig_title <- toupper(paste(folder_name, deparse(substitute(.id)), id_date$date[i], sep = "_"))

    f_screenplot(tbl_screening, fig_title, folder_name, .datafreq)
    
    out1 <- append(out1, list(alpha_screen))
    out2 <- append(out2, list(alpha_cor))
    out3 <- append(out3, list(tbl_screening))
  }
  names(out1) <- names(out2) <- names(out3) <- id_date$date
  
  print("completed!")
  
  return(list(out1, out2, out3))
}

results <- f_main(sp_df, 
                  factor_df, 
                  .factor_model = params$factor, 
                  id$green, 
                  date_col,
                  .model = "fw",
                  .datafreq = params$datafreq,
                  .method = "pearson",
                  params$window,
                  params$bucket,
                  params$control)



# source(here::here("function", "screening_funs_V2.R"))
# 
# 
# date_col <- date_col[1:12, ]
# 
# ret_mat2 <- sp_df %>%
#   filter(permno %in% id$green$`2011 Q4`) %>%
#   filter(date >= "2009-01-01",
#          date <= "2011-12-01") %>%
#   select(date, permno, ret_rf) %>%
#   pivot_wider(names_from = permno, values_from = ret_rf) %>%
#   janitor::remove_empty(which = "cols") %>%
#   select(-date) %>%
#   as.matrix()
#   
#   
#   f_ret_mat(sp_df, id$green, "2011 q4", "2009-01-01", "2011-12-01")
# factor_mat2 <- f_factor_mat(factor_df, "2009-01-01", "2011-12-01")
# 
# 
# alpha_screen <- PeerPerformance::alphaScreening(ret_mat2, factors = factor_mat2, control = params$control)
# 
# test <- f_tbl_screening(alpha_screen, 5)
# 
# attr(test)
# f_screenplot(test, 0, 0, "mthly")























