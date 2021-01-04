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
params$datafreq <- "mthly"                   # Data frequency : "mthly" or "dly"       
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
  arrange(permno) %>% 
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

# Inputs declaration

date_col <- ghg_df %>% select(date) %>% distinct() %>% pluck()

# Identify a list of permno for each quarter / month

f_create_id <- function(df, status) {
  df %>% 
    filter(status == {{ status }}) %>%
    select(date, permno) %>%
    group_by(date) %>% 
    mutate(row = row_number()) %>%
    pivot_wider(names_from = date, values_from = permno) %>%
    select(-row) %>% 
    map(unlist)
}

id <- list()
id["green"] <- ghg_df %>% f_create_id("G") %>% list()
id["brown"] <- ghg_df %>% f_create_id("B") %>% list()

# Prepare inputs according to options

paste_ <- partial(paste, sep = "_")


f_return_date <- function(date_col, window, model, datafreq) {
  
  date_yqtr <- compose(as.Date.yearqtr, as.yearqtr)
  date_ymon <- compose(as.Date.yearmon, as.yearmon)
  
  dt_qtr <- map(date_col, date_yqtr)
  dt_mon <- map(date_col, date_ymon)

  if (model == "bw") {
    if (datafreq == "mthly") {
      start_date <- dt_qtr %>% map(~ .x %m-% months(window - 3))
      end_date   <- dt_qtr %>% map(~ .x %m+% months(2))
    } else {
      start_date <- dt_mon %>% map(~ .x %m-% months(window - 1))
      end_date   <- dt_mon %>% map(~ .x %m+% months(1) %m-% days(1))
    }
  } else {
    if (datafreq == "mthly") {
      start_date <- dt_qtr %>% map(~ .x %m+% months(3))
      end_date   <- dt_qtr %>% map(~ .x %m+% months(window + 2))
    } else {
      start_date <- dt_mon %>% map(~ .x %m+% months(1))
      end_date   <- dt_mon %>% map(~ .x %m+% months(window + 1) %m-% days(1))
    }
  }
  out <- tibble(start_date, end_date) %>% unnest(cols = c(start_date, end_date))
  return(out)
}

# Main function -----------------------------------------------------------

f_main <- function(data,
                   factor,
                   factor_model = c("three", "six"),
                   id,
                   date_col,
                   model = c("bw", "fw"),
                   datafreq = c("mthly", "dly"),
                   window,
                   bucket,
                   control) {
  
  factor_model <- match.arg(factor_model)
  model        <- match.arg(model)
  datafreq     <- match.arg(datafreq)
  out_path     <- paste_(model, window, "m", factor_model, "figs")
  
  if(datafreq == "mthly") {
    wind_adj <- params$window / 3
    fw_start <- 4
  } else { 
    wind_adj <- params$window
    fw_start <- 12
  }

  if (model == "bw") {
    id_date  <- date_col[-(1:(wind_adj - 1)), ]
  } else {
    id_date  <- date_col[fw_start:(nrow(date_col) - wind_adj), ]
  }

  dt <- f_return_date(id_date, window, model, datafreq)

  for (i in seq_along(id_date$date)) {
    print(id_date$date[i])
    print(dt$start_date[i])
    print(dt$end_date[i])
  }
}


  
f_main(sp_df, 
       factor_df, 
       params$factor, 
       id$green, 
       date_col,
       model = "fw",
       datafreq = "mthly",
       params$window,
       params$bucket,
       params$control)
  
  
  
  
  


# function to create id



# function to convert df to matrix

f_ret_mat <- function(df, id) {
  ret_mat <- df %>%
    filter(permno %in% id) %>%
    filter(date >= start_date,
           date <= end_date) %>%
    select(date, permno, ret_rf) %>%
    pivot_wider(names_from = permno, values_from = ret_rf) %>%
    janitor::remove_empty(which = "cols") %>% 
    select(-date) %>%
    as.matrix()
}

f_factor_mat <- function(factor_df, start_date, end_date) {
  factor_mat <- factor_df %>% 
    filter(date >= start_date,
           date <= end_date) %>%
    select(-date) %>% 
    as.matrix()
}








ret_mat <- sp_df %>% 
  filter(permno %in% id$green[[12]]) %>% 
  filter(date >= start_date,
         date <= end_date) %>%
  select(date, permno, ret_rf) %>%
  pivot_wider(names_from = permno, values_from = ret_rf) %>%
  janitor::remove_empty(which = "cols") %>% 
  select(-date) %>%
  as.matrix()

factor_mat <- factor_df %>% 
  filter(date >= start_date,
         date <= end_date) %>%
  select(-date) %>% 
  as.matrix()

# alpha screen
alpha_screen <- PeerPerformance::alphaScreening(ret_mat, factors = factor_mat, control = control)

# function for correlation



# function for tbl screening




# function for results output









.f_alpha_cor <- function(ret_mat, factor_mat, input_date) {
  lst <-  list()
  
  for (i in 1:ncol(ret_mat)) {
    fit <-  lm(ret_mat[, i] ~ factor_mat)
    resid <- list(fit$residuals)
    names(resid) <- names(ret_mat[i])
    lst <- c(lst, resid)
  }
  
  out <- t(purrr::map_df(lst, ~as.data.frame(t(.x),stringsAsFactors = FALSE)))
  colnames(out) <- colnames(ret_mat)
  
  out <- out %>% 
    as_tibble() %>% 
    pivot_longer(cols = everything(), names_to = "permno", values_to = "residuals") %>%
    group_by(permno) %>% 
    mutate(row = row_number()) %>% 
    arrange(permno) %>%  
    widyr::pairwise_cor(permno, row, residuals, upper = FALSE, method = c("pearson")) %>% 
    mutate(date = input_date)
  
  return(out)
}

f_alpha_cor <- compiler::cmpfun(.f_alpha_cor)








.f_alpha_screen <- function(df, factor_df, fctr_flag, id, input_date, datafreq, model, window, control, bucket) {
  out1 <- out2 <- out3 <-  list()
  
  for (i in 1:length(input_date)) {
    print(input_date[i])
    
    if (model == "bw") {
      folder <- paste0("backward_", window, "m_", fctr_flag, "_data")
      
      if (datafreq == "mthly") {
        start_date <- as.Date.yearqtr(input_date[i]) %m-% months(window - 3)
        end_date <- as.Date.yearqtr(input_date[i]) %m+% months(2)
      } else {
        start_date <- as.Date.yearmon(input_date[i]) %m-% months({{ window }} - 1)
        end_date <- as.Date.yearmon(input_date[i]) %m+% months(1) %m-% days(1)
      }
      
    } else {
      folder <- paste0("forward_", window, "_", fctr_flag, "_data")
      
      if (datafreq == "mthly") {
        start_date <- as.Date.yearqtr(input_date[i]) %m+% months(3)
        end_date <- as.Date.yearqtr(input_date[i]) %m+% months(window + 2)
      } else {
        start_date <- as.Date.yearmon(input_date[i]) %m+% months(1)
        end_date <- as.Date.yearmon(input_date[i]) %m+% months({{ window + 1}}) %m-% days(1)  
      }  
    }
    
    print(start_date)
    print(end_date)
    
    ret_mat <- df %>%
      filter(permno %in% eval(parse(text = tolower(paste0(id, stringr::str_replace(input_date[i], " ", "_")))))) %>%
      filter(date >= start_date,
             date <= end_date) %>%
      select(date, permno, ret_rf) %>%
      pivot_wider(names_from = permno, values_from = ret_rf) %>%
      janitor::remove_empty(which = "cols") %>% 
      select(-date) %>%
      as.matrix()
    
    factor_mat <- factor_df %>% 
      filter(date >= start_date,
             date <= end_date) %>%
      select(-date) %>% 
      as.matrix()
    
    alpha_screen <- PeerPerformance::alphaScreening(ret_mat, factors = factor_mat, control = control)
    alpha_cor <- f_alpha_cor(ret_mat, factor_mat, input_date[i])
    
    result <- alpha_screen %>% 
      as_tibble() %>%
      select(alpha, pizero, pipos, pineg) %>%
      f_tbl_screening(floor(nrow(.) / bucket)) %>%
      as.tibble()
    
    title <- paste0("_", window,"m_rollingwindow_", id, input_date[i])
    
    f_screenplot(result, title, folder, datafreq)
    
    out1 <- append(out1, list(result))
    out2 <- append(out2, list(alpha_screen$n))
    out3 <- append(out3, list(alpha_cor))
    
  }
  names(out1) <- names(out2) <- names(out3) <- {{ input_date }}
  print("Completed.")
  
  return(list(out1, out2, out3))
}

f_alpha_screen <- compiler::cmpfun(.f_alpha_screen)




















pos    <- order(df$alpha, decreasing = TRUE)
n      <- nrow(df)
ngroup <- floor(n / nblock)
tbl    <- matrix(data = NA, nrow = nblock, ncol = 4)
for (i in 1:nblock) {
  idx <- pos[((i - 1) * ngroup + 1):(i * ngroup)]
  if (i == nblock) {
    idx <- pos[((i - 1) * ngroup + 1):n]
  }
  idx           <- idx[!is.na(idx)]
  tbl[i, 1]     <- mean(df$alpha[idx])
  tbl[i, 2:4]   <- colMeans(df[idx, c("pipos", "pizero", "pineg")])
  colnames(tbl) <- c("alpha", "pipos", "pizero", "pineg")
}
if (do_norm) {
  tmp <- tbl[, 2:4]
  tmp <- sweep(tmp, 2, rowSums(tmp), "/")
  tbl[, 2:4] <- tmp
}
return(tbl)







.f_tbl_screening  <- function(df, nblock, do_norm = TRUE) {
  pos    <- order(df$alpha, decreasing = TRUE)
  n      <- nrow(df)
  ngroup <- floor(n / nblock)
  tbl    <- matrix(data = NA, nrow = nblock, ncol = 4)
  for (i in 1:nblock) {
    idx <- pos[((i - 1) * ngroup + 1):(i * ngroup)]
    if (i == nblock) {
      idx <- pos[((i - 1) * ngroup + 1):n]
    }
    idx           <- idx[!is.na(idx)]
    tbl[i, 1]     <- mean(df$alpha[idx])
    tbl[i, 2:4]   <- colMeans(df[idx, c("pipos", "pizero", "pineg")])
    colnames(tbl) <- c("alpha", "pipos", "pizero", "pineg")
  }
  if (do_norm) {
    tmp <- tbl[, 2:4]
    tmp <- sweep(tmp, 2, rowSums(tmp), "/")
    tbl[, 2:4] <- tmp
  }
  return(tbl)
}

f_tbl_screening <- compiler::cmpfun(.f_tbl_screening)





.f_screenplot <- function(df, title, folder, datafreq) {
  
  dir <- here::here("output", "figures", folder)
  ifelse(!dir.exists(file.path(dir)), dir.create(file.path(dir)), FALSE)
  png(file = here::here(dir, paste0("fig_screenplot_", title, ".png")))
  
  mult <- if(datafreq == "mthly") 12 else 365
  
  cex = cex.axis = 0.8
  par(mfrow = c(1, 2))
  plot(mult * 100 * sort(df$alpha, decreasing = TRUE), 
       1:length(df$alpha), type = 'b', las = 1, pch = 20, cex = cex, cex.axis = cex.axis, 
       xlab = "", ylab = "", main = "alpha", axes = FALSE)
  box(); grid()
  labs = seq(-40, 40, by = 10)
  axis(side = 1, at = labs, labels = paste0(labs, "%"), cex.axis = cex.axis)
  axis(side = 2, at = 1:length(df$alpha), labels = 1:length(df$alpha), las = 1, cex.axis = cex.axis)
  
  mainstr = expression(hat(pi)^'+'*' / '*hat(pi)^0*' / '*hat(pi)^'-')
  barplot(t(100 * df[, 2:4]), horiz = TRUE, names.arg = NULL, col = c(gray(0), gray(0.8), gray(0.5)),
          space = 0, xlab = "", xlim = c(0, 100), cex.names = cex.axis, border = NA,
          cex.axis = cex.axis, main = mainstr, las = 1, axes = FALSE)
  labs = seq(0, 100, by = 25)
  axis(side = 1, at = labs, labels = paste0(labs, "%"), cex.axis = cex.axis)
  box()
  x = seq(from = 104, to = -4, length.out = 200)
  y = seq(from = 0, to = 100, length.out = 200)
  par(new = TRUE); plot(x, y, type = 'l', lty = "dashed", lwd = 1, xlim = c(0, 100), ylim = c(0, 100), axes = FALSE, ylab = "", xlab = "")
  par(new = TRUE); plot(x - 27.5, y, type = 'l', lty = "dashed", lwd = 1, xlim = c(0, 100), ylim = c(0, 100), axes = FALSE, ylab = "", xlab = "")
  par(new = TRUE); plot(x + 27.5, y, type = 'l', lty = "dashed", lwd = 1, xlim = c(0, 100), ylim = c(0, 100), axes = FALSE, ylab = "", xlab = "")
  mtext(title, side = 3, line = -1.5, outer = TRUE, cex = 1.25)
  dev.off()
}

f_screenplot <- compiler::cmpfun(.f_screenplot)





















source(here::here("code", "source.R"))

# inputs
factor     <- if (params$factor == "all") factor_df else factor_df[, 1:4]
fctr_flag  <- if (params$factor == "all") "6-factor model" else "3-factor model"
date_vec   <- ghg_df %>% select(date) %>% distinct() %>% pluck() %>% as.matrix() %>% as.yearqtr()
datafreq   <- params$datafreq
window     <- params$window
wind_adj   <- if(params$datafreq == "mthly") params$window / 3 else params$window
fw_start   <- if(params$datafreq == "mthly") 4 else 12
control    <- params$ctr
bucket     <- params$bucket

# id
id_green <- ghg_df %>% f_create_id(date, "G")
id_brown <- ghg_df %>% f_create_id(date, "B")

# backward-looking
date_bw <- date_vec[-(1:(wind_adj - 1))]
model <- "bw"

result_g_bw <- f_alpha_screen(sp_df, factor, fctr_flag, id = "id_green$x", date_bw, datafreq, model, window, control, bucket)
result_b_bw <- f_alpha_screen(sp_df, factor, fctr_flag, id = "id_brown$x", date_bw, datafreq, model, window, control, bucket)

# forward-looking
date_fw <- date_vec[fw_start:(length(date_vec) - wind_adj)]
model <- "fw"

result_g_fw <- f_alpha_screen(sp_df, factor, fctr_flag, id = "id_green$x", date_fw, datafreq, model, window, control, bucket)
result_b_fw <- f_alpha_screen(sp_df, factor, fctr_flag, id = "id_brown$x", date_fw, datafreq, model, window, control, bucket)
