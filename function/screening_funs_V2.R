shhh <- suppressPackageStartupMessages # It's a library, so shhh!
shhh(require(lubridate))               # Date management

# Functions Descriptions --------------------------------------------------

# 1. f_create_esg_type:  Create ESG type by quantile of rolling mean of esg variable
# 2. f_create_trans_mat: Create transition matrix
# 3. f_create_id:        Identify a list of permno by ESG group
# 4. f_create_dates:     create dates for loop
# 5. f_ret_mat:          Generate ret_mat from data_df
# 6. f_factor_mat:       Generate factor_mat from factor_df
# 7. alpha screen:       Alpha screening function
# 8. f_alpha_cor:        Alpha correlations
# 9. f_tbl_screening:    Summary table of probability ratios
# 10. f_screenplot:      Create screenplot

# f_main_process:        Runs the previous 10 functions.

# 11. f_tidy_screening:  Create tidy df of pi results for plotting
# 12. f_tidy_n_obs:      Create tidy df of concordant obs for plotting
# 13. f_tidy_alpha_cor:  Create tidy df of alpha cor plotting

# 15. f_plot_hist_obs:   Plot histograms of concordant obs
# 16. f_plot_hist_cor:   Plot histograms of alpha cor
# 17. f_plot_line_pi:    Plot line plot of pi ratios

# 18. f_port_permno:     Create list of permno for portfolio
# 19. f_port_ret:        Calculate returns with rebalancing

# -------------------------------------------------------------------------

.f_create_esg_type <- function(.df, .esg_var, .datafreq, .window) {
  
  .esg_var <- match.arg(.esg_var, c("ghg", "envscore"))
  .datafreq <- match.arg(.datafreq,  c("monthly", "daily"))
  
  mean_na_rm <- partial(mean, na.rm = TRUE)
  quantile_na_rm <- partial(quantile, na.rm =  TRUE)

  .df %>%
    select(date, permno, .data[[.esg_var]]) %>% 
    mutate(date = if(.datafreq == "monthly") {as.yearqtr(date)}
           else {as.yearmon(date)}) %>%
    group_by(date, permno) %>%
    summarize(mean_esg_var = mean_na_rm(.data[[.esg_var]])) %>% 
    group_by(permno) %>%
    mutate(rollwin = rollapplyr(mean_esg_var,
                                width = if(.datafreq == "monthly") {
                                  .window / 3}
                                else {.window},
                                FUN = mean_na_rm,
                                fill = NA,
                                partial = TRUE,
                                align = "right")) %>%
    mutate_at(vars(mean_esg_var, rollwin), ~ replace(., is.nan(.), NA)) %>%
    group_by(date) %>%
    mutate(q_25 = quantile_na_rm(rollwin, probs = 0.25),
           q_50 = quantile_na_rm(rollwin, probs = 0.50),
           q_75 = quantile_na_rm(rollwin, probs = 0.75)) %>%
    mutate(esg_type = case_when(is.na(rollwin)  ~ "U",
                                rollwin <= q_25 ~ "G",
                                rollwin >= q_75 ~ "B",
                                rollwin < q_75 & rollwin > q_25 ~ "N")) %>%
    arrange(permno, date) %>%
    ungroup()
}
f_create_esg_type <- compiler::cmpfun(.f_create_esg_type)

# -------------------------------------------------------------------------

.f_create_trans_mat <- function(.df) {
  
  trans_mat <- .df %>%
    select(date, permno, esg_type) %>%
    pivot_wider(names_from = permno, values_from = esg_type) %>%
    select(-date) %>% 
    as.matrix()
  
  mcFit <- markovchainFit(data = trans_mat, byrow = FALSE)
  
  mcFit$estimate@transitionMatrix %>% 
    rbind(mcFit$upperEndpointMatrix, mcFit$lowerEndpointMatrix)
  
}
f_create_trans_mat <- compiler::cmpfun(.f_create_trans_mat)

# -------------------------------------------------------------------------

.f_create_id <- function(.df, .esg_type) {
  .df %>% 
    filter(esg_type == {{ .esg_type }}) %>%
    select(date, permno) %>%
    group_by(date) %>% 
    mutate(row = row_number()) %>%
    pivot_wider(names_from = date, values_from = permno) %>%
    select(-row) %>% 
    map(unlist)
}
f_create_id <- compiler::cmpfun(.f_create_id)

# -------------------------------------------------------------------------

.f_create_dates <- function(.date, .window, .model, .datafreq) {
  
  .model    <- match.arg(.model, c("bw", "fw"))
  .datafreq <- match.arg(.datafreq, c("monthly", "daily"))
  
  wind_adj <- if(.datafreq == "monthly") .window / 3 else .window
  fw_start <- if(.datafreq == "monthly") 4 else 12
  id_date  <- if(.model == "bw") {.date[-(1:(wind_adj - 1)), ]} 
              else {.date[fw_start:(nrow(.date) - wind_adj), ]}

  date_yqtr <- compose(as.Date.yearqtr, as.yearqtr)
  date_ymon <- compose(as.Date.yearmon, as.yearmon)
  date_qtr  <- map(id_date, date_yqtr)
  date_mon  <- map(id_date, date_ymon)

  if (.model == "bw") {
    if (.datafreq == "monthly") {
      start_date <- date_qtr %>% map(~ .x %m-% months(.window - 3))
      end_date   <- date_qtr %>% map(~ .x %m+% months(2))
    } else {
      start_date <- date_mon %>% map(~ .x %m-% months(.window - 1))
      end_date   <- date_mon %>% map(~ .x %m+% months(1) %m-% days(1))
    }
  } else {
    if (.datafreq == "monthly") {
      start_date <- date_qtr %>% map(~ .x %m+% months(3))
      end_date   <- date_qtr %>% map(~ .x %m+% months(.window + 2))
    } else {
      start_date <- date_mon %>% map(~ .x %m+% months(1))
      end_date   <- date_mon %>% map(~ .x %m+% months(.window + 1) %m-% days(1))
    }
  }
  out <- cbind(id_date, start_date, end_date)
  colnames(out) <- c("id_date", "start_date", "end_date")
  return(out)
}
f_create_dates <- compiler::cmpfun(.f_create_dates)

# -------------------------------------------------------------------------

.f_ret_mat <- function(.df, .id, .id_date, .start_date, .end_date) {
  
  eval_expr <- compose(eval, parse)
  id_string <- paste0(".id$'", .id_date,"'")
  id_vec    <- eval_expr(text = id_string)

  ret_mat <- .df %>%
    filter(permno %in% id_vec) %>%
    filter(date >= .start_date,
           date <= .end_date) %>%
    select(date, permno, ret_rf) %>%
    pivot_wider(names_from = permno, values_from = ret_rf) %>%
    janitor::remove_empty(which = "cols") %>%
    select(-date) %>%
    as.matrix()
  
  return(ret_mat)
}
f_ret_mat <- compiler::cmpfun(.f_ret_mat)

# -------------------------------------------------------------------------

.f_factor_mat <- function(.factor, .start_date, .end_date) {
  factor_mat <- .factor %>% 
    filter(date >= .start_date,
           date <= .end_date) %>%
    select(-date) %>% 
    as.matrix()
  
  return(factor_mat)
}
f_factor_mat <- compiler::cmpfun(.f_factor_mat)

# -------------------------------------------------------------------------

.f_alpha_cor <- function(.ret, .factor, .date, .method) {
  
  lst <-  list()
  
  for (i in 1:ncol(.ret)) {
    fit <-  lm(.ret[, i] ~ .factor)
    resid <- list(fit$residuals)
    names(resid) <- names(.ret[i])
    lst <- append(lst, resid)
  }
  
  alpha_df <- t(map_df(lst, ~ as_tibble(t(.x))))
  colnames(alpha_df) <- colnames(.ret)
  
  out <- alpha_df %>% 
    as_tibble() %>% 
    pivot_longer(cols = everything(), names_to = "permno", values_to = "residuals") %>%
    group_by(permno) %>% 
    mutate(row = row_number()) %>% 
    arrange(permno) %>%  
    widyr::pairwise_cor(permno, row, residuals, upper = FALSE, method = .method) %>% 
    mutate(date = .date)
  
  return(out)
}
f_alpha_cor <- compiler::cmpfun(.f_alpha_cor)

# -------------------------------------------------------------------------

.f_tbl_screening  <- function(.df, .nblock, do_norm = TRUE) {
  
  .df <- .df %>%
    as_tibble() %>%
    select(alpha, pizero, pipos, pineg)

  pos    <- order(.df$alpha, decreasing = TRUE)
  n      <- nrow(.df)
  ngroup <- floor(n / .nblock)
  tbl    <- matrix(data = NA, nrow = .nblock, ncol = 4)
  
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
  if (do_norm) {
    tmp <- tbl[, 2:4]
    tmp <- sweep(tmp, 1, rowSums(tmp), "/")
    tbl[, 2:4] <- tmp
  }

  tbl <- tbl %>% as_tibble()
  return(tbl)
}
f_tbl_screening <- compiler::cmpfun(.f_tbl_screening)

# -------------------------------------------------------------------------

.f_screenplot <- function(.df, .fig_title, .datafreq) {
  
  dir <- here::here("output", "screeningplots")
  ifelse(!dir.exists(file.path(dir)), dir.create(file.path(dir)), FALSE)
  png(here::here(dir, paste0("fig_screenplot_", .fig_title, ".png")), width = 700, height = 700)
  
  mult <- if(.datafreq == "monthly") 12 else 252
  
  cex = cex.axis = 0.8
  par(mfrow = c(1, 2))
  plot(mult * 100 * sort(.df$alpha, decreasing = TRUE), 
       1:length(.df$alpha), type = 'b', las = 1, pch = 20, cex = cex, cex.axis = cex.axis, 
       xlab = "", ylab = "", main = "alpha", axes = FALSE)
  box(); grid()
  labs = seq(-40, 40, by = 10)
  axis(side = 1, at = labs, labels = paste0(labs, "%"), cex.axis = cex.axis)
  axis(side = 2, at = 1:length(.df$alpha), labels = 1:length(.df$alpha), las = 1, cex.axis = cex.axis)
  
  mainstr = expression(hat(pi)^'+'*' / '*hat(pi)^0*' / '*hat(pi)^'-')
  barplot(t(100 * .df[, 2:4]), horiz = TRUE, names.arg = NULL, col = c(gray(0), gray(0.8), gray(0.5)),
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
  mtext(.fig_title, side = 3, line = -1.5, outer = TRUE, cex = 1.25)
  dev.off()
}
f_screenplot <- compiler::cmpfun(.f_screenplot)

# -------------------------------------------------------------------------

.f_tidy_screening <- function(.tbl_screening, .id_date) {
  mean_pi <- function(.tbl_screening) {
    .tbl_screening %>%
      summarize(mean_pipos = mean(pipos, na.rm = TRUE),
                mean_pineg = mean(pineg, na.rm = TRUE))
  }
  
  .tbl_screening %>%
    map(mean_pi) %>%
    transpose() %>%
    map(rbind) %>% 
    unlist() %>% 
    matrix(byrow=T, ncol = 2) %>% 
    `colnames<-`(c("pipos", "pineg")) %>%
    as_tibble() %>% 
    mutate(date = .id_date) %>%
    pivot_longer(-date)
}
f_tidy_screening <- compiler::cmpfun(.f_tidy_screening)

# -------------------------------------------------------------------------

.f_tidy_n_obs <- function(.alphascreen, .id_date) {
  .alphascreen %>%
    map("n") %>% 
    bind_rows() %>%
    mutate(date = .id_date) %>% 
    pivot_longer(-date, names_to = "permno", values_to = "obs") 
}
f_tidy_n_obs <- compiler::cmpfun(.f_tidy_n_obs)

# -------------------------------------------------------------------------

.f_tidy_alpha_cor <- function(.alpha_cor) {
  .alpha_cor %>%
    bind_rows() %>% 
    select(date, correlation) %>% 
    pivot_longer(-date) %>% 
    filter(!is.na(value))
}
f_tidy_alpha_cor <- compiler::cmpfun(.f_tidy_alpha_cor)

# -------------------------------------------------------------------------

.f_plot_hist_cor <- function(.tidy_alpha_cor, .model_name)  {
  p <- .tidy_alpha_cor %>% 
    ggplot(aes(value)) +
    geom_histogram(bins = 30) +
    facet_wrap(~ factor(date)) +
    geom_vline(xintercept = -0.3, lty = "dashed") +
    geom_vline(xintercept = 0.3, lty = "dashed") + 
    labs(title = paste0("Correlations of Alphas For ", .model_name, "."),
         subtitle = "Dashed lines correspond to -0.3 and +0.3")
  
  alpha_cor_name <- paste(.model_name, params$window, "m", params$datafreq, "data", 
                          params$factor, "factor_model_alpha_cor.png", sep = "_")
  ggsave(p, filename = here("Output", "figures", alpha_cor_name), width = 8, height = 8, dpi = 150)
  
  return(p)
}
f_plot_hist_cor <- compiler::cmpfun(.f_plot_hist_cor)

# -------------------------------------------------------------------------

.f_port_permno <- function(.alpha_screen, .id, .id_date) {
  id_names <- names(.alpha_screen$n)
  
  permno <- .alpha_screen %>%
    as_tibble() %>%
    select(alpha, pipos, pineg) %>% 
    mutate(permno = id_names)
  
  top_10 <- permno %>% slice_max(pipos, n = 10)
  top_10_pct <- permno %>% slice_max(pipos, prop = .10)
  bottom_10 <- permno %>% slice_max(pineg, n = 10)
  bottom_10_pct <- permno %>% slice_max(pineg, prop = .10)
  top_10_alpha <- permno %>% slice_max(alpha, n = 10)
  top_10_alpha_pct <- permno %>% slice_max(alpha, prop = .10)
  bottom_10_alpha <- permno %>% slice_min(alpha, n = 10)
  bottom_10_alpha_pct <- permno %>% slice_min(alpha, prop = .10)
  benchmark_minus_20 <- permno %>% filter(!permno %in% c(top_10$permno, bottom_10$permno))
  benchmark_minus_20_pct <- permno %>% filter(!permno %in% c(top_10_pct$permno, bottom_10_pct$permno))
  
  list(top_10$permno, bottom_10$permno, benchmark_minus_20$permno,
       top_10_pct$permno, bottom_10_pct$permno, benchmark_minus_20_pct$permno,
       top_10_alpha$permno, top_10_alpha_pct$permno, bottom_10_alpha$permno, bottom_10_alpha_pct$permno) %>% 
    `names<-`(c("top_10", "bottom_10", "benchmark_minus_20",
                "top_10_pct", "bottom_10_pct", "benchmark_minus_20_pct",
                "top_10_alpha", "top_10_alpha_pct", "bottom_10_alpha", "bottom_10_alpha_pct"))
}
f_port_permno <- compiler::cmpfun(.f_port_permno)

# -------------------------------------------------------------------------

.f_port_ret <- function(.df, .port_permno, .id_date, .datafreq) {
  
  date_yqtr <- compose(as.Date.yearqtr, as.yearqtr)
  date_ymon <- compose(as.Date.yearmon, as.yearmon)
  
  port_ret <- function(.df, .permno, .id_date, .datafreq, .port_name) {
    test <- .df %>%
      filter(if (.datafreq == "monthly") {
        between(date, date_yqtr(.id_date) %m+% months(3),
                date_yqtr(.id_date) %m+% months(5))
      } else {
        between(date, date_ymon(.id_date) %m+% months(1),
                date_ymon(.id_date) %m+% months(2) %m-% days(1))
      }) %>% 
      filter(permno %in% .permno) %>%
      group_by(date) %>%
      summarize(ret = mean(ret_rf, na.rm = TRUE),
                port = .port_name,
                period = .id_date)
  }
  
  top_10_ret <- .df %>% port_ret(.port_permno$top_10, .id_date, .datafreq, "top_10")
  top_10_pct_ret <- .df %>% port_ret(.port_permno$top_10_pct, .id_date, .datafreq, "top_10_pct")
  bottom_10_ret <- .df %>% port_ret(.port_permno$bottom_10, .id_date, .datafreq, "bottom_10")
  bottom_10_pct_ret <- .df %>% port_ret(.port_permno$bottom_10_pct, .id_date, .datafreq, "bottom_10_pct")
  top_10_alpha_ret <- .df %>% port_ret(.port_permno$top_10_alpha, .id_date, .datafreq, "top_10_alpha")
  top_10_alpha_pct_ret <- .df %>% port_ret(.port_permno$top_10_alpha_pct, .id_date, .datafreq, "top_10_alpha_pct")
  bottom_10_alpha_ret <- .df %>% port_ret(.port_permno$bottom_10_alpha, .id_date, .datafreq, "bottom_10_alpha")
  bottom_10_alpha_pct_ret <- .df %>% port_ret(.port_permno$bottom_10_alpha_pct, .id_date, .datafreq, "bottom_10_alpha_pct")
  benchmark_minus_20_ret <- .df %>% port_ret(.port_permno$benchmark_minus_20, .id_date, .datafreq, "benchmark_minus_20")
  benchmark_minus_20_pct_ret <- .df %>% port_ret(.port_permno$benchmark_minus_20_pct, .id_date, .datafreq, "benchmark_minus_20_pct")
  
  list(top_10_ret, top_10_alpha_ret, bottom_10_ret, bottom_10_alpha_ret, benchmark_minus_20_ret,
       top_10_pct_ret, top_10_alpha_pct_ret, bottom_10_pct_ret, bottom_10_alpha_pct_ret, benchmark_minus_20_pct_ret) %>% 
    `names<-`(c("top_10", "top_10_alpha", "bottom_10", "bottom_10_alpha", "benchmark_minus_20",
                "top_10_pct", "top_10_alpha_pct", "bottom_10_pct", "bottom_10_alpha_pct", "benchmark_minus_20_pct"))
}
f_port_ret <- compiler::cmpfun(.f_port_ret)

