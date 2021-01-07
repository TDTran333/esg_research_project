shhh <- suppressPackageStartupMessages # It's a library, so shhh!
shhh(require(lubridate))               # Date management

# Functions Descriptions --------------------------------------------------

# 1. f_create_id:       Identify a list of permno for each period for a group
# 2. f_return_date:     create start_date and end_date for loop
# 3. f_ret_mat:         Generate ret_mat from data_df
# 4. f_factor_mat:      Generate factor_mat from factor_df
# 5. alpha screen:      Alpha screening function
# 6. f_alpha_cor:       Alpha correlations
# 7. f_tbl_screening:   Summary table of probability ratios
# 8. f_screenplot:      Create screenplot


# -------------------------------------------------------------------------

.f_create_id <- function(.df, .status) {
  .df %>% 
    filter(status == {{ .status }}) %>%
    select(date, permno) %>%
    group_by(date) %>% 
    mutate(row = row_number()) %>%
    pivot_wider(names_from = date, values_from = permno) %>%
    select(-row) %>% 
    map(unlist)
}
f_create_id <- compiler::cmpfun(.f_create_id)

# -------------------------------------------------------------------------

.f_return_date <- function(.date, .window, .model, .datafreq) {
  
  date_yqtr <- compose(as.Date.yearqtr, as.yearqtr)
  date_ymon <- compose(as.Date.yearmon, as.yearmon)
  date_qtr  <- map(.date, date_yqtr)
  date_mon  <- map(.date, date_ymon)
  
  if (.model == "bw") {
    if (.datafreq == "mthly") {
      start_date <- date_qtr %>% map(~ .x %m-% months(.window - 3))
      end_date   <- date_qtr %>% map(~ .x %m+% months(2))
    } else {
      start_date <- date_mon %>% map(~ .x %m-% months(.window - 1))
      end_date   <- date_mon %>% map(~ .x %m+% months(1) %m-% days(1))
    }
  } else {
    if (.datafreq == "mthly") {
      start_date <- date_qtr %>% map(~ .x %m+% months(3))
      end_date   <- date_qtr %>% map(~ .x %m+% months(.window + 2))
    } else {
      start_date <- date_mon %>% map(~ .x %m+% months(1))
      end_date   <- date_mon %>% map(~ .x %m+% months(.window + 1) %m-% days(1))
    }
  }
  out <- tibble(start_date, end_date) %>% unnest(cols = c(start_date, end_date))
  return(out)
}
f_return_date <- compiler::cmpfun(.f_return_date)

# -------------------------------------------------------------------------

.f_ret_mat <- function(.df, .id, .id_date, .start_date, .end_date) {
  
  eval_expr <- compose(eval, parse)
  id_string <- paste0(".id$'", .id_date,"'")
  id_vec <- eval_expr(text = id_string)

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

.f_tbl_screening  <- function(.df, .bucket, do_norm = TRUE) {
  
  .df <- .df %>%
    as_tibble() %>%
    select(alpha, pizero, pipos, pineg)

  pos    <- order(.df$alpha, decreasing = TRUE)
  n      <- nrow(.df)
  nblock <- floor(n / .bucket)
  ngroup <- floor(n / nblock)
  tbl    <- matrix(data = NA, nrow = nblock, ncol = 4)
  
  for (i in 1:nblock) {
    idx <- pos[((i - 1) * ngroup + 1):(i * ngroup)]
    if (i == nblock) {
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

.f_screenplot <- function(.df, .fig_title, .folder_name, .datafreq) {
  
  dir <- here::here("output", "figures", .folder_name)
  ifelse(!dir.exists(file.path(dir)), dir.create(file.path(dir)), FALSE)
  png(file = here::here(dir, paste0("fig_screenplot_", .fig_title, ".png")), width = 700, height = 700)
  
  mult <- if(.datafreq == "mthly") 12 else 252
  
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


