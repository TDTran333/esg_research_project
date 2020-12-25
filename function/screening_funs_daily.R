shhh <- suppressPackageStartupMessages # It's a library, so shhh!
shhh(require(lubridate))               # Date management

.f_create_id <- function(df, input_date, status) {
  {{ df }} %>% 
    filter(status == {{ status }}) %>%
    select({{ input_date }}, permno) %>%
    pivot_wider(names_from = {{ input_date }}, values_from = permno) %>% 
    clean_names() %>%
    map(., unlist)
}

f_create_id <- compiler::cmpfun(.f_create_id)

# -------------------------------------------------------------------------

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

# -------------------------------------------------------------------------

.f_screenplot <- function(df, title, folder) {
  
  dir <- here::here("output", "figures", folder)
  ifelse(!dir.exists(file.path(dir)), dir.create(file.path(dir)), FALSE)
  
  png(file = here::here(dir, paste0("fig_screenplot_", title, ".png")))
  cex = cex.axis = 0.8
  par(mfrow = c(1, 2))
  plot(365 * 100 * sort(df$alpha, decreasing = TRUE), 1:length(df$alpha), type = 'b', las = 1, pch = 20, cex = cex, cex.axis = cex.axis, 
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

# -------------------------------------------------------------------------

.f_alpha_screen <- function(df, factor_df, id, input_date, model, window, control) {
  out1 <- list()
  # out2 <- list()
  for (i in 1:length(input_date)) {
    print(input_date[i])
    
    if (model == "bw") {
      folder <- paste0("backward_", window, "m")
      start_date <- as.Date.yearmon(input_date[i]) %m-% months({{ window }} - 1)
      end_date <- as.Date.yearmon(input_date[i]) %m+% months(1) %m-% days(1)
    } else {
      folder <- paste0("forward_", window, "m")
      start_date <- as.Date.yearmon(input_date[i]) %m+% months(1)
      end_date <- as.Date.yearmon(input_date[i]) %m+% months({{ window + 1}}) %m-% days(1)
    }

    ret <- df %>%
      filter(permno %in% eval(parse(text = tolower(paste0(id, stringr::str_replace(input_date[i], " ", "_")))))) %>%
      filter(date >= start_date,
             date <= end_date) %>%
      select(date, permno, ret_rf) %>%
      pivot_wider(names_from = permno, values_from = ret_rf) %>%
      janitor::remove_empty(which = "cols") %>% 
      select(-date) %>%
      as.matrix()
    
    fctr <- factor_df %>% 
      filter(date >= start_date,
             date <= end_date) %>%
      select(-date) %>% 
      as.matrix()
    
    alpha_screen <- PeerPerformance::alphaScreening(ret, factors = fctr, control = control)
    
    result <- alpha_screen %>% 
      as_tibble() %>%
      select(alpha, pizero, pipos, pineg) %>%
      f_tbl_screening(floor(nrow(.) / params$bucket)) %>%
      as.tibble()
    
    f_screenplot(result, paste0("_", window,"y_ma_", id, {{i}}), folder)
    
    out1 <- append(out1, list(result))
    # out2 <- append(out2, list(alpha_screen))
    
  }
  names(out1) <- {{ input_date }}
  # names(out2) <- {{ input_date }}
  print("Completed.")
  # return(list(out1, out2))
  return(out1)
}

f_alpha_screen <- compiler::cmpfun(.f_alpha_screen)

# -------------------------------------------------------------------------
.f_summ_pi <- function(df, input_date) {
  mean_pi <- function(df) {
    {{ df }} %>%
      summarize(mean_pipos = mean(pipos, na.rm = TRUE),
                mean_pineg = mean(pineg, na.rm = TRUE))
  }
  
  tmp <- {{ df }} %>%
    map(mean_pi) %>%
    transpose() %>%
    do.call(rbind, .) %>%
    as.data.frame()
  
  tmp2 <- matrix(unlist(tmp), byrow=T, ncol=2)
  colnames(tmp2) <- c("pipos", "pineg")
  tmp2 %>% 
    as_tibble() %>% 
    mutate(date = {{ input_date }})
}

f_summ_pi <- compiler::cmpfun(.f_summ_pi)