f_tbl_screening  <- function(df, nblock, do_norm = TRUE) {
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