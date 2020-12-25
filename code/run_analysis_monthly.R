# For best practice, restart R session before running script. Ctrl/Cmd + Shift + F10

source(here::here("code", "source.R"))

# Parameters --------------------------------------------------------------

params          <- list()         
params$ctr      <- list(nCore = 3)           # Number of core and other controls
params$start    <- 2009
params$end      <- 2017
params$subDir   <- paste(params$start, params$end, sep = "-")
params$datafreq <- "mthly"                   # "mthly" or "dly"
params$window   <- 36                        # Rolling window
params$bucket   <- 5                         # Number of firms per bucket
params$factor   <- "three"                   # "all" for six-factor model otherwise 3

# Load Data ---------------------------------------------------------------
sp <- readRDS(here("data", params$subDir, paste0(params$datafreq, "_data_sp.rds")))
factor <- readRDS(here("data", params$subDir, paste0(params$datafreq, "_factor.rds")))

# Generate groups ---------------------------------------------------------
ghg_df <- sp %>%
  select(date, permno, ghg) %>% 
  mutate(date = if(params$datafreq == "mthly") {as.yearqtr(date)}
                else {as.yearmon(date)}) %>%
  group_by(date, permno) %>%
  summarize(mean_ghg = mean(ghg, na.rm = TRUE)) %>%
  group_by(permno) %>%
  mutate(rollwin = rollapplyr(mean_ghg, 
                              width = if(params$datafreq == "mthly") {
                                         params$window / 3}
                                      else {params$window}, 
                              FUN = function(x) mean(x, na.rm = TRUE), 
                              fill = NA, 
                              partial = TRUE,
                              align = "right")) %>%
  mutate_at(vars(mean_ghg, rollwin), ~replace(., is.nan(.), NA)) %>% 
  group_by(date) %>% 
  mutate(q_25 = quantile(rollwin, probs = 0.25, na.rm = TRUE),
         q_75 = quantile(rollwin, probs = 0.75, na.rm = TRUE)) %>%
  mutate(status = case_when(is.na(rollwin)  ~ "U",
                            rollwin <= q_25 ~ "G",
                            rollwin >= q_75 ~ "B",                           
                            rollwin < q_75 & rollwin > q_25 ~ "N")) %>%
  arrange(permno)

# Transition Matrix -------------------------------------------------------
trans_mat <- ghg_df %>%
  select(date, permno, status) %>%
  pivot_wider(names_from = permno, values_from = status) %>%
  ungroup() %>% 
  select(-date) %>% 
  as.matrix()

mcFit <- markovchainFit(data = trans_mat, byrow = FALSE)

mcFit$estimate@transitionMatrix %>% 
  rbind(mcFit$upperEndpointMatrix, mcFit$lowerEndpointMatrix) %>% 
  write.csv(here("output", 
                 paste0(params$window, 
                       "m_rollingwindow_", 
                       params$datafreq, 
                       "_data_", 
                       "transmat.csv")))

# Alpha screening ---------------------------------------------------------
source(here::here("code", "source.R"))

# inputs
factor     <- if (params$factor == "all") factor else factor[, 1:3]
fctr_flag  <- if (params$factor == "all") "6-factor model" else "3-factor model"
date_vec   <- ghg_df %>% select(date) %>% distinct() %>% pluck() %>% as.matrix() %>% as.yearqtr()
datafreq   <- params$datafreq
window     <- params$window
wind_adj   <- if(params$datafreq == "mthly") params$window / 3 else params$window
control    <- params$ctr
bucket     <- params$bucket

# id
id_green <- ghg_df %>% f_create_id(date, "G")
id_brown <- ghg_df %>% f_create_id(date, "B")

# backward-looking
date_bw <- date_vec[-(1:(wind_adj - 1))]
model <- "bw"

result_g_bw <- f_alpha_screen(sp, factor, fctr_flag, id = "id_green$x", date_bw, datafreq, model, window, control, bucket)
result_b_bw <- f_alpha_screen(sp, factor, fctr_flag, id = "id_brown$x", date_bw, datafreq, model, window, control, bucket)

# forward-looking
date_fw <- date_vec[4:(length(date_vec) - wind_adj)]
model <- "fw"

result_g_fw <- f_alpha_screen(sp, factor, fctr_flag, id = "id_green$x", date_fw, datafreq, model, window, control, bucket)
result_b_fw <- f_alpha_screen(sp, factor, fctr_flag, id = "id_brown$x", date_fw, datafreq, model, window, control, bucket)

# Analysis ----------------------------------------------------------------
t1 <- result_g_bw %>% 
  f_summ_pi(date_bw) %>% 
  as_tibble() %>% 
  mutate(model = "result_g_bw") %>% 
  pivot_longer(-c(date, model))

t2 <- result_b_bw %>% 
  f_summ_pi(date_bw) %>% 
  as_tibble() %>% 
  mutate(model = "result_b_bw") %>% 
  pivot_longer(-c(date, model))

t3 <- result_g_fw %>% 
  f_summ_pi(date_fw) %>% 
  as_tibble() %>% 
  mutate(model = "result_g_fw") %>% 
  pivot_longer(-c(date, model))

t4 <- result_b_fw %>% 
  f_summ_pi(date_fw) %>% 
  as_tibble() %>% 
  mutate(model = "result_b_fw") %>% 
  pivot_longer(-c(date, model))

rbind(t1, t2, t3, t4) %>% 
  ggplot(aes(date, value, color = name)) +
  geom_line(size = 1.5) +
  facet_wrap(~model, ncol = 1) +
  labs(title = paste0("Mean Out/Under-Performance Ratios using ",params$window, "-months rolling window."),
       subtitle = paste0("Backward-looking and forward-looking for Green and Brown firms using ", fctr_flag),
       x = "Date",
       y = "Percent") +
  scale_y_continuous(labels = percent)

ggsave(here("Output", "figures", paste0(params$window, "m_", datafreq, "_data_", fctr_flag, "_mean_pi.png")))

# Correlation matrices ----------------------------------------------------

# library(widyr)
# 
# sp %>%
#   select(date, permno, ret_rf) %>%
#   widyr::pairwise_cor(permno, date, ret_rf, upper = FALSE, method = c("pearson")) %>%
#   ggplot(aes(correlation)) +
#   geom_histogram(bins = 30) +
#   labs(title = "Pearson correlation of returns for SP1500.",
#        x = "Correlation",
#        y = "count")
# 
# sp %>%
#   select(date, permno, ret_rf) %>%
#   widyr::pairwise_cor(permno, date, ret_rf, upper = FALSE, method = c("spearman")) %>%
#   ggplot(aes(correlation)) +
#   geom_histogram(bins = 30) +
#   labs(title = "Pearson's Rho of returns for SP1500.",
#        x = "Correlation",
#        y = "count")
# 
# mat <- sp %>%
#   select(date, permno, ret_rf) %>%
#   pivot_wider(names_from = permno, values_from = ret_rf) %>%
#   select(-date)
# 
# fctr <- factor %>%
#   select(-date) %>%
#   as.matrix()
# 
# gli <- list()
# for (i in 1:ncol(mat)) {
#   fit <-  lm(as.matrix(mat[[i]]) ~ fctr)
#   tmp <- list(fit$residuals)
#   names(tmp) <- names(mat[i])
#   df <- append(df, list(tmp))
# }
# 
# df %>% matrix(unlist(tmp), byrow=T, ncol=1)
# 
# df <- list()
# for (i in 1:ncol(mat)) {
#   fit <-  lm(as.matrix(mat[[i]]) ~ fctr)
#   tmp <- list(fit$residuals)
#   names(tmp) <- names(mat[i])
#   tm %>% as.tibble()
# }

         