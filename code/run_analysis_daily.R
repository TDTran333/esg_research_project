# For best practice, restart R session before running script. Ctrl/Cmd + Shift + F10

source(here::here("code", "source_daily.R"))

# Parameters --------------------------------------------------------------

params          <- list()         
params$ctr      <- list(nCore = 3)           # Number of core and other controls
params$start    <- 2009
params$end      <- 2017
params$window   <- 12                         # Rolling window
params$subDir   <- paste(params$start, params$end, sep = "-")            
params$bucket   <- 5                         # Number of firms per bucket
params$factor   <- "all"                     # "all" for six-factor model otherwise 3

# Load Data ---------------------------------------------------------------
sp <- readRDS(here("data", params$subDir, "dly_data_sp.rds"))
factor <- readRDS(here("data", params$subDir, "dly_factor.rds"))

# Generate groups ---------------------------------------------------------
ghg_df <- sp %>%
  select(date, permno, ghg) %>% 
  group_by(yearmon = as.yearmon(date, "%m/%Y"), permno) %>%
  summarize(ghg_mon = mean(ghg, na.rm = TRUE)) %>%
  group_by(permno) %>%
  mutate(rollwin = rollapplyr(ghg_mon, 
                              width = params$window,
                              FUN = function(x) mean(x, na.rm = TRUE), 
                              fill = NA, 
                              partial = TRUE,
                              align = "right")) %>%
  mutate_at(vars(ghg_mon, window), ~replace(., is.nan(.), NA)) %>% 
  group_by(yearmon) %>% 
  mutate(q_25 = quantile(rollwin, probs = 0.25, na.rm = TRUE),
         q_75 = quantile(rollwin, probs = 0.75, na.rm = TRUE)) %>%
  mutate(status = case_when(is.na(rollwin)  ~ "U",
                            rollwin <= q_25 ~ "G",
                            rollwin >= q_75 ~ "B",                           
                            rollwin < q_75 & rollwin > q_25 ~ "N")) %>%
  arrange(permno)

# Transition Matrix -------------------------------------------------------
trans_mat <- ghg_df %>%
  select(yearmon, permno, status) %>%
  pivot_wider(names_from = permno, values_from = status) %>%
  ungroup() %>% 
  select(-yearmon) %>% 
  as.matrix()

mcFit <- markovchainFit(data = trans_mat, byrow = FALSE)

mcFit$estimate@transitionMatrix %>% 
  rbind(mcFit$upperEndpointMatrix, mcFit$lowerEndpointMatrix) %>% 
  write.csv(here("output", paste0(params$window,"m_", "transmat_daily.csv")))

# Alpha screening ---------------------------------------------------------
source(here::here("code", "source_daily.R"))

id_green <- ghg_df %>% f_create_id(yearmon, "G")
id_brown <- ghg_df %>% f_create_id(yearmon, "B")

# Factor
factor    <- if (params$factor == "all") factor else factor[, 1:3]
fctr_flag <- if (params$factor == "all") "6-factor model" else "3-factor model"

# Backward looking
input_date <- ghg_df %>% select(month) %>% distinct() %>% pluck() %>% as.matrix() %>% as.yearmon()
date_bw <- input_date[-(1:params$window - 1)]

result_g_bw <- f_alpha_screen(sp, factor, id = "id_green$", date_bw, model = "bw", params$window, params$ctr)
result_b_bw <- f_alpha_screen(sp, factor, id = "id_brown$", date_bw, model = "bw", params$window, params$ctr)

#Forward looking
date_fw <- input_date[params$window:(length(input_date) - params$window)]

result_g_fw <- f_alpha_screen(sp, factor, id = "id_green$", date_fw, model = "fw", params$window, params$ctr)
result_b_fw <- f_alpha_screen(sp, factor, id = "id_brown$", date_fw, model = "fw", params$window, params$ctr)

# Analysis ----------------------------------------------------------------
t1 <- result_g_bw %>% 
  f_summ_pi(date_bw) %>% 
  as_tibble() %>% 
  mutate(month = date_bw, 
         model = "result_g_bw") %>% 
  pivot_longer(-c(month, model))

t2 <- result_b_bw %>% 
  f_summ_pi(date_bw) %>% 
  as_tibble() %>% 
  mutate(month = date_bw, 
         model = "result_b_bw") %>% 
  pivot_longer(-c(month, model))

t3 <- result_g_fw %>% 
  f_summ_pi(date_fw) %>% 
  as_tibble() %>% 
  mutate(month = date_fw, 
         model = "result_g_fw") %>% 
  pivot_longer(-c(month, model))

t4 <- result_b_fw %>% 
  f_summ_pi(date_fw) %>% 
  as_tibble() %>% 
  mutate(month = date_fw, 
         model = "result_b_fw") %>% 
  pivot_longer(-c(month, model))

rbind(t1, t2, t3, t4) %>% 
  ggplot(aes(month, value, color = name)) +
  geom_line(size = 1.5) +
  facet_wrap(~model, ncol = 1) +
  labs(title = paste0("Mean Out/Under-Performance Ratios using", params$window, "-month rolling window."),
       subtitle = "Backward-looking and forward-looking for Green and Brown firms.",
       x = "Year",
       y = "Percent") +
  scale_y_continuous(labels = percent)

ggsave(here("Output", "figures", paste0(params$window, "m_daily_mean_pipos_pineg.png")))
         