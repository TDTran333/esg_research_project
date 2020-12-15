library(tidyverse)
library(zoo)
library(lubridate)

load(here::here("data", "2009-2017", "mthly_data_sp.rda"))
mthly_data_sp <- mthly_data_sp %>% 
  mutate(ret_rf = ret_rf / 100)
mthly_data_sp[mthly_data_sp$date == "2016-03-01" & mthly_data_sp$permno == "78170", "ret_rf"] <- mthly_data_sp[mthly_data_sp$date == "2016-03-01" & mthly_data_sp$permno == "78170", "ret_rf"]/100

load(here::here("data", "2009-2017", "mthly_factor_mat.rda"))

avg_ghg <- mthly_data_sp %>%
  filter(spmim == 10) %>% 
  group_by(permno) %>% 
  summarize(avg_ghg = mean(ghg, na.rm = TRUE)) %>%
  filter(!is.nan(avg_ghg)) 

avg_ghg %>% 
  ggplot(aes(avg_ghg)) +
  geom_histogram()

ghg_df <- mthly_data_sp %>%
  select(date, permno, ghg) %>% 
  group_by(year(date), permno) %>%
  summarize(avg_ghg_y = mean(ghg, na.rm = TRUE)) %>%
  group_by(permno) %>%
  mutate(avg_ghg_5y = rollapplyr(avg_ghg_y, 
                             width = 5,
                             FUN = function(x) mean(x, na.rm = TRUE), 
                             fill = NA, 
                             partial = TRUE,
                             align = "right")) %>%
  mutate_at(vars(avg_ghg_5y), ~replace(., is.nan(.), NA)) %>% 
  rename(c("year" = "year(date)")) %>% 
  group_by(year) %>% 
  mutate(q_25 = quantile(avg_ghg_5y, probs = 0.25, na.rm = TRUE),
         q_75 = quantile(avg_ghg_5y, probs = 0.75, na.rm = TRUE)) %>%
  mutate(status = case_when(is.na(avg_ghg_5y) ~ "U",
                            avg_ghg_5y <= q_25~ "G",
                            avg_ghg_5y >= q_75 ~ "B",                           
                            avg_ghg_5y < q_75 & avg_ghg_5y > q_25 ~ "N")) %>%
  arrange(permno)

color = c("#E41A1C", "#4DAF4A", "#377EB8", "#984EA3")

ghg_df %>% 
  group_by(year, status) %>% 
  count() %>% 
  ggplot(aes(year, n, fill = status)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = n),  position = position_stack(vjust = .5)) +
  scale_fill_manual(values = color) +
  scale_x_continuous(breaks = 2009:2017)


# Group -------------------------------------------------------------------

green_id <- ghg_df %>% 
  filter(status == "G") %>%
  select(year, permno) %>%
  group_by(year, permno) %>%
  pivot_wider(names_from = year, values_from = permno)

brown_id <- ghg_df %>% 
  filter(status == "B") %>%
  select(year, permno) %>%
  group_by(year, permno) %>%
  pivot_wider(names_from = year, values_from = permno)

ctr <- list(nCore = 1)
# Green - Backward --------------------------------------------------------

ret_g <- mthly_data_sp %>%
  filter(permno %in% green_id$`2013`[[1]]) %>%
  filter(year(date) <= 2013) %>% 
  select(date, permno, ret_rf) %>%
  pivot_wider(names_from = permno, values_from = ret_rf) %>%
  select(-date) %>% 
  as.matrix()

factor_mat <- mthly_factor %>% 
  filter(year(date) <= 2013) %>% 
  select(-date) %>%
  as.matrix()

result_g <- PeerPerformance::alphaScreening(ret_g, factors = factor_mat, control = ctr)
result_g %>% f_screenplot("Green firms - 6f models - year 2013")

ret_g <- mthly_data_sp %>%
  filter(permno %in% green_id$`2017`[[1]]) %>%
  filter(year(date) <= 2017, year(date) > 2012) %>% 
  select(date, permno, ret_rf) %>%
  pivot_wider(names_from = permno, values_from = ret_rf) %>%
  select(-date) %>% 
  as.matrix()

factor_mat <- mthly_factor %>% 
  filter(year(date) <= 2017, year(date) > 2012) %>% 
  select(-date) %>%
  as.matrix()

result_g <- PeerPerformance::alphaScreening(ret_g, factors = factor_mat, control = ctr)
result_g %>% f_screenplot("Green firms - 6f models - year 2017")

# Brown - Backward --------------------------------------------------------

ret_b <- mthly_data_sp %>%
  filter(permno %in% brown_id$`2013`[[1]]) %>%
  filter(year(date) <= 2013) %>% 
  select(date, permno, ret_rf) %>%
  pivot_wider(names_from = permno, values_from = ret_rf) %>%
  select(-date) %>% 
  as.matrix()

factor_mat <- mthly_factor %>% 
  filter(year(date) <= 2013) %>% 
  select(-date) %>%
  as.matrix()

result_b <- PeerPerformance::alphaScreening(ret_b, factors = factor_mat, control = ctr)
result_b %>% f_screenplot("Brown firms - 6f models - year 2013")

ret_b <- mthly_data_sp %>%
  filter(permno %in% brown_id$`2017`[[1]]) %>%
  filter(year(date) <= 2017, year(date) > 2012) %>% 
  select(date, permno, ret_rf) %>%
  pivot_wider(names_from = permno, values_from = ret_rf) %>%
  select(-date) %>% 
  as.matrix()

factor_mat <- mthly_factor %>% 
  filter(year(date) <= 2017, year(date) > 2012) %>% 
  select(-date) %>%
  as.matrix()

result_b <- PeerPerformance::alphaScreening(ret_b, factors = factor_mat, control = ctr)
result_b %>% f_screenplot("Brown firms - 6f models - year 2017")

mthly_data_sp %>%
  filter(permno %in% brown_id$`2017`[[1]]) %>%
  filter(year(date) <= 2017, year(date) > 2012) %>% 
  select(date, permno, ret_rf) %>%
  group_by(permno) %>% 
  summarize(sumret = sum(ret_rf)) %>% 
  arrange(desc(sumret)) %>% 
  head()


mthly_data_sp %>% 
  filter(permno == 78170)

# Green - Forward ---------------------------------------------------------

ret_g <- mthly_data_sp %>%
  filter(permno %in% green_id$`2009`[[1]]) %>%
  filter(year(date) <= 2014, year(date) >= 2010) %>% 
  select(date, permno, ret_rf) %>%
  pivot_wider(names_from = permno, values_from = ret_rf) %>%
  select(-date) %>% 
  as.matrix()

factor_mat <- mthly_factor %>% 
  filter(year(date) <= 2013) %>% 
  select(-date) %>%
  as.matrix()

result_g <- PeerPerformance::alphaScreening(ret_g, factors = factor_mat, control = ctr)
result_g %>% f_screenplot("Green firms - 6f models - year 2009")

ret_g <- mthly_data_sp %>%
  filter(permno %in% green_id$`2012`[[1]]) %>%
  filter(year(date) <= 2017, year(date) >= 2013) %>% 
  select(date, permno, ret_rf) %>%
  pivot_wider(names_from = permno, values_from = ret_rf) %>%
  select(-date) %>% 
  as.matrix()

factor_mat <- mthly_factor %>% 
  filter(year(date) <= 2017, year(date) > 2012) %>% 
  select(-date) %>%
  as.matrix()

result_g <- PeerPerformance::alphaScreening(ret_g, factors = factor_mat, control = ctr)
result_g %>% f_screenplot("Green firms - 6f models - year 2012")

# Brown - Forward ---------------------------------------------------------

ret_b <- mthly_data_sp %>%
  filter(permno %in% brown_id$`2009`[[1]]) %>%
  filter(year(date) <= 2014, year(date) >= 2010) %>% 
  select(date, permno, ret_rf) %>%
  pivot_wider(names_from = permno, values_from = ret_rf) %>%
  select(-date) %>% 
  as.matrix()

factor_mat <- mthly_factor %>% 
  filter(year(date) <= 2013) %>% 
  select(-date) %>%
  as.matrix()

result_b <- PeerPerformance::alphaScreening(ret_b, factors = factor_mat, control = ctr)
result_b %>% f_screenplot("Brown firms - 6f models - year 2009")

ret_b <- mthly_data_sp %>%
  filter(permno %in% brown_id$`2012`[[1]]) %>%
  filter(year(date) <= 2017, year(date) >= 2013) %>% 
  select(date, permno, ret_rf) %>%
  pivot_wider(names_from = permno, values_from = ret_rf) %>%
  select(-date) %>% 
  as.matrix()

factor_mat <- mthly_factor %>% 
  filter(year(date) <= 2017, year(date) > 2012) %>% 
  select(-date) %>%
  as.matrix()

result_b <- PeerPerformance::alphaScreening(ret_b, factors = factor_mat, control = ctr)
result_b %>% f_screenplot("Brown firms - 6f models - year 2012")










f_screenplot <- function(df, title) {
  pi <- df[8:10] %>% 
    bind_rows() %>% 
    select(pipos, pizero, pineg) %>%
    arrange(desc(pipos), pineg, pizero)
  
  cex = cex.axis = 0.8
  par(mfrow = c(1, 2))
  plot(12 * 100 * sort(df$alpha, decreasing = TRUE), 1:length(df$alpha), type = 'b', las = 1, pch = 20, cex = cex, cex.axis = cex.axis, 
       xlab = "", ylab = "", main = "alpha", axes = FALSE)
  box(); grid()
  labs = seq(-100, 100, by = 10)
  axis(side = 1, at = labs, labels = paste0(labs, "%"), cex.axis = cex.axis)
  axis(side = 2, at = 1:length(df$alpha), labels = 1:length(df$alpha), las = 1, cex.axis = cex.axis)
  
  mainstr = expression(hat(pi)^'+'*' / '*hat(pi)^0*' / '*hat(pi)^'-')
  barplot(t(100 * pi), horiz = TRUE, names.arg = NULL, col = c(gray(0), gray(0.8), gray(0.5)),
          space = 0, xlab = "", xlim = c(0, 100), cex.names = cex.axis, border = NA,
          cex.axis = cex.axis, main = mainstr, las = 1, axes = FALSE)
  labs = seq(0, 100, by = 25)
  axis(side = 1, at = labs, labels = paste0(labs, "%"), cex.axis = cex.axis)
  box()
  x = seq(from = 104, to = -4, length.out = 200)
  y = seq(from = 0, to = 100, length.out = 200)
  par(new = TRUE); plot(x, y, type = 'l', lty = "dashed", lwd = 1, xlim = c(0, 100), ylim = c(0, 100), axes = FALSE, ylab = "", xlab = "")
  par(new = TRUE); plot(x-27.5, y, type = 'l', lty = "dashed", lwd = 1, xlim = c(0, 100), ylim = c(0, 100), axes = FALSE, ylab = "", xlab = "")
  par(new = TRUE); plot(x+27.5, y, type = 'l', lty = "dashed", lwd = 1, xlim = c(0, 100), ylim = c(0, 100), axes = FALSE, ylab = "", xlab = "")
  mtext(title, side = 3, line = -1.5, outer = TRUE, cex = 1.25)
}
