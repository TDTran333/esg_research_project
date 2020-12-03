library(tidyverse)
library(magrittr)
library(PeerPerformance)
library(scales)
library(forcats)

data(hfdata)

rets <-  hfdata[,1:50]
ctr <- list(nCore = 3)
df <- alphaScreening(rets, control = ctr)

pi <- df[8:10] %>% 
  bind_rows() %>%
  select(pipos, pizero, pineg) %>% 
  arrange(desc(pipos), pineg, pizero) %>% 
  mutate(index = index(.)) %>%
  pivot_longer(1:3, names_to = "type", values_to = "pi") %>% 
  mutate(type = factor(type, levels = c("pineg", "pizero", "pipos")))

pi %>% 
  ggplot(aes(index, pi, fill = type, color = type)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_y_continuous(labels = percent) +
  coord_flip() + 
  theme(axis.title.y = element_blank(),
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank())

pi %>% 
  ggplot(aes(index, pi, fill = type)) +
  geom_bar(position = "stack", stat = "identity") +
  coord_flip() +
  labs(title = "Probability of Out-/Equal-/Under-performance")


pi %>% 
  filter(type == "pizero") %>% 
  ggplot(aes(index, pi)) +
  geom_bar( stat = "identity")


f_make_graphs <- function(df) {
  pi <- df[8:10] %>% 
    bind_rows() %>% 
    select(pipos, pizero, pineg) %>%
    arrange(desc(pipos), pineg, pizero)
  
  cex = cex.axis = 0.8
  par(mfrow = c(1, 2))
  plot(12 * 100 * sort(df$alpha, decreasing = TRUE), 1:length(df$alpha), type = 'b', las = 1, pch = 20, cex = cex, cex.axis = cex.axis, 
       xlab = "", ylab = "", main = "alpha", axes = FALSE)
  box(); grid()
  labs = seq(-30, 30, by = 10)
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
}

f_make_graphs(df)
