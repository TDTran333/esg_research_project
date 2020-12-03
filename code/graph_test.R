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
  mutate(type = factor(type, levels = c("pipos", "pizero", "pineg")))

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
  coord_flip()


pi %>% 
  filter(type == "pipos") %>% 
  ggplot(aes(index, pi)) +
  geom_bar( stat = "identity")


data(hfdata)
rets <-  hfdata[,1:50]
ctr <- list(nCore = 3)
df <- alphaScreening(rets, control = ctr)
test <- f.tbl.screening(df, 60)


.f.tbl.screening = function(hf, nblock, do.norm = TRUE){
  pos = order(hf$alpha, decreasing = TRUE)
  n = nrow(hf)
  ngroup = floor(n / nblock)
  tbl = matrix(data = NA, nrow = nblock, ncol = 4)
  for (i in 1 : nblock){
    idx = pos[((i-1)*ngroup+1) : (i*ngroup)]
    if (i == nblock){
      idx = pos[((i-1)*ngroup+1) : n]
    }
    idx = idx[!is.na(idx)]
    tbl[i,1]   = mean(hf$alpha[idx])
    tbl[i,2:4] = colMeans(hf[idx,c("pipos", "pizero", "pineg")])
  }
  if (do.norm) {
    #browser()
    tmp = tbl[,2:4]
    tmp = sweep(tmp, 2, rowSums(tmp), "/")
    tbl[,2:4] = tmp 
  }
  return(tbl)
}
f.tbl.screening = compiler::cmpfun(.f.tbl.screening)
