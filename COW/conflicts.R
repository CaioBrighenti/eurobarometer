library(tidyverse)
str <- "COW/MID 4.3/MIDB\ 4.3.csv"
str_cc <- "COW/ccodes.csv"
cc <- read.csv(str_cc) %>% as_tibble()
dat <- read.csv(str) %>% as_tibble() %>% left_join(cc,by=c("stabb" = "StateAbb", "ccode" = "CCode"))


den_conflicts <- dat %>%
  filter(stabb == "DEN", styear > 1945) %>%
  dplyr::select(dispnum3,stabb,StateNme,styear,stmon,endyear,endmon,fatality,hiact,orig) %>%
  distinct(dispnum3, .keep_all = TRUE) %>%
  arrange(styear,stmon)

spain_conflicts <- dat %>%
  filter(stabb == "SPN", styear > 1945) %>%
  dplyr::select(dispnum3,stabb,StateNme,styear,stmon,endyear,endmon,fatality,hiact,orig) %>%
  distinct(dispnum3, .keep_all = TRUE) %>%
  arrange(styear,stmon)

den_conflicts %>%
  mutate(hiact = as.factor(hiact)) %>%
  ggplot(aes(x=styear)) +
  geom_density(aes(fill="Denmark")) +
  geom_vline(xintercept = 1973) +
  scale_fill_grey() +
  theme_bw() +
  ggtitle("Distribution of conflicts over time")


spain_conflicts %>%
  mutate(hiact = as.factor(hiact)) %>%
  ggplot(aes(x=styear)) +
  geom_density(aes(fill="Spain")) +
  geom_vline(xintercept = 1986)
