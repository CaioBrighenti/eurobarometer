library(foreign)
library(tidyverse)
'%!in%' <- function(x,y)!('%in%'(x,y))

dat <- read.dta("ZA3521_v2-0-1.dta") %>%
  as_tibble()

dat %>%
  filter(!is.na(income)) %>%
  dplyr::select(nation1, year) %>%
  group_by(nation1) %>%
  table()

## DEMOGRAPHICS
dat_demo <- dat %>%
  dplyr::select(nation1,year,income) %>%
  group_by(nation1,year) %>%
  summarize(income = mean(income, na.rm = TRUE))

## MATERIALIST
dat_mat <- dat %>%
  filter(matpmat %in% c("materialist", "postmat","mixed")) %>%
  group_by(year, nation1) %>%
  count(matpmat) %>%
  spread(matpmat,n) %>%
  mutate(total = materialist + mixed + postmat,
         mat_index = ((1*materialist) + (.5 * mixed) + (0*postmat))/total)

eu_mat <- dat_mat %>%
  filter(nation1 %!in% c("denmark", "spain")) %>%
  group_by(year) %>%
  summarise(eu_mat_index = mean(mat_index))
dat_mat %>%
  left_join(eu_mat, by=c("year")) %>%
  ggplot(aes(x=year)) +
  geom_line(aes(y=mat_index,color="Materialist Index")) +
  geom_line(aes(y=eu_mat_index, color = "EU Average"), alpha=0.5, linetype="longdash")+
  facet_wrap(~nation1)


## model
dat_mod <- dat_mat %>%
  left_join(dat_demo, by=c("nation1","year"))
mod <- lm(mat_index~as.factor(year)+income,data=dat_mod)
summary(mod)

coefs <- coef(mod) %>%
  enframe() %>% 
  filter(grepl("year",name)) %>%
  mutate(year = gsub("as.factor(year)","",name,fixed=TRUE))

coefs %>%
  ggplot(aes(x=name,y=value,group=1)) +
  geom_line()
