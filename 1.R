library(foreign)
library(tidyverse)
'%!in%' <- function(x,y)!('%in%'(x,y))

dat <- read.dta("ZA3521_v2-0-1.dta") %>%
  as_tibble()

dat %>%
  filter(!is.na(valpri1)) %>%
  dplyr::select(nation1, year) %>%
  group_by(nation1) %>%
  table()

## MATERIALIST
dat_mat <- dat %>%
  filter(matpmat %in% c("materialist", "postmat")) %>%
  group_by(year, nation1) %>%
  count(matpmat) %>%
  spread(matpmat,n) %>%
  mutate(total = materialist + postmat, mat_prop = materialist / total, post_prop = postmat / total) %>%
  filter(nation1 %!in% c("GERMANY-EAST", "norway", "finland","sweden","austria"))
eu_mat <- dat_mat %>%
  filter(nation1 %!in% c("denmark", "spain")) %>%
  group_by(year) %>%
  summarise(eu_mat_prop = mean(mat_prop), eu_post_prop = mean(post_prop))
dat_mat %>%
  left_join(eu_mat, by=c("year")) %>%
  ggplot(aes(x=year)) +
  geom_line(aes(y=mat_prop,color="Materialist")) +
  geom_line(aes(y=post_prop, color="Post-Materialist")) +
  geom_line(aes(y=eu_mat_prop, color = "Materialist"), alpha=0.5, linetype="longdash")+
  geom_line(aes(y=eu_post_prop, color = "Post-Materialist"), alpha=0.5, linetype="longdash")+
  facet_wrap(~nation1)

dat_mat %>%
  left_join(eu_mat, by=c("year")) %>%
  ggplot(aes(x=year)) +
  geom_line(aes(y=mat_prop,color=nation1)) +
  geom_line(aes(y=eu_mat_prop, color = "EU Average"), alpha=0.5, linetype="longdash")
dat_mat %>%
  left_join(eu_mat, by=c("year")) %>%
  ggplot(aes(x=year)) +
  geom_line(aes(y=post_prop,color=nation1)) +
  geom_line(aes(y=eu_post_prop, color = "EU Average"), alpha=0.5, linetype="longdash")

## HAPPINSS
dat %>%
  filter(happinss %in% c("VERY HAPPY", "PRETTY HAPPY", "NOT TOO HAPPY")) %>%
  group_by(year,nation1) %>%
  count(happinss) %>%
  spread(happinss, n) %>%
  mutate(total = `VERY HAPPY` + `PRETTY HAPPY` + `NOT TOO HAPPY`) %>%
  mutate(`VERY HAPPY` = `VERY HAPPY` / total, 
         `PRETTY HAPPY` = `PRETTY HAPPY` / total, 
         `NOT TOO HAPPY` = `NOT TOO HAPPY` / total) %>%
  ggplot(aes(x=year)) +
  geom_line(aes(y=`VERY HAPPY`, color='VERY HAPPY')) +
  geom_line(aes(y=`PRETTY HAPPY`, color='PRETTY HAPPY')) +
  geom_line(aes(y=`NOT TOO HAPPY`, color='NOT TOO HAPPY')) +
  facet_wrap(~nation1)

## VAL PRI
table(dat$valpri1)
dat_valpri <- dat %>%
  filter(nation1 %!in% c("finland", "sweden", "austria")) %>%
  filter(valpri1 %in% c("MAINTAIN ORDER", "MORE PARTICIP.", "FIGHT INFLATION", "FREEDOM OF SPEECH")) %>%
  group_by(year, nation1) %>%
  count(valpri1) %>%
  spread(valpri1, n) %>%
  mutate(total =  `MAINTAIN ORDER` + `MORE PARTICIP.` + `FIGHT INFLATION` + `FREEDOM OF SPEECH`) %>%
  mutate(`MAINTAIN ORDER` = `MAINTAIN ORDER` / total,
         `MORE PARTICIP.` = `MORE PARTICIP.` /total,
         `FIGHT INFLATION` = `FIGHT INFLATION` / total,
         `FREEDOM OF SPEECH` = `FREEDOM OF SPEECH` /total)

eu_valpri <- dat_valpri %>%
  filter(nation1 %!in% c("denmark", "spain")) %>%
  group_by(year) %>%
  summarise(eu_order = mean(`MAINTAIN ORDER`), 
            eu_particip = mean(`MORE PARTICIP.`),
            eu_inflation = mean(`MORE PARTICIP.`),
            eu_freedom = mean(`FREEDOM OF SPEECH`))

dat_valpri %>%
  left_join(eu_valpri, by=c("year")) %>%
  ggplot(aes(x=year)) +
  geom_line(aes(y=`MAINTAIN ORDER`, color="Maintain Order")) +
  geom_line(aes(y=eu_order, color="Maintain Order"), alpha = 0.5, linetype="longdash") +
  geom_line(aes(y=`MORE PARTICIP.`, color="More Participation")) +
  geom_line(aes(y=eu_particip, color="More Participation"), alpha = 0.5, linetype="longdash") +
  geom_line(aes(y=`FIGHT INFLATION`, color="Fight Inflation")) +
  geom_line(aes(y=eu_inflation, color="Fight Inflation"), alpha = 0.5, linetype="longdash") +
  geom_line(aes(y=`FREEDOM OF SPEECH`, color="Freedom of Speech")) +
  geom_line(aes(y=eu_freedom, color="Freedom of Speech"), alpha = 0.5, linetype="longdash") +
  facet_wrap(~nation1)
