#####################################
######### LOAD DATA/LIBS ############
#####################################
library(foreign)
library(tidyverse)
library(lme4)
library(tsibble)
library(imputeTS)
library(forecast)
'%!in%' <- function(x,y)!('%in%'(x,y))

dat <- read.dta("data/ZA3521_v2-0-1.dta") %>%
  as_tibble()

#####################################
######## MAT.RATIO TREND ############
#####################################
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

### TIME SERIES
years<-seq(1970,1999) %>% enframe() %>% dplyr::select(-name) %>% rename(year = value)
## EU 
eu_mat <- years %>% left_join(eu_mat,by=c("year")) %>% na_interpolation()
eu_mat_ts <- as.ts(eu_mat$eu_mat_index)
trend_eu_mat = ma(eu_mat_ts, order = 4, centre = T)

## Denmark
den_mat <- dat_mat %>%
  filter(nation1 == "denmark") %>%
  ungroup() %>%
  dplyr::select(year,mat_index)
den_mat <- years %>% left_join(den_mat,by=c("year")) %>% na_interpolation()
den_mat_ts <- as.ts(den_mat$mat_index)
trend_den_mat = ma(den_mat_ts, order = 4, centre = T) 

## Spain
spa_mat <- dat_mat %>%
  filter(nation1 == "spain") %>%
  ungroup() %>%
  dplyr::select(year,mat_index)
spa_mat <- years %>% left_join(spa_mat,by=c("year")) %>% na_interpolation()
spa_mat_ts <- as.ts(spa_mat$mat_index)
trend_spa_mat = ma(spa_mat_ts, order = 4, centre = T)

## PLOTS
mat_trends <- tibble(
  year = seq(1970,1999),
  eu = trend_eu_mat,
  denmark = trend_den_mat,
  spain = trend_spa_mat
) %>%
  mutate(denmark = ifelse(year < 1973,NA,denmark),
         spain = ifelse(year < 1986,NA,spain))

# trend plot
mat_trends %>%
  ggplot(aes(x=year)) +
  geom_line(aes(y=eu,color="EU"),linetype="dotted",lwd=1.5) +
  geom_line(aes(y=denmark,color="Denmark"),lwd=1.5) +
  geom_line(aes(y=spain,color="Spain"),lwd=1.5) +
  theme_minimal() +
  geom_vline(xintercept = c(1973,1986),linetype="dotted") +
  theme_bw() +
  ggtitle("Materialism in European countries over time") +
  xlab("year") +
  ylab("materialism index")

# distance plot
mat_trends %>%
  filter(!is.na(eu)) %>%
  mutate(dist_den = eu - denmark,dist_spa = eu - spain) %>%
  ggplot(aes(x=year)) +
  geom_line(aes(y=dist_den,color="Denmark"),lwd=1.5) +
  geom_line(aes(y=dist_spa,color="Spain"),lwd=1.5) +
  geom_hline(yintercept=0,linetype="longdash") +
  theme_bw() +
  ggtitle("Distance from EU in materialism level") +
  xlab("year") +
  ylab("distance from EU average")
  

#####################################
##### SUPPORT FOR INTEGRATION #######
#####################################
dat_membership <- dat %>%
  filter(membrshp %in% c("GOOD THING", "NEITHER NOR","BAD THING")) %>%
  group_by(year, nation1) %>%
  count(membrshp) %>%
  spread(membrshp,n) %>%
  mutate(total = `GOOD THING`+ `NEITHER NOR`+`BAD THING`,
         membership_index = ((`GOOD THING`*1)+ (`NEITHER NOR`*0.5)+(`BAD THING`*0))/total)

eu_membership <- dat_membership %>%
  filter(nation1 %!in% c("denmark", "spain")) %>%
  group_by(year) %>%
  summarise(eu_membership_index = mean(membership_index))

dat_membership %>%
  left_join(eu_membership, by=c("year")) %>%
  ggplot(aes(x=year)) +
  geom_line(aes(y=membership_index,color="membershiperialist Index")) +
  geom_line(aes(y=eu_membership_index, color = "EU Average"), alpha=0.5, linetype="longdash")+
  facet_wrap(~nation1)

### TIME SERIES
## EU 
eu_membership <- years %>% left_join(eu_membership,by=c("year")) %>% na_interpolation()
eu_membership_ts <- as.ts(eu_membership$eu_membership_index)
trend_eu_membership = ma(eu_membership_ts, order = 4, centre = T)

## Denmark
den_membership <- dat_membership %>%
  filter(nation1 == "denmark") %>%
  ungroup() %>%
  dplyr::select(year,membership_index)
den_membership <- years %>% left_join(den_membership,by=c("year")) %>% na_interpolation()
den_membership_ts <- as.ts(den_membership$membership_index)
trend_den_membership = ma(den_membership_ts, order = 4, centre = T)

## Spain
spa_membership <- dat_membership %>%
  filter(nation1 == "spain") %>%
  ungroup() %>%
  dplyr::select(year,membership_index)
spa_membership <- years %>% left_join(spa_membership,by=c("year")) %>% na_interpolation()
spa_membership_ts <- as.ts(spa_membership$membership_index)
trend_spa_membership = ma(spa_membership_ts, order = 4, centre = T)

## PLOTS
membership_trends <- tibble(
  year = seq(1970,1999),
  eu = trend_eu_membership,
  denmark = trend_den_membership,
  spain = trend_spa_membership
)  %>%
  mutate(denmark = ifelse(year < 1973,NA,denmark),
         spain = ifelse(year < 1986,NA,spain))

# trend plot
membership_trends %>%
  ggplot(aes(x=year)) +
  geom_line(aes(y=eu,color="EU"),linetype="dotted",lwd=1.5) +
  geom_line(aes(y=denmark,color="Denmark"),lwd=1.5) +
  geom_line(aes(y=spain,color="Spain"),lwd=1.5) +
  geom_vline(xintercept = c(1973,1986),linetype="dotted") +
  theme_bw() +
  ggtitle("Support for EU membership in European countries over time") +
  xlab("year") +
  ylab("support for membership")

# distance plot
membership_trends %>%
  filter(!is.na(eu)) %>%
  mutate(dist_den = eu - denmark,dist_spa = eu - spain) %>%
  ggplot(aes(x=year)) +
  geom_line(aes(y=dist_den,color="Denmark"),lwd=1.5) +
  geom_line(aes(y=dist_spa,color="Spain"),lwd=1.5) +
  geom_hline(yintercept=0,linetype="longdash") +
  theme_bw() +
  ggtitle("Distance from EU in support for EU membership") +
  xlab("year") +
  ylab("distance from EU average")
