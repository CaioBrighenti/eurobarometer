#####################################
######### LOAD DATA/LIBS ############
#####################################
library(foreign)
library(tidyverse)
library(lme4)
library(tsibble)
library(imputeTS)
library(forecast)
library(cowplot)
'%!in%' <- function(x,y)!('%in%'(x,y))

dat <- read.dta("data/ZA3521_v2-0-1.dta") %>%
  as_tibble() %>%
  filter(nation1 != "norway")

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
  summarise(eu_mat_index = weighted.mean(mat_index,total))

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
p1 <- mat_trends %>%
  ggplot(aes(x=year)) +
  geom_line(aes(y=eu,color="EU"),linetype="dotted",lwd=1.5) +
  geom_line(aes(y=denmark,color="Denmark"),lwd=1.5) +
  geom_line(aes(y=spain,color="Spain"),lwd=1.5) +
  theme_minimal() +
  geom_vline(xintercept = c(1973,1986),linetype="dotted") +
  theme_bw() +
  scale_color_grey() +
  ggtitle("Materialism in European countries over time") +
  xlab("year") +
  ylab("materialism index")

# distance plot
p2 <- mat_trends %>%
  filter(!is.na(eu)) %>%
  mutate(dist_den = eu - denmark,dist_spa = eu - spain) %>%
  ggplot(aes(x=year)) +
  geom_line(aes(y=dist_den,color="Denmark"),lwd=1.5) +
  geom_line(aes(y=dist_spa,color="Spain"),lwd=1.5) +
  geom_hline(yintercept=0,linetype="longdash") +
  theme_bw() +
  scale_color_grey() +
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
  summarise(eu_membership_index = weighted.mean(membership_index,total))

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

## All
# all_membership <- dat_membership %>%
#   dplyr::select(year,nation1,membership_index) %>%
#   spread(nation1,membership_index)
# all_membership <- years %>% left_join(all_membership,by=c("year")) %>% na_interpolation()
# all_membership_ts <- as.ts(all_membership[,-1])
# trend_all_membership = ma(all_membership_ts,order=4,centre=T)

## PLOTS
membership_trends <- tibble(
  year = seq(1970,1999),
  eu = trend_eu_membership,
  denmark = trend_den_membership,
  spain = trend_spa_membership
)  %>%
  mutate(denmark = ifelse(year < 1973,NA,denmark),
         spain = ifelse(year < 1986,NA,spain))

# membership_trends2 <- as_tibble(trend_all_membership)
# names(membership_trends2) <- names(all_membership)[-1]
# membership_trends2 <- membership_trends2 %>% 
#   mutate(year=seq(1970,1999)) %>%
#   gather(nation1,membership_index,`france`:`austria`) %>%
#   right_join(years_joined,by=c("nation1")) %>%
#   mutate(membership_index = ifelse(year < year_joined,NA,membership_index))

# trend plot
p3 <- membership_trends %>%
  ggplot(aes(x=year)) +
  geom_line(aes(y=eu,color="EU"),linetype="dotted",lwd=1.5) +
  geom_line(aes(y=denmark,color="Denmark"),lwd=1.5) +
  geom_line(aes(y=spain,color="Spain"),lwd=1.5) +
  geom_vline(xintercept = c(1973,1986),linetype="dotted") +
  theme_bw() +
  scale_color_grey() +
  ggtitle("Support for EU membership in European countries over time") +
  xlab("year") +
  ylab("support for membership")

# membership_trends2 %>%
#   ggplot(aes(x=year,y=membership_index))+
#   geom_line(aes(color=nation1),lwd=1) +
#   geom_line(data = membership_trends,aes(x=year,y=eu,color="EU"),linetype="dotted",lwd=1) +
#   theme_bw() +
#   ggtitle("Support for EU membership in European countries over time") +
#   xlab("year") +
#   ylab("support for membership") +
#   facet_wrap(~year_joined)

# distance plot
p4 <- membership_trends %>%
  filter(!is.na(eu)) %>%
  mutate(dist_den = eu - denmark,dist_spa = eu - spain) %>%
  ggplot(aes(x=year)) +
  geom_line(aes(y=dist_den,color="Denmark"),lwd=1.5) +
  geom_line(aes(y=dist_spa,color="Spain"),lwd=1.5) +
  geom_hline(yintercept=0,linetype="longdash") +
  geom_vline(xintercept = c(1973,1986),linetype="dotted") +
  theme_bw() +
  scale_color_grey() +
  ggtitle("Distance from EU in support for EU membership") +
  xlab("year") +
  ylab("distance from EU average")


plot_grid(p1,p2,p3,p4, labels = c('A', 'B', 'C', 'D'), label_size = 12)

