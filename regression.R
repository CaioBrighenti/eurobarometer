library(foreign)
library(tidyverse)
library(lme4)
'%!in%' <- function(x,y)!('%in%'(x,y))

dat <- read.dta("data/ZA3521_v2-0-1.dta") %>%
  as_tibble()

nation_years <- dat %>%
  dplyr::select(nation1, year) %>%
  group_by(nation1) %>%
  summarise(total_years = n_distinct(year)) %>%
  mutate(join_year = 2003 - total_years)

## DEMOGRAPHICS
dat_demo <- dat %>%
  group_by(nation1,year) %>%
  filter(income %in% c(1,2,3,4,5,6,7,8,9,10,11,12,13),
         sex != "inap") %>%
  mutate(sex_num = ifelse(sex == "male",1,0)) %>%
  dplyr::select(id,nation1,year,income,sex_num, age)
  # summarize(income = mean(income, na.rm = TRUE),
  #           male_prop = mean(sex_num),
  #           age = mean(age))

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
dat_mod <- dat %>%
  dplyr::select(id,nation1,matpmat) %>%
  filter(matpmat %in% c("materialist", "postmat","mixed")) %>%
  mutate(mat_ratio = ifelse(matpmat == "materialist",1,
                            ifelse(matpmat == "postmaterialist",0,
                                   0.5))) %>%
  dplyr::select(-matpmat) %>%
  inner_join(dat_demo, by=c("id","nation1"))
dat_mod_eu <- dat_mod %>%
  mutate(nation2 = ifelse(nation1 %in% c("denmark","spain"),nation1,"eu"))
  


mod <- lm(mat_ratio~as.factor(year)+as.factor(income)+as.factor(sex_num)+age,data=dat_mod)
summary(mod)

coefs <- coef(mod) %>%
  enframe() %>% 
  filter(grepl("year",name)) %>%
  mutate(year = as.numeric(gsub("as.factor(year)","",name,fixed=TRUE)),coef = value+0.6312) %>%
  dplyr::select(year, coef)

coefs %>%
  right_join(eu_mat,by=c("year")) %>%
  gather(group,val, coef:eu_mat_index) %>%
  ggplot(aes(x=year,group=1)) +
  geom_line(aes(y=val,group=group,color=group)) +
  geom_point(aes(y=val,color=group))


## MIXED EFFECTS MODEL
dat_me <- dat_mod_eu %>%
  mutate(mat_ratio = as.numeric(mat_ratio),
         year = as.factor(year),
         income = as.numeric(income),
         nation2 = as.factor(nation2)) %>%
  left_join(nation_years,by=c("nation1")) %>%
  mutate(years_since_joining = as.numeric(as.character(year)) - join_year)
dat_me_sample <- dat_me[sample(seq(1,nrow(dat_me)),size=10000),]

mod.me <- lmer(mat_ratio ~ years_since_joining+(1|nation1),
                  data=dat_me_sample)
summary(mod.me)
coef(mod.me)
