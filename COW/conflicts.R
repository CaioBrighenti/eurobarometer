#####################################
######### LOAD DATA/LIBS ############
#####################################
library(tidyverse)
library(data.table)
str <- "COW/MID 4.3/MIDB\ 4.3.csv"
str_cc <- "COW/ccodes.csv"
cc <- read.csv(str_cc) %>% as_tibble()
eu_countries <- read.csv("data/join_dates.csv") %>% as_tibble() %>% rename(country = `ï..country`)
## NEED TO FIX GERMAN LABELING AND LOOK FOR OTHER COUNTRIES WITH ISSUES
dat <- fread(str) %>% 
  as_tibble() %>% 
  left_join(cc,by=c("stabb" = "StateAbb", "ccode" = "CCode")) %>%
  left_join(eu_countries,by=c("StateNme" = "country")) %>%
  filter(!is.na(year_joined))


#####################################
######## COUNT CONFLICTS ############
#####################################
## denmark
den_conflicts <- dat %>%
  filter(stabb == "DEN", styear > 1945) %>%
  dplyr::select(dispnum3,stabb,StateNme,styear,stmon,endyear,endmon,fatality,hiact,orig) %>%
  distinct(dispnum3, .keep_all = TRUE) %>%
  arrange(styear,stmon)

## spain
spain_conflicts <- dat %>%
  filter(stabb == "SPN", styear > 1945) %>%
  dplyr::select(dispnum3,stabb,StateNme,styear,stmon,endyear,endmon,fatality,hiact,orig) %>%
  distinct(dispnum3, .keep_all = TRUE) %>%
  arrange(styear,stmon)

## eu
eu_conflicts <- dat %>%
  filter(styear > 1945)  %>%
  dplyr::select(dispnum3,stabb,StateNme,styear,stmon,endyear,endmon,fatality,hiact,orig,year_joined) %>%
  distinct(dispnum3, .keep_all = TRUE) %>%
  arrange(styear,stmon) %>%
  filter(styear >= year_joined)


  
  
### counts
years <- tibble(styear = seq(1945,2019))
### denmark
den_counts <- den_conflicts %>%
  group_by(styear) %>%
  count() %>%
  right_join(years) %>%
  mutate(denmark = ifelse(is.na(n),0,n)) %>%
  dplyr::select(-n)

### spain
spain_counts <- spain_conflicts %>%
  group_by(styear) %>%
  count() %>%
  right_join(years) %>%
  mutate(spain = ifelse(is.na(n),0,n)) %>%
  dplyr::select(-n)

### eu
eu_counts <- eu_conflicts %>%
  group_by(styear) %>%
  count() %>%
  right_join(years) %>%
  mutate(eu = ifelse(is.na(n),0,n)) %>%
  dplyr::select(-n)

### PLOTS
den_counts %>%
  left_join(spain_counts) %>%
  left_join(eu_counts) %>%
  gather(country,n,`denmark`:`eu`) %>%
  ggplot(aes(x=styear,y=n)) +
  geom_col(aes(fill=country),position = "dodge")


