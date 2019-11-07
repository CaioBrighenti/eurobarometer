#####################################
######### LOAD DATA/LIBS ############
#####################################
library(tidyverse)
library(data.table)
library(cowplot)
str <- "COW/MID 4.3/MIDB\ 4.3.csv"
str_cc <- "COW/ccodes.csv"
cc <- read.csv(str_cc) %>% as_tibble()
eu_countries <- read.csv("data/join_dates.csv") %>% as_tibble() %>% rename(country = `ï..country`)
eu_years <- read.csv("data/eu_timeline.csv") %>% as_tibble() %>% rename(year = `ï..year`)
## NEED TO FIX GERMAN LABELING AND LOOK FOR OTHER COUNTRIES WITH ISSUES
dat <- fread(str) %>% 
  as_tibble() %>% 
  left_join(cc,by=c("stabb" = "StateAbb", "ccode" = "CCode")) %>%
  left_join(eu_countries,by=c("StateNme" = "country")) %>%
  filter(!is.na(year_joined))


#####################################
######## COUNT CONFLICTS ############
#####################################

#### FILTER ####
## denmark
den_conflicts <- dat %>%
  filter(stabb == "DEN", styear > 1952) %>%
  dplyr::select(dispnum3,stabb,StateNme,styear,stmon,endyear,endmon,fatality,hostlev,orig) %>%
  distinct(dispnum3, .keep_all = TRUE) %>%
  arrange(styear,stmon)

## spain
spain_conflicts <- dat %>%
  filter(stabb == "SPN", styear > 1952) %>%
  dplyr::select(dispnum3,stabb,StateNme,styear,stmon,endyear,endmon,fatality,hostlev,orig) %>%
  distinct(dispnum3, .keep_all = TRUE) %>%
  arrange(styear,stmon)

## eu
eu_conflicts <- dat %>%
  filter(styear > 1952)  %>%
  dplyr::select(dispnum3,stabb,StateNme,styear,stmon,endyear,endmon,fatality,hostlev,orig,year_joined) %>%
  distinct(dispnum3, .keep_all = TRUE) %>%
  arrange(styear,stmon) %>%
  filter(styear >= year_joined)


#### GET COUNTS ####
years <- tibble(styear = seq(1952,2019))
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
  dplyr::select(-n) %>%
  left_join(eu_years,by=c("styear" = "year")) %>%
  mutate(eu_avg = eu / num_countries)

#### PLOTS ####
year_bins <- c("1952-54","1955-59","1960-64","1965-69","1970-74","1975-79","1980-84",
               "1985-89","1990-94","1995-99","2000-03")
all_counts <- den_counts %>%
  left_join(spain_counts) %>%
  left_join(eu_counts) %>%
  dplyr::select(-eu,-num_countries) %>%
  mutate(year_bin = floor((styear - 1950) / 5) ) %>%
  filter(styear < 2003) %>%
  group_by(year_bin) %>%
  dplyr::select(-styear) %>%
  summarise_all(list(sum = sum)) %>%
  mutate(year_bin=year_bins)

all_counts %>%
  gather(country,n,`denmark_sum`:`eu_avg_sum`) %>%
  ggplot(aes(x=year_bin,y=n)) +
  geom_col(aes(fill=country),position = "dodge") +
  theme_bw() +
  scale_x_discrete(limits = levels(all_counts$year_bin)) + 
  ggtitle("Number of interstate conflicts or disputes over time") +
  ylab("year period") +
  xlab("number of conflicts or disputes")

#####################################
##### CONFLICT INTENSITY ############
#####################################
### denmark
den_intensity <- den_conflicts %>%
  group_by(styear) %>%
  summarise(hostlev = mean(hostlev),fatality = mean(fatality)) %>%
  right_join(years) %>%
  mutate(den_hostlev = ifelse(is.na(hostlev),0,hostlev),
         den_fatality = ifelse(is.na(fatality),0,fatality)) %>%
  dplyr::select(-hostlev,-fatality)

### spain
spain_intensity <- spain_conflicts %>%
  group_by(styear) %>%
  summarise(hostlev = mean(hostlev),fatality = mean(fatality)) %>%
  right_join(years) %>%
  mutate(spain_hostlev = ifelse(is.na(hostlev),0,hostlev),
         spain_fatality = ifelse(is.na(fatality),0,fatality)) %>%
  dplyr::select(-hostlev,-fatality)

### eu
eu_intensity <- eu_conflicts %>%
  group_by(styear) %>%
  summarise(hostlev = mean(hostlev),fatality = mean(fatality)) %>%
  right_join(years) %>%
  mutate(eu_hostlev = ifelse(is.na(hostlev),0,hostlev),
         eu_fatality = ifelse(is.na(fatality),0,fatality)) %>%
  dplyr::select(-hostlev,-fatality)

## GROUP
all_intensity <- den_intensity %>%
  left_join(spain_intensity) %>%
  left_join(eu_intensity) %>%
  mutate(year_bin = floor((styear - 1950) / 5) ) %>%
  filter(styear < 2003) %>%
  group_by(year_bin) %>%
  dplyr::select(-styear) %>%
  summarise_all(list(mean = mean)) %>%
  mutate(year_bin=year_bins)

#####################################
########## BEFORE/AFTER #############
#####################################
den_counts %>%
  left_join(den_intensity) %>%
  mutate(EU = ifelse(styear >= 1973,"AFTER","BEFORE")) %>%
  group_by(EU) %>%
  summarize(mean_count = mean(denmark),den_hostlev = mean(den_hostlev))

spain_counts %>%
  left_join(spain_intensity) %>%
  mutate(EU = ifelse(styear >= 1986,"AFTER","BEFORE")) %>%
  group_by(EU) %>%
  summarize(mean_count = mean(spain),spain_hostlev = mean(spain_hostlev))


#####################################
########## FINAL PLOTS #############
#####################################
p1 <- all_counts %>%
  ggplot(aes(x=year_bin,group = 1)) +
  geom_line(aes(y=eu_avg_sum,color="EU average"),linetype="dotted",lwd=1.5) +
  geom_line(aes(y=denmark_sum,color="Denmark"),lwd=1.5) +
  geom_line(aes(y=spain_sum,color="Spain"),lwd=1.5) +
  geom_vline(xintercept = c(5,8),linetype="dotted") +
  theme_minimal() +
  scale_color_grey() +
  ggtitle("Number of interstate disputes in European countries over time") +
  xlab("year") +
  ylab("number of disputes")

p2 <- all_intensity %>%
  ggplot(aes(x=year_bin,group = 1)) +
  geom_line(aes(y=eu_hostlev_mean,color="EU average"),linetype="dotted",lwd=1.5) +
  geom_line(aes(y=den_hostlev_mean,color="Denmark"),lwd=1.5) +
  geom_line(aes(y=spain_hostlev_mean,color="Spain"),lwd=1.5) +
  geom_vline(xintercept = c(5,8),linetype="dotted") +
  theme_minimal() +
  scale_color_grey() +
  ggtitle("Average hostility level in European conflicts over time") +
  xlab("year") +
  ylab("average hostility level")

plot_grid(p1,p2,labels = c('E', 'F'), label_size = 12,nrow=2)

