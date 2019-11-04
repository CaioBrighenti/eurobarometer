source("trends.R")

#####################################
######## MAT.RATIO MODEL ############
#####################################
years_joined <- read.csv("data/join_dates.csv") %>% as_tibble() %>% dplyr::select(nation1,year_joined)

eu_mat2 <- dat_mat %>%
  group_by(year) %>%
  summarise(eu_mat_index = weighted.mean(mat_index,total))

dat_mat2 <- dat_mat %>%
  dplyr::select(year,nation1,mat_index) %>%
  left_join(eu_mat2,by=c("year")) %>%
  rename(eu_avg = eu_mat_index) %>%
  mutate(dist = abs(mat_index - eu_avg)) %>%
  left_join(years_joined,by=c("nation1")) %>%
  mutate(years_since_join = year - year_joined)


dat_mat2 %>%
  filter(year_joined != 1952) %>%
  ggplot(aes(x=years_since_join,y=dist)) +
  geom_line(aes(color=nation1)) +
  geom_hline(yintercept=0,linetype="dotted") +
  facet_wrap(~year_joined)


#####################################
######### MEMBERSHIP GOOD ###########
#####################################
eu_membership2 <- dat_membership %>%
  group_by(year) %>%
  summarise(eu_membership_index = weighted.mean(membership_index,total))

dat_membership2 <- dat_membership %>%
  dplyr::select(year,nation1,membership_index) %>%
  left_join(eu_membership2,by=c("year")) %>%
  rename(eu_avg = eu_membership_index) %>%
  mutate(dist = abs(membership_index - eu_avg)) %>%
  left_join(years_joined,by=c("nation1")) %>%
  mutate(years_since_join = year - year_joined)


dat_membership2 %>%
  filter(year_joined != 1952) %>%
  ggplot(aes(x=years_since_join,y=dist)) +
  geom_line(aes(color=nation1)) +
  geom_smooth() +
  geom_hline(yintercept = 0,linetype="dotted") +
  facet_wrap(~year_joined)