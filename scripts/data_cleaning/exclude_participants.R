## Exclude participants

## create 

rewards_bt <-
  read_rds("A_GeneratedFiles/bootstrapped_random_rewards.rds")

# ## check what data would be excluded
# data_excluded <- data %>% 
#   ungroup() %>% 
#   group_by(uniqueID) %>%
#   mutate(p_value_rand = t.test(points, rewards_bt, alternative = "greater") %>%
#            .$p.value) %>%
#   ungroup() %>%
#   dplyr::filter(p_value_rand > 0.05) %>% 
#   select(uniqueID, group) %>% 
#   distinct() %>% 
#   group_by(group) %>% 
#   summarise(count = n())

# only take participants who are significantly better than random
data <- data %>% 
  ungroup() %>% 
  group_by(uniqueID) %>%
  mutate(p_value_rand = t.test(points, rewards_bt, alternative = "greater") %>% 
           .$p.value) %>%
  ungroup() %>% 
  dplyr::filter(p_value_rand < 0.05)

data <-
  data %>% 
  group_by(unique_rounds) %>%  
  mutate(
    mean_points = mean(points),
    sd_points = sd(points) ,
    z = (points - mean_points) / sd_points,
    gem_cell = choice[match(round_gem_found, trial)],
    #data_source = 'experiment', 
    gemlabel = ifelse(gempresent == 0, "gem absent", "gem present"))   


# final exclusion: participants who attempted to refresh task to find where gems are, and 1 round (?) with NAs
data <- data %>%
  dplyr::filter(attempt_refresh <= 0 ) %>%
  dplyr::filter(!is.na(points)) %>%
  dplyr::filter(!is.na(gempresent)) %>%
  dplyr::filter(!is.na(demo_type))

