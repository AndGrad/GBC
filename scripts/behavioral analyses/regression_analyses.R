
## load packages and prepare data

## load packages
source("scripts/utils/check_pkgs.R")

## load data
all_data <- read_csv(file = paste0(here(), "/data/social/data_social_all_participants_08-2024.csv")) %>% 
  mutate(demo_quality_f = as.factor(demo_quality),
         age_f = factor(group, levels = c("adults", "adolescents")))

## social learning differences

## create count variable
data_regression_copy <- all_data %>%
  group_by(uniqueID, round, gem_found, age_f, demo_quality_f, treatment) %>%
  filter(social_info_use == "copy") %>%
  count(social_info_use)

## mean copy by group %>% 
data_regression_copy %>% 
  ungroup() %>% 
  #filter(n!=0) %>% 
  select(treatment, n ) %>% 
  group_by(treatment) %>% 
  summarise(mean_copy = mean(n),
            sd = sd (n))

if (file.exists(paste0(here(),'/modelfits/copy_age_advisor_model.RData'))) {
  

} else {
  
  prior_cauchy <- brms::prior_string("cauchy(0, .1)")
  
  ## specify model and fit
  copy_age_advisor_model <- brms::brm(formula = n ~ 1 + demo_quality_f * age_f + (1 + demo_quality_f | uniqueID),
                                      data = data_regression_copy,
                                      prior = prior_cauchy,
                                      family = poisson(),
                                      iter = 6000,
                                      chains = 6,
                                      cores = 6) 
  
  ## save results
  save(file = paste0(here(),'/G_Analysis_bevioral_data_social/modelfits/copy_age_advisor_model.RData'), copy_age_advisor_model)
  
}

## vizualize mean player point performance
all_data %>% 
  ungroup() %>% 
  group_by(uniqueID) %>% 
  mutate(mean_points_player = mean(tot_points, na.rm = TRUE)) %>% 
  select(mean_points_player, age_f,uniqueID) %>% 
  distinct() %>% 
  ungroup() %>%
  ggplot(aes(x=age_f, y=mean_points_player))+
  geom_boxplot() +
  geom_point() 

## just raw difference in points across conditions
# t_test_points_gem <- all_data %>% 
#   filter(gem_found == 1) %>% 
#   ungroup() %>% 
#   group_by(uniqueID) %>% 
#   mutate(mean_points_player = mean(tot_points, na.rm = TRUE)) %>% 
#   select(mean_points_player, age_f, uniqueID) %>% 
#   distinct() %>% 
#   ungroup() %>% 
#   stats::t.test(mean_points_player ~ age_f, data = ., alternative = "two.sided")

data_regression_points <- all_data %>%
  ungroup() %>%
  select(uniqueID, age_f, demo_quality_f, tot_points, treatment) %>% 
  distinct() %>% 
  group_by() %>%
  mutate(mean_points = mean(tot_points),
         sd = sd(tot_points),
         scaled_points = (tot_points - mean_points)/(sd*0.5))

data_regression_points_trial <- all_data %>%
  ungroup() %>%
  select(uniqueID, age_f, demo_quality_f,demo_quality_f, treatment, trial, points) %>% 
  mutate(mean_points = mean(points),
         sd = sd(points),
         scaled_points = (points - mean_points)/(sd*0.5)) 

#summary(lmer(scaled_points ~ age_f * trial  + (1 |  uniqueID), data = data_regression_points_trial))

## load model if fitting has been done, if not fit model

if #(file.exists(paste0(here(),'/G_Analysis_bevioral_data_social/modelfits/performance_age_demonstrator_model.RData')) & 
(file.exists(paste0(here(),'/modelfits/points_age_trial_advisor_model.RData')))
  #)  
{
    #
} else {
  
  ## define priors 
  #prior_cauchy <- brms::prior_string("normal(0, .1)")
  prior_normal <- brms::prior_string("normal(0, .1)")
  
  ## model specification
  points_age_trial_advisor_model <-
    brms::brm(formula = scaled_points ~ demo_quality_f * age_f * trial + (1|uniqueID),
              prior = prior_normal,
              sample_prior = TRUE,
              data = data_regression_points_trial,
              iter = 6000,
              chains = 6,
              cores = 6 )
  
  # points_age_trial_advisor_model2 <-
  #   brms::brm(formula = scaled_points ~ demo_quality_f * age_f + trial + (1|demo_quality_f | uniqueID),
  #             prior = prior_normal,
  #             sample_prior = TRUE,
  #             data = data_regression_points_trial,
  #             iter = 4000,
  #             chains = 6,
  #             cores = 6 )
  
  ## save
  save("points_age_trial_advisor_model",
       file = paste0(here(),'/G_Analysis_bevioral_data_social/modelfits/points_age_trial_advisor_model.RData'))
}
#

## Supplement


## Gem performance

## select only one observation per participant
data_regression_gem <- all_data %>%  
  filter(gem_found == 1) %>% 
  select(round_gem_found, age_f, demo_quality_f, uniqueID) %>% 
  distinct() 

## gem found when by group across all rounds
data_regression_gem %>% 
  ungroup() %>% 
  select(age_f, round_gem_found, demo_quality_f, uniqueID ) %>% 
  group_by(uniqueID) %>% 
  mutate( mean_round_gem_found = mean(round_gem_found),
          sd = sd (round_gem_found)) %>% 
  select(mean_round_gem_found, age_f,uniqueID) %>% 
  distinct() %>% 
  ungroup() %>% 
  stats::t.test(mean_round_gem_found ~ age_f, data = ., alternative = "two.sided", paired = FALSE)

## mean round gem found by group and social information use 
data_regression_gem %>% 
  ungroup() %>% 
  select(age_f, round_gem_found, demo_quality_f ) %>% 
  group_by(age_f, demo_quality_f) %>% 
  summarise( mean_round_gem_found = mean(round_gem_found),
             sd = sd(round_gem_found)) 

## load model if fitting has been done, if not fit model
if (file.exists(paste0(here(),'/G_Analysis_bevioral_data_social/modelfits/gem_age_advisor_model.RData'))){
  
  base::load(paste0(here(),'/G_Analysis_bevioral_data_social/modelfits/gem_age_advisor_model.RData'))
  
} else {
  
  ## model specification
  # gem_age_advisor_model <- brms::brm(formula = round_gem_found ~  demo_quality_f * age_f + (1 | uniqueID),
  #           data = data_regression_gem,
  #           family = poisson(),
  #           iter = 6000,
  #           chains = 6) 
  
  
  gem_freq_age_advisor_model <- all_data %>% 
    filter(gempresent == 1 ) %>% 
    select(uniqueID,  gem_found, round_gem_found, gempresent, age_f, demo_quality_f) %>%
    distinct() %>%
    brms::brm(data = ., gem_found ~ demo_quality_f * age_f + (1|uniqueID), 
              family =bernoulli(link = "logit"))
  
  ## are there differences in round of gem found when gem is found by social info? 
  gem_when_age <- all_data %>% 
    filter(gem_found_how == "copy" & demo_quality_f == "best") %>% 
    select(round_gem_found, age_f, uniqueID) %>% 
    distinct() %>% 
    brms::brm(formula = round_gem_found ~   age_f + (1 | uniqueID),
              data = .,
              family = poisson())
  
  ## save results
  # save(file = paste0(here(),'/G_Analysis_bevioral_data_social/modelfits/gem_age_advisor_model.RData'), gem_age_advisor_model)
  save(file = paste0(here(),'/G_Analysis_bevioral_data_social/modelfits/gem_freq_age_advisor_model.RData'), gem_freq_age_advisor_model)
  save(file = paste0(here(),'/G_Analysis_bevioral_data_social/modelfits/gem_when_age.RData'), gem_when_age)
  
}

tab_model(gem_freq_age_advisor_model)
plot_model(gem_age_advisor_model)

brms::conditional_effects(gem_age_advisor_model)
summary(gem_age_advisor_model)

summary(gem_when_age)
plot_model(gem_when_age)
tab_model(gem_when_age)
