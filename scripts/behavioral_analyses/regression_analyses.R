## load packages and prepare datab----------------------------------------------

## load packages
source("scripts/utils/check_pkgs.R")

## load data
all_data <- read_csv(file = paste0(here(), "/data/data_social_all_participants_08-2024.csv")) %>% 
  mutate(demo_quality_f = factor(demo_quality, levels = c("worst", "medium", "best")),
         age_f = factor(age_f, levels = c("adults", "adolescents")))

## exploration differences -----------------------------------------------------

## create dataset
boxes_data <- 
all_data %>% 
  group_by(round, uniqueID, age_f, demo_quality_f) %>% 
  summarise(boxes_opened = n_distinct(choice)) ## n of unique choice = n of different boxes opened

#describe boxes data
boxes_data%>%ungroup()%>%summarise(mean=mean(boxes_opened),sd=sd(boxes_opened))
boxes_data%>%group_by(age_f)%>%summarise(mean=mean(boxes_opened),sd=sd(boxes_opened))
boxes_data%>%group_by(demo_quality_f)%>%summarise(mean=mean(boxes_opened),sd=sd(boxes_opened))
boxes_data%>%group_by(age_f,demo_quality_f)%>%summarise(mean=mean(boxes_opened),sd=sd(boxes_opened))

if (file.exists(paste0(here(),'/modelfits/exploration_age_advisor.RData'))) {
  
} else {
  
  prior <- brms::prior_string("normal(0, 0.1)", class = "b")
  
  exploration_age_advisor <- brms::brm(boxes_opened ~ age_f * demo_quality_f + (1 + demo_quality_f| uniqueID),
                                             data = boxes_data,
                                             prior = prior,
                                             family = poisson(),
                                             iter = 6000,
                                             chains = 6,
                                             cores = 6)
  
  #specify model and fit
  exploration_age_advisor_prior <- brms::brm(boxes_opened ~ age_f * demo_quality_f + (1 + demo_quality_f| uniqueID),
                                 data = boxes_data,
                                 prior = prior,
                                 sample_prior = "only",
                                 family = poisson(),
                                 iter = 6000,
                                 chains = 6,
                                 cores = 6)
  ## save results
  save(file = paste0(here(),'/modelfits/exploration_age_advisor.RData'), exploration_age_advisor)
  save(file = paste0(here(),'/modelfits/exploration_age_advisor_prior.RData'), exploration_age_advisor_prior)
  
}

## social learning differences--------------------------------------------------

## create count variable
data_regression_copy <-  all_data %>% 
  mutate(copy=ifelse(social_info_use == "copy",1,0))%>%
  group_by(uniqueID, round, age_f, demo_quality_f)%>%
  summarise(n=sum(copy))

#describe copy data
data_regression_copy%>%ungroup()%>%summarise(mean=mean(n),sd=sd(n))
data_regression_copy%>%group_by(age_f)%>%summarise(mean=mean(n),sd=sd(n))
data_regression_copy%>%group_by(demo_quality_f)%>%summarise(mean=mean(n),sd=sd(n))
data_regression_copy%>%group_by(age_f,demo_quality_f)%>%summarise(mean=mean(n),sd=sd(n))

if (file.exists(paste0(here(),'/modelfits/copy_age_advisor_model.RData'))) {
  

} else {
  
  prior <- brms::prior_string("normal(0, 0.1)", class = "b")
  
  ## specify model and fit
  copy_age_advisor_model <- brms::brm(formula = n ~ 1 + demo_quality_f * age_f + (1 + demo_quality_f | uniqueID),
                                       data = data_regression_copy,
                                       prior = prior,
                                       family = zero_inflated_poisson(),
                                       iter = 6000,
                                       chains = 6,
                                       cores = 6) 
  
  copy_age_advisor_model_priors <-brms::brm(formula = n ~ 1 + demo_quality_f * age_f + (1 + demo_quality_f | uniqueID),
                                             data = data_regression_copy,
                                             prior = prior,
                                             family = zero_inflated_poisson(),
                                             sample_prior = "only",
                                             iter = 6000,
                                             chains = 6,
                                             cores = 6)  
   ## save results
  save(file = paste0(here(),'/modelfits/copy_age_advisor_model.RData'), copy_age_advisor_model)
  save(file = paste0(here(),'/modelfits/copy_age_advisor_model_priors.RData'), copy_age_advisor_model_priors)
  
}

## performance differences------------------------------------------------------

## vizualize mean player point performance
all_data %>% 
  ungroup() %>% 
  group_by(uniqueID, demo_quality_f) %>% 
  mutate(mean_points_player = mean(tot_points, na.rm = TRUE)) %>% 
  select(mean_points_player, age_f,uniqueID, demo_quality_f) %>% 
  distinct() %>% 
  ungroup() %>%
  ggplot(aes(x=age_f, y=mean_points_player))+
  geom_boxplot() +
  geom_point() +
  facet_wrap(~demo_quality_f)

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
  select(uniqueID, age_f, demo_quality_f, treatment, trial, points) %>% 
  mutate(mean_points = mean(points),
         sd = sd(points),
         scaled_points = (points - mean_points)/(sd*0.5)) 

#describe boxes data
data_regression_points_trial%>%ungroup()%>%summarise(mean=mean(points),sd=sd(points))
data_regression_points_trial%>%group_by(age_f)%>%summarise(mean=mean(points),sd=sd(points))
data_regression_points_trial%>%group_by(demo_quality_f)%>%summarise(mean=mean(points),sd=sd(points))
data_regression_points_trial%>%group_by(age_f,demo_quality_f)%>%summarise(mean=mean(points),sd=sd(points))


data_regression_points_trial %>% 
  ggplot(aes(x = age_f, y = scaled_points)) +
  #ggplot2::geom_point(aes(shape = age_f)) +
  stat_summary(aes(color = age_f))+
  facet_wrap(~demo_quality_f)

## load model if fitting has been done, if not fit model

if #(file.exists(paste0(here(),'/G_Analysis_bevioral_data_social/modelfits/performance_age_demonstrator_model.RData')) & 
(file.exists(paste0(here(),'/modelfits/points_age_trial_advisor_model.RData')))
  #)  
{
    #
} else {
  
  ## define priors 
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
  
  points_age_trial_advisor_model_priors <-
    brms::brm(formula = scaled_points ~ demo_quality_f * age_f * trial + (1| uniqueID),
              prior = prior_normal,
              sample_prior = "only",
              data = data_regression_points_trial,
              iter = 6000,
              chains = 6,
              cores = 6 )

  ## save
  save("points_age_trial_advisor_model",
       file = paste0(here(),'/modelfits/points_age_trial_advisor_model.RData'))
  save("points_age_trial_advisor_model_priors",
       file = paste0(here(),'/modelfits/points_age_trial_advisor_model_priors.RData'))

}
