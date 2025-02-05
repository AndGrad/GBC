## load packages and prepare data

## load packages
source("scripts/utils/check_pkgs.R")

'%!in%' <- function(x,y)!('%in%'(x,y))

options(scipen = 999,digits = 4)

## function to extract coefficients
source("utils/extract_coefficients.R")

## function to calculate Bayes Factor
source("utils/calculate_BF.R")

## set custom theme
source("https://raw.githubusercontent.com/AndGrad/custom_themes_ggplot/main/theme_plot.R")

## load model fit file
load(file = paste0(here(),'/G_Analysis_bevioral_data_social/modelfits/copy_age_advisor_model.RData'))

table_social_learning <- bayestestR::describe_posterior(copy_age_advisor_model, ci = .95, rope_range = c(-.1, .1))
table_social_learning %>% 
  dplyr::select( -c("ROPE_CI", "ROPE_high", "ROPE_low","Rhat","ESS"))

BF_table_social_learning <- bayestestR::bayesfactor_parameters(copy_age_advisor_model,
                                                      effects = "fixed",
                                                      direction = "two-sided",
                                                      null = 0)
plot(BF_table_social_learning)

## differences in points scored by group

## just raw difference in points across conditions
# t_test_points <- all_data %>% 
#   ungroup() %>% 
#   group_by(uniqueID) %>% 
#   mutate(mean_points_player = mean(tot_points, na.rm = TRUE)) %>% 
#   select(mean_points_player, age_f, uniqueID) %>% 
#   distinct() %>% 
#   ungroup() %>% 
#   stats::t.test(mean_points_player ~ age_f, data = ., alternative = "two.sided")

# points difference

## load model fit object 
base::load(paste0(here(),'/G_Analysis_bevioral_data_social/modelfits/points_age_trial_advisor_model.RData'))

table_points <- bayestestR::describe_posterior(points_age_trial_advisor_model, ci = .95, rope_range = c(-.1, .1))
table_points %>% 
  dplyr::select( -c("ROPE_CI", "ROPE_high", "ROPE_low","Rhat","ESS"))

tabb
BF_table_points <- bayestestR::bayesfactor_parameters(points_age_trial_advisor_model,
                                               effects = "fixed",
                                               direction = "two-sided",
                                               null = 0)
