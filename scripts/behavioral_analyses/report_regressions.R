## load packages and prepare data

## load packages
source("scripts/utils/check_pkgs.R")

'%!in%' <- function(x,y)!('%in%'(x,y))

options(scipen = 999,digits = 4)

source("https://raw.githubusercontent.com/AndGrad/custom_themes_ggplot/main/theme_plot.R")

## differences in exploration
## load model fit file
load(file = paste0(here(),'/modelfits/exploration_age_advisor.RData'))
load(file = paste0(here(),'/modelfits/exploration_age_advisor_prior.RData'))

## extract model parameters
regression_table_exploration<- bayestestR::describe_posterior(exploration_age_advisor, ci = .95, rope_range = c(-.1, .1)) %>% 
  dplyr::select( -c("CI", "ROPE_CI", "ROPE_high", "ROPE_low"))

regression_table_exploration <- as.data.frame(regression_table_exploration) %>% 
  mutate(pd = pd*100,
         ROPE_Percentage = ROPE_Percentage*100)

## names to assign to the params (for covenience when calling results below)

## calculate BFs
BF_table_exploration<- bayestestR::bayesfactor_parameters(exploration_age_advisor,
                                                          prior = exploration_age_advisor_prior)

#make into df
BF_table_exploration <- as.data.frame(BF_table_exploration) %>% 
  dplyr::select(Parameter, log_BF) %>% 
  mutate(BF = exp(log_BF)) %>% 
  select(-c(log_BF))

complete_table_exploration <- merge(regression_table_exploration, BF_table_exploration, by = "Parameter", sort = FALSE)

## round decimals to 2 except for Rhat
complete_table_exploration <- complete_table_exploration %>% 
  dplyr::mutate(across(
    .cols = setdiff(names(complete_table_exploration)[sapply(complete_table_exploration, is.numeric)], "Rhat"),
    .fns = ~ round(.x, 2)
  ),
  BF = if_else(BF> 100,          # test: is x larger than 100?
               ">100",           # yes → set to ">100"
               as.character(BF)   
  ))

## clean parameters names
complete_table_exploration$Parameter <- c(
  "Intercept",
  "Adolescents",
  "Quality (Medium)",
  "Quality (High)",
  "Quality (Medium) X Adolescents",
  "Quality (High) X Adolescents")

## Create table and save into word file 
ft <- flextable::flextable(complete_table_exploration)

# Format to resemble APA
ft <- fontsize(ft, size = 8, part = "all")
ft <- align(ft, align = "center", part = "all")

# Add table to Word document
doc <- officer::read_docx()
doc <- flextable::body_add_flextable(doc, ft)

print(doc, target = "tables/regression_table_exploration.docx")

## differences in social learning

## load model fit file
load(file = paste0(here(),'/modelfits/copy_age_advisor_model.RData'))
load(file = paste0(here(),'/modelfits/copy_age_advisor_model_priors.RData'))

## extract model parameters
regression_table_social_learning <- bayestestR::describe_posterior(copy_age_advisor_model, ci = .95, rope_range = c(-.1, .1)) %>% 
  dplyr::select( -c("CI", "ROPE_CI", "ROPE_high", "ROPE_low"))

regression_table_social_learning <- as.data.frame(regression_table_social_learning) %>% 
  mutate(pd = pd*100,
         ROPE_Percentage = ROPE_Percentage*100)

## names to assign to the params (for covenience when calling results below)

## calculate BFs
BF_table_social_learning <- bayestestR::bayesfactor_parameters(copy_age_advisor_model, prior = copy_age_advisor_model_priors)

#make into df
BF_table_social_learning <- as.data.frame(BF_table_social_learning) %>% 
  dplyr::select(Parameter, log_BF) %>% 
  mutate(BF = exp(log_BF)) %>% 
  select(-c(log_BF))

complete_table_social_learning <- merge(regression_table_social_learning, BF_table_social_learning, by = "Parameter", sort = FALSE)

## round decimals to 2 except for Rhat
complete_table_social_learning <- complete_table_social_learning %>% 
  dplyr::mutate(across(
    .cols = setdiff(names(complete_table_social_learning)[sapply(complete_table_social_learning, is.numeric)], "Rhat"),
    .fns = ~ round(.x, 2)
  ),
    BF = if_else(BF> 100,           # test: is x larger than 100?
                ">100",           # yes → set to ">100"
                as.character(BF)   
  ))

## clean parameters names
complete_table_social_learning$Parameter <- c(
  "Intercept",
  "Quality (Medium)",
  "Quality (High)",
  "Adolescents",
  "Quality (Medium) X Adolescents",
  "Quality (High) X Adolescents")

## Create table and save into word file 
ft <- flextable::flextable(complete_table_social_learning)

# Format to resemble APA
ft <- fontsize(ft, size = 8, part = "all")
ft <- align(ft, align = "center", part = "all")

# Add table to Word document
doc <- officer::read_docx()
doc <- flextable::body_add_flextable(doc, ft)

print(doc, target = "tables/regression_social_learning.docx")

# this is for visualization
posterior_draws<-copy_age_advisor_model%>%
  tidybayes::gather_draws(
  `b_age_fadolescents`,`b_demo_quality_fmedium:age_fadolescents`,`b_demo_quality_fbest:age_fadolescents`
  )%>%
  mutate(
    study="pooled",
    what="posteriors"
  )


prior_draws<-copy_age_advisor_model_priors%>%
  tidybayes::gather_draws(
 `b_age_fadolescents`,`b_demo_quality_fmedium:age_fadolescents`,`b_demo_quality_fbest:age_fadolescents`
  )%>%
  mutate(
    study="pooled",
    what="priors"
  )

pp_df<-rbind(
  posterior_draws,
  prior_draws,
) %>% mutate(beta = factor(.variable, levels = c(
  "b_demo_quality_fbest:age_fadolescents",
  "b_demo_quality_fmedium:age_fadolescents",
  "b_age_fadolescents"
)), fill = factor(what, levels = c("priors", "posteriors"))
  )

pp_plot <- pp_df %>%
  ggplot(aes(
    x = .value,
    y = beta,
    fill = after_stat(x < 0),
    alpha = fill
  )) +
  tidybayes::stat_halfeye(
    normalize = "panels",
    interval_color = "red",
    point_color = "red",
      
  ) +
 # stat_summary(fun = "median") +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  #geom_vline(aes(xintercept=-0.03),linetype="dotted")+
  #geom_vline(aes(xintercept=0.03),linetype="dotted")+
  scale_y_discrete(
    name = "regressor",
    labels = c(
      "Quality (High) X Adolescents",
      "Quality (Medium) X Adolescents",
      "Adolescents"
    )
  ) +
  scale_x_continuous(name = expression(beta * "  weight"),
                     breaks = c(-0.4, -0.2, 0, 0.2, 0.4)) +
  scale_fill_manual(name = expression(beta * "<0"),
                    values = c("#56B4E9", "#009E73")) +
  scale_alpha_manual(name = "", values = c(0.2, 1)) +
  coord_cartesian(xlim = c(-.4, .4)) +
  # guides(fill=F)+
  theme_bw(14) +
  theme(aspect.ratio = 1)

pp_plot
ggsave("figures/prior_post_copy_age_model.png", pp_plot)

## -------------------------------------differences in points scored by group

# points difference

## load model fit object 
base::load(paste0(here(),'/modelfits/points_age_trial_advisor_model.RData'))
base::load(paste0(here(),'/modelfits/points_age_trial_advisor_model_priors.RData'))

regression_table_performance <- bayestestR::describe_posterior(points_age_trial_advisor_model, ci = .95, rope_range = c(-.1, .1)) %>% 
  dplyr::select( -c("CI","ROPE_CI", "ROPE_high", "ROPE_low"))

regression_table_performance <- as.data.frame(regression_table_performance) %>% 
  mutate(pd = pd*100,
         ROPE_Percentage = ROPE_Percentage*100)

BF_table_performance <- bayestestR::bayesfactor_parameters(points_age_trial_advisor_model,
                                                           prior = points_age_trial_advisor_model_priors)
#make into df
BF_table_performance <- as.data.frame(BF_table_performance) %>% 
  select(Parameter, log_BF) %>% 
  mutate(BF = exp(log_BF)) %>% 
  select(-c(log_BF))

complete_table_performance <- merge(regression_table_performance, BF_table_performance,
                                    by = "Parameter",
                                    sort = FALSE) 

complete_table_performance <- complete_table_performance %>% 
  dplyr::mutate(across(
    .cols = setdiff(names(complete_table_performance)[sapply(complete_table_performance, is.numeric)], "Rhat"),
    .fns = ~ round(.x, 2)
  ),,
  BF = if_else(BF> 100,           # test: is x larger than 100?
               ">100",           # yes → set to ">100"
               as.character(BF)   ))

complete_table_performance$Parameter <- c(
  "Intercept",
  "Quality (Medium)",
  "Quality (High)",
  "Adolescents",
  "Trial",
  "Quality (Medium) X Adolescents",
  "Quality (High) X Adolescents",
  "Quality (Medium) X Trial",
  "Quality (High) X Trial",  
  "Adolescents X Trial",
  "Quality (Medium) X Adolescents X Trial",
  "Quality (High) X Adolescents X Trial"
  )

## Create table and save into word file 
ft <- flextable::flextable(complete_table_performance)

# Format to resemble APA
ft <- fontsize(ft, size = 7, part = "all")
ft <- align(ft, align = "center", part = "all")

# Add table to Word document
doc <- officer::read_docx()
doc <- flextable::body_add_flextable(doc, ft)

print(doc, target = "tables/regression_performance.docx")

# this is for visualization
posterior_draws<-points_age_trial_advisor_model%>%
  tidybayes::gather_draws(
    `b_age_fadolescents`,`b_demo_quality_fmedium:age_fadolescents`,`b_demo_quality_fbest:age_fadolescents`
  )%>%
  mutate(
    study="pooled",
    what="posteriors"
  )


prior_draws<-points_age_trial_advisor_model_priors%>%
  tidybayes::gather_draws(
    `b_age_fadolescents`,`b_demo_quality_fmedium:age_fadolescents`,`b_demo_quality_fbest:age_fadolescents`
  )%>%
  mutate(
    study="pooled",
    what="priors"
  )

pp_df<-rbind(
  posterior_draws,
  prior_draws,
) %>% mutate(beta = factor(.variable, levels = c(
  "b_demo_quality_fbest:age_fadolescents",
  "b_demo_quality_fmedium:age_fadolescents",
  "b_age_fadolescents"
)), fill = factor(what, levels = c("priors", "posteriors"))
)

pp_plot <- pp_df %>%
  ggplot(aes(
    x = .value,
    y = beta,
    fill = after_stat(x < 0),
    alpha = fill
  )) +
  tidybayes::stat_halfeye(
    normalize = "panels",
    interval_color = "red",
    point_color = "red",
    
  ) +
  # stat_summary(fun = "median") +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  #geom_vline(aes(xintercept=-0.03),linetype="dotted")+
  #geom_vline(aes(xintercept=0.03),linetype="dotted")+
  scale_y_discrete(
    name = "regressor",
    labels = c(
      "Quality (H) X Adolescents",
      "Quality (Medium) X Adolescents",
      "Adolescents"
    )
  ) +
  scale_x_continuous(name = expression(beta * "  weight"),
                     breaks = c(-0.4, -0.2, 0, 0.2, 0.4)) +
  scale_fill_manual(name = expression(beta * "<0"),
                    values = c("#56B4E9", "#009E73")) +
  scale_alpha_manual(name = "", values = c(0.2, 1)) +
  coord_cartesian(xlim = c(-.4, .4)) +
  # guides(fill=F)+
  theme_bw(14) +
  theme(aspect.ratio = 1)

pp_plot
ggsave("figures/prior_post_copy_age_model.png", pp_plot)

