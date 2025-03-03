###-------------------------------------------------------------------------
###-------------------------------------------------------------------------
###                                                                      ---
###                               FIGURE 2                               ---
###                                                                      ---
###-------------------------------------------------------------------------
###-------------------------------------------------------------------------

## Script to reproduce figure 2 from paper

## load packages
pacman::p_load(tidyverse, gghalves, here, lmerTest, ggthemes, cowplot, ggh4x, grid)

## load data
all_data <- read_csv(file = paste0(here(), "/data/data_social_all_participants_08-2024.csv"))

labels_a <- c(
  `worst` = "Low quality 
social information:
Demonstrator explores until the end of the task
",
  `medium` =  "Medium quality social information:
Demonstrator settles for positive outcome
",
  `best` = "High quality
 social information:
Demonstrator finds a gem" )

labels_b <- c(
  `worst` = "Low quality
",
  `medium` =  "Medium quality
",
  `best` = "High quality"
 )


custom_y <- list(
  scale_y_continuous(limits = c(0, 175)),
  scale_y_continuous(limits = c(0, 70)),
  scale_y_continuous(limits = c(0, 70))
)

# ## panel A
# a <-
#   all_data %>%
#   group_by(uniqueID, age_f) %>%
#   select(uniqueID,
#          age_f,
#          tot_points,
#          gem_found,
#          gempresent) %>%
#   distinct() %>%
#   group_by(uniqueID, gem_found, age_f) %>%
#   summarise(mean_points = (mean(tot_points))) %>% 
#   ggplot(aes(x = age_f,
#              y = mean_points,
#              #color = age_f,
#              shape = age_f)) +
#   geom_half_boxplot(errorbar.draw = FALSE, notch = TRUE) +
#   geom_half_point(alpha = .4) +
#   
#   # geom_signif(
#   #   comparisons = list(c("adolescents", "adults")),
#   #   map_signif_level = TRUE, y_position = 1050,tip_length = 0, textsize = 20, vjust = 500, )+
#   stat_summary(
#     geom = "point",
#     size = 2,
#     stroke = 1,
#     color = "black",
#     fill = "orange",
#     position = position_dodge(width = -1)
#   ) +
#   scale_shape_manual(values = c(21, 23)) +
#   facet_wrap(~ gem_found, labeller = as_labeller(labels_a), scales = "free_y") +
#   #ylim(c(500,1400))+
#   labs(#subtitle = 'especially g',
#     x = "Age group",
#     y = 'Points per round',
#     tag = "A") +
#   theme_base(base_size = 15) +
#   guides(color = none,
#          shape = none) +
#   theme(legend.position = "none",
#         plot.background = element_blank())
# a

a <-
all_data %>%
  group_by(uniqueID, trial, group) %>%
  ggplot(aes(x = trial, y = points, color = group)) +
  stat_summary(
                aes(
                 #size = 2,
                 #stroke = 1,
                 #fill = demo_quality_f
                 #alpha = .2
                )) +
 
  scale_color_brewer(type = "qual", palette = 2, name = "Age", label = c("adolescents", "adults"))+
    facet_wrap(~ demo_quality_f,
               labeller = as_labeller(labels_a),
               scales = "free_y") + 
  facetted_pos_scales(y = custom_y) +
    #ylim(c(500,1400))+
    labs(#subtitle = 'especially g',
      x = "Trial",
      y = 'Points',
      tag = "A") +
    theme_base(base_size = 15) +
    theme(#legend.position = 'none',
          plot.background = element_blank(),
          strip.background = element_blank(),
          strip.text.x = element_blank()) +
  guides(
    #color = FALSE,
    shape = guide_legend(override.aes = list(
      #alpha = .5,
      shape = c(21,23)
      #size  = 2,
      #fill = c("#e41a1c", "#377eb8", "#4daf4a")
    )),
    #fill = FALSE,
    fill = guide_legend(override.aes = list(
      #alpha = .5,
      #shape = 21,
      #size  = 2,
      #fill = c("#e41a1c", "#377eb8", "#4daf4a")
    )),
    #label = FALSE,
    #   fill = FALSE,
    #    shape = FALSE
  ) 
#    guides(shape = guide_legend(override.aes = list(size  =1 )))
  a

  
## panel C## panel Cgempresent
b <-
  all_data %>% group_by(demo_type, uniqueID, round, age_f, gem_found, age, demo_quality_f) %>%
  filter(social_info_use == "copy") %>%
  count(social_info_use) %>%
  ggplot(aes(
    x = factor(demo_quality_f),
    y = n,
    #shape = age_f,
    color = age_f,
  )) +
  geom_half_point(alpha = .1) +
  geom_half_boxplot(errorbar.draw = FALSE, notch = TRUE, alpha = .2)+
  stat_summary(
    aes(    fill = demo_quality_f
),
    geom = "point",
    size = 2,
#color = "black",
    stroke = 1,
    position = position_dodge(width = .75)
  ) +
  geom_hline(yintercept = 25 / 64,
             linetype = "dotted",
             color = "red") +
  labs(y = 'N of "copy" per round',
       x = "Quality of social information",
       tag = "B") +
  scale_color_brewer(type = "qual", palette = 2, name = "Age", label = c("adolescents", "adults"))+
  
  # scale_fill_brewer(
  #   type = "qual",
  #   palette =
  #     6,
  #   name = "Quality of social information",
  #   label = c(
  #     "High: Finds a gem",
  #     "Medium: Exploits a non-gem",
  #     "Low: Explores until the end"
  #   )

 # scale_x_discrete(labels = c("High (Gem)", "Medium", "Low")) +
  #scale_shape_manual(name = "Age group", values = c(21, 23)) +
  facet_wrap(~ demo_quality_f,  scales = "free_x") +
  theme_base(15) +
  theme(#legend.position = "none" ,
        plot.background = element_blank(),
        legend.key = element_blank(),
        axis.title.x=element_blank(),
        #axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        strip.text.x = element_blank(),
        panel.spacing = unit(2, "lines")) +
  
  guides(
    #color = FALSE,
    shape = guide_legend(override.aes = list(
      #alpha = .5,
      shape = c(21,23)
      #size  = 2,
      #fill = c("#e41a1c", "#377eb8", "#4daf4a")
    )),
    fill = FALSE,
      #alpha = .5,
      #shape = 21,
      #size  = 2,
      #fill = c("#e41a1c", "#377eb8", "#4daf4a")
    )
 b 
 
 ## combine panels
 upper <- 
   cowplot::plot_grid(
     a,b,
     #labels = c("a","b"),
     #align = "H",
     nrow = 2,
    rel_widths =  c(.7, .2),
    rel_heights = c(1,.9)
   )
 upper
 
 

 # Combine combined plot and legend using plot_grid() 
 plot_grid(upper, legend,ncol=2,rel_widths = c(1, .4))
 upper
 ## panel C
 
#  ## re filter each treatment
# 
#  dataset1 <- all_data %>% 
#    filter( gem_found == 1 & demo_type == "gem_found")
#  
#  dataset2 <- all_data %>% 
#    filter(demo_type != "gem_found" & gem_found == 1)
#  
#  ## add information about proportion of gem found
#  prop_gem_found <- all_data %>%
#    filter(gempresent == 1) %>%
#    ungroup %>%
#    group_by(demo_type, gem_found, age_f) %>%
#    select(uniqueID, round, gem_found, round_gem_found, gempresent, age_f) %>%
#    distinct() %>%
#    select(gem_found, round_gem_found, age_f) %>%
#    summarise(mean_round_found = mean(round_gem_found, na.rm = TRUE),
#              n = n()) %>%
#    ungroup() %>%
#    group_by(demo_type, age_f) %>%
#    mutate(freq = round(n / sum(n),2)) %>% 
#    filter(gem_found == 1) %>% 
#    select(demo_type, age_f, freq) 
#  
# 
#  data_plot <- bind_rows(dataset1, dataset2) %>% 
#    select(round_gem_found, age_f, demo_type, uniqueID) %>%
#    group_by(uniqueID) %>%
#    distinct() %>%
#    left_join(., prop_gem_found, by = c("age_f", "demo_type"))
#   
# c <- data_plot %>%
#    ggplot(aes(
#      x = demo_type,
#      y = round_gem_found,
#      shape = age_f,
#      color = demo_type,
#      fill = demo_type
#    )) +
#    geom_half_boxplot(
#      errorbar.draw = FALSE,
#      notch = TRUE,
#      alpha = .2,
#      show.legend = FALSE
#    ) +
#    geom_half_point(alpha = 0.2) +
#    stat_summary(
#      geom = "point",
#      size = 2,
#      stroke = 1,
#      color = "black",
#      position = position_dodge(width = .75)
#    ) +
#   geom_label(data = prop_gem_found, aes(x = demo_type, y = 26.5, label = freq),show.legend = FALSE, 
#             colour = "white", fontface = "bold",
#             position=position_dodge(width=.75)) +
#    scale_color_brewer(type = "qual", palette = 6) +
#    scale_fill_brewer(
#      type = "qual",
#      palette =
#        6,
#      name = "Quality of social information",
#      label = c(
#        "High: Finds a gem",
#        "Medium: Exploits a non-gem",
#        "Low: Explores until the end"
#      )
#    ) +
#    scale_shape_manual(name = "Age group", values = c(21, 23)) +
#    scale_x_discrete(labels = c("High (Gem)", "Medium", "Low")) +
#    labs(x = "Quality of social information",
#         y = 'N of clicks to find a gem',
#         tag = "C") +
#    #facet_wrap(~demo_type) +
#    theme_base(base_size = 15) +
#    guides(
#      color = FALSE,
#      fill = guide_legend(override.aes = list(
#       alpha = .5,
#       shape = 21,
#       size  =
#         4,
#       fill = c("#e41a1c", "#377eb8", "#4daf4a"))),
#      label = FALSE,
#     #fill = FALSE,
#      shape = guide_legend(override.aes = list(size = 4))
#    ) +
#    theme(plot.background = element_blank())
#  
#  c
#  
 
## regression slopes 
#  
#   load(file = paste0(here(),'/G_Analysis_bevioral_data_social/modelfits/poission_regression_all_rounds_slopes.RData'))
#   
#   poisson_plot <- 
#     plot_model(
#       model_random_slopes,
#       # axis.lim = c(.2, 2),
#       axis.labels = rev(
#         c(
#           "Quality (Medium)",
#           "Quality (Worst)",
#           "Adolescents",
#           "Quality (Medium) X Adolescents",
#           "Quality (Worst) X Adolescents"
#         )
#       ),
#       title = "", vline.color = "grey", vline = 2,show.values = TRUE, 
#     ) +
#     ylim(.2,2)+
#     theme_base(base_size = 15)+
#     theme(plot.background = element_blank())
#   
# 
  lower <-
    cowplot::plot_grid(
      c,
      #labels = c("a","b"),
      #align = "H",
      nrow = 1,
      rel_widths =  c(.6, .4)
    )


## combine panels
figure2 <-
  cowplot::plot_grid(
    upper
   # labels = c("","c"),
    #align = "H",
   # nrow = 2,
    #rel_widths =  c(1, 1)
  )

figure2

## save figure
ggsave("figures/figure2.png", figure2, height = 6, width = 10, scale = 1)

