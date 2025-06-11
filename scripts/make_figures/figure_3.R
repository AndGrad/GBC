###-------------------------------------------------------------------------
###-------------------------------------------------------------------------
###                                                                      ---
###                               FIGURE 3                              ---
###                                                                      ---
###-------------------------------------------------------------------------
###-------------------------------------------------------------------------

## Script to reproduce figure 3 from paper, behavioral results

## load packages
pacman::p_load(tidyverse, gghalves, here, lmerTest, ggthemes, cowplot, ggh4x, grid)

## load data
all_data <- read_csv(file = paste0(here(), "/data/data_social_all_participants_08-2024.csv")) %>% 
  mutate(age_f = factor(age_f, levels = c("adolescents", "adults"))) %>% 
  mutate(demo_quality_f = factor(demo_quality, levels = c("best", "medium", "worst")))
      
## make plot labels

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


## panel A: exploration --------------------------------------------------------

## create dataset
boxes_data <- 
  all_data %>% 
  group_by(round, uniqueID, age_f, demo_quality_f) %>% 
  summarise(boxes_opened = n_distinct(choice)) %>% 
  ungroup() %>% 
  group_by(uniqueID, age_f, demo_quality_f) %>% 
  summarise(mean_explore = mean(boxes_opened)) ## n of unique choice = n of different boxes opened

## plot: n unique boxes by group and demonstrator
a <- boxes_data %>% 
 ggplot(aes(
    x = demo_quality_f, 
    y = mean_explore,
    #shape = age_f,
    color = age_f,
  )) +
  geom_half_point(alpha = .1) +
  geom_half_violin(aes(fill = age_f))+
  stat_summary(
    aes(fill = age_f),
    #geom = "point",
    #size = 2,
  color = "black",
  shape = 21,
    stroke = 1,
    position = position_dodge(width = .75) 
  ) +
  labs(y = 'N of unique tiles\n opened per round',
       x = "Quality of social information",
       tag = "A") +
  scale_color_brewer(type = "qual", palette = 2, name = "Age", label = c("Adolescents", "Adults"))+
  scale_fill_brewer(type = "qual", palette = 2, name = "Age")+
  scale_x_discrete(labels = c("","","")) +
  #scale_shape_manual(name = "Age group", values = c(21, 23)) +
  facet_wrap(~ demo_quality_f,  scales = "free_x") +
  theme_base(12) +
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

a

## social learning -------------------------------------------------------------

## plot: n of copy by by group and demonstrator
b <-
  all_data %>% group_by(uniqueID, round, age_f, demo_quality_f) %>%
  filter(social_info_use == "copy") %>%
  count(social_info_use) %>%
  ungroup() %>% 
  group_by(uniqueID, age_f, demo_quality_f) %>% 
  summarise(mean_copy = mean(n)) %>% 
  ggplot(aes(
    x = factor(demo_quality_f),
    y = mean_copy,
    #shape = age_f,
    color = age_f,
  )) +
  geom_half_point(alpha = .1) +
  geom_half_violin(aes(fill = age_f))+
  stat_summary(
    aes(fill = age_f),
    #geom = "point",
    #size = 2,
    color = "black",
    shape = 21,
    stroke = 1,
    position = position_dodge(width = .75) 
  ) +
  geom_hline(yintercept = 25 / 64,
             linetype = "dotted",
             color = "red") +
  labs(y = 'N of "copy" per round',
       x = "Quality of social information",
       tag = "B") +
  scale_color_brewer(type = "qual", palette = 2, name = "Age", label = c("Adolescents", "Adults"))+
  scale_fill_brewer(type = "qual", palette = 2, name = "Age", label = c("Adults", "Adolescents"))+
  scale_x_discrete(labels = c("","","")) +
  #scale_shape_manual(name = "Age group", values = c(21, 23)) +
  facet_wrap(~ demo_quality_f,  scales = "free_x") +
  theme_base(12) +
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

 c <- all_data %>%
   ungroup() %>% 
   group_by(uniqueID, age_f, demo_quality_f, trial) %>% 
   ggplot(aes(x = trial, y = points, color = age_f, demo_quality_f)) +
   stat_summary(
     aes(
       #size = 2,
       #stroke = 1,
       #fill = demo_quality_f
       #alpha = .2
     )) +
   scale_color_brewer(type = "qual", palette = 2, name = "Age", label = c("Adolescents", "Adults"))+
   facet_wrap(~ demo_quality_f,
              labeller = as_labeller(labels_a),
              scales = "free_y") + 
   facetted_pos_scales(y = custom_y) +
   #ylim(c(500,1400))+
   labs(#subtitle = 'especially g',
     x = "Trial",
     y = 'Points',
     tag = "C") +
   theme_base(base_size = 12) +
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
 c
 
 ## combine panels
 figure3 <- 
   cowplot::plot_grid(
     a,b,c,
     align = "v",
     nrow = 3,
      
    #rel_widths =  c(.2, .7),
    rel_heights = c(.9,.9,1)
   )
 
 figure3
 
## save figure
ggsave("figures/figure3.png", figure3, height = 8.3, width = 10, scale = 1, dpi = 300)

