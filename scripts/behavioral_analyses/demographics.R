##################################################################
##                     Demographic overview                     ##
##################################################################

## load packages
source("scripts/utils/check_pkgs.R")

## leoad demographics data
data_demographics <- read_csv(file = paste0(here(), "/data/data_social_all_participants_full_sample_demographics"))
data_after_cleaning <- read_csv(file = paste0(here(), "/data/data_social_all_participants_08-2024.csv"))

## demographics before excluding participants

## adults gender and age
data_demographics %>% 
  select(group, gender, uniqueID) %>% 
  filter(group=="adults") %>% 
  distinct() %>% 
  group_by(gender) %>% 
  summarise(count = n())

data_demographics %>% 
  select(group, gender, uniqueID, age) %>% 
  filter(group=="adults") %>% 
  distinct() %>% 
  reframe( mean = mean(age, na.rm=TRUE),
           sd = sd(age, na.rm =TRUE),
           range = range(age, na.rm = TRUE))


## adolescents gender and age
data_demographics %>% 
  select(group, gender, uniqueID) %>% 
  filter(group=="adolescents") %>% 
  distinct() %>% 
  group_by(gender) %>% 
  summarise(count = n())

data_demographics %>% 
  select(group, gender, uniqueID, age) %>% 
  filter(group=="adolescents") %>% 
  distinct() %>% 
  reframe( mean = mean(age, na.rm=TRUE),
           sd = sd(age, na.rm =TRUE),
           range = range(age, na.rm = TRUE))


## demographics after excluding participants

## adults gender and age
data_after_cleaning %>% 
  select(group, gender, uniqueID) %>% 
  filter(group=="adults") %>% 
  distinct() %>% 
  group_by(gender) %>% 
  summarise(count = n())

data_after_cleaning %>% 
  select(group, gender, uniqueID, age) %>% 
  filter(group=="adults") %>% 
  distinct() %>% 
  reframe( mean = mean(age, na.rm=TRUE),
           sd = sd(age, na.rm =TRUE),
           range = range(age, na.rm = TRUE))

## adolescents gender and age
data_after_cleaning %>% 
  select(group, gender, uniqueID) %>% 
  filter(group=="adolescents") %>% 
  distinct() %>% 
  group_by(gender) %>% 
  summarise(count = n())

data_after_cleaning %>% 
  select(group, uniqueID, age) %>% 
  dplyr::filter(group=="adolescents") %>% 
  distinct() %>%
  reframe( mean = mean(age, na.rm=TRUE),
           sd = sd(age, na.rm =TRUE),
           range = range(age, na.rm = TRUE))


## make a demographic table for both

data_after_cleaning %>% 
  select(group, uniqueID, age, gender) %>% 
  group_by(group) %>% 
  distinct() %>%
  reframe( mean = mean(age, na.rm=TRUE),
           sd = sd(age, na.rm =TRUE),
           range = range(age, na.rm = TRUE))

#### make a plot

## first row
group_totals_before <- data_demographics %>%
  select(uniqueID, age, group) %>% 
  distinct() %>% 
  group_by(group) %>%
  summarise(total = n())

before_cleaning <- data_demographics %>% 
  ungroup() %>% 
  select(uniqueID, age, group) %>% 
  distinct() %>% 
  ggplot() +
  geom_bar(aes(x = age, fill = group), binwidth = 1) +
  labs(y = "count",
       tag = "A") +
  annotate("text",
           x = Inf,
           y = Inf,
           label = paste(group_totals_before$group, "N:", group_totals_before$total, collapse = "\n"),
           hjust = 1.1, vjust = 1.1,
           size = 4, fontface = "italic") +
  theme_classic(base_size = 15) +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) 

## second row
group_totals_after <- data_after_cleaning %>%
  select(uniqueID, age, group) %>% 
  distinct() %>% 
  group_by(group) %>%
  summarise(total = n())


after_cleaning <- data_after_cleaning %>% 
  ungroup() %>% 
  select(uniqueID, age, group) %>% 
  distinct() %>% 
  ggplot() +
  geom_bar(aes(x = age, fill = group), binwidth = 1) +
  labs(y = "count", tag = "B") +
  theme_classic(base_size = 15) +
  annotate("text",
           x = Inf,
           y = Inf,
           label = paste(group_totals_after$group, "N:", group_totals_after$total, collapse = "\n"),
           hjust = 1.1, vjust = 1.1,
           size = 4, fontface = "italic") 

combined_plot <- before_cleaning + after_cleaning  + 
  plot_layout(ncol = 1, guides = "collect") & 
  theme(legend.position = "right", panel.border = element_blank())
combined_plot


ggsave("figures/demographics_plot.png", combined_plot)

