##################################################################
##                     Demographic overview                     ##
##################################################################

## load packages
source("scripts/utils/check_pkgs.R")

## leoad demographics data
data_demographics <- read_csv(file = paste0(here(), "/data/data_social_all_participants_full_sample_demographics"))
data_demographics_prolific <- read.csv(file =paste0(here(), "/data/adults_demo_prolific_ds_prova.csv" )) %>% 
  dplyr::select(playerNr, Ethnicity.simplified, Country.of.birth, Language, Student.status, Employment.status, Nationality) %>% 
  rename(player = playerNr) %>% 
  distinct()

data_after_cleaning <- read_csv(file = paste0(here(), "/data/data_social_all_participants_08-2024.csv"))

## load prolific data
data_demographics_adults <- data_demographics %>% 
  dplyr::filter(group == "adults") %>% 
  select(player, gender, age) %>% 
  distinct()
  
data_demographics_adults_ds <- left_join(data_demographics_adults, data_demographics_prolific, by="player")%>% 
  mutate(across(where(is.character), ~na_if(., "DATA_EXPIRED")))

summary_table <- data_demographics_adults_ds %>%
  pivot_longer(
      cols = c( Ethnicity.simplified, Country.of.birth, Language, Student.status, Employment.status, Nationality),
      names_to = "Variable",
    values_to = "Category",
  ) %>%
  mutate(Category = ifelse(is.na(Category), "Missing (NA)", Category)) %>%
  group_by(Variable) %>%
  count(Category, name = "Count") %>%
  mutate(Proportion = Count / sum(Count)) %>%
  ungroup() %>%
  arrange(Variable, desc(Proportion))

summary_table_percent <- summary_table %>%
  mutate(Percentage = scales::percent(Proportion, accuracy = 0.1))

summary_table_percent
  
ft <- flextable::flextable(summary_table_percent)

# Format to resemble APA
ft <- fontsize(ft, size = 7, part = "all")
ft <- align(ft, align = "center", part = "all")

# Add table to Word document
doc <- officer::read_docx()
doc <- flextable::body_add_flextable(doc, ft)

print(doc, target = "tables/prolific_demographics.docx")

plot_data <- summary_table %>%
  mutate(Variable = str_replace_all(Variable, "\\.", " ")) %>%
  group_by(Variable) %>%
  mutate(Category = fct_reorder(Category, Proportion)) %>%
  ungroup()

summary_plot <- ggplot(plot_data, aes(x = Proportion, y = Category, fill = Variable)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = scales::percent(Proportion, accuracy = 0.1)),
            hjust = -0.1, 
            size = 3,     
            color = "black") +
  facet_wrap(~Variable, scales = "free_y", ncol = 2) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold", size = 12), 
    panel.grid.major.y = element_blank(), 
    panel.spacing = unit(2, "lines"),     
    axis.text.y = element_text(size = 9),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray30"),
    plot.caption = element_text(hjust = 1, size = 8, color = "gray50")
  ) +
    labs(
    title = "Proportional Distribution of Respondent Characteristics",
    subtitle = "Proportions of categories within each demographic and socio-economic variable.",
    x = "Proportion",
    y = "", 
    caption = "Data Source: User-provided summary table."
  ) +
  scale_x_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.2)))


ggsave("summary_demographics_plot.png", plot = summary_plot, width = 12, height = 10, dpi = 300)



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

