# 0. Load libraries ----

library(tidyverse)
library(viridis)
library(cowplot)
library(ggpubr)
library(jsonlite)
library(openxlsx2)

# 1. Load data ----

height = 11.69
width = 8.27

## Cohort count ----

cohortCount <- read.csv("data/cohort_counts.csv")

## Concept sets ----

concept_set <- function(data) {
  concepts <- data %>% pluck("concept")
  concepts$includeDescendanst <- data %>% pluck("includeDescendants")
  concepts$isExcluded <- data %>% pluck("isExcluded")
  return(concepts)
}

cohort_json <- fromJSON("data/concepts/concept_sets.json")
concept_sets <- cohort_json$ConceptSets$expression %>% pluck("items")
names(concept_sets) <- cohort_json$ConceptSets$name

MDD_JA <- concept_set(concept_sets$MDD_JA)
SSRIs_JA <- concept_set(concept_sets$SSRIs_JA)
NSRIs_JA <- concept_set(concept_sets$NSRIs_JA)
Other_ADs <- concept_set(concept_sets$Other_ADs)
Psycholeptics_JA <- concept_set(concept_sets$Psycholeptics_JA)


MDD_JA %>% write_xlsx("data/concepts/MDD_JA.xlsx")
SSRIs_JA %>% write_xlsx("data/concepts/SSRIs_JA.xlsx")
NSRIs_JA %>% write_xlsx("data/concepts/NSRIs_JA.xlsx")
Other_ADs %>% write_xlsx("data/concepts/Other_ADs.xlsx")
Psycholeptics_JA %>% write_xlsx("data/concepts/Psycholeptics_JA.xlsx")


## Incidence rate ----

incidence_sex_year <- read.csv("data/incidence_rate/incidence_sex_year.csv")

incidence_age_sex_year <- read.csv("data/incidence_rate/incidence_age_sex_year.csv")

## Cohort characterization ----


cohortCharacterization <- read.csv("data/cohort_characterization/cohort_characterization_MDD_JA_All_time.csv")
cohortCharacterization$cohortName <- "MDD_JA All time"
cohortCharacterization_timeInvariant <- cohortCharacterization %>% filter(TEMPORAL_CHOICES == "Time Invariant")
cohortCharacterization_365dto_31d <- cohortCharacterization %>% filter(TEMPORAL_CHOICES == "T (-365d to -31d)")
cohortCharacterization_30dto_1d <- cohortCharacterization %>% filter(TEMPORAL_CHOICES == "T (-30d to -1d)")


cohortCharacterization_MDD_SSRI_RW <- read.csv("data/cohort_characterization/cohort_characterization_MDD_SSRI_RW.csv")
cohortCharacterization_MDD_SSRI_RW$cohortName <- "MDD_SSRI_RW"
cohortCharacterization_MDD_SSRI_RW_timeInvariant <- cohortCharacterization_MDD_SSRI_RW %>% filter(TEMPORAL_CHOICES == "Time Invariant")
cohortCharacterization_MDD_SSRI_RW_365dto_31d <- cohortCharacterization_MDD_SSRI_RW %>% filter(TEMPORAL_CHOICES == "T (-365d to -31d)")
cohortCharacterization_MDD_SSRI_RW_30dto_1d <- cohortCharacterization_MDD_SSRI_RW %>% filter(TEMPORAL_CHOICES == "T (-30d to -1d)")

cohortCharacterization_MDD_NSRI_RW <- read.csv("data/cohort_characterization/cohort_characterization_MDD_NSRI_RW.csv")
cohortCharacterization_MDD_NSRI_RW$cohortName <- "MDD_NSRI_RW"
cohortCharacterization_MDD_NSRI_RW_timeInvariant <- cohortCharacterization_MDD_NSRI_RW %>% filter(TEMPORAL_CHOICES == "Time Invariant")
cohortCharacterization_MDD_NSRI_RW_365dto_31d <- cohortCharacterization_MDD_NSRI_RW %>% filter(TEMPORAL_CHOICES == "T (-365d to -31d)")
cohortCharacterization_MDD_NSRI_RW_30dto_1d <- cohortCharacterization_MDD_NSRI_RW %>% filter(TEMPORAL_CHOICES == "T (-30d to -1d)")

cohortCharacterization_MDD_otherAds_RW <- read.csv("data/cohort_characterization/cohort_characterization_MDD_otherAds_RW.csv")
cohortCharacterization_MDD_otherAds_RW$cohortName <- "MDD_otherAds_RW"
cohortCharacterization_MDD_otherAds_RW_timeInvariant <- cohortCharacterization_MDD_otherAds_RW %>% filter(TEMPORAL_CHOICES == "Time Invariant")
cohortCharacterization_MDD_otherAds_RW_365dto_31d <- cohortCharacterization_MDD_otherAds_RW %>% filter(TEMPORAL_CHOICES == "T (-365d to -31d)")
cohortCharacterization_MDD_otherAds_RW_30dto_1d <- cohortCharacterization_MDD_otherAds_RW %>% filter(TEMPORAL_CHOICES == "T (-30d to -1d)")

## Index event ----

index_event_MDD_JA_All_time <- read.csv("data/index_events/MDD_JA_All_time_196.csv")
index_event_MDD_JA_All_time$cohortName <- "MDD_JA All time"

index_event_MDD_JA_All_time <- index_event_MDD_JA_All_time %>%
  mutate(
    Prop = round(IMASIS_sep_persons / cohortCount %>% filter(Cohort.Name == unique(index_event_MDD_JA_All_time$cohortName)) %>%
      pull(IMASIS_sep_persons), 3)
  )


index_event_MDD_SSRI_RW <- read.csv("data/index_events/MDD_SSRI_RW_230.csv")
index_event_MDD_SSRI_RW$cohortName <- "MDD_SSRI_RW"

index_event_MDD_SSRI_RW <- index_event_MDD_SSRI_RW %>%
  mutate(
    Prop = round(IMASIS_sep_persons / cohortCount %>% filter(Cohort.Name == unique(index_event_MDD_SSRI_RW$cohortName)) %>%
                   pull(IMASIS_sep_persons), 3)
  )
index_event_MDD_SSRI_RW$IMASIS_sep_persons[index_event_MDD_SSRI_RW$IMASIS_sep_persons < 0] <- NA


index_event_MDD_NSRI_RW <- read.csv("data/index_events/MDD_NSRI_RW_231.csv")
index_event_MDD_NSRI_RW$cohortName <- "MDD_NSRI_RW"
index_event_MDD_NSRI_RW <- index_event_MDD_NSRI_RW %>%
  mutate(
    Prop = round(IMASIS_sep_persons / cohortCount %>% filter(Cohort.Name == unique(index_event_MDD_NSRI_RW$cohortName)) %>%
                   pull(IMASIS_sep_persons), 3)
  )
index_event_MDD_NSRI_RW$IMASIS_sep_persons[index_event_MDD_NSRI_RW$IMASIS_sep_persons < 0] <- NA


index_event_MDD_otherAds_RW <- read.csv("data/index_events/MDD_otherAds_RW_237.csv")
index_event_MDD_otherAds_RW$cohortName <- "MDD_otherAds_RW"

index_event_MDD_otherAds_RW <- index_event_MDD_otherAds_RW %>%
  mutate(
    Prop = round(IMASIS_sep_persons / cohortCount %>% filter(Cohort.Name == unique(index_event_MDD_otherAds_RW$cohortName)) %>%
                   pull(IMASIS_sep_persons), 3)
  )
index_event_MDD_otherAds_RW$IMASIS_sep_persons[index_event_MDD_otherAds_RW$IMASIS_sep_persons < 0] <- NA


## Visit context ----

visit_context_MDD_JA_All_time <- read.csv("data/visit_context/visit_context_MDD_JA_All_time.csv")

visit_context_MDD_JA_All_time <- visit_context_MDD_JA_All_time %>%
  rename(
    Before = IMASIS_sep_Before,
    During = IMASIS_sep_During,
    Simultaneous = IMASIS_sep_Simultaneous,
    After = IMASIS_sep_After
  )

## Time distribution ----

timeDistribution <- read.csv("data/time_distribution/time_distribution.csv")

## Cohort overlap ----

cohortOverlap <- read.csv("data/cohort_overlap/cohort_overlap_retrieval.csv")

## Compare characterization ----

SSRI_vs_NSRI_timeInvariant <- read.csv("data/cohort_comparison/SSRI_vs_NSRI_timeInvariant.csv")
SSRI_vs_otherAds_timeInvariant <- read.csv("data/cohort_comparison/SSRI_vs_otherAds_timeInvariant.csv")
NSRI_vs_otherAds_timeInvariant <- read.csv("data/cohort_comparison/NSRI_vs_otherAds_timeInvariant.csv")

SSRI_vs_NSRI_365dto1d <- read.csv("data/cohort_comparison/SSRI_vs_NSRI_365dto31d.csv")
SSRI_vs_otherAds_365dto1d <- read.csv("data/cohort_comparison/SSRI_vs_otherAds_365dto1d.csv")
NSRI_vs_otherAds_365dto1d <- read.csv("data/cohort_comparison/NSRI_vs_otherAds_365dto1d.csv")

# 2. Cohort characterization ----

cohort_count_196 <- cohortCount %>% filter(Cohort.Name  == "MDD_JA All time") %>% pull(IMASIS_sep_persons)

## Figure 1 ----

## Non-binary variables

dt_tab <- tibble(
  Variable = "N",
  Value = as.character(cohort_count_196)
)

dt_tab <- dt_tab %>% bind_rows(
  cohortCharacterization_timeInvariant %>% filter(COVARIATE_NAME == "gender = FEMALE") %>%
    mutate(
      Variable = "Female, n (%)",
      Value = paste0(round(MEAN1 * cohort_count_196, 0), " (",MEAN1*100, ")"),
      .keep = "none"
    )
)

dt_tab <- dt_tab %>% bind_rows(
  cohortCharacterization_timeInvariant %>% filter(COVARIATE_NAME == "age in years") %>%
    mutate(
      Variable = "Mean age, years (SD)",
      Value = paste0(MEAN1, " (",round(SD1, 2), ")"),
      .keep = "none"
    )
)


dt_tab <- dt_tab %>% bind_rows(
  cohortCharacterization %>%
    filter(
      COVARIATE_NAME %in% c(
        #"CHADS2",
        #"CHADS2VASc",
        "Charlson index - Romano adaptation",
        "Diabetes Comorbidity Severity Index (DCSI)"
      )
    ) %>%
    select(COVARIATE_NAME, MEAN1, SD1) %>%
    mutate(
      Variable = paste(COVARIATE_NAME, "(SD)"),
      Value = paste0(MEAN1, " (", round(SD1, 2), ")"),
      .keep = "none"
    ) 
) 


Fig1A <- dt_tab %>% #slice(match(dt_var_order, Variable)) %>% 
  ggtexttable(rows = NULL, theme = ttheme("light", base_size = 16)) %>% 
  tab_add_title(text = "Newly diagnosed MDD\nIMASIS Hospital del Mar, Jan 2010–Jun 2025", face = "bold", size = 16) 
Fig1A


## Binary variables

## Time distribution

timeDistribution_MDD_JA_All_time <- timeDistribution %>% filter(Cohort.Id == 196)

timeDistribution_MDD_JA_All_time$Time.Measure <- c("Before diagnosis", "After diagnosis", "time (days) between cohort start and end")

Fig1B <- timeDistribution_MDD_JA_All_time %>% filter(Time.Measure != "time (days) between cohort start and end") %>%
  ggplot(aes(x = Time.Measure)) +
  geom_boxplot(
    aes(
      ymin   = P10, # whiskers represent P10 and P90 percentiles
      lower  = P25,
      middle = Median,
      upper  = P75,
      ymax   = P90
    ),
    stat = "identity",
    color = "slateblue",
    fill = "slateblue",
    alpha = 0.33,
    width = 0.4
  ) +
  geom_point(aes(y = Average), shape = 18, size = 5, color = "firebrick") +
  #coord_flip() +
  theme_cowplot() +
  labs(
    title = "Available follow-up time after and before MDD diagnosis",
    subtitle = "IMASIS Hospital del Mar, Jan 2010–Jun 2025, n = 6320",
    x = NULL,
    y = "Time (days)"
  ) + facet_wrap(~Time.Measure, scale = "free") +
  theme(axis.text.x = element_blank())
Fig1B

tmpFig <- timeDistribution_MDD_JA_All_time %>% filter(Time.Measure != "time (days) between cohort start and end") %>%
  rename("Time (days)" = Time.Measure,
         SD = S.D) %>% select('Time (days)', Average:Max) %>%
  ggtexttable(rows = NULL, theme = ttheme("light", base_size = 10)) +
  theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "mm"))

Fig1B <- plot_grid(Fig1B, tmpFig, ncol = 1)



Fig1C <- index_event_MDD_JA_All_time %>% arrange(Prop) %>%
  mutate(Concept.Name = factor(Concept.Name, levels = Concept.Name)) %>%
  ggplot(aes(x = Prop*100, y = Concept.Name)) +
  geom_segment(aes(y = Concept.Name, x = 0, xend = 100*Prop), color = "firebrick") +
  geom_point(aes(y = Concept.Name, x = 100*Prop, size = IMASIS_sep_persons), color = "firebrick") +
  scale_size(name = "Patient\ncount (n)") +
  labs(
    title = "Concepts driving cohort entry (MDD diagnosis)",
    subtitle = "IMASIS Hospital del Mar, Jan 2010–Jun 2025, n = 6320",
    x = "Proportion of cohort (%)",
    y = "Concept name"
  ) +
  theme_cowplot()
Fig1C


AgeGroup <- cohortCharacterization_timeInvariant %>% filter(ANALYSIS_NAME == "DemographicsAgeGroup") %>%
  mutate(COVARIATE_NAME = str_remove(COVARIATE_NAME, pattern = "age group:  "))

Fig1D <- AgeGroup %>% ggplot(aes(x = COVARIATE_NAME, y = 100*MEAN1, fill = MEAN1)) +
  geom_bar(aes(colour = MEAN1),position = "dodge", stat = "identity", alpha = 0.66) +
  geom_text(aes(label = paste0(MEAN1*100,"%")), position = position_dodge(width = 1)) +
  scale_color_gradient(low = "#d0d1e6", high = "#045a8d") +
  scale_fill_gradient(low = "#d0d1e6", high = "#045a8d") +
  theme_minimal_hgrid() +
  labs(
    title = "Age distribution at MDD diagnosis",
    subtitle = "IMASIS Hospital del Mar, Jan 2010–Jun 2025, n = 6320",
    x = "Age group (years)",
    y = "Proportion of cohort (%)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = "none", color = "none")
Fig1D

IndexYear <- cohortCharacterization_timeInvariant %>% filter(ANALYSIS_NAME == "DemographicsIndexYear") %>%
  mutate(COVARIATE_NAME = as.numeric(str_remove(COVARIATE_NAME, pattern = "index year: ")))

Fig1E <- IndexYear %>% ggplot(aes(x = COVARIATE_NAME, y = MEAN1*100)) +
  geom_line(color = "firebrick", lwd = 1.5) +
  geom_point(color = "firebrick", fill = "firebrick", alpha = 0.5, size = 5) +
  scale_x_continuous(name = "Year of diagnosis", breaks = 2010:2025) +
  ylim(0,15) +
  ylab("Proportion of cohort (%)") +
  theme_minimal_hgrid() +
  labs(title = "Cohort accrual by year of MDD diagnosis",
       subtitle = "IMASIS Hospital del Mar, Jan 2010–Jun 2025, n = 6320")
Fig1E

IndexMonth <- cohortCharacterization_timeInvariant %>% filter(ANALYSIS_NAME == "DemographicsIndexMonth") %>%
  mutate(COVARIATE_NAME = as.numeric(str_remove(COVARIATE_NAME, pattern = "index month: ")))

Fig1F <- IndexMonth %>% ggplot(aes(x = COVARIATE_NAME, y = MEAN1*100)) +
  geom_line(color = "slateblue", lwd = 1.5) +
  geom_point(color = "slateblue", fill = "slateblue", alpha = 0.5, size = 5) +
  geom_hline(yintercept = 100/12, lty = "dashed", color = "gray50") +
  scale_x_continuous(name = "Month of diagnosis", breaks = 1:12, labels = month.abb) +
  ylim(0,12.5) +
  ylab("Proportion of cohort (%)") +
  theme_minimal_hgrid() +
  labs(title = "MDD diagnoses by calendar month",
       subtitle = "Pooled Jan 2010–Jun 2025, n = 6320",
       caption = "Reference line at 8.3% indicates a uniform monthly distribution")
Fig1F

Fig1 <- plot_grid(Fig1A, Fig1B,Fig1C,Fig1D,Fig1E, Fig1F, labels = "AUTO", ncol = 2)
ggsave(filename = "figures/Figure01.pdf", plot = Fig1, height = 2*height, width = 2.5*width)


## Figure 2 ----

# Visit context


Fig2A <- 
  visit_context_MDD_JA_All_time %>%
    pivot_longer(cols = Before:After, names_to = "Context", values_to = "Count") %>%
    mutate(
      Context = factor(
        Context,
        levels = c("Before", "During", "Simultaneous", "After"),
        labels = c(
          "Visits before (ended before index)",
          "Visits ongoing (active at index, started earlier)",
          "Starting simultaneous (started on index date)",
          "Visits after (started after index)"
        )
      )
    ) %>%
    ggplot(aes(x = str_to_sentence(Visit.Concept.Name), y = Count, fill = Count)) +
    geom_bar(aes(colour = Count), position = "dodge", stat = "identity", alpha = 0.66) +
    geom_text(aes(label = Count),
              position = position_dodge(width = 1), vjust = -0.3, size = 5) +
    scale_color_gradient(low = "#d0d1e6", high = "#045a8d") +
    scale_fill_gradient(low = "#d0d1e6", high = "#045a8d") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    theme_cowplot(font_size = 16) +
    labs(
      title    = "Healthcare visits relative to the incident MDD diagnosis date",
      subtitle = "Cohort: MDD_JA All time (first-ever MDD diagnosis, age \u2265 12, 2010\u20132025)",
      x        = "Care setting (OMOP visit concept)",
      y        = "Number of visits",
      caption  = paste(
        "Index date = cohort start date = first MDD diagnosis.",
        "Counts are visits, not unique patients; if a person re-enters the cohort, visits may be double-counted",
        "(not applicable here \u2014 cohort entry is limited to the earliest event per person).",
        sep = "\n"
      )
    ) +
    theme(
      axis.text.x  = element_text(angle = 45, hjust = 1),
      plot.caption = element_text(hjust = 0, face = "italic", size = 8.5),
      strip.text   = element_text(face = "bold", size = 16, lineheight = 1.1)
    ) +
    guides(fill = "none", color = "none") +
    facet_wrap(~Context)


visit_context_MDD_JA_All_time <- visit_context_MDD_JA_All_time %>% add_row(
  Visit.Concept.Name = "Total",
  Before = sum(visit_context_MDD_JA_All_time$Before, na.rm = T),
  During = sum(visit_context_MDD_JA_All_time$During, na.rm = T),
  Simultaneous = sum(visit_context_MDD_JA_All_time$Simultaneous, na.rm = T),
  After = sum(visit_context_MDD_JA_All_time$After, na.rm = T)
)

Fig2B <- 
  visit_context_MDD_JA_All_time %>%
    rename("Care setting (OMOP visit concept)" = Visit.Concept.Name,
           "Visits before" = Before,
           "Visits ongoing" = During,
           "Starting simultaneous" = Simultaneous,
           "Visits after" = After) %>%
  ggtexttable(rows = NULL, theme = ttheme("light", base_size = 16))

Fig2 <- plot_grid(Fig2A, Fig2B, ncol = 1, rel_heights = c(0.7, 0.3))
ggsave("figures/Figure02.pdf", height = height*1.5, width = width*2)




## Figure 3 ----

## Top frequent conditions (overlap) and drugs (start) in from 1 year to 1 month prior index

Fig3A <- 
  cohortCharacterization_365dto_31d %>%
  filter(ANALYSIS_NAME == "ConditionEraOverlap") %>%
  mutate(Count = round(MEAN1 * cohortCount %>% 
                         filter(Cohort.Name == unique(cohortCharacterization_365dto_31d$cohortName)) %>% 
                         pull(IMASIS_sep_persons)),
         COVARIATE_NAME = str_remove(COVARIATE_NAME, pattern = "condition_era: ")
  ) %>% slice_max(MEAN1, n = 20) %>%
  arrange(MEAN1) %>%
  mutate(COVARIATE_NAME = factor(COVARIATE_NAME, COVARIATE_NAME))%>%
  ggplot(aes(x = COVARIATE_NAME, y = MEAN1*100)) +
  geom_segment(aes(y=0, yend = MEAN1*100), color = "slateblue1") +
  geom_point(aes(size = Count), color = "slateblue1", ) + 
  labs(
    title = "Most frequent comorbidities in the year prior to MDD diagnosis",
    subtitle = "Window: 365 to 31 days before cohort entry, n = 6320",
    x = NULL,
    y = "Proportion of cohort (%)"
  ) +
  #geom_text(aes(y = MEAN1*100, label = Count), vjust = -1, size = 4, color = "slateblue1") +
  coord_flip() +
  scale_size(name = "Patient\ncount (n)") +
  theme_cowplot(font_size = 20) + theme(plot.title.position = "plot")
Fig3A  

Fig3B <-   
  cohortCharacterization_365dto_31d %>%
  filter(ANALYSIS_NAME == "DrugEraStart") %>%
  mutate(Count = round(MEAN1 * cohortCount %>% 
                         filter(Cohort.Name == unique(cohortCharacterization_365dto_31d$cohortName)) %>% 
                         pull(IMASIS_sep_persons)),
         COVARIATE_NAME = str_remove(COVARIATE_NAME, pattern = "drug_era: ") %>% str_to_title()
  ) %>% slice_max(MEAN1, n = 20) %>%
  arrange(MEAN1) %>%
  mutate(COVARIATE_NAME = factor(COVARIATE_NAME, COVARIATE_NAME))%>%
  ggplot(aes(x = COVARIATE_NAME, y = MEAN1*100)) +
  geom_segment(aes(y=0, yend = MEAN1*100), color = "slateblue1") +
  geom_point(aes(size = Count), shape = 18, color = "slateblue1") + 
  labs(
    title = "Most frequent initiated medications in the year before MDD diagnosis",
    subtitle = "Window: 365 to 31 days before cohort entry, n = 6320",
    x = NULL,
    y = "Proportion of cohort (%)"
  ) +
  #geom_text(aes(y = MEAN1*100, label = Count), vjust = -1, size = 4, color = "slateblue1") +
  coord_flip() +
  scale_size(name = "Patient\ncount (n)") +
  theme_cowplot(font_size = 20) + theme(plot.title.position = "plot")
Fig3B


## Top frequent conditions (overlap) and drugs (start) in 1 month prior index

Fig3C <- 
  cohortCharacterization_30dto_1d %>%
  filter(ANALYSIS_NAME == "ConditionEraOverlap") %>%
  mutate(Count = round(MEAN1 * cohortCount %>% 
                         filter(Cohort.Name == unique(cohortCharacterization_365dto_31d$cohortName)) %>% 
                         pull(IMASIS_sep_persons)),
         COVARIATE_NAME = str_remove(COVARIATE_NAME, pattern = "condition_era: ")
  ) %>% slice_max(MEAN1, n = 10) %>%
  arrange(MEAN1) %>%
  mutate(COVARIATE_NAME = factor(COVARIATE_NAME, COVARIATE_NAME))%>%
  ggplot(aes(x = COVARIATE_NAME, y = MEAN1*100)) +
  geom_segment(aes(y=0, yend = MEAN1*100), color = "firebrick") +
  geom_point(aes(size = Count), color = "firebrick") + 
  labs(
    title = "Most frequent comorbidities in the month before MDD diagnosis",
    subtitle = "Window: 30 days to 1 day before index, n = 6320",
    x = NULL,
    y = "Proportion of cohort (%)"
  ) +
  #geom_text(aes(y = MEAN1*100, label = Count), vjust = -1, size = 4, color = "darkorchid3") +
  coord_flip() +
  scale_size(name = "Patient\ncount (n)") +
  theme_cowplot(font_size = 20) + theme(plot.title.position = "plot")
Fig3C

Fig3D <- 
  cohortCharacterization_30dto_1d %>%
  filter(ANALYSIS_NAME == "DrugEraStart") %>%
  mutate(Count = round(MEAN1 * cohortCount %>% 
                         filter(Cohort.Name == unique(cohortCharacterization_365dto_31d$cohortName)) %>% 
                         pull(IMASIS_sep_persons)),
         COVARIATE_NAME = str_remove(COVARIATE_NAME, pattern = "drug_era: ") %>% str_to_title()
  ) %>% slice_max(MEAN1, n = 20) %>%
  arrange(MEAN1) %>%
  mutate(COVARIATE_NAME = factor(COVARIATE_NAME, COVARIATE_NAME))%>%
  ggplot(aes(x = COVARIATE_NAME, y = MEAN1*100)) +
  geom_segment(aes(y=0, yend = MEAN1*100), color = "firebrick") +
  geom_point(aes(size = Count), shape = 18, color = "firebrick") + 
  labs(
    title = "Most frequent initiated medications in the month before MDD diagnosis",
    subtitle = "Window: 30 days to 1 day before index, n = 6320",
    x = NULL,
    y = "Proportion of cohort (%)",
  ) +
  #geom_text(aes(y = MEAN1*100, label = Count), vjust = -1, size = 4, color = "darkorchid3") +
  coord_flip() +
  scale_size(name = "Patient\ncount (n)") +
  theme_cowplot(font_size = 20) + theme(plot.title.position = "plot")
Fig3D

Fig3 <- plot_grid(Fig3A, Fig3B, Fig3C, Fig3D, labels = "AUTO", ncol = 2)
ggsave("figures/Figure03.pdf", plot = Fig3, height = 1.5*height, width = width*2.5)


# 3. Epidemiology of diagnosed MDD ----

## Figure 4 ----

incidence_sex_year_MDD_JA_All_time <- incidence_sex_year %>% filter(cohortName == "MDD_JA All time")
incidence_sex_year_MDD_JA_All_time$cohortCount[incidence_sex_year_MDD_JA_All_time$cohortCount<0] <- NA
incidence_sex_year_MDD_JA_All_time$calendarYear <- ymd(incidence_sex_year_MDD_JA_All_time$calendarYear, truncated = 2L)

incidence_age_sex_year_MDD_JA_All_time <- incidence_age_sex_year %>% filter(cohortName == "MDD_JA All time")
incidence_age_sex_year_MDD_JA_All_time$cohortCount[incidence_age_sex_year_MDD_JA_All_time$cohortCount<0] <- NA
incidence_age_sex_year_MDD_JA_All_time$calendarYear <- ymd(incidence_age_sex_year_MDD_JA_All_time$calendarYear, truncated = 2L)


mean(incidence_sex_year_MDD_JA_All_time %>% filter(gender == "Female") %>% pull(incidenceRate) / incidence_sex_year_MDD_JA_All_time %>% filter(gender == "Male") %>% pull(incidenceRate))


incidence_sex_year_MDD_JA_All_time %>% filter(calendarYear == "2018-01-01", gender == "Female") %>% pull(cohortCount) %>% range()
## Incidence by year, sex, and age

Fig4A <- 
  incidence_sex_year_MDD_JA_All_time %>% 
  ggplot(aes(x = calendarYear, y = cohortCount)) +
  geom_line(aes(colour = gender)) +
  geom_point(aes(colour = gender, fill = gender, shape = gender), alpha = 0.5, size = 5) +
  scale_color_brewer(name = "Gender", type = "qual", palette = 2) +
  scale_fill_brewer(name = "Gender", type = "qual", palette = 2) +
  scale_shape(name = "Gender") +
  theme_cowplot() +
  labs(title = "Newly diagnosed MDD cases at Hospital del Mar, by year and sex",
       subtitle = "Incident cases, ages ≥12, Jan 2010–Jun 2025*",
       x = "Year of diagnosis",
       y = "Number of incident cases")
Fig4A


Fig4B <- 
  incidence_sex_year_MDD_JA_All_time %>% 
  ggplot(aes(x = calendarYear, y = incidenceRate)) +
  geom_line(aes(colour = gender)) +
  geom_point(aes(colour = gender, fill = gender, shape = gender), alpha = 0.5, size = 5) +
  scale_color_brewer(name = "Gender", type = "qual", palette = 2) +
  scale_fill_brewer(name = "Gender", type = "qual", palette = 2) +
  scale_shape(name = "Gender") +
  theme_cowplot() +
  labs(title = "MDD incidence rate at Hospital del Mar, by year and sex",
       subtitle = "Incident cases per 1000 person-years, ages ≥12, Jan 2010–Jun 2025",
       x = "Year of diagnosis",
       y = "Incidence rate (per 1000 person-years)",
       caption = "Rate per 1000 person-years of eligible observation")

Fig4C <- 
  incidence_age_sex_year_MDD_JA_All_time %>% filter(ageGroup != "0-9") %>%
  ggplot(aes(x = calendarYear, y = cohortCount)) +
  geom_line(aes(colour = gender)) +
  geom_point(aes(colour = gender, fill = gender, shape = gender), alpha = 0.5, size = 5) +
  scale_color_brewer(name = "Gender", type = "qual", palette = 2) +
  scale_fill_brewer(name = "Gender", type = "qual", palette = 2) +
  scale_shape(name = "Gender") +
  theme_cowplot() +
  labs(title = "Newly diagnosed MDD cases at Hospital del Mar, by year and sex stratified by 10-year age band",
       subtitle = "Incident cases, ages ≥12, Jan 2010–Jun 2025",
       x = "Year of diagnosis",
       y = "Number of incident cases") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ageGroup, nrow = 1)

Fig4D <- 
  incidence_age_sex_year_MDD_JA_All_time %>% filter (ageGroup != "0-9") %>% 
  ggplot(aes(x = calendarYear, y = incidenceRate)) +
  geom_line(aes(colour = gender)) +
  geom_point(aes(colour = gender, fill = gender, shape = gender), alpha = 0.5, size = 5) +
  scale_color_brewer(name = "Gender", type = "qual", palette = 2) +
  scale_fill_brewer(name = "Gender", type = "qual", palette = 2) +
  scale_shape(name = "Gender") +
  theme_cowplot() +
  labs(title = "MDD incidence rate at Hospital del Mar, by year and sex stratified by 10-year age band",
       subtitle = "Incident cases per 1000 person-years, ages ≥12, Jan 2010–Jun 2025",
       x = "Year of diagnosis",
       y = "Incidence rate (per 1000 person-years)",
       caption = "Rate per 1000 person-years of eligible observation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ageGroup, nrow = 1)

Fig4AB <- plot_grid(Fig4A, Fig4B, ncol = 2, labels = "AUTO")

Fig4 <- plot_grid(Fig4AB, Fig4C, Fig4D, ncol = 1, labels = c("", "C", "D"))
ggsave(filename = "figures/Figure04.pdf", plot = Fig4, height = height, width = width*2)


# 4. AD type characterization  ----

## Time distribution

timeDistribution_SSRI_NSRI_otherAD <- timeDistribution %>% filter(Cohort.Id %in% c(230, 231, 237))

timeDistribution_SSRI_NSRI_otherAD$Time.Measure <- factor(c("Prior treatment initiation", "Available follow-up after initiation (days)", "Initial treatment episode duration"),
                                                          levels = c("Initial treatment episode duration", "Available follow-up after initiation (days)", "Prior treatment initiation"))

timeDistribution_SSRI_NSRI_otherAD <- timeDistribution_SSRI_NSRI_otherAD %>%
  mutate(Cohort = factor(
    Cohort,
    levels = Cohort,
    labels = c(
      "SSRI",
      "SSRI",
      "SSRI",
      "NSRI",
      "NSRI",
      "NSRI",
      "Other ADs",
      "Other ADs",
      "Other ADs"
    )
  ))

## Figure 5 ----


hlines <- data.frame(Average = c(30, 90, 180 ,365))
hlines$Time.Measure <- factor("Initial treatment episode duration")


Fig5A <- 
  timeDistribution_SSRI_NSRI_otherAD %>% filter(Time.Measure != "Prior treatment initiation") %>%
  ggplot(aes(x = Cohort)) +
  geom_boxplot(
    aes(
      ymin   = P10, # whiskers represent P10 and P90 percentiles
      lower  = P25,
      middle = Median,
      upper  = P75,
      ymax   = P90
    ),
    stat = "identity",
    color = "slateblue",
    fill = "slateblue",
    alpha = 0.33,
    width = 0.4
  ) +
  geom_point(aes(y = Average), shape = 18, size = 5, color = "firebrick") +
  #coord_flip() +
  theme_cowplot() +
  labs(
    title = "Persistence on initial antidepressant episode vs available follow-up, by drug class",
    subtitle = "First-ever class exposure with prior MDD diagnosis, IMASIS Hospital del Mar, Jan 2010–Jun 2025",
    x = NULL,
    y = "Episode persistence (days)",
    caption = "Episode = continuous exposure with ≤7-day gaps between prescriptions. Available follow-up = days from AD initiation to end of observation period. Box: P25–P75. Line: median. Diamond: mean. Whiskers: P10-P90."
  ) + facet_wrap(~Time.Measure, scale = "free") +
  geom_hline(data = hlines, aes(yintercept = Average), lty = 2, lwd = 0.25, alpha = 0.5) +
  geom_text(data = hlines, aes(x = 0.55, y = Average, label = Average), size = 3, color = "gray50")
Fig5A

## Drug class characterization


index_event_ADs <- bind_rows(index_event_MDD_SSRI_RW, index_event_MDD_NSRI_RW, index_event_MDD_otherAds_RW)
index_event_ADs$cohortName <- factor(index_event_ADs$cohortName, 
                                     levels = c("MDD_SSRI_RW", "MDD_NSRI_RW", "MDD_otherAds_RW"),
                                     labels = c("SSRI (n=2080)", "NSRI (n=610)", "Other ADs (n=2834)"))

Fig5B <- 
  index_event_ADs %>% arrange(Prop) %>%
  slice_max(Prop, n = 10, by = cohortName) %>%
  mutate(Concept.Name = factor(Concept.Name, levels = Concept.Name)) %>%
  ggplot(aes(x = Prop*100, y = Concept.Name)) +
  geom_segment(aes(y = Concept.Name, x = 0, xend = 100*Prop), color = "firebrick", show.legend = FALSE) +
  geom_point(aes(y = Concept.Name, x = 100*Prop, size = IMASIS_sep_persons), color = "firebrick") +
  scale_color_brewer(name = "Patient\ncount (n)", type = "qual", palette = 6) +
  #scale_shape_manual(name = "Patient\ncount (n)", values = c(19,17,18)) +
  scale_size(name = "Patient\ncount (n)") +
  coord_flip() +
  labs(
    title = "10 most common medications driving cohort entry, by class",
    subtitle = "IMASIS Hospital del Mar, Jan 2010–Jun 2025",
    x = "Proportion of cohort (%)",
    y = "Concept name"
  ) +
  theme_cowplot() +
  xlim(c(0,25)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~cohortName, scale = "free")
Fig5B

# Age distribution

AgeGroupADs_timeInvariant <- bind_rows(
  cohortCharacterization_MDD_SSRI_RW_timeInvariant,
  cohortCharacterization_MDD_NSRI_RW_timeInvariant,
  cohortCharacterization_MDD_otherAds_RW_timeInvariant
)

AgeGroupADs_timeInvariant <-  AgeGroupADs_timeInvariant %>% filter(ANALYSIS_NAME == "DemographicsAgeGroup") %>%
  mutate(COVARIATE_NAME = str_remove(COVARIATE_NAME, pattern = "age group:  "))


AgeGroupADs_timeInvariant$cohortName <- factor(AgeGroupADs_timeInvariant$cohortName, 
                                     levels = c("MDD_SSRI_RW", "MDD_NSRI_RW", "MDD_otherAds_RW"),
                                     labels = c("SSRI (n=2080)", "NSRI (n=610)", "Other ADs (n=2834)"))

Fig5C <- 
  AgeGroupADs_timeInvariant %>% ggplot(aes(x = COVARIATE_NAME, y = 100*MEAN1, fill = cohortName)) +
    geom_bar(aes(colour = cohortName),position = "dodge", stat = "identity", alpha = 0.66) +
    #geom_text(aes(label = paste0(MEAN1*100,"%"), color = cohortName), position = position_dodge(width = 1), angle = 45, vjust = -1, hjust = 0) +
    theme_minimal_hgrid() +
    labs(
      title = "Age distribution at first AD administration, by drug class",
      subtitle = "IMASIS Hospital del Mar, Jan 2010–Jun 2025",
      x = "Age group (years)",
      y = "Proportion of class cohort (%)"
    ) +
    scale_color_brewer(name = "Drug class", type = "qual", palette = 6) +
    scale_fill_brewer(name = "Drug class", type = "qual", palette = 6) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
Fig5C  



# AD prescription by year, sex, and drug class

incidence_sex_year_ADs <- tibble()

drug_classes <- c("MDD_SSRI_RW", "MDD_NSRI_RW", "MDD_otherAds_RW")

for (i in drug_classes) {
  df <- incidence_sex_year %>% filter(cohortName == i)
  df$cohortCount[df$cohortCount<0] <- NA
  df$calendarYear <- ymd(df$calendarYear, truncated = 2L)
  incidence_sex_year_ADs <- bind_rows(incidence_sex_year_ADs, df)
}

incidence_sex_year_ADs$cohortName <- factor(incidence_sex_year_ADs$cohortName, 
                                               levels = c("MDD_SSRI_RW", "MDD_NSRI_RW", "MDD_otherAds_RW"),
                                               labels = c("SSRI (n=2080)", "NSRI (n=610)", "Other ADs (n=2834)"))

Fig5D <- 
  incidence_sex_year_ADs %>% 
    ggplot(aes(x = calendarYear, y = incidenceRate)) +
    geom_line(aes(colour = gender)) +
    geom_point(aes(colour = gender, fill = gender, shape = gender), alpha = 0.5, size = 5) +
    scale_color_brewer(name = "Gender", type = "qual", palette = 2) +
    scale_fill_brewer(name = "Gender", type = "qual", palette = 2) +
    scale_shape(name = "Gender") +
    theme_cowplot() +
    labs(title = "AD first prescription rate at Hospital del Mar, by year, sex, and drug class",
         subtitle = "Incident cases per 1000 person-years, ages ≥12, Jan 2010–Jun 2025",
         x = "Year of prescription",
         y = "Prescription rate (per 1000 person-years)",
         caption = "Rate per 1000 person-years of eligible observation") +
    facet_wrap(~cohortName)


Fig5AB <- plot_grid(Fig5A, Fig5B, ncol = 1, labels = "AUTO", rel_heights = c(0.4, 0.6))
Fig5CD <- plot_grid(Fig5C, Fig5D, nrow = 1, labels = c("C", "D"))
Fig5 <- plot_grid(Fig5AB, Fig5CD, ncol = 1, rel_heights = c(0.66, 0.33))
ggsave("figures/Figure05.pdf", plot = Fig5, height = height*2, width = width*2)

# Previous comorbidities and drugs

## Figure 6 ----

calculateCounts <- function(data, drug_classes, analysis_name) {
  df <- tibble()
  for (i in drug_classes) {
    df <- df %>% bind_rows(
      data %>%
        filter(ANALYSIS_NAME == analysis_name, cohortName == i) %>%
        mutate(
          Count = round(
            MEAN1 * cohortCount %>%
              filter(Cohort.Name == i) %>%
              pull(IMASIS_sep_persons)
          )
        )
    )
  }
  return(df)
}

# 1 year prior AD

cohortCharacterization_AD_365dto_31d <- bind_rows(
  cohortCharacterization_MDD_SSRI_RW_365dto_31d,
  cohortCharacterization_MDD_NSRI_RW_365dto_31d,
  cohortCharacterization_MDD_otherAds_RW_365dto_31d
)


Conditions_AD_365dto_31d <- calculateCounts(
  cohortCharacterization_AD_365dto_31d,
  drug_classes = drug_classes,
  analysis_name = "ConditionEraOverlap"
) %>%
  mutate(
    COVARIATE_NAME = str_remove(COVARIATE_NAME, pattern = "condition_era: "),
    cohortName = factor(.data$cohortName, 
           levels = c("MDD_SSRI_RW", "MDD_NSRI_RW", "MDD_otherAds_RW"),
           labels = c("SSRI (n=2080)", "NSRI (n=610)", "Other ADs (n=2834)"))
  )




Fig6A <- 
  Conditions_AD_365dto_31d %>% 
  filter(!CONCEPT_ID %in% MDD_JA$CONCEPT_ID[-which(MDD_JA$isExcluded)]) %>%
  slice_max(MEAN1, n = 10, by = cohortName) %>%
    arrange((MEAN1)) %>%
    mutate(
      unique_name = paste(cohortName, COVARIATE_NAME, sep = "___"),
      unique_name = factor(unique_name, levels = unique(unique_name))
    ) %>%
    ggplot(aes(x = MEAN1*100, y = unique_name)) +
    geom_segment(aes(y = unique_name, x = 0, xend = MEAN1*100), color = "slateblue1", show.legend = FALSE) +
    geom_point(aes(size = Count), color = "slateblue1") +
    labs(
      title = "Selected most frequent comorbidities in the year prior to AD initiation, by drug class",
      subtitle = "Window: 365 to 31 days before cohort entry",
      x = "Proportion of cohort class (%)",
      y = NULL,
      caption = "Condition concepts related to MDD diagnosis are not presented"
    ) +
    scale_y_discrete(labels = function(x) sub(".*___", "", x)) +
    scale_size(name = "Patient\ncount (n)") +
    #coord_flip() +
    theme_cowplot() +
    facet_wrap(~cohortName, scale = "free")
Fig6A


# 1 month prior

cohortCharacterization_AD_30dto_1d <- bind_rows(
  cohortCharacterization_MDD_SSRI_RW_30dto_1d,
  cohortCharacterization_MDD_NSRI_RW_30dto_1d,
  cohortCharacterization_MDD_otherAds_RW_30dto_1d
)


Conditions_AD_30dto_1d <- calculateCounts(
  cohortCharacterization_AD_30dto_1d,
  drug_classes = drug_classes,
  analysis_name = "ConditionEraOverlap"
) %>%
  mutate(
    COVARIATE_NAME = str_remove(COVARIATE_NAME, pattern = "condition_era: "),
    cohortName = factor(.data$cohortName, 
                        levels = c("MDD_SSRI_RW", "MDD_NSRI_RW", "MDD_otherAds_RW"),
                        labels = c("SSRI (n=2080)", "NSRI (n=610)", "Other ADs (n=2834)"))
  )


Fig6B <- 
  Conditions_AD_30dto_1d %>%
  filter(!CONCEPT_ID %in% MDD_JA$CONCEPT_ID[-which(MDD_JA$isExcluded)]) %>%
  slice_max(MEAN1, n = 10, by = cohortName) %>%
    arrange((MEAN1)) %>%
    mutate(
      unique_name = paste(cohortName, COVARIATE_NAME, sep = "___"),
      unique_name = factor(unique_name, levels = unique(unique_name))
    ) %>%
    ggplot(aes(x = MEAN1*100, y = unique_name)) +
    geom_segment(aes(y = unique_name, x = 0, xend = MEAN1*100), color = "firebrick", show.legend = FALSE) +
    geom_point(aes(size = Count), color = "firebrick") +
    labs(
      title = "Selected most frequent comorbidities in the month prior to AD initiation, by drug class",
      subtitle = "Window: 30 to 1 days before cohort entry",
      x = "Proportion of cohort class (%)",
      y = NULL,
      caption = "Condition concepts related to MDD are not presented"
    ) +
    scale_y_discrete(labels = function(x) sub(".*___", "", x)) +
    scale_size(name = "Patient\ncount (n)") +
    theme_cowplot() +
    facet_wrap(~cohortName, scale = "free")
Fig6B

Fig6 <- plot_grid(Fig6A, Fig6B, ncol = 1, labels = "AUTO")
ggsave("figures/Figure06.pdf", plot = Fig6, height = 1.5*height, width = 2.5*width)

## Figure 7 ----

Drugs_AD_365dto_31d <- calculateCounts(
  cohortCharacterization_AD_365dto_31d,
  drug_classes = drug_classes,
  analysis_name = "DrugEraStart"
) %>%
  mutate(
    COVARIATE_NAME = str_remove(COVARIATE_NAME, pattern = "drug_era: ") %>% str_to_title(),
    cohortName = factor(.data$cohortName, 
                        levels = c("MDD_SSRI_RW", "MDD_NSRI_RW", "MDD_otherAds_RW"),
                        labels = c("SSRI (n=2080)", "NSRI (n=610)", "Other ADs (n=2834)"))
  )


Fig7A <- 
  Drugs_AD_365dto_31d %>%
  slice_max(MEAN1, n = 20, by = cohortName) %>%
  arrange((MEAN1)) %>%
  mutate(
    unique_name = paste(cohortName, COVARIATE_NAME, sep = "___"),
    unique_name = factor(unique_name, levels = unique(unique_name))
  ) %>%
  ggplot(aes(x = MEAN1*100, y = unique_name)) +
  geom_segment(aes(y = unique_name, x = 0, xend = MEAN1*100), color = "slateblue4", show.legend = FALSE) +
  geom_point(aes(size = Count), color = "slateblue4") +
  labs(
    title = "Most frequent prior medications in the year before AD initiation, by drug class",
    subtitle = "Window: 365 to 31 days before cohort entry",
    x = "Proportion of cohort class (%)",
    y = NULL
  ) +
  scale_y_discrete(labels = function(x) sub(".*___", "", x)) +
  scale_size(name = "Patient\ncount (n)") +
  theme_cowplot() +
  facet_wrap(~cohortName, scale = "free")
Fig7A




Drugs_AD_30dto_1d <- calculateCounts(
  cohortCharacterization_AD_30dto_1d,
  drug_classes = drug_classes,
  analysis_name = "DrugEraStart"
) %>%
  mutate(
    COVARIATE_NAME = str_remove(COVARIATE_NAME, pattern = "drug_era: ") %>% str_to_title(),
    cohortName = factor(.data$cohortName, 
                        levels = c("MDD_SSRI_RW", "MDD_NSRI_RW", "MDD_otherAds_RW"),
                        labels = c("SSRI (n=2080)", "NSRI (n=610)", "Other ADs (n=2834)"))
  )


Fig7B <- 
  Drugs_AD_30dto_1d %>%
    slice_max(MEAN1, n = 20, by = cohortName) %>%
    arrange((MEAN1)) %>%
    mutate(
      unique_name = paste(cohortName, COVARIATE_NAME, sep = "___"),
      unique_name = factor(unique_name, levels = unique(unique_name))
    ) %>%
    ggplot(aes(x = MEAN1*100, y = unique_name)) +
    geom_segment(aes(y = unique_name, x = 0, xend = MEAN1*100), color = "firebrick4", show.legend = FALSE) +
    geom_point(aes(size = Count), color = "firebrick4") +
    labs(
      title = "Most frequent prior medications in the month before AD initiation, by drug class",
      subtitle = "Window: 30 to 1 days before cohort entry",
      x = "Proportion of cohort class (%)",
      y = NULL
    ) +
    scale_y_discrete(labels = function(x) sub(".*___", "", x)) +
    scale_size(name = "Patient\ncount (n)") +
    theme_cowplot() +
    facet_wrap(~cohortName, scale = "free")

Fig7 <- plot_grid(Fig7A, Fig7B, ncol = 1, labels = "AUTO")
ggsave("figures/Figure07.pdf", plot = Fig7, height = height*1.5, width = width*3.5)

# Compare characterization

cohortComp <- function(data, cohortCount, target, comparator) {
  n_target <- filter(cohortCount, Cohort.Name == target) %>% pull(IMASIS_sep_persons)
  n_comparator <- filter(cohortCount, Cohort.Name == comparator) %>% pull(IMASIS_sep_persons)
  data <- data %>% mutate(
    target_N = round(target * n_target,0),
    comparator_N = round(comparator * n_comparator,0)
  )
  return(data)
}

SSRI_vs_NSRI_timeInvariant$Comparison <- "SSRI_vs_NSRI"
SSRI_vs_NSRI_timeInvariant <- 
  cohortComp(
    data = SSRI_vs_NSRI_timeInvariant,
    cohortCount = cohortCount,
    target = "MDD_SSRI_RW",
    comparator = "MDD_NSRI_RW"
  ) 

SSRI_vs_otherAds_timeInvariant$Comparison <- "SSRI_vs_otherAds"
SSRI_vs_otherAds_timeInvariant <- 
  cohortComp(
    data = SSRI_vs_otherAds_timeInvariant,
    cohortCount = cohortCount,
    target = "MDD_SSRI_RW",
    comparator = "MDD_otherAds_RW"
  ) 

NSRI_vs_otherAds_timeInvariant$Comparison <- "NSRI_vs_otherAds"
NSRI_vs_otherAds_timeInvariant <- 
  cohortComp(
    data = NSRI_vs_otherAds_timeInvariant,
    cohortCount = cohortCount,
    target = "MDD_NSRI_RW",
    comparator = "MDD_otherAds_RW"
  )


SSRI_vs_NSRI_365dto1d$Comparison <- "SSRI_vs_NSRI"
SSRI_vs_NSRI_365dto1d <- 
  cohortComp(
    data = SSRI_vs_NSRI_365dto1d,
    cohortCount = cohortCount,
    target = "MDD_SSRI_RW",
    comparator = "MDD_NSRI_RW"
  ) %>% filter(analysisName %in% c("ConditionEraGroupOverlap", "DrugEraGroupOverlap"))

SSRI_vs_otherAds_365dto1d$Comparison <- "SSRI_vs_otherAds"
SSRI_vs_otherAds_365dto1d <- 
  cohortComp(
    data = SSRI_vs_otherAds_365dto1d,
    cohortCount = cohortCount,
    target = "MDD_SSRI_RW",
    comparator = "MDD_otherAds_RW"
  ) %>% filter(analysisName %in% c("ConditionEraGroupOverlap", "DrugEraGroupOverlap"))

NSRI_vs_otherAds_365dto1d$Comparison <- "NSRI_vs_otherAds"
NSRI_vs_otherAds_365dto1d <- 
  cohortComp(
    data = NSRI_vs_otherAds_365dto1d,
    cohortCount = cohortCount,
    target = "MDD_NSRI_RW",
    comparator = "MDD_otherAds_RW"
  ) %>% filter(analysisName %in% c("ConditionEraGroupOverlap", "DrugEraGroupOverlap"))


## Table 3: Baseline characteristics by drug class ----

n_SSRI <- cohortCount %>% filter(Cohort.Name == "MDD_SSRI_RW") %>% pull(IMASIS_sep_persons)
n_NSRI <- cohortCount %>% filter(Cohort.Name == "MDD_NSRI_RW") %>% pull(IMASIS_sep_persons)
n_otherADs <- cohortCount %>% filter(Cohort.Name == "MDD_otherAds_RW") %>% pull(IMASIS_sep_persons)

get_comp_row_timeInv <- function(cov, label, type = c("prop", "mean"), analysis = c("timeInvariant", "-365dto-1d")) {
  type <- match.arg(type)
  analysis <- match.arg(analysis)
  if (analysis == "timeInvariant") {
  r_sn <- SSRI_vs_NSRI_timeInvariant %>% filter(tolower(covariateName) == tolower(cov))
  r_so <- SSRI_vs_otherAds_timeInvariant %>% filter(tolower(covariateName) == tolower(cov))
  r_no <- NSRI_vs_otherAds_timeInvariant %>% filter(tolower(covariateName) == tolower(cov))
  } 
  else if (analysis == "-365dto-1d") {
    r_sn <- SSRI_vs_NSRI_365dto1d %>% filter(tolower(covariateName) == tolower(cov))
    r_so <- SSRI_vs_otherAds_365dto1d %>% filter(tolower(covariateName) == tolower(cov))
    r_no <- NSRI_vs_otherAds_365dto1d %>% filter(tolower(covariateName) == tolower(cov))
  }

  if (type == "prop") {
    ssri_fmt <- paste0(round(r_sn$target * n_SSRI), " (", round(r_sn$target * 100, 1), "%)")
    nsri_fmt <- paste0(round(r_sn$comparator * n_NSRI), " (", round(r_sn$comparator * 100, 1), "%)")
    other_fmt <- paste0(round(r_so$comparator * n_otherADs)," (", round(r_so$comparator * 100, 1), "%)")
  } else {
    ssri_fmt <- paste0(round(r_sn$target, 2), " (", round(r_sn$sdT, 2), ")")
    nsri_fmt <- paste0(round(r_sn$comparator,2), " (", round(r_sn$sdC, 2), ")")
    other_fmt <- paste0(round(r_so$comparator,2), " (", round(r_so$sdC, 2), ")")
  }

  tibble(
    Characteristic = label,
    !!paste0("SSRI (n=", n_SSRI, ")") := ssri_fmt,
    !!paste0("NSRI (n=", n_NSRI, ")") := nsri_fmt,
    !!paste0("Other ADs (n=", n_otherADs,")") := other_fmt,
    `StdDiff SSRI vs NSRI` = round(r_sn$StdDiff, 3),
    `StdDiff SSRI vs Other ADs` = round(r_so$StdDiff, 3),
    `StdDiff NSRI vs Other ADs` = round(r_no$StdDiff, 3)
  )
}

Table5 <- bind_rows(
  get_comp_row_timeInv("Gender = female", "Female, n (%)", type = "prop", analysis = "timeInvariant"),
  get_comp_row_timeInv("Age in years", "Mean age, years (SD)", type = "mean", analysis = "timeInvariant"),
  get_comp_row_timeInv("Charlson index - romano adaptation", "Charlson index, mean (SD)", type = "mean", analysis = "timeInvariant"),
  get_comp_row_timeInv("Diabetes comorbidity severity index (dcsi)", "DCSI, mean (SD)", type = "mean", analysis = "timeInvariant")
)

Table5

ggTable5 <- 
  Table5 %>%
    ggtexttable(rows = NULL, theme = ttheme("light", base_size = 11)) %>%
    tab_add_title(
      text = "Table 3. Baseline characteristics by first antidepressant class. IMASIS Hospital del Mar, Jan 2010–Jun 2025",
      face = "bold", size = 12
    )
ggsave(filename = "figures/Table03.pdf", ggTable5, height = width, width = height*1.5)
ggsave(filename = "figures/Table03.png", ggTable5, height = width, width = height*1.5, dpi = 300)


SSRI_vs_NSRI_365dto1d %>% filter(analysisName == "ConditionEraGroupOverlap") %>%
  slice_max(order_by = StdDiff, n = 5) 


SSRI_vs_otherAds_365dto1d %>% filter(analysisName == "ConditionEraGroupOverlap") %>%
  slice_max(order_by = StdDiff, n = 5) 


NSRI_vs_otherAds_365dto1d %>% filter(analysisName == "ConditionEraGroupOverlap") %>%
  slice_max(order_by = StdDiff, n = 5) 

## Table 4: Conditions in the previous year by drug class ----

Table4 <- bind_rows(
  get_comp_row_timeInv("Mild depression", "Mild depression, n (%)", type = "prop", analysis = "-365dto-1d"),
  get_comp_row_timeInv("Thoughts of self harm", "Thoughts of self harm, n (%)", type = "prop", analysis = "-365dto-1d"),
  get_comp_row_timeInv("Suicidal thoughts", "Suicidal thoughts, n (%)", type = "prop", analysis = "-365dto-1d"),
  get_comp_row_timeInv("Disturbance in thinking", "Disturbance in thinking, n (%)", type = "prop", analysis = "-365dto-1d"),
  get_comp_row_timeInv("Lesion of joint", "Lesion of joint, n (%)", type = "prop", analysis = "-365dto-1d"),
  get_comp_row_timeInv("Disorder caused by cocaine", "Disorder caused by cocaine, n (%)", type = "prop", analysis = "-365dto-1d"),
  get_comp_row_timeInv("Bipolar disorder", "Bipolar disorder, n (%)", type = "prop", analysis = "-365dto-1d"),
  get_comp_row_timeInv("Recurrent major depressive episodes, severe, with psychosis", "Recurrent major depressive episodes,\nsevere, with psychosis, n (%)", type = "prop", analysis = "-365dto-1d"),
  get_comp_row_timeInv("Peripheral nerve disease", "Peripheral nerve disease, n (%)", type = "prop", analysis = "-365dto-1d"),
  get_comp_row_timeInv("Recurrent depression", "Recurrent depression, n (%)", type = "prop", analysis = "-365dto-1d"),
)

Table4

ggTable4 <- 
  Table4 %>% rename("Condition group" = Characteristic) %>%
    ggtexttable(rows = NULL, theme = ttheme("light", base_size = 11)) %>%
    tab_add_title(
      text = "Table 4. Comparison of conditions in the previous year to AD exposure, by drug class. IMASIS Hospital del Mar, Jan 2010–Jun 2025",
      face = "bold", size = 12
    )
ggsave(filename = "figures/Table04.pdf", ggTable4, height = width, width = height*1.5)
ggsave(filename = "figures/Table04.png", ggTable4, height = width, width = height*1.5, dpi = 300)




SSRI_vs_NSRI_365dto1d %>% filter(analysisName == "DrugEraGroupOverlap") %>%
  slice_max(order_by = StdDiff, n = 10) 


SSRI_vs_otherAds_365dto1d %>% filter(analysisName == "DrugEraGroupOverlap") %>%
  slice_max(order_by = StdDiff, n = 10) 


NSRI_vs_otherAds_365dto1d %>% filter(analysisName == "DrugEraGroupOverlap") %>%
  slice_max(order_by = StdDiff, n = 10) 


## Table 5: Drugs in the previous year by drug class ----

Table5 <- bind_rows(
  get_comp_row_timeInv("Psychoanaleptics", "Psychoanaleptics, n (%)", type = "prop", analysis = "-365dto-1d"),
  get_comp_row_timeInv("Venlafaxine", "Venlafaxine, n (%)", type = "prop", analysis = "-365dto-1d"),
  get_comp_row_timeInv("Nervous system", "Nervous system, n (%)", type = "prop", analysis = "-365dto-1d"),
  get_comp_row_timeInv("Synthetic anticholinergic agents in combination with psycholeptics", "Synthetic anticholinergic agents in\ncombination with psycholeptics, n (%)", type = "prop", analysis = "-365dto-1d"),
  get_comp_row_timeInv("Mirtazapine", "Mirtazapine, n (%)", type = "prop", analysis = "-365dto-1d"),
  get_comp_row_timeInv("Trazodone", "Trazodone, n (%)", type = "prop", analysis = "-365dto-1d"),
  get_comp_row_timeInv("Sertraline", "Sertraline, n (%)", type = "prop", analysis = "-365dto-1d"),
  get_comp_row_timeInv("Duloxetine", "Duloxetine, n (%)", type = "prop", analysis = "-365dto-1d"),
  get_comp_row_timeInv("Escitalopram", "Escitalopram, n (%)", type = "prop", analysis = "-365dto-1d"),
  get_comp_row_timeInv("Opium alkaloids and derivatives ", "Opium alkaloids and derivatives , n (%)", type = "prop", analysis = "-365dto-1d"),
  get_comp_row_timeInv("Citalopram", "Citalopram, n (%)", type = "prop", analysis = "-365dto-1d"),
  get_comp_row_timeInv("Diazepines, oxazepines, thiazepines and oxepines", "Diazepines, oxazepines, thiazepines and oxepines, n (%)", type = "prop", analysis = "-365dto-1d"),
)

Table5

ggTable5 <- 
  Table5 %>% rename("Drug group" = Characteristic) %>%
    ggtexttable(rows = NULL, theme = ttheme("light", base_size = 11)) %>%
    tab_add_title(
      text = "Table 5. Comparison of drugs in the previous year to AD exposure, by drug class. IMASIS Hospital del Mar, Jan 2010–Jun 2025",
      face = "bold", size = 12
    )
ggsave(filename = "figures/Table05.pdf", ggTable5, height = width, width = height*1.5)
ggsave(filename = "figures/Table05.png", ggTable5, height = width, width = height*1.5, dpi = 300)


# 5. First AD exposure after first MDD diagnosis ----

cohortOverlap_cal <- function(data, cohort, T_window_names = c("0", "1 to 30", "31 to 365", ">365")) {
  data <- na.omit(data)
  data <- data %>% filter(Comparator == cohort)
  Target_n <- sum(data[1, c("Target_only", "Both")])
  AD_total <- data %>% filter(T_window == "T (-9999d to 9999d)") %>% pull(Both)
  data <- data %>% filter(T_window %in% c("T (0d to 0d)", "T (1d to 30d)", "T (31d to 365d)")) %>% 
    select(T_window, Both)
  data <- data %>% bind_rows(
    tibble(
      T_window = "T (>365)",
      Both = AD_total - sum(data$Both)
    )
  ) %>% rename(
    Count = Both
  )
  
  data <- data %>% mutate_at(c("Count"),
                             .funs = list(
                               Prop_MDD = function(x) {
                                 x / Target_n
                               }
                             ))
  CumSum <- apply(data[, c("Count", "Prop_MDD")], 2, cumsum)
  data$Count_sum <- CumSum[,"Count"]
  data$Prop_sum <- CumSum[, "Prop_MDD"]
  
  if (!is.null(T_window_names)) {
    data$T_window <- T_window_names
    data$T_window <- factor(data$T_window, levels = T_window_names)
  }
  
  return(data)
}



(SSRI <- cohortOverlap_cal(cohortOverlap, 
                           cohort = 230, 
                           T_window_names = c("0 (same day)", "1 to 30 days", "31 to 365 days", ">365 days")
) %>% mutate(cohort = "MDD_SSRI_RW"))
(NSRI <- cohortOverlap_cal(cohortOverlap, 
                           cohort = 231,
                           T_window_names =  c("0 (same day)", "1 to 30 days", "31 to 365 days", ">365 days")
) %>% mutate(cohort = "MDD_NSRI_RW"))
(otherADs <- cohortOverlap_cal(cohortOverlap, 
                               cohort = 237,
                               T_window_names = c("0 (same day)", "1 to 30 days", "31 to 365 days", ">365 days")
) %>% mutate(cohort = "MDD_otherAds_RW"))

(Psycholeptics <- cohortOverlap_cal(cohortOverlap, 
                                    cohort = 229,
                                    T_window_names =  c("0 (same day)", "1 to 30 days", "31 to 365 days", ">365 days")
) %>% mutate(cohort = "MDD_Psycholeptics_RW"))

ADafterMDD <- bind_rows(SSRI, NSRI, otherADs, Psycholeptics)
ADafterMDD$cohort <- factor(ADafterMDD$cohort,
                            levels = c("MDD_SSRI_RW", "MDD_NSRI_RW",
                                       "MDD_otherAds_RW", "MDD_Psycholeptics_RW"),
                            labels = c("SSRI", "NSRI", "Other ADs", "Psycholeptics"))

# ADafterMDD <- ADafterMDD %>%
#   group_by(T_window) %>%
#   mutate(Prop_of_total = Count_sum / sum(Count_sum)) %>%
#   ungroup()

## Figure 8 ----

Fig8A <- 
  ADafterMDD %>% ggplot(aes(x = T_window, y = 100*Prop_sum, group = cohort)) +
  geom_point(aes(color = cohort, shape = cohort), size = 5, position = position_dodge(width = 0.25)) +
  geom_line(aes(x = T_window, y = 100 * Prop_sum, color = cohort), 
            alpha = 0.5, position = position_dodge(width = 0.25), lty=2) +
  #geom_segment(aes(y = 0, yend = 100*Prop_sum, colour = cohort), position = position_dodge(width = 0.5), lty=2) +
  geom_text(aes(label = paste0(round(Prop_sum*100, 2),"%"), color = cohort), 
            position = position_dodge(width = 0.25), vjust = -0.5, hjust = 0, angle = 45,
            show.legend = FALSE) +
  labs(
    title = "Cumulative incidence of antidepressant and psycholeptic initiation after newly-diagnosed MDD",
    subtitle = "Among 6320 newly-diagnosed MDD patients, IMASIS Hospital del Mar, Jan 2010–Jun 2025",
    y = "Cumulative proportion of the cohort (%)",
    x = "Time after MDD diagnosis",
    caption = "Follow-up varies by patient (median 1290 days, IQR 704 - 2220); patients with shorter follow-up cannot contribute to later time points, which biases later estimates downward."
  ) + 
  scale_color_brewer(type = "qual", palette = 2,  
                     name = "Drug class",
                     labels = c("SSRI", "NSRI", "Other ADs", "Psycholeptics")) +
  scale_shape_manual(values = c(19,15,17,18),  
                     name = "Drug class",
                     labels = c("SSRI", "NSRI", "Other ADs", "Psycholeptics")) +
  scale_x_discrete(labels = c("Same day (0 days)", "30 days", "365 days", "During follow-up")) +
  theme_cowplot()
Fig8A

Fig8B <- 
  ADafterMDD %>% 
  mutate(
    Prop_MDD = round(Prop_MDD*100,2),
    Prop_sum = round(Prop_sum*100, 2)
  ) %>%
  rename(
    "Time window" = T_window,
    "N" = Count,
    "Proportion of the cohort (%)" = Prop_MDD,
    "Cumulative sum" = Count_sum,
    "Cumulative proportion (%)" = Prop_sum,
    "Drug class" = cohort
  ) %>% 
  ggtexttable(rows = NULL,  theme = ttheme("light", base_size = 16)) %>% 
  tab_add_title(text = "Antidepressant and psycholeptic initiation after newly-diagnosed MDD, by drug class and time window", face = "bold", size = 16)
Fig8B

Fig8 <- plot_grid(Fig8A, Fig8B, ncol = 1)
Fig8
ggsave(filename = "figures/Figure08.pdf", plot = Fig8, height = height, width = width*2)

writeLines(capture.output(sessionInfo()), "sessionInfo.txt")
