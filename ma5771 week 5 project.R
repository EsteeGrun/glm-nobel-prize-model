# ============================================================
# MA5771 Week 5 - Data Prep + Exploratory Data Analysis (EDA)
# Project: Modeling the Probability That a Nobel Prize Is Shared
# Author: Katia Schrodinger
# Dataset: nobel_prizes_1901-2025_cleaned.csv
# Response: is_shared (0/1)
# Explanatory: award_year (numeric), category (factor), sex (factor),
#              winners_per_category (numeric count)
# ============================================================

# ---- Packages ----
library(tidyverse)
library(janitor)
library(scales)
library(stringr)

# ---- 1) Import Data ----
data_path <- "/Users/Katia/Desktop/nobel_prizes_1901-2025_cleaned.csv"

raw <- readr::read_csv(data_path, show_col_types = FALSE) %>%
  janitor::clean_names()

# Confirm raw is a data frame now
print(class(raw))
print(names(raw)[1:12])

# ---- 2) Select Variables + Clean ----
# NOTE: winners_per_category is in your dataset and replaces is_repeat_winner. [1](https://o365coloradoedu-my.sharepoint.com/personal/liri6518_colorado_edu/_layouts/15/Doc.aspx?sourcedoc=%7B60CA9759-5985-42CB-8394-7337551A0385%7D&file=nobel_prizes_1901-2025_cleaned.csv&action=default&mobileredirect=true)
df <- raw %>%
  dplyr::select(is_shared, award_year, category, sex, winners_per_category, birth_date) %>%
  dplyr::mutate(
    is_shared = as.integer(is_shared),
    award_year = as.integer(award_year),
    winners_per_category = as.integer(winners_per_category),
    category = as.factor(category),
    sex = as.factor(sex)
  ) %>%
  dplyr::filter(
    !is.na(is_shared),
    !is.na(award_year),
    !is.na(category),
    sex %in% c("male", "female"),
    !is.na(winners_per_category)
  )

# ---- 3) Derived Variables (optional but helpful) ----
df <- df %>%
  mutate(
    year_c_decade = (award_year - mean(award_year)) / 10,
    birth_year = suppressWarnings(as.integer(stringr::str_extract(birth_date, "\\d{4}"))),
    age_at_award = if_else(!is.na(birth_year), award_year - birth_year, NA_integer_)
  )

# Check final dataset
glimpse(df)

# ---- 4) Basic Summaries ----
n_total <- nrow(df)
p_shared <- mean(df$is_shared)

cat("\n--- Sample size ---\n")
print(n_total)

cat("\n--- Overall proportion shared ---\n")
print(p_shared)

cat("\n--- Shared vs Not Shared counts ---\n")
print(table(df$is_shared))

cat("\n--- Category counts ---\n")
print(table(df$category))

cat("\n--- Sex counts ---\n")
print(table(df$sex))

cat("\n--- Winners per category counts ---\n")
print(table(df$winners_per_category))

# ---- 5) EDA Tables ----
# Table A: Proportion shared by category
tab_category <- df %>%
  group_by(category) %>%
  summarise(
    N = n(),
    Shared = sum(is_shared),
    Prop_Shared = mean(is_shared),
    .groups = "drop"
  ) %>%
  mutate(Prop_Shared = round(Prop_Shared, 4)) %>%
  arrange(desc(Prop_Shared))

cat("\n--- Table: Proportion shared by category ---\n")
print(tab_category)

# Table B: Proportion shared by sex
tab_sex <- df %>%
  group_by(sex) %>%
  summarise(
    N = n(),
    Shared = sum(is_shared),
    Prop_Shared = mean(is_shared),
    .groups = "drop"
  ) %>%
  mutate(Prop_Shared = round(Prop_Shared, 4))

cat("\n--- Table: Proportion shared by sex ---\n")
print(tab_sex)

# Table C: Proportion shared by winners_per_category
tab_winners <- df %>%
  group_by(winners_per_category) %>%
  summarise(
    N = n(),
    Shared = sum(is_shared),
    Prop_Shared = mean(is_shared),
    .groups = "drop"
  ) %>%
  mutate(Prop_Shared = round(Prop_Shared, 4)) %>%
  arrange(winners_per_category)

cat("\n--- Table: Proportion shared by winners_per_category ---\n")
print(tab_winners)

# ---- 6) Cross-tabulations ----
cat("\n--- xtabs: is_shared by category ---\n")
print(xtabs(~ is_shared + category, data = df))

cat("\n--- xtabs: is_shared by sex ---\n")
print(xtabs(~ is_shared + sex, data = df))

cat("\n--- xtabs: is_shared by winners_per_category ---\n")
print(xtabs(~ is_shared + winners_per_category, data = df))

# ---- 7) Plots ----
dir.create("week5_figures", showWarnings = FALSE)
dir.create("week5_tables", showWarnings = FALSE)

# Plot 1: Proportion shared by category
p1 <- df %>%
  group_by(category) %>%
  summarise(Prop_Shared = mean(is_shared), .groups = "drop") %>%
  ggplot(aes(x = category, y = Prop_Shared)) +
  geom_col(fill = "#2C7FB8") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Prize Category", y = "Proportion Shared") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

print(p1)
ggsave("week5_figures/plot1_prop_shared_by_category.png", p1, width = 7, height = 4.5, dpi = 300)

# Plot 2: Proportion shared by sex
p2 <- df %>%
  group_by(sex) %>%
  summarise(Prop_Shared = mean(is_shared), .groups = "drop") %>%
  ggplot(aes(x = sex, y = Prop_Shared)) +
  geom_col(fill = "#41B6C4") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Sex", y = "Proportion Shared") +
  theme_minimal(base_size = 12)

print(p2)
ggsave("week5_figures/plot2_prop_shared_by_sex.png", p2, width = 6, height = 4.5, dpi = 300)

# Plot 3: Proportion shared by winners_per_category
p3 <- df %>%
  group_by(winners_per_category) %>%
  summarise(Prop_Shared = mean(is_shared), N = n(), .groups = "drop") %>%
  ggplot(aes(x = factor(winners_per_category), y = Prop_Shared)) +
  geom_col(fill = "#7FCDBB") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Winners per Category", y = "Proportion Shared") +
  theme_minimal(base_size = 12)

print(p3)
ggsave("week5_figures/plot3_prop_shared_by_winners_per_category.png", p3, width = 7, height = 4.5, dpi = 300)

# Plot 4: Annual trend (annual proportion shared)
p4 <- df %>%
  group_by(award_year) %>%
  summarise(Prop_Shared = mean(is_shared), N = n(), .groups = "drop") %>%
  ggplot(aes(x = award_year, y = Prop_Shared)) +
  geom_line(color = "#1D91C0", linewidth = 0.7) +
  suppressMessages(geom_smooth(method = "loess", se = TRUE, color = "#08306B")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Award Year", y = "Annual Proportion Shared") +
  theme_minimal(base_size = 12)

print(p4)
ggsave("week5_figures/plot4_annual_prop_shared.png", p4, width = 7, height = 4.5, dpi = 300)

# Plot 5 (optional): Age at award by shared status
p5 <- df %>%
  filter(!is.na(age_at_award), age_at_award >= 0, age_at_award <= 120) %>%
  mutate(Shared_Status = if_else(is_shared == 1, "Shared", "Not Shared")) %>%
  ggplot(aes(x = Shared_Status, y = age_at_award, fill = Shared_Status)) +
  geom_boxplot(alpha = 0.75) +
  labs(x = "Shared Status", y = "Age at Award (Years)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

print(p5)
ggsave("week5_figures/plot5_age_by_shared_status.png", p5, width = 6.5, height = 4.5, dpi = 300)

# ---- 8) Save Tables ----
readr::write_csv(tab_category, "week5_tables/table_prop_shared_by_category.csv")
readr::write_csv(tab_sex, "week5_tables/table_prop_shared_by_sex.csv")
readr::write_csv(tab_winners, "week5_tables/table_prop_shared_by_winners_per_category.csv")

cat("\nWeek 5 EDA complete.\nFigures saved in: week5_figures/\nTables saved in: week5_tables/\n")
