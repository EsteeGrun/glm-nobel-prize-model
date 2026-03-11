# ============================================================
# MA5771 Week 6 - Complete Analysis + Section 3 Outputs
# Project: Modeling the Probability That a Nobel Prize Is Shared
# Author: Katia Schrodinger
# Dataset: nobel_prizes_1901-2025_cleaned.csv
#
# Response: is_shared (0/1)
# Predictors used for Week 6 modeling:
#   - award_year (scaled by decades, centered)
#   - category (categorical)
#   - sex (categorical)
#   - winners_per_year (numeric)
#
# Note: winners_per_category is NOT used as a predictor here because in this dataset
# it is not independent of category in the filtered sample. [1](https://o365coloradoedu-my.sharepoint.com/personal/liri6518_colorado_edu/Documents/Microsoft%20Copilot%20Chat%20Files/ma5771-final-project-instruction.pdf)
#
# Outputs (saved to folders):
#   week6_figures/ : Figures 1–7 (EDA + required diagnostics)
#   week6_tables/  : Tables 1–4 (EDA + deviance tests + coefficients)
# ============================================================

# -------------------------------
# 0) Packages
# -------------------------------
pkgs <- c("tidyverse", "janitor", "scales", "broom", "statmod", "car")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install)
invisible(lapply(pkgs, library, character.only = TRUE))

alpha <- 0.05

# -------------------------------
# 1) Import data
# -------------------------------
# Update this if needed:
data_path <- "/Users/Katia/Desktop/nobel_prizes_1901-2025_cleaned.csv"

raw <- readr::read_csv(data_path, show_col_types = FALSE) %>%
  janitor::clean_names()

# -------------------------------
# 2) Select variables + clean
# -------------------------------
df <- raw %>%
  select(is_shared, award_year, category, sex, winners_per_year, winners_per_category, birth_date) %>%
  mutate(
    is_shared = as.integer(is_shared),
    award_year = as.integer(award_year),
    winners_per_year = as.integer(winners_per_year),
    category = factor(category),
    sex = factor(sex)
  ) %>%
  filter(
    !is.na(is_shared),
    !is.na(award_year),
    !is.na(category),
    sex %in% c("male", "female"),
    !is.na(winners_per_year)
  ) %>%
  mutate(
    # Center year at mean and scale by decades
    year_c_decade = (award_year - mean(award_year)) / 10
  ) %>%
  mutate(
    # Optional age variable (EDA only)
    birth_year = suppressWarnings(as.integer(stringr::str_extract(birth_date, "\\d{4}"))),
    age_at_award = if_else(!is.na(birth_year), award_year - birth_year, NA_integer_)
  ) %>%
  mutate(
    # Reference levels for interpretation
    category = relevel(category, ref = "Physics"),
    sex = relevel(sex, ref = "male")
  )

cat("\nFiltered N =", nrow(df), "\n")
cat("Overall proportion shared =", mean(df$is_shared), "\n")

# -------------------------------
# 3) Create output folders
# -------------------------------
dir.create("week6_figures", showWarnings = FALSE)
dir.create("week6_tables", showWarnings = FALSE)

# -------------------------------
# 4) Section 3 EDA tables
# -------------------------------
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

tab_sex <- df %>%
  group_by(sex) %>%
  summarise(
    N = n(),
    Shared = sum(is_shared),
    Prop_Shared = mean(is_shared),
    .groups = "drop"
  ) %>%
  mutate(Prop_Shared = round(Prop_Shared, 4))

tab_year <- df %>%
  group_by(award_year) %>%
  summarise(
    N = n(),
    Prop_Shared = mean(is_shared),
    .groups = "drop"
  )

write_csv(tab_category, "week6_tables/Table1_EDA_ByCategory.csv")
write_csv(tab_sex, "week6_tables/Table2_EDA_BySex.csv")

# -------------------------------
# 5) Section 3 EDA figures
# -------------------------------

# Figure 1: Proportion shared by category
fig1 <- tab_category %>%
  ggplot(aes(x = category, y = Prop_Shared)) +
  geom_col(fill = "#2C7FB8") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Prize Category", y = "Proportion Shared") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

ggsave("week6_figures/Figure1_PropShared_ByCategory.png", fig1, width = 7, height = 4.5, dpi = 300)

# Figure 2: Proportion shared by sex
fig2 <- tab_sex %>%
  ggplot(aes(x = sex, y = Prop_Shared)) +
  geom_col(fill = "#41B6C4") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Sex", y = "Proportion Shared") +
  theme_minimal(base_size = 12)

ggsave("week6_figures/Figure2_PropShared_BySex.png", fig2, width = 6, height = 4.5, dpi = 300)

# Figure 3: Annual proportion shared over time
fig3 <- tab_year %>%
  ggplot(aes(x = award_year, y = Prop_Shared)) +
  geom_line(color = "#1D91C0", linewidth = 0.7) +
  geom_smooth(method = "loess", se = TRUE, color = "#08306B") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Award Year", y = "Annual Proportion Shared") +
  theme_minimal(base_size = 12)

ggsave("week6_figures/Figure3_AnnualPropShared_Time.png", fig3, width = 7, height = 4.5, dpi = 300)

# -------------------------------
# 6) GLM fitting + model selection
# -------------------------------
# Main effects model
m_main <- glm(
  is_shared ~ year_c_decade + category + sex + winners_per_year,
  family = binomial(link = "logit"),
  data = df
)

# Interaction model: category-specific time trends
m_int <- glm(
  is_shared ~ year_c_decade * category + sex + winners_per_year,
  family = binomial(link = "logit"),
  data = df
)

# Compare models
aic_comp <- AIC(m_main, m_int)
print(aic_comp)

lrt_comp <- anova(m_main, m_int, test = "Chisq")
print(lrt_comp)

# Choose final model (use interaction if it improves fit)
m_final <- m_int

# -------------------------------
# 7) REQUIRED inference tables (Section 3)
# -------------------------------

# Table 3: Analysis of deviance (Type II LR tests)
anova_type2 <- car::Anova(m_final, type = 2, test.statistic = "LR")
anova_df <- as.data.frame(anova_type2)

# Check actual column names (helpful if anything differs)
print(names(anova_df))

# Coerce to correct types safely
anova_df$Df <- as.integer(anova_df$Df)

# The LR statistic column may be named "LR Chisq" (with a space)
anova_df$Chisq <- signif(as.numeric(as.character(anova_df$`LR Chisq`)), 4)

# p-values column is usually "Pr(>Chisq)"
anova_df$p_value <- round(as.numeric(as.character(anova_df$`Pr(>Chisq)`)), 4)

# Final table to save
anova_out <- data.frame(
  Term = rownames(anova_df),
  Df = anova_df$Df,
  Chisq = anova_df$Chisq,
  p_value = anova_df$p_value,
  row.names = NULL
)

write_csv(anova_out, "week6_tables/Table3_AnalysisOfDeviance_TypeII.csv")

# Table 4: Final model coefficients + odds ratios + 95% CI
coef_tab <- broom::tidy(m_final) %>%
  mutate(
    OR = exp(estimate),
    OR_low = exp(estimate - 1.96 * std.error),
    OR_high = exp(estimate + 1.96 * std.error),
    estimate = round(estimate, 4),
    std.error = signif(std.error, 4),
    statistic = signif(statistic, 4),
    p.value = round(p.value, 4),
    OR = round(OR, 4),
    OR_low = round(OR_low, 4),
    OR_high = round(OR_high, 4)
  )

write_csv(coef_tab, "week6_tables/Table4_FinalModel_Coefficients_OR.csv")

# -------------------------------
# 8) REQUIRED diagnostic plots (Section 3) 
# -------------------------------

# Figure 4: Standardized deviance residuals vs fitted values (constant-information scale)
mu_hat <- fitted(m_final)
cis <- asin(sqrt(mu_hat))                        # constant-information scale for binomial
r_dev_std <- rstandard(m_final, type = "deviance")

fig4 <- ggplot(data.frame(cis = cis, r = r_dev_std), aes(x = cis, y = r)) +
  geom_point(alpha = 0.55) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted Values (Constant-Information Scale)", y = "Standardized Deviance Residuals") +
  theme_minimal(base_size = 12)

ggsave("week6_figures/Figure4_ResVsCIS.png", fig4, width = 7, height = 4.5, dpi = 300)

# Figure 5: Working responses vs linear predictors
eta_hat <- predict(m_final, type = "link")
z_work <- eta_hat + residuals(m_final, type = "working")

fig5 <- ggplot(data.frame(eta = eta_hat, z = z_work), aes(x = eta, y = z)) +
  geom_point(alpha = 0.55) +
  geom_smooth(se = FALSE, color = "#08306B") +
  labs(x = "Linear Predictors (η̂)", y = "Working Responses (z)") +
  theme_minimal(base_size = 12)

ggsave("week6_figures/Figure5_WorkingVsEta.png", fig5, width = 7, height = 4.5, dpi = 300)

# Figure 6: Q–Q plot of quantile residuals
qres <- statmod::qresiduals(m_final)

png("week6_figures/Figure6_QQ_QuantileResiduals.png", width = 2100, height = 1500, res = 300)
qqnorm(qres, main = "")
qqline(qres, col = "red", lwd = 2)
dev.off()

# Figure 7: Cook’s distance
cd <- cooks.distance(m_final)

fig7 <- ggplot(data.frame(i = seq_along(cd), cd = cd), aes(x = i, y = cd)) +
  geom_point(alpha = 0.6) +
  labs(x = "Observation Index", y = "Cook's Distance") +
  theme_minimal(base_size = 12)

ggsave("week6_figures/Figure7_CooksDistance.png", fig7, width = 7, height = 4.5, dpi = 300)

# -------------------------------
# 9) (Optional) Prediction plot by category over time (nice for conclusions)
# -------------------------------
newdat <- expand_grid(
  year_c_decade = seq(min(df$year_c_decade), max(df$year_c_decade), length.out = 80),
  category = levels(df$category),
  sex = levels(df$sex)[1],                     # male reference
  winners_per_year = median(df$winners_per_year)
)

newdat$pred <- predict(m_final, newdata = newdat, type = "response")
year_mean <- mean(df$award_year)
newdat$award_year <- year_mean + 10 * newdat$year_c_decade

fig8 <- ggplot(newdat, aes(x = award_year, y = pred, color = category)) +
  geom_line(linewidth = 0.8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Award Year", y = "Predicted Probability of Sharing", color = "Category") +
  theme_minimal(base_size = 12)

ggsave("week6_figures/Figure8_PredictedProb_ByCategoryOverTime.png", fig8, width = 7, height = 4.5, dpi = 300)

cat("\n✅ Week 6 complete.\nFigures saved in: week6_figures/\nTables saved in: week6_tables/\n")

# ============================================================
# Table 5: Interaction summary via predicted probabilities
# Uses the final model (m_final) with year × category interaction
# Saves: week6_tables/Table5_PredictedProb_InteractionSummary.csv
# ============================================================

# Pick comparison years (you can change these)
early_year <- 1910
late_year  <- 2010

# If you prefer "first year vs last year in your dataset", use these instead:
# early_year <- min(df$award_year)
# late_year  <- max(df$award_year)

# Fix other predictors at representative values
sex_ref <- levels(df$sex)[1]                          # should be "male" if you set it as reference
wpy_ref <- median(df$winners_per_year, na.rm = TRUE)  # winners_per_year fixed at median

# Build prediction data
pred_dat <- expand.grid(
  award_year = c(early_year, late_year),
  category = levels(df$category),
  sex = sex_ref,
  winners_per_year = wpy_ref
)

# Compute the model's centered decade scale (must match the model)
pred_dat$year_c_decade <- (pred_dat$award_year - mean(df$award_year)) / 10

# Predict probabilities
pred_dat$pred_prob <- predict(m_final, newdata = pred_dat, type = "response")

# Reshape into a compact Table 5
table5 <- pred_dat %>%
  select(category, award_year, pred_prob) %>%
  tidyr::pivot_wider(
    names_from = award_year,
    values_from = pred_prob,
    names_prefix = "PredProb_"
  ) %>%
  mutate(
    Change = .data[[paste0("PredProb_", late_year)]] - .data[[paste0("PredProb_", early_year)]]
  ) %>%
  mutate(across(starts_with("PredProb_"), ~ round(.x, 4))) %>%
  mutate(Change = round(Change, 4)) %>%
  arrange(desc(.data[[paste0("PredProb_", late_year)]]))

# Add clear column names for Word
table5_pretty <- table5 %>%
  rename(
    `Prize Category` = category,
    !!paste0("Predicted P(shared) in ", early_year) := paste0("PredProb_", early_year),
    !!paste0("Predicted P(shared) in ", late_year)  := paste0("PredProb_", late_year),
    `Change (Late - Early)` = Change
  )

print(table5_pretty)

# Save it
dir.create("week6_tables", showWarnings = FALSE)
readr::write_csv(table5_pretty, "week6_tables/Table5_PredictedProb_InteractionSummary.csv")
