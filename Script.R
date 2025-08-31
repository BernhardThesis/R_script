#BACHELOR-THESIS

# Install packages if not yet installed (commented out)
# install.packages("summarytools")
# install.packages("rstudioapi")
# install.packages("tidyr")
# install.packages("estimatr")
# install.packages("rlang")
# install.packages("gtsummary")
# install.packages("knitr")
# install.packages("purrr")
# install.packages("gt")
# install.packages("kableExtra")

# Load required libraries
library(gtsummary)     # descriptive tables
library(knitr)         # LaTeX/Markdown table rendering
library(rlang)         # programming with R expressions
library(rstudioapi)    # RStudio functions (e.g., viewing data)
library(ggplot2)       # data visualization
library(stargazer)     # regression tables
library(modelsummary)  # regression tables (flexible output)
library(dplyr)         # data manipulation
library(summarytools)  # descriptive stats
library(estimatr)      # robust regressions
library(purrr)         # functional programming (map, etc.)
library(gt)            # table rendering
library(kableExtra)    # advanced table styling

# Show dataset in console and open data viewer
data_main_final
View(data_main_final)

# Quick scatter plot of two variables
ggplot(data_main_final, aes(Income, Political_Preference)) +
  geom_point()

######################
# MAIN ANALYSES
# (OLS with HC3 robust standard errors)
######################

# Recode variables: Choice as numeric, Treatment & Urbanisation as factors
data_main_final$Choice <- as.numeric(as.character(data_main_final$Choice))
data_main_final$Treatment <- as.factor(data_main_final$Treatment)
data_main_final$Urbanisation <- as.factor(data_main_final$Urbanisation)

# Run OLS with Treatment only
model_treatments <- lm_robust(Choice ~ Treatment, data = data_main_final, se_type = "HC3")

# Create new variables for abstraction and contribution and run regression
data_main_final <- data_main_final %>%
  mutate(
    Abstraction  = ifelse(Treatment %in% c(1, 2), "abstract", "concrete"),
    Contribution = ifelse(Treatment %in% c(1, 3), "yes", "no")
  )

model_main_effects <- lm_robust(
  Choice ~ Abstraction + Contribution,
  data = data_main_final,
  se_type = "HC3"
)

# Manual calculation of conditional effects (mean differences)
((0.024+0.096)/2)-((0.032+0.074)/2)   # Matching effect
((0.024+0.032)/2)-((0.096+0.074)/2)   # Abstractness effect

# Add controls step by step
model_treatments_gender <- lm_robust(Choice ~ Treatment + Gender, data = data_main_final, se_type = "HC3")
model_treatments_gender_orderValue <- lm_robust(Choice ~ Treatment + Gender + Order_Value_EUR,
                                                data = data_main_final,
                                                se_type = "HC3")
model_full <- lm_robust(
  Choice ~ Treatment + Gender + Order_Value_EUR + Income_EUR + Urbanisation + Political_Preference,
  data = data_main_final,
  se_type = "HC3"
)

# Show regression summaries
summary(model_treatments)
summary(model_main_effects)
summary(model_treatments_gender)
summary(model_treatments_gender_orderValue)
summary(model_full)

# Create main regression results table
models_main <- list(
  "(1) Treatments"       = model_treatments,
  "(2) Main effects"     = model_main_effects,
  "(3) Gender"           = model_treatments_gender,
  "(4) Order value"      = model_treatments_gender_orderValue,
  "(5) All covariates"   = model_full
)

modelsummary(
  models_main,
  stars = c('*' = .1, '**' = .05, '***' = .01),   # significance stars
  gof_omit = 'RMSE|IC',                           # omit RMSE and IC from GOF
  coef_rename = c(...),                           # rename coefficients
  title = "Main regression results",
  notes = "OLS; heteroskedasticity-robust (HC3) standard errors in parentheses.",
  output = "main_regression_table.tex"
)

# Separate regressions with regional/personal covariates
model_personal_urbanisation <- lm_robust(Choice ~ Treatment + Urbanisation, data = data_main_final, se_type = "HC3")
model_personal_income <- lm_robust(Choice ~ Treatment + Income_EUR, data = data_main_final, se_type = "HC3")
model_personal_politicalpreference <- lm_robust(Choice ~ Treatment + Political_Preference, data = data_main_final, se_type = "HC3")
model_personal_ordervalue <- lm_robust(Choice ~ Treatment + Order_Value_EUR, data = data_main_final, se_type = "HC3")

models_personal <- list(
  "(1) Urbanisation"         = model_personal_urbanisation,
  "(2) Income"               = model_personal_income,
  "(3) Political Preference" = model_personal_politicalpreference,
  "(4) Order Value"          = model_personal_ordervalue
)

modelsummary(models_personal, ...)

# Correlation check for multicollinearity
cor(Urbanisation, Political_Preference)   # (wrong: should use numeric)
cor(data_main_final$Political_Preference, as.numeric(data_main_final$Urbanisation),
    method = "spearman", use = "complete.obs")

# Interpretation: rural areas → more right-leaning

# Main descriptive statistics table with gtsummary
tbl_main <- tbl_summary(
  data = data_clean,   # descriptive stats dataset
  by = Treatment,      # group by treatment
  missing = "no",      # exclude missing
  statistic = list(...),   # define stats for continuous/categorical
  digits = list(...),      # rounding rules
  label = list(...),       # variable labels
  include = everything()
) %>% add_overall(last = TRUE)   # add overall column

# Compute min–max for continuous vars and append to descriptive table
cont_vars <- data_clean %>% select(where(is.numeric)) %>% names()
minmax_fmt <- map_chr(cont_vars, ~ {...}) %>% setNames(cont_vars)

tbl_main2 <- tbl_main %>%
  modify_table_body(~ .x %>% mutate(...)) %>%
  modify_footnote(update = stat_0 ~ "n (%); Mean (SD); [min–max]")

# Render descriptive table in GT (markdown rendering)
gt_tbl <- tbl_main2 %>% as_gt() %>% fmt_markdown(columns = "stat_0")

# Alternative outputs: as LaTeX with kable, with different styles
latex_tbl <- tbl_main2 %>% as_kable(...) %>% kable_styling(...)

# Check distribution of Order Value (SD, variance, min/max)
sd(data_main_final$Order_Value_EUR)
var(data_main_final$Order_Value_EUR)
max(data_main_final$Order_Value_EUR)
min(data_main_final$Order_Value_EUR)

# Independence/randomisation checks:
# chi-square tests for categorical vars, Kruskal–Wallis for metric vars
chi_urb <- table(data_main_final$Treatment, data_main_final$Urbanisation)
chi_gender <- table(data_main_final$Treatment, data_main_final$Gender)
chisq.test(chi_gender)
kruskal.test(Income_EUR ~ Treatment, data = data_main_final)
kruskal.test(Political_Preference ~ Treatment, data = data_main_final)
kruskal.test(Order_Value_EUR ~ Treatment, data = data_main_final)

# Collect test results in table and export as LaTeX
out <- tibble::tribble(...) %>% mutate(...)
out %>% kable(format = "latex", ...)

# Pairwise non-parametric tests for continuous variables
pairwise.wilcox.test(...)

# Pairwise chi-square tests for categorical variables
pairwise_chi <- function(var) {...}
results <- do.call(rbind, lapply(vars, pairwise_chi))

# Distribution checks for Order Value, Political Preference, Income
qqnorm(...); qqline(...)
shapiro.test(...)
hist(...); lines(density(...))

#######################
# ROBUSTNESS CHECKS
#######################

# Logistic regressions (Choice as binary outcome)
model_treatments_log <- glm(Choice ~ Treatment, ..., family = binomial)
model_treatments_gender_log <- glm(Choice ~ Treatment + Gender, ..., family = binomial)
model_treatments_gender_ordervalue_log <- glm(..., family = binomial)
model_treatments_all_log <- glm(..., family = binomial)
model_effects_log <- glm(Choice ~ Abstraction + Contribution, ..., family = binomial)

# Export logistic regressions as OR with CI
modelsummary(list(...), exponentiate = TRUE, ...)

# Repeat logistic regressions with personal/regional variables
models_regional_log <- list(...)
modelsummary(models_regional_log, exponentiate = TRUE, ...)

# Robustness: exclude extreme order values (95th and 99th percentiles)
cutoff_95 <- quantile(df$Order_Value_EUR, 0.95, na.rm = TRUE)
data_filtered_95 <- subset(df, Order_Value_EUR <= cutoff_95)
# Repeat all main regressions on filtered dataset
model_treatments_95 <- lm_robust(...)
...
modelsummary(models_main_95, ...)

cutoff_99 <- quantile(df$Order_Value_EUR, 0.99, na.rm = TRUE)
data_filtered_99 <- subset(df, Order_Value_EUR <= cutoff_99)
# Repeat regressions again
model_treatments_99 <- lm_robust(...)
...
modelsummary(models_main_99, ...)

# Robustness: heteroskedasticity plots
plot(model_ordervalue_95, which = 1)
plot(model_treatments_gender, which = 1)
plot(model_effects, which = 3)

# Robustness: recode Urbanisation as ordinal numeric
data_main_final$Urbanisation <- as.numeric(factor(data_main_final$Urbanisation, ordered = TRUE))
model_personal_urbanisation_ordinal <- lm_robust(...)
modelsummary(model_personal_urbanisation_ordinal, ...)

# Robustness: test if political preference differs in abstract vs. concrete subsamples
sub_abstract <- subset(data_main_final, Treatment %in% c(1, 2))
sub_concrete <- subset(data_main_final, Treatment %in% c(3, 4))
model_personal_political_abstract <- lm_robust(...)
model_personal_political_concrete <- lm_robust(...)
modelsummary(list(...), ...)

# Robustness: exclude problematic observations from T1
data_clean_T1_condition <- data_main_final[!(df$Order_ID %in% c(...)), ]
# Repeat all main regressions on this reduced dataset
model_treatments <- lm_robust(...)
...
modelsummary(models_main, ...)
