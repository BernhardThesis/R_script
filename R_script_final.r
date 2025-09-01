#BACHELOR-THESIS

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
library(gtsummary)
library(knitr)
library(rlang)
library(rstudioapi)
library(ggplot2)
library(stargazer)
library(modelsummary)
library(dplyr)
library(summarytools)
library(estimatr)
library(purrr)
library(gt)
library(kableExtra)

data_main_final
View(data_main_final)

#Scatter plot Test
ggplot(data_main_final, aes(Income, Political_Preference)) +
  geom_point()

######################
# MAIN ANALYSES
# (OLS with HC3 robust standard errors)
######################

# Coding Treatment , Choice and Urbanisation as factors (dummies)
data_main_final$Choice <- as.numeric(as.character(data_main_final$Choice))
data_main_final$Treatment <- as.factor(data_main_final$Treatment)
data_main_final$Urbanisation <- as.factor(data_main_final$Urbanisation)

# (1) Regression Treatment (Abstract and Matching)
model_treatments <- lm_robust(Choice ~ Treatment, data = data_main_final, se_type = "HC3")

# (2) Isolated main effects (Abstract and Matching)
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

#OR MANUALLY WITH MEANS FROM TABLE (same results as above)

#Conditional matching
((0.024+0.096)/2)-((0.032+0.074)/2)

#Abstractness
((0.024+0.032)/2)-((0.096+0.074)/2)

# (3) Regression Gender
model_treatments_gender <- lm_robust(Choice ~ Treatment + Gender, data = data_main_final, se_type = "HC3")

# (4) Regression OrderValue
model_treatments_gender_orderValue <- lm_robust(Choice ~ Treatment + Gender + Order_Value_EUR,
                                                data = data_main_final,
                                                se_type = "HC3")

# (5) Regression including all covariates
model_full <- lm_robust(
  Choice ~ Treatment + Gender + Order_Value_EUR + Income_EUR + Urbanisation + Political_Preference,
  data = data_main_final,
  se_type = "HC3"
)

# Summary
summary(model_treatments)
summary(model_main_effects)
summary(model_treatments_gender)
summary(model_treatments_gender_orderValue)
summary(model_full)

# Creating main regression table with modelsummary

# Collect models in a list
models_main <- list(
  "(1) Treatments"                           = model_treatments,
  "(2) Main effects"                         = model_main_effects,
  "(3) Gender"                  = model_treatments_gender,
  "(4) Order value"    = model_treatments_gender_orderValue,
  "(5) All covariates"                       = model_full
)

# Create table and rename variables
modelsummary(
  models_main,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  gof_omit = 'RMSE|IC',
  coef_rename = c(
    "(Intercept)" = "Constant",
    "Treatment2" = "T2 vs. T1",
    "Treatment3" = "T3 vs. T1",
    "Treatment4" = "T4 vs. T1",
    "Abstractionconcrete" = "Concreteness",
    "Contributionyes" = "Conditional Contribution",
    "Gender1" = "Female vs. male",
    "Income_EUR" = "Income",
    "Urbanisation2" = "Urbanisation: Medium vs. high",
    "Urbanisation3" = "Urbanisation: Low vs. high",
    "Political_Preference" = "Political index (0-10)",
    "Order_Value_EUR" = "Order Value"
  ),
  title = "Main regression results",
  notes     = "OLS; heteroskedasticity-robust (HC3) standard errors in parentheses.",
  output    = "main_regression_table.tex", # latex for Export, main_regression_table.tex for file
)

# Regressions Regional / Personal Variables separate

model_personal_urbanisation <- lm_robust(Choice ~ Treatment + Urbanisation,
                                         data = data_main_final,
                                         se_type = "HC3")

model_personal_income <- lm_robust(Choice ~ Treatment + Income_EUR, data = data_main_final, se_type = "HC3")
model_personal_politicalpreference <- lm_robust(Choice ~ Treatment + Political_Preference,
                                                data = data_main_final,
                                                se_type = "HC3")
model_personal_ordervalue <- lm_robust(Choice ~ Treatment + Order_Value_EUR,
                                       data = data_main_final,
                                       se_type = "HC3")

# Create table for 4 models (regional/personal) with modelsummary

# Collect models in a list
models_personal <- list(
  "(1) Urbanisation" = model_personal_urbanisation,
  "(2) Income" = model_personal_income,
  "(3) Political Preference" = model_personal_politicalpreference,
  "(4) Order Value" = model_personal_ordervalue
)

# Create table and rename variables
modelsummary(
  models_personal,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  gof_omit = 'RMSE|IC',
  coef_rename = c(
    "(Intercept)" = "Constant",
    "Treatment2" = "T2 vs. T1",
    "Treatment3" = "T3 vs. T1",
    "Treatment4" = "T4 vs. T1",
    "Income_EUR" = "Income",
    "Urbanisation2" = "Urbanisation: Medium vs. high",
    "Urbanisation3" = "Urbanisation: Low vs. high",
    "Political_Preference" = "Political index (0-10)",
    "Order_Value_EUR" = "Order Value"
  ),
  title = "Regression results: personal and regional characteristics",
  notes     = "OLS; heteroskedasticity-robust (HC3) standard errors in parentheses.",
  output    = "default", # latex for Export, personal_regressions_table.tex for file
)

summary(model_personal_urbanisation)
summary(model_personal_income)
summary(model_personal_politicalpreference)
summary(model_personal_ordervalue)


#Correlation Urbanisation - Political_Preference (multicollinearity check)
cor(Urbanisation, Political_Preference)
cor(data_main_final$Political_Preference, as.numeric(data_main_final$Urbanisation), method = "spearman", use = "complete.obs")
# Interpretation: The more rural the region (higher value of Urbanisation), the more right-leaning the political preference (higher value of Political_Preference).


# Main descriptive table, create with GTSUMMARY

# 1) Main table
tbl_main <- tbl_summary(
  data = data_clean,
  by = Treatment,
  missing = "no",
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  digits = list(
    all_continuous() ~ 2,
    all_categorical() ~ c(0, 1)
  ),
  label = list(
    Order_Value_EUR ~ "Order Value (€)",
    Income_EUR ~ "Annual income of households (€)",
    Political_Preference ~ "Political left-right-index (0-10)",
    Gender ~ "Gender",
    Choice ~ "Eco-option chosen",
    Urbanisation ~ "Degree of Urbanisation"
  ),
  include = everything()
) %>%
  add_overall(last = TRUE)

# 2) Calculate Min–Max (Overall)
cont_vars <- data_clean %>% select(where(is.numeric)) %>% names()
minmax_fmt <- map_chr(cont_vars, ~ {
  x <- data_clean[[.x]]
  x <- x[!is.na(x)]
  paste0(
    formatC(min(x), format = "f", digits = 1),
    "–",
    formatC(max(x), format = "f", digits = 1)
  )
}) %>% setNames(cont_vars)

# 3) Overall-Spalte mit Markdown-Zeilenumbruch + [min–max]
tbl_main2 <- tbl_main %>%
  modify_table_body(~ .x %>%
                      mutate(
                        stat_0 = ifelse(
                          variable %in% names(minmax_fmt),
                          # zwei Leerzeichen + \n erzwingen einen Markdown-Umbruch
                          paste0(stat_0, "  \n[", minmax_fmt[variable], "]"),
                          stat_0
                        )
                      )
  ) %>%
  # Einzeilige Fußnote (ohne Umbruch)
  modify_footnote(
    update = stat_0 ~ "n (%); Mean (SD); [min–max]"
  )

# 4) In gt umwandeln und die Overall-Spalte als Markdown rendern
gt_tbl <- tbl_main2 %>%
  as_gt() %>%
  fmt_markdown(columns = "stat_0")   # interpretiert "  \n" als Zeilenumbruch

gt_tbl

# 1) Main table with Overall
tbl_main <- tbl_summary(
  data = data_clean,
  by = Treatment,
  missing = "no",
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  digits = list(
    all_continuous() ~ 2,
    all_categorical() ~ c(0, 1)
  ),
  label = list(
    Order_Value_EUR ~ "Order Value (€)",
    Income_EUR ~ "Annual income of households (€)",
    Political_Preference ~ "Political left-right-index (0-10)",
    Gender ~ "Gender",
    Choice ~ "Eco-option chosen",
    Urbanisation ~ "Degree of Urbanisation"
  ),
  include = everything()
) %>%
  add_overall(last = TRUE)

# 2) Min–Max for continuous variables (from full sample)
cont_vars <- data_clean %>% select(where(is.numeric)) %>% names()
minmax_fmt <- map_chr(cont_vars, ~ {
  x <- data_clean[[.x]]
  x <- x[!is.na(x)]
  paste0(
    formatC(min(x), format = "f", digits = 1),
    "–",
    formatC(max(x), format = "f", digits = 1)
  )
}) %>% setNames(cont_vars)

# 3) Add "[min–max]" on a NEW LINE in the Overall column (stat_0)
tbl_main2 <- tbl_main %>%
  modify_table_body(~ .x %>%
                      mutate(
                        stat_0 = ifelse(
                          variable %in% names(minmax_fmt),
                          paste0(stat_0, " \\\\ [", minmax_fmt[variable], "]"),  # LaTeX line break
                          stat_0
                        )
                      )
  ) %>%
  modify_footnote(
    update = stat_0 ~ "n (%); Mean (SD); [min–max]"
  )

# 4) Render to LaTeX (kable) — allow line breaks inside cells
latex_tbl <- tbl_main2 %>%
  as_kable(format = "latex", booktabs = TRUE, escape = FALSE) %>%
  kable_styling(latex_options = c("hold_position"))  # optional styling

latex_tbl


# 1) Haupttabelle mit Overall
tbl_main <- tbl_summary(
  data = data_clean,
  by = Treatment,
  missing = "no",
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  digits = list(
    all_continuous() ~ 2,
    all_categorical() ~ c(0, 1)
  ),
  label = list(
    Order_Value_EUR ~ "Order Value (€)",
    Income_EUR ~ "Annual income of households (€)",
    Political_Preference ~ "Political left-right-index (0-10)",
    Gender ~ "Gender",
    Choice ~ "Eco-option chosen",
    Urbanisation ~ "Degree of Urbanisation"
  ),
  include = everything()
) %>%
  add_overall(last = TRUE)

# 2) Min–Max für kontinuierliche Variablen
cont_vars <- data_clean %>% select(where(is.numeric)) %>% names()
minmax_fmt <- map_chr(cont_vars, ~ {
  x <- data_clean[[.x]]
  x <- x[!is.na(x)]
  paste0("[", formatC(min(x), format = "f", digits = 1),
         "–", formatC(max(x), format = "f", digits = 1), "]")
}) %>% setNames(cont_vars)

# 3) Min–Max am Ende der Overall-Zelle anhängen (gleiche Zeile)
tbl_main2 <- tbl_main %>%
  modify_table_body(~ .x %>%
                      mutate(
                        stat_0 = ifelse(
                          variable %in% names(minmax_fmt),
                          paste0(as.character(stat_0), " ", minmax_fmt[variable]),
                          as.character(stat_0)
                        )
                      )
  ) %>%
  modify_footnote(update = stat_0 ~ "n (%); Mean (SD); [min–max]")

# 4) LaTeX ausgeben
latex_tbl <- tbl_main2 %>%
  as_kable(
    format = "latex",
    booktabs = TRUE,
    escape = TRUE,
    longtable = TRUE        # <- wichtig für threeparttable + Fußnoten
  ) %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down"),
    full_width = TRUE
  ) %>%
  footnote(
    general = c("^1 n (\\%); Mean (SD).",
                "^2 n (\\%); Mean (SD); [min--max]."),
    threeparttable = TRUE,
    escape = FALSE
  )

latex_tbl

#CHECKING SD FOR ORDER_VALUE_EUR
sd(data_main_final$Order_Value_EUR)
var(data_main_final$Order_Value_EUR)
max(data_main_final$Order_Value_EUR)
min(data_main_final$Order_Value_EUR)


#Overall Independence of treatment groups (Chi-square and Kruskal-Wallis)

#Urbanisation
chi_urb <- table(data_main_final$Treatment, data_main_final$Urbanisation)
chisq.test(chi_urb) 
#fisher.test(chi_urb, workspace = 2e8)

#Gender
chi_gender <- table(data_main_final$Treatment, data_main_final$Gender)
chisq.test(chi_gender)
# fisher.test(chi_gender, workspace = 2e8)

#Income
kruskal.test(Income_EUR ~ Treatment, data = data_main_final)

#Political Preference
kruskal.test(Political_Preference ~ Treatment, data = data_main_final)
#chi_politic <- table(data_main_final$Treatment, data_main_final$Gender)
#chisq.test(chi_politic)
Order value
# chi_ordervalue <- table(data_main_final$Treatment, data_main_final$Order_Value_EUR)
# chisq.test(chi_ordervalue)

kruskal.test(Order_Value_EUR ~ Treatment, data = data_main_final)

# Ergebnisse einsammeln
chi_urb_res    <- suppressWarnings(chisq.test(chi_urb))
fish_urb_res   <- fisher.test(chi_urb, workspace = 2e8) #not needed

chi_gender_res <- suppressWarnings(chisq.test(chi_gender))
fish_gender_res<- fisher.test(chi_gender, workspace = 2e8) #not needed

kw_income  <- kruskal.test(Income_EUR ~ Treatment, data = data_main_final)
kw_pol     <- kruskal.test(Political_Preference ~ Treatment, data = data_main_final)
kw_order   <- kruskal.test(Order_Value_EUR ~ Treatment, data = data_main_final)

# Tabellendaten bauen
out <- tibble::tribble(
  ~Variable,              ~Test,              ~Statistic,                          ~df,                      ~p_value,
  "Urbanisation",         "Chi-squared",      unname(chi_urb_res$statistic),       unname(chi_urb_res$parameter), chi_urb_res$p.value,
  "Urbanisation",         "Fisher's Exact",   NA,                                   NA,                          fish_urb_res$p.value,
  "Gender",               "Chi-squared",      unname(chi_gender_res$statistic),    unname(chi_gender_res$parameter), chi_gender_res$p.value,
  "Gender",               "Fisher's Exact",   NA,                                   NA,                          fish_gender_res$p.value,
  "Income",               "Kruskal–Wallis",   unname(kw_income$statistic),         unname(kw_income$parameter),     kw_income$p.value,
  "Political preference", "Kruskal–Wallis",   unname(kw_pol$statistic),            unname(kw_pol$parameter),        kw_pol$p.value,
  "Order value",          "Kruskal–Wallis",   unname(kw_order$statistic),          unname(kw_order$parameter),      kw_order$p.value
) %>%
  mutate(
    Statistic = round(as.numeric(Statistic), 3),
    df        = ifelse(is.na(df), NA, as.integer(df)),
    p_value   = round(as.numeric(p_value), 4)
  )

# LaTeX-Ausgabe
out %>%
  kable(format = "latex", booktabs = TRUE,
        caption = "Randomisation check: test results",
        col.names = c("Variable","Test","Statistic","df","p-value"),
        align = c("l","l","r","r","r")) %>%
  kable_styling(latex_options = "hold_position")


# Pairwise test independence for metric variables (Income, Order Value, Political Preference)
pairwise.wilcox.test(data_main_final$Income_EUR, data_main_final$Treatment, p.adjust.method = "BH")
pairwise.wilcox.test(data_main_final$Political_Preference, data_main_final$Treatment, p.adjust.method = "BH")
pairwise.wilcox.test(data_main_final$Order_Value_EUR, data_main_final$Treatment, p.adjust.method = "BH")


#Pairwise Chi-square for categorical variables (Gender, Urbanisation)
df <- data_main_final
vars <- c("Gender", "Urbanisation")
treatments <- unique(df$Treatment)

pairwise_chi <- function(var) {
  combs <- combn(treatments, 2, simplify = FALSE)
  res <- lapply(combs, function(pair) {
    sub <- df[df$Treatment %in% pair, ]
    tab <- table(sub$Treatment, sub[[var]])
    ct  <- chisq.test(tab)
    data.frame(group1 = pair[1],
               group2 = pair[2],
               p_val  = ct$p.value)
  })
  out <- do.call(rbind, res)
  out$p_adj_BH <- p.adjust(out$p_val, method = "BH")
  out$variable <- var
  out
}

results <- do.call(rbind, lapply(vars, pairwise_chi))
results

#Distribution Order Value
qqnorm(data_main_final$Order_Value_EUR, main = "Q-Q Plot Order Value")
qqline(data_main_final$Order_Value_EUR, col = "red")

shapiro.test(data_main_final$Order_Value_EUR)

hist(data_main_final$Order_Value_EUR,
     breaks = 30,
     main = "Order Value Distribution",
     xlab = "Order Value (€)",
     col = "lightblue")

# Dichtekurve drüberlegen
lines(density(data_main_final$Order_Value_EUR, na.rm = TRUE), col = "red", lwd = 2)


#Distribution Political Preference
qqnorm(data_main_final$Political_Preference, main = "Q-Q Plot Political Preference")
qqline(data_main_final$Political_Preference, col = "red")

shapiro.test(data_main_final$Political_Preference)

hist(data_main_final$Political_Preference,
     breaks = 30,
     main = "Political Preference Distribution",
     xlab = "Political left-right index (0-10)",
     col = "lightblue")

# Dichtekurve drüberlegen
lines(density(data_main_final$Income_EUR, na.rm = TRUE), col = "red", lwd = 2)

#Distribution Income
qqnorm(data_main_final$Income_EUR, main = "Q-Q Plot Income")
qqline(data_main_final$Income_EUR, col = "red")

shapiro.test(data_main_final$Order_Value_EUR)

hist(data_main_final$Income_EUR,
     breaks = 30,
     main = "Income Distribution",
     xlab = "Annual Household Income (€)",
     col = "lightblue")

# Dichtekurve drüberlegen
lines(density(data_main_final$Income_EUR, na.rm = TRUE), col = "red", lwd = 2)

#######################
# ROBUSTNESS CHECKS
#######################

# Logistic regressions

# Treatment only
model_treatments_log <- glm(Choice ~ Treatment, data = data_main_final, family = binomial)

# Treatment + Gender
model_treatments_gender_log <- glm(Choice ~ Treatment + Gender, data = data_main_final, family = binomial)
summary(model_treatments_gender_log)

# Treatment + Gender + Order Value
model_treatments_gender_ordervalue_log <- glm(Choice ~ Treatment + Gender + Order_Value_EUR,
                                              data = data_main_final,
                                              family = binomial)

# Treatment + All Variables
model_treatments_all_log <- glm(
  Choice ~ Treatment + Gender + Order_Value_EUR + Income_EUR + Urbanisation + Political_Preference,
  data = data_main_final,
  family = binomial
)

# (2) Isolated main effects (Abstract and Matching)
model_effects_log <- glm(Choice ~ Abstraction + Contribution,
                         data = data_main_final,
                         family = binomial)


# Create table with Odd Ratios and CI
modelsummary(
  list(
    "(1) Treatments"        = model_treatments_log,
    "(2) Main Effects"          = model_effects_log,
    "(3) Gender"    = model_treatments_gender_log,
    "(4) Order Value" = model_treatments_gender_ordervalue_log,
    "(5) All covariates" = model_treatments_all_log
  ),
  exponentiate = TRUE, 
  statistic    = "({conf.low} - {conf.high})",
  stars = c('*' = .1, '**' = .05, '***' = .01),
  gof_omit = 'RMSE|IC',
  coef_rename = c(
    "(Intercept)" = "Constant",
    "Treatment2" = "T2 vs. T1",
    "Treatment3" = "T3 vs. T1",
    "Treatment4" = "T4 vs. T1",
    "Abstractionconcrete" = "Concreteness",
    "Contributionyes" = "Conditional Contribution",
    "Gender1" = "Female vs. male",
    "Income_EUR" = "Income",
    "Urbanisation2" = "Urbanisation: Medium vs. high",
    "Urbanisation3" = "Urbanisation: Low vs. high",
    "Political_Preference" = "Political index (0-10)",
    "Order_Value_EUR" = "Order Value"
  ),
  output       = "logistic_main.tex", # logistic_main.tex for file
  title        = "Logistic regression robustness checks main models (Odds Ratios with 95% CI)"
)

# Regional and Personal Variables
model_personal_gender_log <- glm(Choice ~ Treatment + Gender, data = data_main_final, family = binomial)
model_personal_urbanisation_log <- glm(Choice ~ Treatment + Urbanisation,
                                       data = data_main_final,
                                       family = binomial)
model_personal_income_log <- glm(Choice ~ Treatment + Income_EUR,
                                 data = data_main_final,
                                 family = binomial)
model_personal_politicalpreference_log <- glm(Choice ~ Treatment + Political_Preference,
                                              data = data_main_final,
                                              family = binomial)
model_personal_ordervalue_log <- glm(Choice ~ Treatment + Order_Value_EUR,
                                     data = data_main_final,
                                     family = binomial)

summary(model_personal_gender_log)
summary(model_personal_urbanisation_log)
summary(model_personal_income_log)
summary(model_personal_politicalpreference_log)
summary(model_personal_ordervalue_log)


# Table for all log-models (regional/personal) with modelsummary
models_regional_log <- list(
  "(1) Urbanisation"         = model_personal_urbanisation_log,
  "(2) Income"           = model_personal_income_log,
  "(3) Political Preference" = model_personal_politicalpreference_log,
  "(4) Order Value"      = model_personal_ordervalue_log
)

# Create table with Odd Ratios and CI
modelsummary(
  models_regional_log,
  exponentiate = TRUE,                             
  statistic    = "({conf.low} - {conf.high})",
  stars = c('*' = .1, '**' = .05, '***' = .01),
  gof_omit = 'RMSE|IC',
  coef_rename = c(
    "(Intercept)" = "Constant",
    "Treatment2" = "T2 vs. T1",
    "Treatment3" = "T3 vs. T1",
    "Treatment4" = "T4 vs. T1",
    "Income_EUR" = "Income",
    "Urbanisation2" = "Urbanisation: Medium vs. high",
    "Urbanisation3" = "Urbanisation: Low vs. high",
    "Political_Preference" = "Political index (0-10)",
    "Order_Value_EUR" = "Order Value"
  ),
  output       = "log_regional.tex", 
  title        = "Logistic regression robustness checks regional and personal variables (Odds Ratios with 95% CI)"
)



# Excluding top 5% of order value MAIN

# Cutoff
cutoff_95 <- quantile(df$Order_Value_EUR, 0.95, na.rm = TRUE)

# Subset
data_filtered_95 <- subset(df, Order_Value_EUR <= cutoff_95)

# (1) Regression Treatment (Abstract and Cond. Contribution)
model_treatments_95 <- lm_robust(Choice ~ Treatment, data = data_filtered_95, se_type = "HC3")

# (2) Isolated main effects (Abstract and Cond. Contribution)
data_filtered_95 <- data_filtered_95 %>%
  mutate(
    Abstraction  = ifelse(Treatment %in% c(1, 2), "abstract", "concrete"),
    Contribution = ifelse(Treatment %in% c(1, 3), "yes", "no")
  )

model_main_effects_95 <- lm_robust(
  Choice ~ Abstraction + Contribution,
  data = data_filtered_95,
  se_type = "HC3"
)

# (3) Regression Gender
model_treatments_gender_95 <- lm_robust(Choice ~ Treatment + Gender, data = data_filtered_95, se_type = "HC3")

# (4) Regression OrderValue
model_treatments_gender_orderValue_95 <- lm_robust(Choice ~ Treatment + Gender + Order_Value_EUR,
                                                data = data_filtered_95,
                                                se_type = "HC3")

# (5) Regression including all covariates
model_full_95 <- lm_robust(
  Choice ~ Treatment + Gender + Order_Value_EUR + Income_EUR + Urbanisation + Political_Preference,
  data = data_filtered_95,
  se_type = "HC3"
)

# Creating table excluding Order Values above 95th percentile

# Collect models in a list
models_main_95 <- list(
  "(1) Treatments"                           = model_treatments_95,
  "(2) Main effects"                         = model_main_effects_95,
  "(3) Gender"                  = model_treatments_gender_95,
  "(4) Order value"    = model_treatments_gender_orderValue_95,
  "(5) All covariates"                       = model_full_95
)

# Create table and rename variables
modelsummary(
  models_main_95,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  gof_omit = 'RMSE|IC',
  coef_rename = c(
    "(Intercept)" = "Constant",
    "Treatment2" = "T2 vs. T1",
    "Treatment3" = "T3 vs. T1",
    "Treatment4" = "T4 vs. T1",
    "Abstractionconcrete" = "Concreteness",
    "Contributionyes" = "Conditional Contribution",
    "Gender1" = "Female vs. male",
    "Income_EUR" = "Income",
    "Urbanisation02" = "Urbanisation: Medium vs. high",
    "Urbanisation03" = "Urbanisation: Low vs. high",
    "Political_Preference" = "Political index (0-10)",
    "Order_Value_EUR" = "Order Value"
  ),
  title = "Exclusion of order values above the 95th percentile: Main regression results",
  notes     = "OLS; heteroskedasticity-robust (HC3) standard errors in parentheses.",
  output    = "95_main_regression.tex", # latex for Export, 95_main_regression.tex for file
)

# Excluding top 1% of order value MAIN

# Cutoff
cutoff_99 <- quantile(df$Order_Value_EUR, 0.99, na.rm = TRUE)

# Subset
data_filtered_99 <- subset(df, Order_Value_EUR <= cutoff_99)


# (1) Regression Treatment (Abstract and Cond. Contribution)
model_treatments_99 <- lm_robust(Choice ~ Treatment, data = data_filtered_99, se_type = "HC3")

# (2) Isolated main effects (Abstract and Cond. Contribution)
data_filtered_99 <- data_filtered_99 %>%
  mutate(
    Abstraction  = ifelse(Treatment %in% c(1, 2), "abstract", "concrete"),
    Contribution = ifelse(Treatment %in% c(1, 3), "yes", "no")
  )

model_main_effects_99 <- lm_robust(
  Choice ~ Abstraction + Contribution,
  data = data_filtered_99,
  se_type = "HC3"
)

# (3) Regression Gender
model_treatments_gender_99 <- lm_robust(Choice ~ Treatment + Gender, data = data_filtered_99, se_type = "HC3")

# (4) Regression OrderValue
model_treatments_gender_orderValue_99 <- lm_robust(Choice ~ Treatment + Gender + Order_Value_EUR,
                                                   data = data_filtered_99,
                                                   se_type = "HC3")

# (5) Regression including all covariates
model_full_99 <- lm_robust(
  Choice ~ Treatment + Gender + Order_Value_EUR + Income_EUR + Urbanisation + Political_Preference,
  data = data_filtered_99,
  se_type = "HC3"
)

# Creating table excluding Order Values above 95th percentile

# Collect models in a list
models_main_99 <- list(
  "(1) Treatments"                           = model_treatments_99,
  "(2) Main effects"                         = model_main_effects_99,
  "(3) Gender"                  = model_treatments_gender_99,
  "(4) Order value"    = model_treatments_gender_orderValue_99,
  "(5) All covariates"                       = model_full_99
)

# Create table and rename variables
modelsummary(
  models_main_99,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  gof_omit = 'RMSE|IC',
  coef_rename = c(
    "(Intercept)" = "Constant",
    "Treatment2" = "T2 vs. T1",
    "Treatment3" = "T3 vs. T1",
    "Treatment4" = "T4 vs. T1",
    "Abstractionconcrete" = "Concreteness",
    "Contributionyes" = "Conditional Contribution",
    "Gender1" = "Female vs. male",
    "Income_EUR" = "Income",
    "Urbanisation02" = "Urbanisation: Medium vs. high",
    "Urbanisation03" = "Urbanisation: Low vs. high",
    "Political_Preference" = "Political index (0-10)",
    "Order_Value_EUR" = "Order Value"
  ),
  title = "Exclusion of order values above the 99th percentile: Main regression results",
  notes     = "OLS; heteroskedasticity-robust (HC3) standard errors in parentheses.",
  output    = "99_main_regression.tex", # latex for Export, 99_main_regression.tex for file
)


# Excluding top 5% of order value REGIONAL
model_personal_urbanisation_95 <- lm_robust(Choice ~ Treatment + Urbanisation,
                                         data = data_filtered_95,
                                         se_type = "HC3")

model_personal_income_95 <- lm_robust(Choice ~ Treatment + Income_EUR, data = data_filtered_95, se_type = "HC3")
model_personal_politicalpreference_95 <- lm_robust(Choice ~ Treatment + Political_Preference,
                                                data = data_filtered_95,
                                                se_type = "HC3")
model_personal_ordervalue_95 <- lm_robust(Choice ~ Treatment + Order_Value_EUR,
                                       data = data_filtered_95,
                                       se_type = "HC3")

# Create table for 4 models (regional/personal) with modelsummary

# Collect models in a list
models_personal_95 <- list(
  "(1) Urbanisation" = model_personal_urbanisation_95,
  "(2) Income" = model_personal_income_95,
  "(3) Political Preference" = model_personal_politicalpreference_95,
  "(4) Order Value" = model_personal_ordervalue_95
)

# Create table and rename variables
modelsummary(
  models_personal_95,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  gof_omit = 'RMSE|IC',
  coef_rename = c(
    "(Intercept)" = "Constant",
    "Treatment2" = "T2 vs. T1",
    "Treatment3" = "T3 vs. T1",
    "Treatment4" = "T4 vs. T1",
    "Abstractionconcrete" = "Concreteness",
    "Contributionyes" = "Conditional Contribution",
    "Gender1" = "Female vs. male",
    "Income_EUR" = "Income",
    "Urbanisation02" = "Urbanisation: Medium vs. high",
    "Urbanisation03" = "Urbanisation: Low vs. high",
    "Political_Preference" = "Political index (0-10)",
    "Order_Value_EUR" = "Order Value"
  ),
  title = "Exclusion of order values above the 95th percentile: Regression results with personal and regional characteristics",
  notes     = "OLS; heteroskedasticity-robust (HC3) standard errors in parentheses.",
  output    = "95_personal_regressions.tex", # latex for Export, 95_personal_regressions.tex for file
)


# Excluding top 1% of order value REGIONAL
model_personal_urbanisation_99 <- lm_robust(Choice ~ Treatment + Urbanisation,
                                         data = data_filtered_99,
                                         se_type = "HC3")

model_personal_income_99 <- lm_robust(Choice ~ Treatment + Income_EUR, data = data_main_final, se_type = "HC3")
model_personal_politicalpreference_99 <- lm_robust(Choice ~ Treatment + Political_Preference,
                                                data = data_filtered_99,
                                                se_type = "HC3")
model_personal_ordervalue_99 <- lm_robust(Choice ~ Treatment + Order_Value_EUR,
                                       data = data_filtered_99,
                                       se_type = "HC3")

# Create table for 4 models (regional/personal) with modelsummary

# Collect models in a list
models_personal_99 <- list(
  "(1) Urbanisation" = model_personal_urbanisation_99,
  "(2) Income" = model_personal_income_99,
  "(3) Political Preference" = model_personal_politicalpreference_99,
  "(4) Order Value" = model_personal_ordervalue_99
)

# Create table and rename variables
modelsummary(
  models_personal_99,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  gof_omit = 'RMSE|IC',
  coef_rename = c(
    "(Intercept)" = "Constant",
    "Treatment2" = "T2 vs. T1",
    "Treatment3" = "T3 vs. T1",
    "Treatment4" = "T4 vs. T1",
    "Abstractionconcrete" = "Concreteness",
    "Contributionyes" = "Conditional Contribution",
    "Gender1" = "Female vs. male",
    "Income_EUR" = "Income",
    "Urbanisation02" = "Urbanisation: Medium vs. high",
    "Urbanisation03" = "Urbanisation: Low vs. high",
    "Political_Preference" = "Political index (0-10)",
    "Order_Value_EUR" = "Order Value"
  ),
  title = "Exclusion of order values above the 99th percentile: Regression results with personal and regional characteristics",
  notes     = "OLS; heteroskedasticity-robust (HC3) standard errors in parentheses.",
  output    = "99_personal_regressions.tex", # latex for Export, 99_personal_regressions.tex for file
)






# Plot für Heteroskedastizität-Überprüfung
plot(model_ordervalue_95, which = 1)   # Residuen vs. Fits
plot(model_treatments_gender, which = 1)   # Residuen vs. Fits
plot(model_effects, which = 3)   # Residuen vs. Fits


# Robustness check: Urbanisation coded ordinal -> 1 overall coefficient
data_main_final$Urbanisation <- as.numeric(factor(data_main_final$Urbanisation, ordered = TRUE))
model_personal_urbanisation_ordinal <- lm_robust(Choice ~ Treatment + Urbanisation,
                                         data = data_main_final,
                                         se_type = "HC3")
summary(model_personal_urbanisation_ordinal)

modelsummary(
  model_personal_urbanisation_ordinal,
  output = "urbanisation_ordinal.tex", # "urbanisation_ordinal.tex" for file
  title = "Robustness check: Ordinal coding of Urbanisation",
  stars = c('*' = .1, '**' = .05, '***' = .01),
  gof_omit = 'RMSE|IC',
  coef_rename = c(
    "(Intercept)"        = "Constant",
    "Treatment2"         = "T2 vs. T1",
    "Treatment3"         = "T3 vs. T1",
    "Treatment4"         = "T4 vs. T1",
    "Urbanisation"       = "Degree of urbanisation (lower vs. higher)"
  )
)



#Check if political preference is more predictive for abstract option

data_main_final$Treatment <- relevel(data_main_final$Treatment, ref = "1")


# Subset with T3 and 4
sub_abstract <- subset(data_main_final, Treatment %in% c(1, 2))
sub_concrete <- subset(data_main_final, Treatment %in% c(3, 4))


model_personal_political_abstract <- lm_robust(Choice ~ Treatment + Political_Preference,
                                                   data = sub_abstract,
                                                   se_type = "HC3")

model_personal_political_concrete <- lm_robust(Choice ~ Treatment + Political_Preference,
                                                data = sub_concrete,
                                                se_type = "HC3")

# Collect models in a list
models_regional_concrete_vs_abstract <- list(
  "(1) Abstract" = model_personal_political_abstract,
  "(2) Concrete" = model_personal_political_concrete
  )

modelsummary(
  models_regional_concrete_vs_abstract,
  output = "models_regional_concrete_vs_abstract.tex",
  title = "Comparison of the effect of Political Preference: abstract vs. concrete",
  stars = c('*' = .1, '**' = .05, '***' = .01),
  gof_omit = 'RMSE|IC',
  coef_rename = c(
    "(Intercept)"        = "Constant",
    "Political_Preference" = "Political index (0-10)"
    
  )
  
)


# Exclude obs which were collected for T1 when only T1 was displayed (robustness-check)
data_main_final <- as.data.frame(data_main_final)

data_clean_T1_condition <- data_main_final[!(df$Order_ID %in% c(20002, 20003, 20004, 20005, 
                                    20006, 20007, 20008, 20009,
                                    20010, 20011, 20012, 20013,
                                    20014, 20015, 20016, 20017,
                                    20018)), ]

# Main regressions ans regional-level-characteristics: estimate again

# (1) Regression Treatment (Abstract and Matching)
model_treatments <- lm_robust(Choice ~ Treatment, data = data_clean_T1_condition, se_type = "HC3")

# (2) Isolated main effects (Abstract and Matching)
data_main_final <- data_clean_T1_condition %>%
  mutate(
    Abstraction  = ifelse(Treatment %in% c(1, 2), "abstract", "concrete"),
    Contribution = ifelse(Treatment %in% c(1, 3), "yes", "no")
  )

model_main_effects <- lm_robust(
  Choice ~ Abstraction + Contribution,
  data = data_clean_T1_condition,
  se_type = "HC3"
)


# (3) Regression Gender
model_treatments_gender <- lm_robust(Choice ~ Treatment + Gender, data = data_clean_T1_condition, se_type = "HC3")

# (4) Regression OrderValue
model_treatments_gender_orderValue <- lm_robust(Choice ~ Treatment + Gender + Order_Value_EUR,
                                                data = data_clean_T1_condition,
                                                se_type = "HC3")

# (5) Regression including all covariates
model_full <- lm_robust(
  Choice ~ Treatment + Gender + Order_Value_EUR + Income_EUR + Urbanisation + Political_Preference,
  data = data_clean_T1_condition,
  se_type = "HC3"
)

# Summary
summary(model_treatments)
summary(model_main_effects)
summary(model_treatments_gender)
summary(model_treatments_gender_orderValue)
summary(model_full)

# Creating main regression table with modelsummary

# Collect models in a list
models_main <- list(
  "(1) Treatments"                           = model_treatments,
  "(2) Main effects"                         = model_main_effects,
  "(3) Gender"                  = model_treatments_gender,
  "(4) Order value"    = model_treatments_gender_orderValue,
  "(5) All covariates"                       = model_full
)

# Create table and rename variables
modelsummary(
  models_main,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  gof_omit = 'RMSE|IC',
  coef_rename = c(
    "(Intercept)" = "Constant",
    "Treatment2" = "T2 vs. T1",
    "Treatment3" = "T3 vs. T1",
    "Treatment4" = "T4 vs. T1",
    "Abstractionconcrete" = "Concreteness",
    "Contributionyes" = "Conditional Contribution",
    "Gender1" = "Female vs. male",
    "Income_EUR" = "Income",
    "Urbanisation2" = "Urbanisation: Medium vs. high",
    "Urbanisation3" = "Urbanisation: Low vs. high",
    "Political_Preference" = "Political index (0-10)",
    "Order_Value_EUR" = "Order Value"
  ),
  title = "Excluded obs. (T1 only): Main regression results",
  notes     = "OLS; heteroskedasticity-robust (HC3) standard errors in parentheses.",
  output    = "ex_t1_main_regression_table.tex", # latex for Export, main_regression_table.tex for file
)

# Regressions Regional / Personal Variables separate

model_personal_urbanisation <- lm_robust(Choice ~ Treatment + Urbanisation,
                                         data = data_clean_T1_condition,
                                         se_type = "HC3")

model_personal_income <- lm_robust(Choice ~ Treatment + Income_EUR, data = data_clean_T1_condition, se_type = "HC3")
model_personal_politicalpreference <- lm_robust(Choice ~ Treatment + Political_Preference,
                                                data = data_clean_T1_condition,
                                                se_type = "HC3")
model_personal_ordervalue <- lm_robust(Choice ~ Treatment + Order_Value_EUR,
                                       data = data_clean_T1_condition,
                                       se_type = "HC3")

# Create table for 4 models (regional/personal) with modelsummary

# Collect models in a list
models_personal <- list(
  "(1) Urbanisation" = model_personal_urbanisation,
  "(2) Income" = model_personal_income,
  "(3) Political Preference" = model_personal_politicalpreference,
  "(4) Order Value" = model_personal_ordervalue
)

# Create table and rename variables
modelsummary(
  models_personal,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  gof_omit = 'RMSE|IC',
  coef_rename = c(
    "(Intercept)" = "Constant",
    "Treatment2" = "T2 vs. T1",
    "Treatment3" = "T3 vs. T1",
    "Treatment4" = "T4 vs. T1",
    "Income_EUR" = "Income",
    "Urbanisation2" = "Urbanisation: Medium vs. high",
    "Urbanisation3" = "Urbanisation: Low vs. high",
    "Political_Preference" = "Political index (0-10)",
    "Order_Value_EUR" = "Order Value"
  ),
  title = "Excluded obs. (T1 only): Regression results: personal and regional characteristics",
  notes     = "OLS; heteroskedasticity-robust (HC3) standard errors in parentheses.",
  output    = "ex_t1_personal_regressions_table.tex", # latex for Export, personal_regressions_table.tex for file
)
