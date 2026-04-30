library(jsonlite)
library(tidyverse)
library(ranger)
library(vip)

PATH <- "c:\\Users\\calli\\Downloads\\BIS 565 Final Project\\"

race_colors <- c(
  "Black"    = "#8B0000",
  "Hispanic" = "#C17B4E",
  "White"    = "#5C8374",
  "Other"    = "#5C6B7A"
)

## Data loading and preparation
df <- fromJSON(paste0(PATH, "Initial_Synth.json")) %>%
  mutate(
    black              = ifelse(race == "Black", 1, 0),
    hispanic           = ifelse(race == "Hispanic", 1, 0),
    prior_vaginal_only = ifelse(prior_vaginal_birth == 1 & prior_vbac == 0, 1, 0)
  )

## 2007 Grobman model - Grobman et al., Obstet Gynecol 2007;109(4):806-812
## w = 3.766 - 0.039(age) - 0.060(bmi) - 0.671(Black) - 0.680(Hispanic)
##     + 0.888(prior_vaginal_birth) + 1.003(prior_vbac) - 0.632(recurrent_indication)
## The 2007 model included only Black and Hispanic dummy coefficients. The Other/Asian
## category received no published coefficient and was absorbed into the intercept,
## modeled identically to White patients. This is a structural feature of the original
## paper, not an assumption of this analysis.
df <- df %>%
  mutate(
    w_2007 = 3.766
             - 0.039 * age
             - 0.060 * bmi
             - 0.671 * black
             - 0.680 * hispanic
             + 0.888 * prior_vaginal_birth
             + 1.003 * prior_vbac
             - 0.632 * recurrent_indication,
    pred_prob_2007 = exp(w_2007) / (1 + exp(w_2007)),
    above_60_2007  = ifelse(pred_prob_2007 >= 0.60, 1, 0)
  )

## 2021 Grobman model - Grobman et al., Am J Obstet Gynecol 2021;225:664.e1-7
## w = -5.952 - 0.023(age) - 0.024(weight_kg) + 0.056(height_cm)
##     - 0.597(recurrent_indication) + 0.868(prior_vaginal_only)
##     + 1.869(prior_vbac) - 0.966(chronic_htn)
df <- df %>%
  mutate(
    w_2021 = -5.952
             - 0.023 * age
             - 0.024 * weight_kg
             + 0.056 * height_cm
             - 0.597 * recurrent_indication
             + 0.868 * prior_vaginal_only
             + 1.869 * prior_vbac
             - 0.966 * chronic_htn,
    pred_prob_2021 = exp(w_2021) / (1 + exp(w_2021)),
    above_60_2021  = ifelse(pred_prob_2021 >= 0.60, 1, 0),
    delta          = pred_prob_2021 - pred_prob_2007
  )

## Core audit - Are predicted VBAC probabilities equitable across race groups under each model?
## White patients serve as the equity reference throughout. This reflects structural advantage
## rather than a neutral standard: White patients experience the greatest healthcare access
## under current conditions and represent the upper bound of equitable counseling access.
print("Mean predicted probability by race:")
print(df %>% group_by(race) %>%
  summarise(
    mean_2007  = round(mean(pred_prob_2007), 3),
    mean_2021  = round(mean(pred_prob_2021), 3),
    mean_delta = round(mean(delta), 3),
    n          = n()
  ))

print("Threshold crossing rates at 60% by race:")
print(df %>% group_by(race) %>%
  summarise(
    pct_above_60_2007 = round(mean(above_60_2007) * 100, 1),
    pct_above_60_2021 = round(mean(above_60_2021) * 100, 1),
    n                 = n()
  ))

## Counterfactual HTN equalization - How much of the remaining 2021 disparity is
## attributable to the chronic HTN proxy?
## Two reference rates are reported for methodological transparency:
## (1) White HTN prevalence (0.186) - primary equity reference; White chosen because they
##     experience the least structural disadvantage, making their rate an upper-bound target
## (2) Population mean HTN prevalence - alternative reference that is less politically
##     loaded and does not assume White outcomes as the aspirational standard
## The sensitivity analysis (charts 4-5) uses White-rate equalization only for consistency.
pop_mean_htn <- mean(df$chronic_htn)
print(paste("White HTN prevalence (primary equity reference): 0.186"))
print(paste("Population mean HTN prevalence (alternative reference):",
            round(pop_mean_htn, 3)))

set.seed(42)
df <- df %>%
  mutate(
    chronic_htn_equalized    = rbinom(n(), 1, prob = 0.186),
    chronic_htn_pop_mean     = rbinom(n(), 1, prob = pop_mean_htn),
    w_2021_equalized         = -5.952
                               - 0.023 * age
                               - 0.024 * weight_kg
                               + 0.056 * height_cm
                               - 0.597 * recurrent_indication
                               + 0.868 * prior_vaginal_only
                               + 1.869 * prior_vbac
                               - 0.966 * chronic_htn_equalized,
    pred_prob_2021_equalized = exp(w_2021_equalized) / (1 + exp(w_2021_equalized)),
    above_60_2021_equalized  = ifelse(pred_prob_2021_equalized >= 0.60, 1, 0),
    w_2021_pop_mean          = -5.952
                               - 0.023 * age
                               - 0.024 * weight_kg
                               + 0.056 * height_cm
                               - 0.597 * recurrent_indication
                               + 0.868 * prior_vaginal_only
                               + 1.869 * prior_vbac
                               - 0.966 * chronic_htn_pop_mean,
    pred_prob_2021_pop_mean  = exp(w_2021_pop_mean) / (1 + exp(w_2021_pop_mean)),
    above_60_2021_pop_mean   = ifelse(pred_prob_2021_pop_mean >= 0.60, 1, 0)
  )

print("Threshold crossing at 60% - four scenarios:")
print(df %>% group_by(race) %>%
  summarise(
    scenario_A_2007              = round(mean(above_60_2007) * 100, 1),
    scenario_B_2021_actual       = round(mean(above_60_2021) * 100, 1),
    scenario_C_equalized_white   = round(mean(above_60_2021_equalized) * 100, 1),
    scenario_D_equalized_popmean = round(mean(above_60_2021_pop_mean) * 100, 1),
    n                            = n()
  ))

gap_table <- df %>%
  group_by(race) %>%
  summarise(
    pct_2007     = mean(above_60_2007) * 100,
    pct_2021     = mean(above_60_2021) * 100,
    pct_eq_white = mean(above_60_2021_equalized) * 100,
    pct_eq_mean  = mean(above_60_2021_pop_mean) * 100
  ) %>%
  filter(race %in% c("Black", "White")) %>%
  pivot_wider(names_from = race,
              values_from = c(pct_2007, pct_2021, pct_eq_white, pct_eq_mean))

bw_gap_2007     <- gap_table$pct_2007_White     - gap_table$pct_2007_Black
bw_gap_2021     <- gap_table$pct_2021_White     - gap_table$pct_2021_Black
bw_gap_eq_white <- gap_table$pct_eq_white_White - gap_table$pct_eq_white_Black
bw_gap_eq_mean  <- gap_table$pct_eq_mean_White  - gap_table$pct_eq_mean_Black

print("Black-White gap in threshold crossing:")
print(paste("2007 model gap:", round(bw_gap_2007, 1), "pts"))
print(paste("2021 model gap (actual):", round(bw_gap_2021, 1), "pts"))
print(paste("2021 model gap (equalized to White HTN):", round(bw_gap_eq_white, 1), "pts"))
print(paste("2021 model gap (equalized to population mean HTN):", round(bw_gap_eq_mean, 1), "pts"))
print(paste("Proxy contribution - White reference:",
            round((bw_gap_2021 - bw_gap_eq_white) / bw_gap_2021 * 100, 1), "%"))
print(paste("Proxy contribution - population mean reference:",
            round((bw_gap_2021 - bw_gap_eq_mean) / bw_gap_2021 * 100, 1), "%"))

## Sensitivity analysis - Are findings robust to the choice of 60% counseling threshold?
thresholds <- seq(0.50, 0.70, by = 0.05)

compute_threshold_stats <- function(data, threshold) {
  data %>%
    mutate(
      above_2007      = ifelse(pred_prob_2007 >= threshold, 1, 0),
      above_2021      = ifelse(pred_prob_2021 >= threshold, 1, 0),
      above_equalized = ifelse(pred_prob_2021_equalized >= threshold, 1, 0)
    ) %>%
    group_by(race) %>%
    summarise(
      pct_2007      = round(mean(above_2007) * 100, 1),
      pct_2021      = round(mean(above_2021) * 100, 1),
      pct_equalized = round(mean(above_equalized) * 100, 1),
      .groups       = "drop"
    ) %>%
    mutate(threshold = threshold)
}

sensitivity_results <- map_dfr(thresholds, ~ compute_threshold_stats(df, .x))

print("Threshold sensitivity results:")
print(sensitivity_results %>%
  arrange(threshold, race) %>%
  select(threshold, race, pct_2007, pct_2021, pct_equalized))

bw_gaps <- sensitivity_results %>%
  filter(race %in% c("Black", "White")) %>%
  select(threshold, race, pct_2007, pct_2021, pct_equalized) %>%
  pivot_wider(names_from = race, values_from = c(pct_2007, pct_2021, pct_equalized)) %>%
  mutate(
    gap_2007               = pct_2007_White - pct_2007_Black,
    gap_2021               = pct_2021_White - pct_2021_Black,
    gap_equalized          = pct_equalized_White - pct_equalized_Black,
    proxy_contribution_pts = gap_2021 - gap_equalized,
    proxy_contribution_pct = round((gap_2021 - gap_equalized) / gap_2021 * 100, 1)
  )

print("Black-White gap and proxy contribution across thresholds:")
print(bw_gaps %>% select(threshold, gap_2007, gap_2021, gap_equalized,
                          proxy_contribution_pts, proxy_contribution_pct))

## Chart 1 - Mean predicted probability by race under both models
chart1_data <- df %>%
  group_by(race) %>%
  summarise(
    mean_2007 = mean(pred_prob_2007),
    mean_2021 = mean(pred_prob_2021)
  ) %>%
  pivot_longer(cols = c(mean_2007, mean_2021),
               names_to  = "model",
               values_to = "mean_prob") %>%
  mutate(
    model = recode(model, "mean_2007" = "2007 Model", "mean_2021" = "2021 Model"),
    race  = factor(race, levels = c("Black", "Hispanic", "Other", "White"))
  )

p1 <- ggplot(chart1_data, aes(x = race, y = mean_prob, fill = race, alpha = model)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0.60, linetype = "dashed", color = "#C0392B", linewidth = 0.8) +
  annotate("text", x = 0.6, y = 0.615, label = "60% counseling threshold",
           color = "#C0392B", size = 3.2, hjust = 0) +
  scale_fill_manual(values = race_colors) +
  scale_alpha_manual(values = c("2007 Model" = 0.5, "2021 Model" = 1.0),
                     guide = guide_legend(override.aes = list(fill = "gray40"))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(title    = "Mean Predicted VBAC Probability by Race",
       subtitle = "2007 vs. 2021 Grobman Models",
       x = NULL, y = "Mean predicted probability", fill = "Race", alpha = "Model") +
  guides(fill = guide_legend(order = 1), alpha = guide_legend(order = 2)) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom",
        panel.grid.major.x = element_blank())

print(p1)
ggsave(paste0(PATH, "chart1_mean_prob.png"), p1, width = 8, height = 5, dpi = 300)

## Chart 2 - Threshold crossing rates under both models
chart2_data <- df %>%
  group_by(race) %>%
  summarise(pct_2007 = mean(above_60_2007) * 100,
            pct_2021 = mean(above_60_2021) * 100) %>%
  pivot_longer(cols = c(pct_2007, pct_2021),
               names_to  = "model",
               values_to = "pct_above") %>%
  mutate(
    model = recode(model, "pct_2007" = "2007 Model", "pct_2021" = "2021 Model"),
    race  = factor(race, levels = c("Black", "Hispanic", "Other", "White"))
  )

p2 <- ggplot(chart2_data, aes(x = race, y = pct_above, fill = race, alpha = model)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = race_colors) +
  scale_alpha_manual(values = c("2007 Model" = 0.6, "2021 Model" = 1.0)) +
  scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
  labs(title    = "Patients Above 60% VBAC Probability Threshold by Race",
       subtitle = "2007 vs. 2021 Grobman Models",
       x = NULL, y = "Percentage of patients above threshold",
       fill = "Race", alpha = "Model") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom",
        panel.grid.major.x = element_blank())

print(p2)
ggsave(paste0(PATH, "chart2_threshold.png"), p2, width = 8, height = 5, dpi = 300)

## Chart 3 - Counterfactual HTN equalization across four scenarios
chart3_data <- df %>%
  group_by(race) %>%
  summarise(
    "2007 Model"                          = mean(above_60_2007) * 100,
    "2021 Model (actual)"                 = mean(above_60_2021) * 100,
    "2021 Model (equalized to White HTN)" = mean(above_60_2021_equalized) * 100,
    "2021 Model (equalized to mean HTN)"  = mean(above_60_2021_pop_mean) * 100
  ) %>%
  pivot_longer(cols = -race, names_to = "scenario", values_to = "pct_above") %>%
  mutate(
    race     = factor(race, levels = c("Black", "Hispanic", "Other", "White")),
    scenario = factor(scenario, levels = c(
      "2007 Model",
      "2021 Model (actual)",
      "2021 Model (equalized to White HTN)",
      "2021 Model (equalized to mean HTN)"
    ))
  )

p3 <- ggplot(chart3_data, aes(x = scenario, y = pct_above, fill = race)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = race_colors) +
  scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
  labs(title    = "Proxy Discrimination: Counterfactual HTN Equalization",
       subtitle = "Threshold crossing rates under four scenarios",
       x = NULL, y = "Percentage of patients above 60% threshold", fill = "Race") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom",
        panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 9))

print(p3)
ggsave(paste0(PATH, "chart3_counterfactual.png"), p3, width = 10, height = 5, dpi = 300)

## Chart 4 - Black-White gap across thresholds (sensitivity analysis)
chart4_data <- bw_gaps %>%
  select(threshold, gap_2007, gap_2021, gap_equalized) %>%
  pivot_longer(cols = c(gap_2007, gap_2021, gap_equalized),
               names_to = "scenario", values_to = "gap") %>%
  mutate(
    scenario = recode(scenario,
                      "gap_2007"      = "2007 Model",
                      "gap_2021"      = "2021 Model (actual)",
                      "gap_equalized" = "2021 Model (equalized to White HTN)"),
    scenario      = factor(scenario, levels = c(
      "2007 Model", "2021 Model (actual)",
      "2021 Model (equalized to White HTN)")),
    threshold_pct = threshold * 100
  )

p4 <- ggplot(chart4_data,
             aes(x = threshold_pct, y = gap, color = scenario, group = scenario)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("2007 Model" = "#8B0000",
                                 "2021 Model (actual)" = "#5C8374",
                                 "2021 Model (equalized to White HTN)" = "#C17B4E")) +
  scale_x_continuous(breaks = c(50, 55, 60, 65, 70),
                     labels = function(x) paste0(x, "%")) +
  scale_y_continuous(limits = c(0, 45), labels = function(x) paste0(x, " pts")) +
  labs(title    = "Black-White Gap in Threshold Crossing Across Thresholds",
       subtitle = "Sensitivity analysis showing robustness of findings",
       x = "Counseling threshold", y = "Black-White gap (percentage points)",
       color = NULL) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

print(p4)
ggsave(paste0(PATH, "chart4_sensitivity.png"), p4, width = 9, height = 5, dpi = 300)

## All-group gap analysis - Does race removal uniformly benefit all groups?

all_gaps <- df %>%
  group_by(race) %>%
  summarise(
    pct_2007      = mean(above_60_2007) * 100,
    pct_2021      = mean(above_60_2021) * 100,
    pct_equalized = mean(above_60_2021_equalized) * 100,
    n             = n()
  )

white_2007      <- all_gaps %>% filter(race == "White") %>% pull(pct_2007)
white_2021      <- all_gaps %>% filter(race == "White") %>% pull(pct_2021)
white_equalized <- all_gaps %>% filter(race == "White") %>% pull(pct_equalized)

all_gaps <- all_gaps %>%
  mutate(
    gap_vs_white_2007      = round(white_2007 - pct_2007, 1),
    gap_vs_white_2021      = round(white_2021 - pct_2021, 1),
    gap_vs_white_equalized = round(white_equalized - pct_equalized, 1),
    proxy_contribution_pts = round(gap_vs_white_2021 - gap_vs_white_equalized, 1),
    proxy_contribution_pct = round(
      ifelse(gap_vs_white_2021 > 0,
             (gap_vs_white_2021 - gap_vs_white_equalized) / gap_vs_white_2021 * 100,
             NA), 1)
  )

print("All race group gaps vs White - 2007 and 2021 models:")
print(all_gaps %>% select(race, pct_2007, pct_2021, pct_equalized,
                           gap_vs_white_2007, gap_vs_white_2021,
                           gap_vs_white_equalized, proxy_contribution_pts,
                           proxy_contribution_pct))

compute_allgroup_gaps <- function(data, threshold) {
  group_rates <- data %>%
    mutate(
      above_2007      = ifelse(pred_prob_2007 >= threshold, 1, 0),
      above_2021      = ifelse(pred_prob_2021 >= threshold, 1, 0),
      above_equalized = ifelse(pred_prob_2021_equalized >= threshold, 1, 0)
    ) %>%
    group_by(race) %>%
    summarise(
      pct_2007      = mean(above_2007) * 100,
      pct_2021      = mean(above_2021) * 100,
      pct_equalized = mean(above_equalized) * 100,
      .groups       = "drop"
    )

  white_row <- group_rates %>% filter(race == "White")

  group_rates %>%
    filter(race != "White") %>%
    mutate(
      threshold              = threshold,
      gap_2007               = white_row$pct_2007 - pct_2007,
      gap_2021               = white_row$pct_2021 - pct_2021,
      gap_equalized          = white_row$pct_equalized - pct_equalized,
      proxy_contribution_pts = gap_2021 - gap_equalized,
      proxy_contribution_pct = round(
        ifelse(gap_2021 > 0,
               (gap_2021 - gap_equalized) / gap_2021 * 100, NA), 1)
    )
}

allgroup_sensitivity <- map_dfr(thresholds, ~ compute_allgroup_gaps(df, .x))

print("All-group gap sensitivity analysis across thresholds:")
print(allgroup_sensitivity %>%
  select(threshold, race, gap_2007, gap_2021, gap_equalized,
         proxy_contribution_pts, proxy_contribution_pct) %>%
  arrange(threshold, race))

## Chart 5 - Gap vs White for all race groups across thresholds
p5 <- ggplot(allgroup_sensitivity,
             aes(x = threshold * 100, y = gap_2021, color = race, group = race)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Black" = "#8B0000", "Hispanic" = "#C17B4E",
                                 "Other" = "#5C6B7A")) +
  scale_x_continuous(breaks = c(50, 55, 60, 65, 70),
                     labels = function(x) paste0(x, "%")) +
  scale_y_continuous(labels = function(x) paste0(x, " pts")) +
  labs(title    = "Gap vs White in Threshold Crossing - All Race Groups",
       subtitle = "2021 model across counseling thresholds",
       x = "Counseling threshold", y = "Gap vs White (percentage points)",
       color = "Race") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

print(p5)
ggsave(paste0(PATH, "chart5_allgroup_gaps.png"), p5, width = 9, height = 5, dpi = 300)

## Parameter tuning - How does the Black race coefficient affect threshold crossing across its range from the published value to zero?
black_coef_values <- seq(-0.671, 0, length.out = 20)

compute_coef_stats <- function(beta_black, data) {
  data %>%
    mutate(
      w_tuned    = 3.766 - 0.039*age - 0.060*bmi + beta_black*black - 0.680*hispanic
                   + 0.888*prior_vaginal_birth + 1.003*prior_vbac
                   - 0.632*recurrent_indication,
      pred_tuned = exp(w_tuned) / (1 + exp(w_tuned)),
      above_60   = ifelse(pred_tuned >= 0.60, 1, 0)
    ) %>%
    group_by(race) %>%
    summarise(
      mean_prob    = round(mean(pred_tuned), 3),
      pct_above_60 = round(mean(above_60) * 100, 1),
      .groups      = "drop"
    ) %>%
    mutate(beta_black = beta_black)
}

coef_tuning_results <- map_dfr(black_coef_values, ~ compute_coef_stats(.x, df))

print("Parameter tuning - Black coefficient from published to zero:")
print(coef_tuning_results %>%
  filter(race %in% c("Black", "White")) %>%
  select(beta_black, race, pct_above_60) %>%
  pivot_wider(names_from = race, values_from = pct_above_60) %>%
  mutate(bw_gap = White - Black) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1))))

## Chart 6a - Black race coefficient tuning
p6a <- coef_tuning_results %>%
  filter(race %in% c("Black", "Hispanic", "Other", "White")) %>%
  ggplot(aes(x = beta_black, y = pct_above_60, color = race, group = race)) +
  geom_line(linewidth = 1.2) +
  geom_vline(xintercept = -0.671, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  annotate("text", x = -0.671, y = 20, label = "Published\n2007 value",
           hjust = 1.1, size = 3, color = "gray40") +
  annotate("text", x = 0, y = 20, label = "2021 model\n(race removed)",
           hjust = -0.1, size = 3, color = "gray40") +
  scale_color_manual(values = race_colors) +
  scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
  labs(title    = "Effect of Black Race Coefficient on Threshold Crossing",
       subtitle = "Varying beta from published -0.671 to 0 across all race groups",
       x = "Black race coefficient value",
       y = "Percentage above 60% threshold", color = "Race") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

print(p6a)
ggsave(paste0(PATH, "chart6a_coef_tuning.png"), p6a, width = 9, height = 5, dpi = 300)

## Intercept adjustment - Can shifting the model intercept alone close the racial gap?
intercept_values <- seq(-2, 2, length.out = 20)

compute_intercept_stats <- function(b0_adjustment, data) {
  data %>%
    mutate(
      w_tuned    = (3.766 + b0_adjustment) - 0.039*age - 0.060*bmi - 0.671*black
                   - 0.680*hispanic + 0.888*prior_vaginal_birth
                   + 1.003*prior_vbac - 0.632*recurrent_indication,
      pred_tuned = exp(w_tuned) / (1 + exp(w_tuned)),
      above_60   = ifelse(pred_tuned >= 0.60, 1, 0)
    ) %>%
    group_by(race) %>%
    summarise(
      mean_prob    = round(mean(pred_tuned), 3),
      pct_above_60 = round(mean(above_60) * 100, 1),
      .groups      = "drop"
    ) %>%
    mutate(intercept_adjustment = b0_adjustment)
}

intercept_tuning_results <- map_dfr(intercept_values, ~ compute_intercept_stats(.x, df))

print("Parameter tuning - intercept adjustment:")
print(intercept_tuning_results %>%
  filter(race %in% c("Black", "White")) %>%
  select(intercept_adjustment, race, pct_above_60) %>%
  pivot_wider(names_from = race, values_from = pct_above_60) %>%
  mutate(bw_gap = White - Black) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1))))

## Chart 6b - Intercept adjustment
p6b <- intercept_tuning_results %>%
  ggplot(aes(x = intercept_adjustment, y = pct_above_60, color = race, group = race)) +
  geom_line(linewidth = 1.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  annotate("text", x = 0, y = 5, label = "Published\nintercept",
           hjust = -0.1, size = 3, color = "gray40") +
  scale_color_manual(values = race_colors) +
  scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
  labs(title    = "Effect of Intercept Adjustment on Threshold Crossing",
       subtitle = "Varying b0 from -2 to +2 holding race coefficient constant",
       x = "Intercept adjustment", y = "Percentage above 60% threshold",
       color = "Race") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

print(p6b)
ggsave(paste0(PATH, "chart6b_intercept_tuning.png"), p6b, width = 9, height = 5, dpi = 300)

## Reverse calculation (Black) - What coefficient value produces equitable threshold crossing for Black patients relative to White?
## Reverse calculation (Hispanic) - Same question for Hispanic patients.
white_rate_2007 <- df %>%
  filter(race == "White") %>%
  summarise(rate = mean(above_60_2007) * 100) %>%
  pull(rate)

print(paste("Equity target (White 2007 threshold crossing rate):",
            round(white_rate_2007, 1), "%"))

find_equity_coef_black <- function(beta_black, data) {
  result <- data %>%
    mutate(
      w_tuned    = 3.766 - 0.039*age - 0.060*bmi + beta_black*black - 0.680*hispanic
                   + 0.888*prior_vaginal_birth + 1.003*prior_vbac
                   - 0.632*recurrent_indication,
      pred_tuned = exp(w_tuned) / (1 + exp(w_tuned)),
      above_60   = ifelse(pred_tuned >= 0.60, 1, 0)
    ) %>%
    group_by(race) %>%
    summarise(pct_above_60 = mean(above_60) * 100, .groups = "drop")

  black_rate <- result %>% filter(race == "Black") %>% pull(pct_above_60)
  white_rate <- result %>% filter(race == "White") %>% pull(pct_above_60)
  data.frame(beta = beta_black, group = "Black", gap = round(white_rate - black_rate, 1))
}

find_equity_coef_hispanic <- function(beta_hispanic, data) {
  result <- data %>%
    mutate(
      w_tuned    = 3.766 - 0.039*age - 0.060*bmi - 0.671*black + beta_hispanic*hispanic
                   + 0.888*prior_vaginal_birth + 1.003*prior_vbac
                   - 0.632*recurrent_indication,
      pred_tuned = exp(w_tuned) / (1 + exp(w_tuned)),
      above_60   = ifelse(pred_tuned >= 0.60, 1, 0)
    ) %>%
    group_by(race) %>%
    summarise(pct_above_60 = mean(above_60) * 100, .groups = "drop")

  hispanic_rate <- result %>% filter(race == "Hispanic") %>% pull(pct_above_60)
  white_rate    <- result %>% filter(race == "White") %>% pull(pct_above_60)
  data.frame(beta = beta_hispanic, group = "Hispanic", gap = round(white_rate - hispanic_rate, 1))
}

equity_search_black    <- map_dfr(seq(-0.671, 0.5, by = 0.01),
                                   ~ find_equity_coef_black(.x, df))
equity_search_hispanic <- map_dfr(seq(-0.680, 0.5, by = 0.01),
                                   ~ find_equity_coef_hispanic(.x, df))

equity_coef_black    <- equity_search_black %>%
  mutate(abs_gap = abs(gap)) %>% arrange(abs_gap) %>% slice(1)
equity_coef_hispanic <- equity_search_hispanic %>%
  mutate(abs_gap = abs(gap)) %>% arrange(abs_gap) %>% slice(1)

print("Reverse calculation - Black coefficient:")
print(paste("Published: -0.671 | Equity-producing:",
            round(equity_coef_black$beta, 3),
            "| Change needed:", round(equity_coef_black$beta - (-0.671), 3)))

print("Reverse calculation - Hispanic coefficient:")
print(paste("Published: -0.680 | Equity-producing:",
            round(equity_coef_hispanic$beta, 3),
            "| Change needed:", round(equity_coef_hispanic$beta - (-0.680), 3)))

## Chart 7 - Policy charter: equity-producing coefficients for Black and Hispanic
chart7_data <- bind_rows(
  equity_search_black %>%
    mutate(group_label = paste0(
      "Black coefficient\n",
      "published: -0.671   equity: +", round(equity_coef_black$beta, 3))),
  equity_search_hispanic %>%
    mutate(group_label = paste0(
      "Hispanic coefficient\n",
      "published: -0.680   equity: +", round(equity_coef_hispanic$beta, 3)))
)

equity_vlines <- data.frame(
  group_label   = c(
    paste0("Black coefficient\npublished: -0.671   equity: +",
           round(equity_coef_black$beta, 3)),
    paste0("Hispanic coefficient\npublished: -0.680   equity: +",
           round(equity_coef_hispanic$beta, 3))
  ),
  published_val = c(-0.671, -0.680),
  equity_val    = c(equity_coef_black$beta, equity_coef_hispanic$beta)
)

p7 <- ggplot(chart7_data, aes(x = beta, y = gap)) +
  geom_line(color = "#5C8374", linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#C0392B", linewidth = 0.8) +
  geom_vline(data = equity_vlines, aes(xintercept = published_val),
             linetype = "dashed", color = "gray40", linewidth = 0.8) +
  geom_vline(data = equity_vlines, aes(xintercept = equity_val),
             linetype = "dashed", color = "#8B0000", linewidth = 0.8) +
  facet_wrap(~ group_label, scales = "free_x") +
  labs(title    = "Reverse Calculation: Policy Charter",
       subtitle = "What coefficient values produce equitable threshold crossing?",
       x = "Race coefficient value",
       y = "Gap vs White in threshold crossing (percentage points)") +
  theme_minimal(base_size = 13) +
  theme(plot.title  = element_text(face = "bold"),
        strip.text  = element_text(face = "bold", size = 10))

print(p7)
ggsave(paste0(PATH, "chart7_policy_charter.png"), p7, width = 11, height = 5, dpi = 300)

## Parametric vs non-parametric model comparison - Do flexible models re-learn racial patterns from clinical variables without explciit race variable?
set.seed(42)
df <- df %>%
  mutate(vbac_outcome = rbinom(n(), 1, prob = pred_prob_2007))

print(paste("Synthetic VBAC outcome rate:", round(mean(df$vbac_outcome) * 100, 1), "%"))
print(df %>% group_by(race) %>%
  summarise(vbac_rate = round(mean(vbac_outcome) * 100, 1), n = n()))

predictors_with_race    <- c("age", "bmi", "prior_vaginal_birth", "prior_vbac",
                              "recurrent_indication", "black", "hispanic")
predictors_without_race <- c("age", "bmi", "prior_vaginal_birth", "prior_vbac",
                              "recurrent_indication", "chronic_htn",
                              "pregestational_diabetes")

df_with_race    <- df %>%
  select(all_of(predictors_with_race), vbac_outcome) %>%
  mutate(vbac_outcome = factor(vbac_outcome, labels = c("no", "yes")))

df_without_race <- df %>%
  select(all_of(predictors_without_race), vbac_outcome) %>%
  mutate(vbac_outcome = factor(vbac_outcome, labels = c("no", "yes")))

lr_with    <- glm(vbac_outcome ~ .,
                  data   = df %>% select(all_of(predictors_with_race), vbac_outcome),
                  family = binomial())

lr_without <- glm(vbac_outcome ~ .,
                  data   = df %>% select(all_of(predictors_without_race), vbac_outcome),
                  family = binomial())

set.seed(42)
rf_with    <- ranger(vbac_outcome ~ ., data = df_with_race, num.trees = 500,
                     importance = "permutation", probability = TRUE)

set.seed(42)
rf_without <- ranger(vbac_outcome ~ ., data = df_without_race, num.trees = 500,
                     importance = "permutation", probability = TRUE)

rf_with_imp <- data.frame(
  variable   = names(rf_with$variable.importance),
  importance = rf_with$variable.importance,
  model      = "RF with race"
) %>% arrange(desc(importance))

rf_without_imp <- data.frame(
  variable   = names(rf_without$variable.importance),
  importance = rf_without$variable.importance,
  model      = "RF without race"
) %>% arrange(desc(importance))

print("Random forest variable importance WITH race:")
print(rf_with_imp)

print("Random forest variable importance WITHOUT race:")
print(rf_without_imp)

print("Logistic regression coefficients WITH race:")
print(round(coef(lr_with), 3))

print("Logistic regression coefficients WITHOUT race:")
print(round(coef(lr_without), 3))

df <- df %>%
  mutate(
    lr_with_prob        = predict(lr_with, type = "response",
                                  newdata = df %>% select(all_of(predictors_with_race))),
    lr_without_prob     = predict(lr_without, type = "response",
                                  newdata = df %>% select(all_of(predictors_without_race))),
    rf_with_prob        = rf_with$predictions[, "yes"],
    rf_without_prob     = rf_without$predictions[, "yes"],
    above_60_lr_with    = ifelse(lr_with_prob >= 0.60, 1, 0),
    above_60_lr_without = ifelse(lr_without_prob >= 0.60, 1, 0),
    above_60_rf_with    = ifelse(rf_with_prob >= 0.60, 1, 0),
    above_60_rf_without = ifelse(rf_without_prob >= 0.60, 1, 0)
  )

print("Threshold crossing at 60% - four model comparison, all race groups:")
print(df %>% group_by(race) %>%
  summarise(
    LR_with_race    = round(mean(above_60_lr_with) * 100, 1),
    LR_without_race = round(mean(above_60_lr_without) * 100, 1),
    RF_with_race    = round(mean(above_60_rf_with) * 100, 1),
    RF_without_race = round(mean(above_60_rf_without) * 100, 1),
    n               = n()
  ))

print("Gap vs White across four models - all race groups:")
ml_white_rates <- df %>%
  filter(race == "White") %>%
  summarise(
    lr_with    = mean(above_60_lr_with) * 100,
    lr_without = mean(above_60_lr_without) * 100,
    rf_with    = mean(above_60_rf_with) * 100,
    rf_without = mean(above_60_rf_without) * 100
  )

ml_gaps <- df %>%
  group_by(race) %>%
  summarise(
    lr_with    = mean(above_60_lr_with) * 100,
    lr_without = mean(above_60_lr_without) * 100,
    rf_with    = mean(above_60_rf_with) * 100,
    rf_without = mean(above_60_rf_without) * 100
  ) %>%
  mutate(
    gap_lr_with    = round(ml_white_rates$lr_with - lr_with, 1),
    gap_lr_without = round(ml_white_rates$lr_without - lr_without, 1),
    gap_rf_with    = round(ml_white_rates$rf_with - rf_with, 1),
    gap_rf_without = round(ml_white_rates$rf_without - rf_without, 1)
  ) %>%
  select(race, gap_lr_with, gap_lr_without, gap_rf_with, gap_rf_without)

print(ml_gaps)

## Chart 8a - Random forest variable importance with and without race
imp_combined <- bind_rows(rf_with_imp, rf_without_imp)

p8a <- ggplot(imp_combined, aes(x = reorder(variable, importance),
                                 y = importance, fill = model)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_fill_manual(values = c("RF with race" = "#8B0000",
                                "RF without race" = "#5C8374")) +
  labs(title    = "Random Forest Variable Importance",
       subtitle = "With vs without race as explicit predictor",
       x = NULL, y = "Permutation importance", fill = NULL) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

print(p8a)
ggsave(paste0(PATH, "chart8a_rf_importance.png"), p8a, width = 9, height = 5, dpi = 300)

## Chart 8b - Threshold crossing by model type and race
ml_threshold_data <- df %>%
  group_by(race) %>%
  summarise(
    "LR with race"    = mean(above_60_lr_with) * 100,
    "LR without race" = mean(above_60_lr_without) * 100,
    "RF with race"    = mean(above_60_rf_with) * 100,
    "RF without race" = mean(above_60_rf_without) * 100
  ) %>%
  pivot_longer(cols = -race, names_to = "model", values_to = "pct_above") %>%
  mutate(
    race  = factor(race, levels = c("Black", "Hispanic", "Other", "White")),
    model = factor(model, levels = c("LR with race", "LR without race",
                                      "RF with race", "RF without race"))
  )

p8b <- ggplot(ml_threshold_data, aes(x = model, y = pct_above, fill = race)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = race_colors) +
  scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
  labs(title    = "Threshold Crossing by Model Type and Race",
       subtitle = "Parametric vs non-parametric, with and without race",
       x = NULL, y = "Percentage above 60% threshold", fill = "Race") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom",
        axis.text.x = element_text(size = 10))

print(p8b)
ggsave(paste0(PATH, "chart8b_ml_threshold.png"), p8b, width = 10, height = 5, dpi = 300)

## Scenario analysis - Does model behavior change with racial composition of the dataset?
run_scenario_analysis <- function(filepath, scenario_name) {
  data <- fromJSON(filepath)

  data <- data %>%
    mutate(
      black              = ifelse(race == "Black", 1, 0),
      hispanic           = ifelse(race == "Hispanic", 1, 0),
      prior_vaginal_only = ifelse(prior_vaginal_birth == 1 & prior_vbac == 0, 1, 0),
      w_2007             = 3.766 - 0.039*age - 0.060*bmi - 0.671*black - 0.680*hispanic
                           + 0.888*prior_vaginal_birth + 1.003*prior_vbac
                           - 0.632*recurrent_indication,
      pred_prob_2007     = exp(w_2007) / (1 + exp(w_2007)),
      w_2021             = -5.952 - 0.023*age - 0.024*weight_kg + 0.056*height_cm
                           - 0.597*recurrent_indication + 0.868*prior_vaginal_only
                           + 1.869*prior_vbac - 0.966*chronic_htn,
      pred_prob_2021     = exp(w_2021) / (1 + exp(w_2021)),
      above_60_2007      = ifelse(pred_prob_2007 >= 0.60, 1, 0),
      above_60_2021      = ifelse(pred_prob_2021 >= 0.60, 1, 0)
    )

  data %>%
    group_by(race) %>%
    summarise(
      pct_above_60_2007 = round(mean(above_60_2007) * 100, 1),
      pct_above_60_2021 = round(mean(above_60_2021) * 100, 1),
      mean_prob_2007    = round(mean(pred_prob_2007), 3),
      mean_prob_2021    = round(mean(pred_prob_2021), 3),
      n                 = n(),
      .groups           = "drop"
    ) %>%
    mutate(scenario = scenario_name)
}

scenario_results <- bind_rows(
  run_scenario_analysis(paste0(PATH, "Initial_Synth.json"),
                        "Grobman 2007 cohort (baseline)"),
  run_scenario_analysis(paste0(PATH, "Synth2_RealisticPopulation.json"),
                        "US birth population"),
  run_scenario_analysis(paste0(PATH, "Synth3_BalancedPopulation.json"),
                        "Equal group sizes"),
  run_scenario_analysis(paste0(PATH, "Synth4_Oversampled.json"),
                        "Oversampled minority")
)

print("Threshold crossing across dataset compositions:")
print(scenario_results %>%
  select(scenario, race, pct_above_60_2007, pct_above_60_2021, n) %>%
  arrange(scenario, race))

print("Black-White gap by scenario:")
scenario_bw_gaps <- scenario_results %>%
  filter(race %in% c("Black", "White")) %>%
  select(scenario, race, pct_above_60_2007, pct_above_60_2021) %>%
  pivot_wider(names_from  = race,
              values_from = c(pct_above_60_2007, pct_above_60_2021)) %>%
  mutate(
    gap_2007 = round(pct_above_60_2007_White - pct_above_60_2007_Black, 1),
    gap_2021 = round(pct_above_60_2021_White - pct_above_60_2021_Black, 1)
  ) %>%
  select(scenario, gap_2007, gap_2021)

print(scenario_bw_gaps)

print("All-group gaps vs White by scenario:")
scenario_all_gaps <- scenario_results %>%
  group_by(scenario) %>%
  mutate(
    white_2007 = pct_above_60_2007[race == "White"],
    white_2021 = pct_above_60_2021[race == "White"],
    gap_2007   = round(white_2007 - pct_above_60_2007, 1),
    gap_2021   = round(white_2021 - pct_above_60_2021, 1)
  ) %>%
  filter(race != "White") %>%
  select(scenario, race, gap_2007, gap_2021, n) %>%
  arrange(scenario, race)

print(scenario_all_gaps)

## Chart 9 - Threshold crossing across four dataset compositions
p9 <- scenario_results %>%
  mutate(
    race     = factor(race, levels = c("Black", "Hispanic", "Other", "White")),
    scenario = factor(scenario, levels = c("Grobman 2007 cohort (baseline)",
                                            "US birth population",
                                            "Equal group sizes",
                                            "Oversampled minority"))
  ) %>%
  pivot_longer(cols = c(pct_above_60_2007, pct_above_60_2021),
               names_to  = "model",
               values_to = "pct_above") %>%
  mutate(model = recode(model, "pct_above_60_2007" = "2007 Model",
                        "pct_above_60_2021" = "2021 Model")) %>%
  ggplot(aes(x = race, y = pct_above, fill = race, alpha = model)) +
  geom_col(position = "dodge") +
  facet_wrap(~ scenario, nrow = 2) +
  scale_fill_manual(values = race_colors) +
  scale_alpha_manual(values = c("2007 Model" = 0.5, "2021 Model" = 1.0)) +
  scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
  labs(title    = "Threshold Crossing Across Dataset Compositions",
       subtitle = "2007 vs 2021 model under four racial composition scenarios",
       x = NULL, y = "Percentage above 60% threshold",
       fill = "Race", alpha = "Model") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        strip.text = element_text(face = "bold", size = 10))

print(p9)
ggsave(paste0(PATH, "chart9_scenarios.png"), p9, width = 11, height = 7, dpi = 300)

## HTN subgroup analysis - Does the racial gap persist within HTN status subgroups, or is it driven entirely by disparate  prevalence?
htn_subgroup <- df %>%
  mutate(htn_group = ifelse(chronic_htn == 1, "HTN positive", "HTN negative")) %>%
  group_by(htn_group, race) %>%
  summarise(
    pct_above_60_2007 = round(mean(above_60_2007) * 100, 1),
    pct_above_60_2021 = round(mean(above_60_2021) * 100, 1),
    n                 = n(),
    .groups           = "drop"
  )

print("HTN-stratified subgroup analysis (all groups, descriptive):")
print(htn_subgroup)

htn_gap <- htn_subgroup %>%
  group_by(htn_group) %>%
  mutate(
    white_2007 = pct_above_60_2007[race == "White"],
    white_2021 = pct_above_60_2021[race == "White"],
    gap_2007   = round(white_2007 - pct_above_60_2007, 1),
    gap_2021   = round(white_2021 - pct_above_60_2021, 1)
  ) %>%
  filter(race %in% c("Black", "Hispanic")) %>%
  select(htn_group, race, pct_above_60_2007, pct_above_60_2021, gap_2007, gap_2021, n)

print("Gap vs White within each HTN subgroup (Black and Hispanic):")
print(htn_gap)

## Chart 10a - Threshold crossing by HTN status and race
htn_viz_data <- htn_subgroup %>%
  pivot_longer(cols = c(pct_above_60_2007, pct_above_60_2021),
               names_to  = "model",
               values_to = "pct_above") %>%
  mutate(
    model = recode(model, "pct_above_60_2007" = "2007 Model",
                   "pct_above_60_2021" = "2021 Model"),
    race  = factor(race, levels = c("Black", "Hispanic", "Other", "White"))
  )

p10a <- ggplot(htn_viz_data, aes(x = race, y = pct_above, fill = race, alpha = model)) +
  geom_col(position = "dodge") +
  facet_wrap(~ htn_group) +
  scale_fill_manual(values = race_colors) +
  scale_alpha_manual(values = c("2007 Model" = 0.5, "2021 Model" = 1.0)) +
  scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
  labs(title    = "Threshold Crossing by HTN Status and Race",
       subtitle = "Does the racial gap persist within HTN subgroups?",
       x = NULL, y = "Percentage above 60% threshold",
       fill = "Race", alpha = "Model") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        strip.text = element_text(face = "bold", size = 12))

print(p10a)
ggsave(paste0(PATH, "chart10a_htn_subgroup.png"), p10a, width = 10, height = 5, dpi = 300)

## BMI subgroup analysis - Does the racial gap concentrate in high-BMI patients? Gap calculations restricted to Black and Hispanic vs White. Other group excluded
df <- df %>%
  mutate(
    bmi_category = case_when(
      bmi < 25              ~ "Normal (<25)",
      bmi >= 25 & bmi < 30  ~ "Overweight (25-30)",
      bmi >= 30 & bmi < 35  ~ "Obese I (30-35)",
      bmi >= 35             ~ "Obese II+ (35+)"
    ),
    bmi_category = factor(bmi_category, levels = c("Normal (<25)", "Overweight (25-30)",
                                                    "Obese I (30-35)", "Obese II+ (35+)"))
  )

bmi_subgroup <- df %>%
  group_by(bmi_category, race) %>%
  summarise(
    pct_above_60_2007 = round(mean(above_60_2007) * 100, 1),
    pct_above_60_2021 = round(mean(above_60_2021) * 100, 1),
    n                 = n(),
    .groups           = "drop"
  )

print("BMI-stratified subgroup analysis:")
print(bmi_subgroup)

bmi_gap <- bmi_subgroup %>%
  group_by(bmi_category) %>%
  mutate(
    white_2007 = pct_above_60_2007[race == "White"],
    white_2021 = pct_above_60_2021[race == "White"],
    gap_2007   = round(white_2007 - pct_above_60_2007, 1),
    gap_2021   = round(white_2021 - pct_above_60_2021, 1)
  ) %>%
  filter(race %in% c("Black", "Hispanic")) %>%
  select(bmi_category, race, pct_above_60_2007, pct_above_60_2021, gap_2007, gap_2021, n)

print("Gap vs White within each BMI category (Black and Hispanic):")
print(bmi_gap)

## Chart 10b - Black-White gap by BMI category
bmi_bw_gap <- bmi_subgroup %>%
  filter(race %in% c("Black", "White")) %>%
  select(bmi_category, race, pct_above_60_2007, pct_above_60_2021) %>%
  pivot_wider(names_from  = race,
              values_from = c(pct_above_60_2007, pct_above_60_2021)) %>%
  mutate(
    gap_2007 = pct_above_60_2007_White - pct_above_60_2007_Black,
    gap_2021 = pct_above_60_2021_White - pct_above_60_2021_Black
  ) %>%
  select(bmi_category, gap_2007, gap_2021) %>%
  pivot_longer(cols = c(gap_2007, gap_2021),
               names_to  = "model",
               values_to = "gap") %>%
  mutate(model = recode(model, "gap_2007" = "2007 Model", "gap_2021" = "2021 Model"))

p10b <- ggplot(bmi_bw_gap, aes(x = bmi_category, y = gap, fill = model, group = model)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("2007 Model" = "#8B0000", "2021 Model" = "#5C8374")) +
  scale_y_continuous(limits = c(0, 60), labels = function(x) paste0(x, " pts")) +
  labs(title    = "Black-White Gap in Threshold Crossing by BMI Category",
       subtitle = "Does racial disparity concentrate in high-BMI patients?",
       x = "BMI category", y = "Black-White gap (percentage points)",
       fill = "Model") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom",
        panel.grid.major.x = element_blank())

print(p10b)
ggsave(paste0(PATH, "chart10b_bmi_subgroup.png"), p10b, width = 9, height = 5, dpi = 300)

## Joint HTN and BMI analysis - Is the gap largest at the intersection of high BMI and HTN, and does the proxy mechanism operate similarly for Black and Hispanic patients?
joint_long <- df %>%
  filter(race %in% c("Black", "Hispanic", "White")) %>%
  mutate(
    risk_profile = case_when(
      chronic_htn == 0 & bmi < 30  ~ "Low BMI, No HTN",
      chronic_htn == 0 & bmi >= 30 ~ "High BMI, No HTN",
      chronic_htn == 1 & bmi < 30  ~ "Low BMI, HTN",
      chronic_htn == 1 & bmi >= 30 ~ "High BMI, HTN"
    ),
    risk_profile = factor(risk_profile, levels = c("Low BMI, No HTN", "High BMI, No HTN",
                                                    "Low BMI, HTN", "High BMI, HTN"))
  ) %>%
  group_by(risk_profile, race) %>%
  summarise(
    pct_above_60_2021 = round(mean(above_60_2021) * 100, 1),
    n                 = n(),
    .groups           = "drop"
  )

joint_white <- joint_long %>%
  filter(race == "White") %>%
  select(risk_profile, white_pct = pct_above_60_2021)

joint_gaps <- joint_long %>%
  filter(race != "White") %>%
  left_join(joint_white, by = "risk_profile") %>%
  mutate(gap_vs_white = round(white_pct - pct_above_60_2021, 1))

print("Joint HTN and BMI interaction - gap vs White by risk profile (Black and Hispanic):")
print(joint_gaps %>%
  select(risk_profile, race, pct_above_60_2021, white_pct, gap_vs_white, n) %>%
  arrange(risk_profile, race))

## Chart 10c - Threshold crossing by joint BMI and HTN risk profile
p10c <- joint_long %>%
  mutate(race = factor(race, levels = c("Black", "Hispanic", "White"))) %>%
  ggplot(aes(x = risk_profile, y = pct_above_60_2021, fill = race)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Black"    = "#8B0000",
                                "Hispanic" = "#C17B4E",
                                "White"    = "#5C8374")) +
  scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
  labs(title    = "Threshold Crossing by Joint BMI and HTN Risk Profile",
       subtitle = "2021 model -- Black and Hispanic vs White patients",
       x = NULL, y = "Percentage above 60% threshold", fill = "Race") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom",
        panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10))

print(p10c)
ggsave(paste0(PATH, "chart10c_joint_profile.png"), p10c, width = 9, height = 5, dpi = 300)

write_csv(df, paste0(PATH, "synth_final.csv"))

