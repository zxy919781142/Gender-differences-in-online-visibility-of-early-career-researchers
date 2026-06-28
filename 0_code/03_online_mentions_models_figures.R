# -----------------------------------------------------------------------------
# 03_online_mentions_models_figures.R
#
# Purpose:
#   Reproduce the online-visibility analyses for Twitter (currently X) mentions:
#   - Main Figure 2: predicted Twitter mention counts and gender differences
#   - Supplementary Figure S10: model-by-model predicted mention counts
#   - Supplementary Figure S11: country-level predicted mention counts
#   - Supplementary Tables for ZINB model coefficients and marginal effects
#
# Manuscript:
#   Gender differences in online visibility of early-career researchers
#
# Required input file:
#   1_data/01_dataset_processed.csv
#
# Optional input file:
#   1_data/00_field_discipline_OECD_author.csv
#
# Outputs:
#   2_result/fig2_ab_online_mentions.pdf
#   2_result/fig2_c_online_mentions_discipline.pdf
#   2_result/fig_S10_online_mentions_by_model.pdf
#   2_result/fig_S11_online_mentions_by_country.pdf
#   2_result/online_mentions_marginal_effects_*.csv
#   2_result/online_mentions_zinb_models.rds
# -----------------------------------------------------------------------------

# ---- 1. Setup ----------------------------------------------------------------

required_packages <- c(
  "dplyr", "tidyr", "readr", "forcats", "stringr", "purrr",
  "ggplot2", "cowplot", "ggtext", "glmmTMB", "broom.mixed",
  "effectsize"
)

missing_packages <- required_packages[!required_packages %in% rownames(installed.packages())]
if (length(missing_packages) > 0) install.packages(missing_packages)
invisible(lapply(required_packages, library, character.only = TRUE))

data_dir <- "1_data"
result_dir <- "2_result"
figure_dir <- file.path(result_dir)
table_dir <- file.path(result_dir)
model_dir <- file.path(result_dir)

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)

analysis_file <- file.path(
  result_dir,
  "01_dataset_processed.csv"
)

if (!file.exists(analysis_file)) {
  stop(
    "Processed analysis file not found. Please run 00_prepare_replication_data.R first: ",
    analysis_file
  )
}

# ---- 2. Helper functions -----------------------------------------------------

set_reference <- function(x, ref) {
  x <- as.factor(x)
  if (ref %in% levels(x)) stats::relevel(x, ref = ref) else x
}

safe_scale <- function(x) {
  if (is.numeric(x)) as.numeric(scale(x)) else x
}

p_label <- function(p) {
  dplyr::case_when(
    is.na(p) ~ "ns",
    p < .001 ~ "p < .001",
    p < .05 ~ sprintf("p = %.3f", p),
    TRUE ~ "ns"
  )
}

format_ame_label <- function(x) {
  ifelse(is.na(x), NA_character_, sprintf("%.2f", x))
}

make_label_data <- function(data, ame_col = "AME", p_col = "AME_p") {
  data %>%
    mutate(
      p_label = p_label(.data[[p_col]]),
      label_new = if_else(
        gender == "Female",
        paste0(
          "<span style='font-size:18pt'>", format_ame_label(.data[[ame_col]]), "</span><br>",
          "<span style='font-size:14pt'>", p_label, "</span>"
        ),
        NA_character_
      ),
      label_colour = case_when(
        .data[[p_col]] >= .05 ~ "grey50",
        .data[[ame_col]] > 0 ~ "#E69F00",
        .data[[ame_col]] < 0 ~ "#56B4E9",
        TRUE ~ "grey50"
      )
    )
}

prepare_analysis_data <- function(data) {
  data %>%
    mutate(
      gender = set_reference(gender, "male"),
      cohort = set_reference(cohort, "2012"),
      Jr_Quantile = set_reference(Jr_Quantile, "Q4"),
      colla_ctr_Y = set_reference(colla_ctr_Y, "N"),
      colla_aff_Y = set_reference(colla_aff_Y, "N"),
      pub_before_cate = set_reference(pub_before_cate, "0"),
      firstauthor_top_100 = set_reference(firstauthor_top_100, "0"),
      discipline_new = set_reference(discipline_new, "unknown"),
      most_ctr = as.factor(most_ctr),
      gender_label = if_else(as.character(gender) == "female", "Female", "Male")
    )
}

fit_or_load_models <- function(data, model_file) {
  if (file.exists(model_file)) {
    message("Loading cached ZINB models: ", model_file)
    return(readRDS(model_file))
  }

  message("Fitting ZINB models. This can take a long time on the full dataset.")

  forms <- list(
    `Model 0` = len_tweet_ori ~ gender,
    `Model 1` = len_tweet_ori ~ gender * discipline_new,
    `Model 2` = len_tweet_ori ~ gender * (cohort + discipline_new),
    `Model 3` = len_tweet_ori ~ gender * (cohort + discipline_new + Jr_Quantile),
    `Model 4` = len_tweet_ori ~ gender * (cohort + discipline_new + Jr_Quantile + pub_before_cate),
    `Model 5` = len_tweet_ori ~ gender * (cohort + discipline_new + Jr_Quantile + pub_before_cate + author_cnt),
    `Model 6` = len_tweet_ori ~ gender * (cohort + discipline_new + Jr_Quantile + pub_before_cate + author_cnt + colla_ctr_Y + colla_aff_Y),
    `Model 7` = len_tweet_ori ~ gender * (cohort + discipline_new + Jr_Quantile + pub_before_cate + author_cnt + colla_ctr_Y + colla_aff_Y + firstauthor_top_100),
    `Model 8` = len_tweet_ori ~ gender * (cohort + discipline_new + Jr_Quantile + pub_before_cate + author_cnt + colla_ctr_Y + colla_aff_Y + firstauthor_top_100 + max_coa_fncr_5y_log)
  )

  models <- purrr::imap(forms, function(form, name) {
    message("Fitting ", name)
    glmmTMB::glmmTMB(
      formula = form,
      ziformula = form,
      data = data,
      family = glmmTMB::nbinom2,
      REML = TRUE
    )
  })

  message("Fitting Model 9 with country-level random intercepts and random slopes for gender")
  models[["Model 9"]] <- glmmTMB::glmmTMB(
    len_tweet_ori ~ gender * (
      Jr_Quantile + discipline_new + cohort + pub_before_cate + author_cnt +
        colla_ctr_Y + colla_aff_Y + max_coa_fncr_5y_log + firstauthor_top_100
    ) + (1 + gender | most_ctr),
    ziformula = ~ gender * (
      Jr_Quantile + discipline_new + cohort + pub_before_cate + author_cnt +
        colla_ctr_Y + colla_aff_Y + max_coa_fncr_5y_log + firstauthor_top_100
    ) + (1 + gender | most_ctr),
    data = data,
    family = glmmTMB::nbinom2,
    REML = TRUE
  )

  saveRDS(models, model_file)
  models
}

prediction_summary <- function(yhat_f, yhat_m) {
  n1 <- length(yhat_f)
  n2 <- length(yhat_m)
  m1 <- mean(yhat_f, na.rm = TRUE)
  m2 <- mean(yhat_m, na.rm = TRUE)
  s1 <- stats::sd(yhat_f, na.rm = TRUE)
  s2 <- stats::sd(yhat_m, na.rm = TRUE)

  sig_f <- stats::t.test(yhat_f, alternative = "two.sided", conf.level = 0.95)
  sig_m <- stats::t.test(yhat_m, alternative = "two.sided", conf.level = 0.95)
  sig_diff <- stats::t.test(yhat_f, yhat_m, alternative = "two.sided", conf.level = 0.95)

  tibble::tibble(
    gender = c("Female", "Male"),
    predict = c(m1, m2),
    predict_low = c(sig_f$conf.int[1], sig_m$conf.int[1]),
    predict_high = c(sig_f$conf.int[2], sig_m$conf.int[2]),
    AME = m1 - m2,
    AME_p = sig_diff$p.value,
    AME_low = sig_diff$conf.int[1],
    AME_high = sig_diff$conf.int[2],
    female_to_male_ratio = m1 / m2
  )
}

predict_gender <- function(model, data, type = "response", re.form = NA) {
  mf_f <- model.frame(model)
  mf_m <- mf_f
  mf_f$gender <- "female"
  mf_m$gender <- "male"

  yhat_f <- predict(model, newdata = mf_f, type = type, re.form = re.form, allow.new.levels = TRUE)
  yhat_m <- predict(model, newdata = mf_m, type = type, re.form = re.form, allow.new.levels = TRUE)
  prediction_summary(yhat_f, yhat_m)
}

predict_gender_by_group <- function(model, data, group_var, type = "response", re.form = NA) {
  groups <- sort(unique(as.character(data[[group_var]])))
  groups <- groups[!is.na(groups)]

  purrr::map_dfr(groups, function(g) {
    mf_f <- model.frame(model)
    mf_m <- mf_f
    mf_f$gender <- "female"
    mf_m$gender <- "male"
    mf_f[[group_var]] <- g
    mf_m[[group_var]] <- g

    yhat_f <- predict(model, newdata = mf_f, type = type, re.form = re.form, allow.new.levels = TRUE)
    yhat_m <- predict(model, newdata = mf_m, type = type, re.form = re.form, allow.new.levels = TRUE)

    prediction_summary(yhat_f, yhat_m) %>%
      mutate(index = group_var, group = g)
  })
}

extract_model_coefficients <- function(models) {
  purrr::imap_dfr(models, function(model, model_name) {
    broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE, conf.method = "Wald", exponentiate = TRUE) %>%
      mutate(model = model_name)
  })
}

# ---- 3. Load data and fit models --------------------------------------------

analysis_data <- readr::read_csv(analysis_file, show_col_types = FALSE) %>%
  prepare_analysis_data()

models <- fit_or_load_models(
  analysis_data,
  file.path(model_dir, "online_mentions_zinb_models.rds")
)

# ---- 4. Marginal effects and predicted values --------------------------------

# Model comparison for Supplementary Figure S10.
model_predictions <- purrr::imap_dfr(models, function(model, model_name) {
  re_form <- if (model_name == "Model 9") NULL else NA
  predict_gender(model, analysis_data, type = "response", re.form = re_form) %>%
    mutate(model = model_name, model_number = as.integer(stringr::str_extract(model_name, "\\d+")))
}) %>%
  make_label_data()

# Main Figure 2a: Baseline Model 0 and Full Model 8.
fig2a_data <- model_predictions %>%
  filter(model %in% c("Model 0", "Model 8")) %>%
  mutate(
    panel = "Overall",
    model_label = recode(model, `Model 0` = "Baseline ZINB Model", `Model 8` = "Full ZINB Model")
  )

# Main Figure 2b-c: subgroup and discipline results from Full Model 8.
full_model <- models[["Model 8"]]
subgroup_vars <- c("cohort", "pub_before_cate", "Jr_Quantile", "discipline_new")
subgroup_results <- purrr::map_dfr(subgroup_vars, ~ predict_gender_by_group(full_model, analysis_data, .x)) %>%
  mutate(
    index_new = recode(
      index,
      cohort = "Cohort",
      pub_before_cate = "Previous Publications",
      Jr_Quantile = "Journal Rank",
      discipline_new = "Discipline"
    )
  ) %>%
  make_label_data()

fig2b_data <- subgroup_results %>% filter(index_new != "Discipline")
fig2c_data <- subgroup_results %>% filter(index_new == "Discipline", group != "unknown")

# Country-level predictions from Model 9 for Supplementary Figure S11.
top20_countries <- analysis_data %>%
  count(most_ctr, sort = TRUE, name = "n") %>%
  filter(!is.na(most_ctr), most_ctr != "") %>%
  slice_head(n = 20) %>%
  pull(most_ctr) %>%
  as.character()

country_results <- purrr::map_dfr(top20_countries, function(country) {
  mf_f <- model.frame(models[["Model 9"]])
  mf_m <- mf_f
  mf_f$gender <- "female"
  mf_m$gender <- "male"
  mf_f$most_ctr <- country
  mf_m$most_ctr <- country

  yhat_f <- predict(models[["Model 9"]], newdata = mf_f, type = "response", re.form = NULL, allow.new.levels = TRUE)
  yhat_m <- predict(models[["Model 9"]], newdata = mf_m, type = "response", re.form = NULL, allow.new.levels = TRUE)

  prediction_summary(yhat_f, yhat_m) %>% mutate(Country = country)
}) %>%
  make_label_data()

# ---- 5. Export tables --------------------------------------------------------

readr::write_csv(model_predictions, file.path(table_dir, "online_mentions_model_predictions.csv"))
readr::write_csv(subgroup_results, file.path(table_dir, "online_mentions_subgroup_marginal_effects.csv"))
readr::write_csv(country_results, file.path(table_dir, "online_mentions_country_predictions.csv"))

coef_table <- extract_model_coefficients(models)
readr::write_csv(coef_table, file.path(table_dir, "online_mentions_zinb_coefficients.csv"))
readr::write_csv(coef_table %>% filter(component == "zi"), file.path(table_dir, "online_mentions_zero_inflation_OR.csv"))
readr::write_csv(coef_table %>% filter(component == "cond"), file.path(table_dir, "online_mentions_count_component_IRR.csv"))

# ---- 6. Plotting -------------------------------------------------------------

plot_theme <- theme_bw() +
  theme(
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 20),
    axis.title.y = element_text(size = 24),
    text = element_text(size = 24),
    strip.text = element_text(size = 22),
    legend.text = element_text(size = 18),
    legend.position = "bottom"
  )

gender_cols <- c("Female" = "#E69F00", "Male" = "#56B4E9")

p_fig2a <- ggplot(fig2a_data, aes(x = model_label, y = predict, colour = gender, fill = gender)) +
  geom_col(position = position_dodge(width = 0.78), alpha = 0.8) +
  geom_errorbar(aes(ymin = predict_low, ymax = predict_high), width = 0.3, linewidth = 1.2,
                position = position_dodge(width = 0.78)) +
  geom_richtext(aes(y = max(predict_high, na.rm = TRUE) * 1.15, label = label_new),
                colour = fig2a_data$label_colour, fill = NA, label.color = NA,
                fontface = 2, size = 6, lineheight = 0.9, show.legend = FALSE) +
  scale_colour_manual(values = gender_cols, name = NULL) +
  scale_fill_manual(values = gender_cols, name = NULL) +
  labs(x = NULL, y = "Predicted count of Twitter mentions") +
  plot_theme

p_fig2b <- ggplot(fig2b_data, aes(x = fct_inorder(group), y = predict, colour = gender, fill = gender)) +
  facet_wrap(~ factor(index_new, c("Cohort", "Previous Publications", "Journal Rank")), scales = "free_x") +
  geom_col(position = position_dodge(width = 0.78), alpha = 0.8) +
  geom_errorbar(aes(ymin = predict_low, ymax = predict_high), width = 0.3, linewidth = 1.2,
                position = position_dodge(width = 0.78)) +
  geom_richtext(aes(y = max(predict_high, na.rm = TRUE) * 1.15, label = label_new),
                colour = fig2b_data$label_colour, fill = NA, label.color = NA,
                fontface = 2, size = 5, lineheight = 0.9, show.legend = FALSE) +
  scale_colour_manual(values = gender_cols, name = NULL) +
  scale_fill_manual(values = gender_cols, name = NULL) +
  labs(x = NULL, y = NULL) +
  plot_theme

p_fig2c <- ggplot(fig2c_data, aes(x = fct_inorder(group), y = predict, colour = gender, fill = gender)) +
  geom_col(position = position_dodge(width = 0.78), alpha = 0.8) +
  geom_errorbar(aes(ymin = predict_low, ymax = predict_high), width = 0.3, linewidth = 1.2,
                position = position_dodge(width = 0.78)) +
  geom_richtext(aes(y = max(predict_high, na.rm = TRUE) * 1.15, label = label_new),
                colour = fig2c_data$label_colour, fill = NA, label.color = NA,
                fontface = 2, size = 4.5, lineheight = 0.9, show.legend = FALSE) +
  scale_colour_manual(values = gender_cols, name = NULL) +
  scale_fill_manual(values = gender_cols, name = NULL) +
  labs(x = NULL, y = "Predicted count of Twitter mentions") +
  plot_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

combined_fig2_ab <- cowplot::plot_grid(
  p_fig2a, p_fig2b,
  labels = c("(a)", "(b)"),
  ncol = 2,
  rel_widths = c(1, 3),
  label_size = 20,
  label_fontface = "bold"
)

p_s10 <- ggplot(model_predictions, aes(x = factor(model_number), y = predict, colour = gender, fill = gender)) +
  geom_col(position = position_dodge(width = 0.78), alpha = 0.8) +
  geom_errorbar(aes(ymin = predict_low, ymax = predict_high), width = 0.3, linewidth = 1.2,
                position = position_dodge(width = 0.78)) +
  geom_richtext(aes(y = max(predict_high, na.rm = TRUE) * 1.15, label = label_new),
                colour = model_predictions$label_colour, fill = NA, label.color = NA,
                fontface = 2, size = 5, lineheight = 0.9, show.legend = FALSE) +
  scale_colour_manual(values = gender_cols, name = NULL) +
  scale_fill_manual(values = gender_cols, name = NULL) +
  labs(x = "Model", y = "Predicted count of Twitter mentions") +
  plot_theme

p_s11 <- ggplot(country_results, aes(x = factor(Country, levels = top20_countries), y = predict, colour = gender, fill = gender)) +
  geom_col(position = position_dodge(width = 0.78), alpha = 0.8) +
  geom_errorbar(aes(ymin = predict_low, ymax = predict_high), width = 0.3, linewidth = 1.2,
                position = position_dodge(width = 0.78)) +
  geom_richtext(aes(y = max(predict_high, na.rm = TRUE) * 1.15, label = label_new),
                colour = country_results$label_colour, fill = NA, label.color = NA,
                fontface = 2, size = 4, lineheight = 0.9, show.legend = FALSE) +
  scale_colour_manual(values = gender_cols, name = NULL) +
  scale_fill_manual(values = gender_cols, name = NULL) +
  labs(x = "Country", y = "Predicted count of Twitter mentions") +
  plot_theme

# ---- 7. Save figures ---------------------------------------------------------

ggsave(file.path(figure_dir, "fig2_ab_online_mentions.pdf"), combined_fig2_ab, width = 28, height = 10, limitsize = FALSE)
ggsave(file.path(figure_dir, "fig2_c_online_mentions_discipline.pdf"), p_fig2c, width = 28, height = 10, limitsize = FALSE)
ggsave(file.path(figure_dir, "fig_S10_online_mentions_by_model.pdf"), p_s10, width = 18, height = 11, limitsize = FALSE)
ggsave(file.path(figure_dir, "fig_S11_online_mentions_by_country.pdf"), p_s11, width = 21, height = 10, limitsize = FALSE)

message("Online-mentions analysis complete. Outputs saved in 2_result")
