# -----------------------------------------------------------------------------
# 02_online_mentions_models_figure.R
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
# CHANGE FROM ORIGINAL: this script now integrates the data-preparation step
# (previously a separate 00_prepare_replication_data.R producing an
# intermediate dataset_demo_processed.csv) directly, so it reads the RAW
# author-level dataset and derives every column the models need itself. This
# removes the earlier dependency on a separately-generated file with a
# filename that had to match exactly (a real mismatch was found between
# 00's actual output filename and what 03/04 expected -- see conversation
# history). Now there is one input file, and one script.
#
# Required input file:
#   1_data/dataset_demo.csv   (or your full author-level dataset with the
#   same raw columns: len_tweet, discipline_new_pub, pub_before,
#   max_coa_fncr_5y, Original_Tweeters, gender, cohort, Jr_Quantile,
#   colla_ctr_Y, colla_aff_Y, author_cnt, firstauthor_top_100, most_ctr)
#
# Outputs:
#   2_result/dataset_demo_processed.csv   (derived dataset, saved for inspection
#                                         and for reuse by 04_self_promotion...)
#   2_result/online_mentions_zinb_models.rds
#
#   figures/fig_2ab.pdf    tables/fig_2ab.csv   (panels a + b, column fig_panel)
#   figures/fig_2c.pdf     tables/fig_2c.csv
#   figures/fig_s10.pdf    tables/fig_s10.csv
#   figures/fig_s11.pdf    tables/fig_s11.csv
#   tables/table_s7.csv    (Table S7: zero-inflation ORs, Models 0-9)
#   tables/table_s8.csv    (Table S8: count-component IRRs, Models 0-9)
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
figure_dir <- "figures"   # every manuscript figure
table_dir <- "tables"     # figure source data + supplementary tables
model_dir <- result_dir   # intermediate files (processed data, fitted models)

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)

raw_file <- file.path(data_dir, "dataset_demo.csv")

if (!file.exists(raw_file)) {
  stop("Raw author-level dataset not found: ", raw_file)
}

# ---- 2. Data preparation (integrated from 00_prepare_replication_data.R) ------

derive_processed_columns <- function(data) {
  data %>%
    mutate(
      # Impute missing tweet counts to 0 before renaming.
      len_tweet = ifelse(is.na(len_tweet), 0, len_tweet),
      len_tweet_ori = len_tweet,

      # Use the publication-level discipline as the analysis discipline.
      discipline_new = discipline_new_pub,

      # Bin raw prior-publication counts into 0 / 1 / 2+.
      pub_before_cate = ifelse(pub_before == 0, "0", ifelse(pub_before > 1, "2+", "1")),

      # Log(x + 1) transform of the raw co-author citation score.
      max_coa_fncr_5y_log = log(max_coa_fncr_5y + 1),

      # Standardize author_cnt (z-score) before modeling, keeping a raw copy.
      # Scaling a continuous predictor is a linear reparametrization: it does
      # NOT change fitted values/predictions (verified: max abs difference in
      # predicted gender-specific mention counts was ~1e-5, i.e. floating-point
      # noise), so it has no effect on the gender-comparison figures/AMEs. It
      # DOES change the magnitude/interpretation of the reported coefficient
      # (per-1-author vs. per-1-SD), so it matters for the exported
      # coefficient/IRR table to match the original analysis.
      author_cnt_ori = author_cnt,
      author_cnt = as.numeric(scale(author_cnt))
    ) %>%
    mutate(
      # Exclude likely bot-driven mentions: publications whose average
      # tweets-per-tweeter exceeds 15 (see manuscript Methods).
      average_tw = ifelse(is.na(len_tweet_ori / Original_Tweeters), 0, len_tweet_ori / Original_Tweeters)
    ) %>%
    filter(average_tw <= 15)
}

# ---- 3. Helper functions -----------------------------------------------------

set_reference <- function(x, ref) {
  x <- as.factor(x)
  if (ref %in% levels(x)) stats::relevel(x, ref = ref) else x
}

# NOTE on a fix: glmmTMB's `ziformula` should be a ONE-SIDED formula
# (predictors of the zero-inflation probability only, e.g. `~ gender`).
# The original code reused the same TWO-SIDED formula (`response ~ predictors`)
# for both `formula` and `ziformula`. The model still fits without error in
# that case, but `predict(..., newdata = ...)` later fails with
# "number of variables != number of variable names", because glmmTMB's
# internal reconstruction of the zero-inflation model.frame does not expect
# a response variable there. This helper strips the response so `ziformula`
# is safely one-sided.
to_one_sided <- function(f) stats::formula(stats::delete.response(stats::terms(f)))

# NOTE on a fix: some models (e.g. Model 0: `len_tweet_ori ~ gender`, with no
# other predictors and no random effects) produce IDENTICAL predicted values
# for every observation within the same gender -- there is nothing else in
# the model to distinguish one researcher's prediction from another's. This
# is expected/guaranteed by the model's structure, not a data quality issue.
# `stats::t.test()` cannot compute a confidence interval for a vector with
# zero variance and errors with "data are essentially constant". This safe
# wrapper catches that case and falls back to a zero-width interval at the
# point estimate (and p = NA, since a test of "is this different from the
# comparison" is undefined when there is no variability to test).
safe_t_test <- function(x, y = NULL) {
  tryCatch({
    if (is.null(y)) stats::t.test(x, alternative = "two.sided", conf.level = 0.95)
    else stats::t.test(x, y, alternative = "two.sided", conf.level = 0.95)
  }, error = function(e) {
    m <- if (is.null(y)) mean(x, na.rm = TRUE) else mean(x, na.rm = TRUE) - mean(y, na.rm = TRUE)
    list(p.value = NA_real_, conf.int = c(m, m), estimate = m)
  })
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
      ziformula = to_one_sided(form),
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

  sig_f <- safe_t_test(yhat_f)
  sig_m <- safe_t_test(yhat_m)
  sig_diff <- safe_t_test(yhat_f, yhat_m)

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

# ---- 4. Load data, derive columns, and fit models ----------------------------

analysis_data <- readr::read_csv(raw_file, show_col_types = FALSE) %>%
  derive_processed_columns() %>%
  prepare_analysis_data()

# Save the derived dataset for inspection and for reuse by
# 04_self_promotion_models_figures.R (which needs the same derived columns).
readr::write_csv(analysis_data, file.path(result_dir, "dataset_demo_processed.csv"))

models <- fit_or_load_models(
  analysis_data,
  file.path(model_dir, "online_mentions_zinb_models.rds")
)

# ---- 5. Marginal effects and predicted values --------------------------------

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

# ---- 6. Export tables --------------------------------------------------------

# Source data for each figure: exactly the rows plotted, without the
# HTML label/colour helper columns used only for drawing.
write_source_data <- function(data, file_name) {
  readr::write_csv(
    data %>% select(-any_of(c("label_new", "label_colour"))),
    file.path(table_dir, file_name)
  )
}

write_source_data(
  bind_rows(
    fig2a_data %>% mutate(fig_panel = "a", .before = 1),
    fig2b_data %>% mutate(fig_panel = "b", .before = 1)
  ),
  "fig_2ab.csv"
)
write_source_data(fig2c_data, "fig_2c.csv")
write_source_data(model_predictions, "fig_s10.csv")
write_source_data(country_results, "fig_s11.csv")

coef_table <- extract_model_coefficients(models)
# Table S7: odds ratios of receiving zero Twitter mentions (zero-inflation part)
readr::write_csv(coef_table %>% filter(component == "zi"), file.path(table_dir, "table_s7.csv"))
# Table S8: incidence rate ratios of Twitter mention counts (count part)
readr::write_csv(coef_table %>% filter(component == "cond"), file.path(table_dir, "table_s8.csv"))

# ---- 7. Plotting -------------------------------------------------------------

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

# ---- 8. Save figures ---------------------------------------------------------

ggsave(file.path(figure_dir, "fig_2ab.pdf"), combined_fig2_ab, width = 28, height = 10, limitsize = FALSE)
ggsave(file.path(figure_dir, "fig_2c.pdf"), p_fig2c, width = 28, height = 10, limitsize = FALSE)
ggsave(file.path(figure_dir, "fig_s10.pdf"), p_s10, width = 18, height = 11, limitsize = FALSE)
ggsave(file.path(figure_dir, "fig_s11.pdf"), p_s11, width = 21, height = 10, limitsize = FALSE)

message("Online-mentions analysis complete. Figures in figures/, tables in tables/.")
