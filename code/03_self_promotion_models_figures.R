# -----------------------------------------------------------------------------
# 03_self_promotion_models_figures.R
#
# Purpose:
#   Reproduce the self-promotion analyses:
#   - Main Figure 3: predicted probabilities of self-promotion and gender gaps
#   - Supplementary Figure S12: model-by-model predicted probabilities
#   - Supplementary Figure S13: country-level predicted probabilities
#   - Supplementary Figures S14-S15: co-author / official-account promotion
#   - Supplementary Tables for logistic model coefficients and marginal effects
#
# Manuscript:
#   Gender differences in online visibility of early-career researchers
#
# CHANGE FROM ORIGINAL: this script now reads the already-processed dataset
# produced by 02_online_mentions_models_figure.R directly, rather than
# re-deriving it from the raw file. 03 and 04 need the exact same derived
# columns (len_tweet_ori, discipline_new, pub_before_cate, max_coa_fncr_5y_log,
# author_cnt scaling, etc.), so running that derivation twice was pure
# duplication; now it happens once, in 02, and both scripts (and
# 04a_run_matching.R) share the one canonical output file.
#
# Required input file:
#   2_result/dataset_demo_processed.csv
#   (produced by 02_online_mentions_models_figure.R -- run that script first)
#
# Outputs:
#   2_result/self_promotion_logistic_models.rds, coauthor_soc_promotion_models.rds
#
#   figures/fig_3ab.pdf    tables/fig_3ab.csv   (panels a + b, column fig_panel)
#   figures/fig_3c.pdf     tables/fig_3c.csv
#   figures/fig_s12.pdf    tables/fig_s12.csv
#   figures/fig_s13.pdf    tables/fig_s13.csv
#   figures/fig_s14.pdf    tables/fig_s14.csv
#   figures/fig_s15.pdf    tables/fig_s15.csv
#   tables/table_s10.csv   (Table S10: self-promotion ORs, Models 0-10)
#   tables/coauthor_soc_promotion_coefficients_OR.csv
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

result_dir <- "2_result"
figure_dir <- "figures"   # every manuscript figure
table_dir <- "tables"     # figure source data + supplementary tables
model_dir <- result_dir   # intermediate files (fitted models)

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)

processed_file <- file.path(
  result_dir,
  "dataset_demo_processed.csv"
)

if (!file.exists(processed_file)) {
  stop(
    "Processed dataset not found. Run 03_online_mentions_models_figures.R first: ",
    processed_file
  )
}

# ---- 2. Helper functions -----------------------------------------------------

set_reference <- function(x, ref) {
  x <- as.factor(x)
  if (ref %in% levels(x)) stats::relevel(x, ref = ref) else x
}

# NOTE on a fix: some models (e.g. Model 0: `self_pro ~ gender`, with no
# other predictors and no random effects) produce IDENTICAL predicted values
# for every observation within the same gender. `stats::t.test()` cannot
# compute a CI for a zero-variance vector and errors with "data are
# essentially constant". This safe wrapper catches that case (see
# 03_online_mentions_models_figures.R for the full explanation/verification).
safe_t_test <- function(x, y = NULL) {
  tryCatch({
    if (is.null(y)) stats::t.test(x, alternative = "two.sided", conf.level = 0.95)
    else stats::t.test(x, y, alternative = "two.sided", conf.level = 0.95)
  }, error = function(e) {
    m <- if (is.null(y)) mean(x, na.rm = TRUE) else mean(x, na.rm = TRUE) - mean(y, na.rm = TRUE)
    list(p.value = NA_real_, conf.int = c(m, m))
  })
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
      others_first = set_reference(others_first, "0"),
      discipline_new = set_reference(discipline_new, "unknown"),
      # Needed for the co-author/institutional-promotion models (Figs. S14-S15).
      coauthor_top_100 = set_reference(coauthor_top_100, "0"),
      most_ctr = as.factor(most_ctr),
      gender_label = if_else(as.character(gender) == "female", "Female", "Male")
    )
}

fit_or_load_models <- function(data, model_file) {
  if (file.exists(model_file)) {
    message("Loading cached logistic models: ", model_file)
    return(readRDS(model_file))
  }

  message("Fitting self-promotion logistic models. This can take a long time on the full dataset.")

  forms <- list(
    `Model 0` = self_pro ~ gender,
    `Model 1` = self_pro ~ gender * discipline_new,
    `Model 2` = self_pro ~ gender * (cohort + discipline_new),
    `Model 3` = self_pro ~ gender * (cohort + discipline_new + Jr_Quantile),
    `Model 4` = self_pro ~ gender * (cohort + discipline_new + Jr_Quantile + pub_before_cate),
    `Model 5` = self_pro ~ gender * (cohort + discipline_new + Jr_Quantile + pub_before_cate + author_cnt),
    `Model 6` = self_pro ~ gender * (cohort + discipline_new + Jr_Quantile + pub_before_cate + author_cnt + colla_ctr_Y + colla_aff_Y),
    `Model 7` = self_pro ~ gender * (cohort + discipline_new + Jr_Quantile + pub_before_cate + author_cnt + colla_ctr_Y + colla_aff_Y + max_coa_fncr_5y_log),
    `Model 8` = self_pro ~ gender * (cohort + discipline_new + Jr_Quantile + pub_before_cate + author_cnt + colla_ctr_Y + colla_aff_Y + max_coa_fncr_5y_log + others_first + tw_others_num + firstauthor_top_100),
    `Model 9` = self_pro ~ gender * (cohort + discipline_new + Jr_Quantile + pub_before_cate + author_cnt + colla_ctr_Y + colla_aff_Y + others_first + tw_others_num + firstauthor_top_100 + max_coa_fncr_5y_log)
  )

  models <- purrr::imap(forms, function(form, name) {
    message("Fitting ", name)
    glmmTMB::glmmTMB(
      formula = form,
      data = data,
      family = binomial
    )
  })

  message("Fitting Model 10 with country-level random intercepts and random slopes for gender")
  models[["Model 10"]] <- glmmTMB::glmmTMB(
    self_pro ~ gender * (
      others_first + tw_others_num + cohort + Jr_Quantile + discipline_new +
        pub_before_cate + author_cnt + colla_ctr_Y + colla_aff_Y +
        max_coa_fncr_5y_log + firstauthor_top_100
    ) + (1 + gender | most_ctr),
    data = data,
    family = binomial
  )

  saveRDS(models, model_file)
  models
}

fit_or_load_coauthor_soc_models <- function(data, model_file) {
  # Fits the two additional models needed for Figs. S14 (promotion by
  # co-authors) and S15 (promotion by official/institutional Twitter
  # accounts). These are separate from the Models 0-10 self-promotion
  # sequence above -- different outcome variables, different (unlogged)
  # co-author citation covariate, matching the exact specification provided.
  if (file.exists(model_file)) {
    message("Loading cached co-author/institutional promotion models: ", model_file)
    return(readRDS(model_file))
  }

  message("Fitting co-author and institutional promotion models.")

  message("Fitting co-author promotion model (Fig. S14)")
  model_coauthor <- glmmTMB::glmmTMB(
    coauthor_pro ~ gender * (
      tw_others_num + cohort + Jr_Quantile + discipline_new + pub_before_cate +
        author_cnt + colla_ctr_Y + colla_aff_Y + max_coa_fncr_5y + firstauthor_top_100
    ) + (1 + gender | most_ctr),
    data = data[data$with_coauthor == 1, ],
    family = binomial
  )

  message("Fitting institutional/official-account promotion model (Fig. S15)")
  model_soc <- glmmTMB::glmmTMB(
    soc_or_edu_pro ~ gender * (
      tw_others_num + cohort + Jr_Quantile + discipline_new + pub_before_cate +
        author_cnt + colla_ctr_Y + colla_aff_Y + max_coa_fncr_5y +
        firstauthor_top_100 + coauthor_top_100
    ) + (1 + gender | most_ctr),
    data = data,
    family = binomial
  )

  models <- list(coauthor = model_coauthor, soc = model_soc)
  saveRDS(models, model_file)
  models
}

prediction_summary <- function(yhat_f, yhat_m, scale_to_percentage_points = TRUE) {
  sig_f <- safe_t_test(yhat_f)
  sig_m <- safe_t_test(yhat_m)
  sig_diff <- safe_t_test(yhat_f, yhat_m)

  m1 <- mean(yhat_f, na.rm = TRUE)
  m2 <- mean(yhat_m, na.rm = TRUE)

  out <- tibble::tibble(
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

  if (scale_to_percentage_points) {
    out <- out %>%
      mutate(across(c(predict, predict_low, predict_high, AME, AME_low, AME_high), ~ .x * 100))
  }

  out
}

predict_gender <- function(model, data, type = "response", re.form = NA) {
  mf_f <- model.frame(model)
  mf_m <- mf_f
  mf_f$gender <- "female"
  mf_m$gender <- "male"

  yhat_f <- predict(model, newdata = mf_f, type = type, re.form = re.form, allow.new.levels = TRUE)
  yhat_m <- predict(model, newdata = mf_m, type = type, re.form = re.form, allow.new.levels = TRUE)
  prediction_summary(yhat_f, yhat_m, scale_to_percentage_points = TRUE)
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

    prediction_summary(yhat_f, yhat_m, scale_to_percentage_points = TRUE) %>%
      mutate(index = group_var, group = g)
  })
}

extract_model_coefficients <- function(models) {
  purrr::imap_dfr(models, function(model, model_name) {
    broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE, conf.method = "Wald", exponentiate = TRUE) %>%
      mutate(model = model_name)
  })
}

# ---- 3. Load already-processed data and fit models ---------------------------

analysis_data <- readr::read_csv(processed_file, show_col_types = FALSE) %>%
  prepare_analysis_data()

models <- fit_or_load_models(
  analysis_data,
  file.path(model_dir, "self_promotion_logistic_models.rds")
)

coauthor_soc_models <- fit_or_load_coauthor_soc_models(
  analysis_data,
  file.path(model_dir, "coauthor_soc_promotion_models.rds")
)

# ---- 4. Marginal effects and predicted probabilities -------------------------

model_predictions <- purrr::imap_dfr(models, function(model, model_name) {
  re_form <- if (model_name == "Model 10") NULL else NA
  predict_gender(model, analysis_data, type = "response", re.form = re_form) %>%
    mutate(model = model_name, model_number = as.integer(stringr::str_extract(model_name, "\\d+")))
}) %>%
  make_label_data()

fig3a_data <- model_predictions %>%
  filter(model %in% c("Model 0", "Model 9")) %>%
  mutate(
    panel = "Overall",
    model_label = recode(model, `Model 0` = "Baseline Logistic Model", `Model 9` = "Full Logistic Model")
  )

full_model <- models[["Model 9"]]
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

fig3b_data <- subgroup_results %>% filter(index_new != "Discipline")
fig3c_data <- subgroup_results %>% filter(index_new == "Discipline", group != "unknown")

top20_countries <- analysis_data %>%
  count(most_ctr, sort = TRUE, name = "n") %>%
  filter(!is.na(most_ctr), most_ctr != "") %>%
  slice_head(n = 20) %>%
  pull(most_ctr) %>%
  as.character()

country_results <- purrr::map_dfr(top20_countries, function(country) {
  mf_f <- model.frame(models[["Model 10"]])
  mf_m <- mf_f
  mf_f$gender <- "female"
  mf_m$gender <- "male"
  mf_f$most_ctr <- country
  mf_m$most_ctr <- country

  yhat_f <- predict(models[["Model 10"]], newdata = mf_f, type = "response", re.form = NULL, allow.new.levels = TRUE)
  yhat_m <- predict(models[["Model 10"]], newdata = mf_m, type = "response", re.form = NULL, allow.new.levels = TRUE)

  prediction_summary(yhat_f, yhat_m, scale_to_percentage_points = TRUE) %>% mutate(Country = country)
}) %>%
  make_label_data()

# ---- 5. Export tables --------------------------------------------------------

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
    fig3a_data %>% mutate(fig_panel = "a", .before = 1),
    fig3b_data %>% mutate(fig_panel = "b", .before = 1)
  ),
  "fig_3ab.csv"
)
write_source_data(fig3c_data, "fig_3c.csv")
write_source_data(model_predictions, "fig_s12.csv")
write_source_data(country_results, "fig_s13.csv")

coef_table <- extract_model_coefficients(models)
# Table S10: odds ratios of self-promotion, Models 0-10
readr::write_csv(coef_table, file.path(table_dir, "table_s10.csv"))

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

p_fig3a <- ggplot(fig3a_data, aes(x = model_label, y = predict, colour = gender)) +
  geom_point(size = 5, position = position_dodge(width = 0.78)) +
  geom_errorbar(aes(ymin = predict_low, ymax = predict_high), width = 0.3, linewidth = 1.2,
                position = position_dodge(width = 0.78)) +
  geom_richtext(aes(y = max(predict_high, na.rm = TRUE) * 1.15, label = label_new),
                colour = fig3a_data$label_colour, fill = NA, label.color = NA,
                fontface = 2, size = 6, lineheight = 0.9, show.legend = FALSE) +
  scale_colour_manual(values = gender_cols, name = NULL) +
  labs(x = NULL, y = "Predicted probability of self-promotion (p.p.)") +
  plot_theme

p_fig3b <- ggplot(fig3b_data, aes(x = fct_inorder(group), y = predict, colour = gender)) +
  facet_wrap(~ factor(index_new, c("Cohort", "Previous Publications", "Journal Rank")), scales = "free_x") +
  geom_point(size = 5, position = position_dodge(width = 0.78)) +
  geom_errorbar(aes(ymin = predict_low, ymax = predict_high), width = 0.3, linewidth = 1.2,
                position = position_dodge(width = 0.78)) +
  geom_richtext(aes(y = max(predict_high, na.rm = TRUE) * 1.15, label = label_new),
                colour = fig3b_data$label_colour, fill = NA, label.color = NA,
                fontface = 2, size = 5, lineheight = 0.9, show.legend = FALSE) +
  scale_colour_manual(values = gender_cols, name = NULL) +
  labs(x = NULL, y = NULL) +
  plot_theme

p_fig3c <- ggplot(fig3c_data, aes(x = fct_inorder(group), y = predict, colour = gender)) +
  geom_point(size = 5, position = position_dodge(width = 0.78)) +
  geom_errorbar(aes(ymin = predict_low, ymax = predict_high), width = 0.3, linewidth = 1.2,
                position = position_dodge(width = 0.78)) +
  geom_richtext(aes(y = max(predict_high, na.rm = TRUE) * 1.15, label = label_new),
                colour = fig3c_data$label_colour, fill = NA, label.color = NA,
                fontface = 2, size = 4.5, lineheight = 0.9, show.legend = FALSE) +
  scale_colour_manual(values = gender_cols, name = NULL) +
  labs(x = NULL, y = "Predicted probability of self-promotion (p.p.)") +
  plot_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

combined_fig3_ab <- cowplot::plot_grid(
  p_fig3a, p_fig3b,
  labels = c("(a)", "(b)"),
  ncol = 2,
  rel_widths = c(1, 3),
  label_size = 20,
  label_fontface = "bold"
)

p_s12 <- ggplot(model_predictions, aes(x = factor(model_number), y = predict, colour = gender)) +
  geom_point(size = 5, position = position_dodge(width = 0.78)) +
  geom_errorbar(aes(ymin = predict_low, ymax = predict_high), width = 0.3, linewidth = 1.2,
                position = position_dodge(width = 0.78)) +
  geom_richtext(aes(y = max(predict_high, na.rm = TRUE) * 1.15, label = label_new),
                colour = model_predictions$label_colour, fill = NA, label.color = NA,
                fontface = 2, size = 5, lineheight = 0.9, show.legend = FALSE) +
  scale_colour_manual(values = gender_cols, name = NULL) +
  labs(x = "Model", y = "Predicted probability of self-promotion (p.p.)") +
  plot_theme

p_s13 <- ggplot(country_results, aes(x = factor(Country, levels = top20_countries), y = predict, colour = gender)) +
  geom_point(size = 5, position = position_dodge(width = 0.78)) +
  geom_errorbar(aes(ymin = predict_low, ymax = predict_high), width = 0.3, linewidth = 1.2,
                position = position_dodge(width = 0.78)) +
  geom_richtext(aes(y = max(predict_high, na.rm = TRUE) * 1.15, label = label_new),
                colour = country_results$label_colour, fill = NA, label.color = NA,
                fontface = 2, size = 4, lineheight = 0.9, show.legend = FALSE) +
  scale_colour_manual(values = gender_cols, name = NULL) +
  labs(x = "Country", y = "Predicted probability of self-promotion (p.p.)") +
  plot_theme

# ---- 7. Save figures ---------------------------------------------------------

ggsave(file.path(figure_dir, "fig_3ab.pdf"), combined_fig3_ab, width = 28, height = 10, limitsize = FALSE)
ggsave(file.path(figure_dir, "fig_3c.pdf"), p_fig3c, width = 28, height = 10, limitsize = FALSE)
ggsave(file.path(figure_dir, "fig_s12.pdf"), p_s12, width = 21, height = 10, limitsize = FALSE)
ggsave(file.path(figure_dir, "fig_s13.pdf"), p_s13, width = 21, height = 10, limitsize = FALSE)

message("Self-promotion analysis complete. Figures in figures/, tables in tables/.")

# ---- 8. Figs. S14-S15: promotion by co-authors and by official accounts -----

subgroup_vars <- c("cohort", "pub_before_cate", "Jr_Quantile", "discipline_new")

compute_promotion_results <- function(model, data) {
  purrr::map_dfr(subgroup_vars, ~ predict_gender_by_group(model, data, .x)) %>%
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
}

coauthor_results <- compute_promotion_results(
  coauthor_soc_models[["coauthor"]],
  analysis_data[analysis_data$with_coauthor == 1, ]
)
soc_results <- compute_promotion_results(coauthor_soc_models[["soc"]], analysis_data)

# Same rows as plotted in Figs. S14/S15 (the "unknown" discipline is not drawn).
write_source_data(
  coauthor_results %>% filter(!(index_new == "Discipline" & group == "unknown")),
  "fig_s14.csv"
)
write_source_data(
  soc_results %>% filter(!(index_new == "Discipline" & group == "unknown")),
  "fig_s15.csv"
)

readr::write_csv(
  extract_model_coefficients(coauthor_soc_models),
  file.path(table_dir, "coauthor_soc_promotion_coefficients_OR.csv")
)

# Shared plotting function: top row = Cohort / Previous Publications /
# Journal Rank (three side-by-side facets); bottom row = Discipline (its own
# wider facet), matching the original Figs. S14/S15 layout.
plot_promotion_figure <- function(results_df, y_label) {
  top_data <- results_df %>% filter(index_new != "Discipline")
  bottom_data <- results_df %>% filter(index_new == "Discipline", group != "unknown")

  p_top <- ggplot(top_data, aes(x = fct_inorder(group), y = predict, colour = gender)) +
    facet_wrap(~ factor(index_new, c("Cohort", "Previous Publications", "Journal Rank")), scales = "free_x") +
    geom_point(size = 5, position = position_dodge(width = 0.78)) +
    geom_errorbar(aes(ymin = predict_low, ymax = predict_high), width = 0.3, linewidth = 1.2,
                  position = position_dodge(width = 0.78)) +
    geom_richtext(aes(y = max(predict_high, na.rm = TRUE) * 1.15, label = label_new),
                  colour = top_data$label_colour, fill = NA, label.color = NA,
                  fontface = 2, size = 5, lineheight = 0.9, show.legend = FALSE) +
    scale_colour_manual(values = gender_cols, name = NULL) +
    labs(x = NULL, y = y_label) +
    plot_theme +
    theme(legend.position = "none")

  p_bottom <- ggplot(bottom_data, aes(x = fct_inorder(group), y = predict, colour = gender)) +
    geom_point(size = 5, position = position_dodge(width = 0.78)) +
    geom_errorbar(aes(ymin = predict_low, ymax = predict_high), width = 0.3, linewidth = 1.2,
                  position = position_dodge(width = 0.78)) +
    geom_richtext(aes(y = max(predict_high, na.rm = TRUE) * 1.15, label = label_new),
                  colour = bottom_data$label_colour, fill = NA, label.color = NA,
                  fontface = 2, size = 4.5, lineheight = 0.9, show.legend = FALSE) +
    scale_colour_manual(values = gender_cols, name = NULL) +
    labs(x = NULL, y = NULL) +
    plot_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")

  cowplot::plot_grid(p_top, p_bottom, ncol = 1, rel_heights = c(1, 1.1))
}

p_s14 <- plot_promotion_figure(coauthor_results, "Predicted probabilities of promotion by co-authors (p.p.)")
p_s15 <- plot_promotion_figure(soc_results, "Predicted probabilities of promotion by official Twitter accounts (p.p.)")

ggsave(file.path(figure_dir, "fig_s14.pdf"), p_s14, width = 16, height = 14, limitsize = FALSE)
ggsave(file.path(figure_dir, "fig_s15.pdf"), p_s15, width = 16, height = 14, limitsize = FALSE)

message("Figs. S14-S15 (co-author / official-account promotion) complete.")
