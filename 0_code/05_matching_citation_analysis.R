################################################################################
# Matching and citation-impact analyses
#
# Manuscript: Gender differences in online visibility of early-career researchers
#
# This script reproduces the matching-based citation-impact analyses:
#   - Main text Fig. 4
#   - Supplementary Figs. S17-S18: general Twitter mentions vs. no mentions
#   - Supplementary Figs. S21-S22: self-promotion vs. others' promotion only
#
# Expected project structure:
#   Data/
#   Result/
#     Figures/
#     Tables/
#     Models/
#
# Main input files:

#   1_data/1_author_12_16_psm_TW_No.csv
#   1_data/1_author_12_16_psm_self_other.csv
#   1_data/2_match_psm_TW_No.csv
#   1_data/2_match_psm_self_other.csv
#
# Main output data:


#
# Outputs:
#   2_result/fig4_matching_citation_impact.pdf
#   2_result/fig_S17_twitter_mentions_subgroups.pdf
#   2_result/fig_S18_twitter_mentions_discipline.pdf
#   2_result/fig_S21_self_promotion_subgroups.pdf
#   2_result/fig_S22_self_promotion_discipline.pdf
#   2_result/*.csv
#   2_result/*.rds
################################################################################

# ---- 0. Setup -----------------------------------------------------------------

required_packages <- c(
  "tidyverse", "MatchIt", "broom", "sandwich", "lmtest", "effectsize",
  "ggplot2", "ggtext", "forcats", "patchwork", "scales", "readr"
)

missing_packages <- required_packages[!required_packages %in% rownames(installed.packages())]
if (length(missing_packages) > 0) {
  stop(
    "Please install missing packages before running this script: ",
    paste(missing_packages, collapse = ", ")
  )
}

invisible(lapply(required_packages, library, character.only = TRUE))

# Optional package for hatched bars in Fig. 4.
has_ggpattern <- requireNamespace("ggpattern", quietly = TRUE)
if (!has_ggpattern) {
  message("Package 'ggpattern' is not installed. Fig. 4 will be drawn without hatching.")
}

set.seed(2026)

DATA_DIR <- "Data"
FIG_DIR <- file.path("2_result")
TAB_DIR <- file.path("2_result")
MOD_DIR <- file.path("2_result")

dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TAB_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(MOD_DIR, recursive = TRUE, showWarnings = FALSE)

# Set to TRUE only if you want to reproduce the matching step from the unmatched
# matching-input files. If FALSE, the script uses the already matched datasets.
RUN_MATCHING <- FALSE

# ---- 1. Helper functions ------------------------------------------------------

read_required_csv <- function(path) {
  if (!file.exists(path)) {
    stop("Required input file not found: ", path)
  }
  readr::read_csv(path, show_col_types = FALSE)
}

relevel_if_present <- function(x, ref) {
  x <- factor(x)
  if (ref %in% levels(x)) stats::relevel(x, ref = ref) else x
}

prepare_matching_data <- function(df) {
  df %>%
    mutate(
      gender = factor(gender, levels = c("female", "male")),
      cohort = relevel_if_present(cohort, "2012"),
      Jr_Quantile = relevel_if_present(Jr_Quantile, "Q4"),
      colla_ctr_Y = relevel_if_present(colla_ctr_Y, "N"),
      firstauthor_top_100 = relevel_if_present(as.character(firstauthor_top_100), "0"),
      pub_before_cate = case_when(
        !is.na(pub_before_cate) ~ as.character(pub_before_cate),
        !is.na(pub_before) & pub_before == 0 ~ "0",
        !is.na(pub_before) & pub_before == 1 ~ "1",
        !is.na(pub_before) & pub_before > 1 ~ "2+",
        TRUE ~ NA_character_
      ),
      pub_before_cate = factor(pub_before_cate, levels = c("0", "1", "2+")),
      author_cnt = as.numeric(author_cnt),
      max_coa_fncr_5y_log = ifelse(
        "max_coa_fncr_5y_log" %in% names(.),
        max_coa_fncr_5y_log,
        log(max_coa_fncr_5y + 1)
      )
    )
}

format_p <- function(p) {
  case_when(
    is.na(p) ~ "",
    p < 0.001 ~ "p < .001",
    p < 0.05 ~ paste0("p = ", sprintf("%.3f", p)),
    TRUE ~ "ns"
  )
}

safe_t_test_p <- function(x, y) {
  x <- x[is.finite(x)]
  y <- y[is.finite(y)]
  if (length(x) < 2 || length(y) < 2 || sd(x) == 0 || sd(y) == 0) return(NA_real_)
  stats::t.test(x, y, alternative = "two.sided")$p.value
}

pooled_ci_diff <- function(x, y, conf = 0.95) {
  x <- x[is.finite(x)]
  y <- y[is.finite(y)]
  n1 <- length(x); n2 <- length(y)
  diff <- mean(x) - mean(y)
  if (n1 < 2 || n2 < 2) return(c(low = NA_real_, high = NA_real_))
  s1 <- stats::sd(x); s2 <- stats::sd(y)
  sp <- ((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2)
  margin <- stats::qt(1 - (1 - conf) / 2, df = n1 + n2 - 2) * sqrt(sp / n1 + sp / n2)
  c(low = diff - margin, high = diff + margin)
}

compute_ame <- function(model, data, treatment_value, control_value, subgroup_vars = NULL) {
  model_data <- model.frame(model)
  data_for_groups <- data[as.integer(rownames(model_data)), , drop = FALSE]
  pred_treat <- model_data
  pred_control <- model_data
  pred_treat$Type <- treatment_value
  pred_control$Type <- control_value

  pred_df <- data_for_groups %>%
    mutate(
      y_treat = as.numeric(stats::predict(model, newdata = pred_treat, type = "response")),
      y_control = as.numeric(stats::predict(model, newdata = pred_control, type = "response"))
    )

  if (is.null(subgroup_vars)) {
    subgroup_vars <- character(0)
  }

  grouping_vars <- c(subgroup_vars, "gender")

  pred_df %>%
    group_by(across(all_of(grouping_vars))) %>%
    summarise(
      n = n(),
      baseline_mean = mean(y_control, na.rm = TRUE),
      treated_mean = mean(y_treat, na.rm = TRUE),
      AME = treated_mean - baseline_mean,
      AME_low = pooled_ci_diff(y_treat, y_control)["low"],
      AME_high = pooled_ci_diff(y_treat, y_control)["high"],
      p_value = safe_t_test_p(y_treat, y_control),
      ratio = treated_mean / baseline_mean,
      cohens_d = tryCatch(effectsize::cohens_d(y_treat, y_control)$Cohens_d, error = function(e) NA_real_),
      cliffs_delta = tryCatch(effectsize::cliffs_delta(y_treat, y_control)$r_rank_biserial, error = function(e) NA_real_),
      .groups = "drop"
    )
}

compute_subgroup_ames <- function(model, data, treatment_value, control_value) {
  subgroup_vars <- c("cohort", "pub_before_cate", "Jr_Quantile", "discipline_new")

  purrr::map_dfr(subgroup_vars, function(v) {
    compute_ame(model, data, treatment_value, control_value, subgroup_vars = v) %>%
      rename(group = all_of(v)) %>%
      mutate(index = v, .before = 1)
  }) %>%
    mutate(
      index_new = recode(index,
        "cohort" = "Cohort",
        "pub_before_cate" = "Previous Publications",
        "Jr_Quantile" = "Journal Rank",
        "discipline_new" = "Discipline"
      ),
      group = as.character(group),
      gender_label = recode(as.character(gender), "female" = "Female", "male" = "Male"),
      p_label = format_p(p_value),
      label_new = paste0(
        "<span style='font-size:14pt'>", sprintf("%.2f", AME), "</span><br>",
        "<span style='font-size:10pt'>", p_label, "</span>"
      ),
      label_colour = case_when(
        p_value >= 0.05 ~ "grey50",
        AME > 0 ~ "#E69F00",
        AME < 0 ~ "#56B4E9",
        TRUE ~ "grey50"
      )
    ) %>%
    arrange(
      factor(index, levels = c("cohort", "pub_before_cate", "Jr_Quantile", "discipline_new")),
      factor(group, levels = c("2012", "2013", "2014", "2015", "2016", "0", "1", "2+", "Q1", "Q2", "Q3", "Q4", "Others")),
      gender
    )
}

fit_matching_models <- function(data) {
  data <- prepare_matching_data(data)
  list(
    baseline = lm(fncr_5_years_early ~ Type * gender, data = data, weights = weights),
    full_without_coauthors = lm(
      fncr_5_years_early ~ Type * (gender + pub_before_cate + cohort + discipline_new + Jr_Quantile +
        colla_ctr_Y + author_cnt + firstauthor_top_100),
      data = data,
      weights = weights
    ),
    full_with_coauthors = lm(
      fncr_5_years_early ~ Type * (gender + pub_before_cate + cohort + discipline_new + Jr_Quantile +
        colla_ctr_Y + author_cnt + firstauthor_top_100 + max_coa_fncr_5y_log),
      data = data,
      weights = weights
    )
  )
}

make_overall_stacked_data <- function(model_list, data, treatment_value, control_value, comparison_label) {
  model_labels <- c(
    baseline = "Baseline model",
    full_without_coauthors = "Full model\nexcludes co-author citations",
    full_with_coauthors = "Full model\nincludes co-author citations"
  )

  purrr::imap_dfr(model_list, function(model, model_name) {
    ame <- compute_ame(model, data, treatment_value, control_value) %>%
      mutate(
        model = model_labels[[model_name]],
        model_order = match(model_name, names(model_labels)),
        type = "Marginal effect",
        value = AME,
        ymin = baseline_mean + AME_low,
        ymax = baseline_mean + AME_high,
        p_label = format_p(p_value),
        label_new = paste0(
          "<span style='font-size:16pt'>", sprintf("%.2f", AME), "</span><br>",
          "<span style='font-size:11pt'>", p_label, "</span>"
        ),
        label_colour = case_when(
          p_value >= 0.05 ~ "grey50",
          AME > 0 ~ "#E69F00",
          AME < 0 ~ "#56B4E9",
          TRUE ~ "grey50"
        )
      )

    baseline <- ame %>%
      transmute(
        gender, n, baseline_mean, treated_mean, AME, AME_low, AME_high,
        p_value, ratio, cohens_d, cliffs_delta,
        model, model_order,
        type = "Baseline value",
        value = baseline_mean,
        ymin = NA_real_,
        ymax = NA_real_,
        p_label = "",
        label_new = NA_character_,
        label_colour = NA_character_
      )

    bind_rows(baseline, ame)
  }) %>%
    mutate(
      comparison = comparison_label,
      gender_label = recode(as.character(gender), "female" = "Female", "male" = "Male"),
      fill_group = interaction(gender_label, type, sep = ": "),
      model = factor(model, levels = model_labels)
    )
}

plot_overall_stacked <- function(plot_data, y_label, y_max = 2.0) {
  base <- ggplot(plot_data, aes(x = gender_label, y = value, fill = fill_group, colour = fill_group))

  if (has_ggpattern) {
    base <- base +
      ggpattern::geom_col_pattern(
        aes(pattern = type, pattern_fill = fill_group),
        width = 0.72,
        position = "stack",
        linewidth = 0.35,
        pattern_density = 0.14,
        pattern_spacing = 0.03,
        pattern_angle = 45,
        show.legend = FALSE
      ) +
      ggpattern::scale_pattern_manual(values = c("Baseline value" = "none", "Marginal effect" = "stripe")) +
      ggpattern::scale_pattern_fill_manual(values = c(
        "Female: Baseline value" = "#E69F00",
        "Male: Baseline value" = "#56B4E9",
        "Female: Marginal effect" = "#E69F00",
        "Male: Marginal effect" = "#56B4E9"
      ))
  } else {
    base <- base + geom_col(width = 0.72, position = "stack", linewidth = 0.35, show.legend = FALSE)
  }

  base +
    facet_grid(~ model) +
    geom_errorbar(
      data = filter(plot_data, type == "Marginal effect"),
      aes(ymin = ymin, ymax = ymax),
      width = 0.30,
      linewidth = 0.80,
      show.legend = FALSE
    ) +
    ggtext::geom_richtext(
      data = filter(plot_data, type == "Marginal effect"),
      aes(x = gender_label, y = y_max * 0.93, label = label_new),
      colour = filter(plot_data, type == "Marginal effect")$label_colour,
      fill = NA,
      label.color = NA,
      lineheight = 0.9,
      fontface = 2,
      show.legend = FALSE
    ) +
    scale_y_continuous(limits = c(0, y_max), breaks = seq(0, y_max, 0.5)) +
    scale_fill_manual(values = c(
      "Female: Baseline value" = "#FBD685",
      "Male: Baseline value" = "#B0E6FF",
      "Female: Marginal effect" = "white",
      "Male: Marginal effect" = "white"
    )) +
    scale_colour_manual(values = c(
      "Female: Baseline value" = "#E69F00",
      "Male: Baseline value" = "#56B4E9",
      "Female: Marginal effect" = "#E69F00",
      "Male: Marginal effect" = "#56B4E9"
    )) +
    labs(x = NULL, y = y_label) +
    theme_bw(base_size = 12) +
    theme(
      legend.position = "none",
      panel.spacing = unit(0, "lines"),
      panel.grid.major.x = element_blank(),
      strip.text = element_text(size = 12),
      axis.text.x = element_text(size = 11),
      axis.text.y = element_text(size = 11)
    )
}

plot_subgroup_ames <- function(df, model_title, y_label, discipline = FALSE, y_limits = NULL) {
  plot_df <- df %>%
    filter(group != "Others", group != "Unk", group != "unknown") %>%
    mutate(
      group = ifelse(group == "Econ", "Eco", group),
      group_order = case_when(
        index == "cohort" ~ match(group, c("2012", "2013", "2014", "2015", "2016")),
        index == "pub_before_cate" ~ match(group, c("0", "1", "2+")),
        index == "Jr_Quantile" ~ match(group, c("Q1", "Q2", "Q3", "Q4")),
        TRUE ~ row_number()
      )
    )

  if (discipline) {
    plot_df <- plot_df %>% filter(index_new == "Discipline")
    facet_formula <- ~ index_new
  } else {
    plot_df <- plot_df %>% filter(index_new != "Discipline")
    facet_formula <- ~ factor(index_new, levels = c("Cohort", "Previous Publications", "Journal Rank"))
  }

  p <- ggplot(plot_df, aes(x = forcats::fct_inorder(group), y = AME, colour = gender_label, fill = gender_label)) +
    facet_wrap(facet_formula, scales = "free_x") +
    geom_col(position = position_dodge(width = 0.75), width = 0.65, alpha = 0.80) +
    geom_errorbar(aes(ymin = AME_low, ymax = AME_high), position = position_dodge(width = 0.75), width = 0.25, linewidth = 0.8) +
    ggtext::geom_richtext(
      aes(y = AME_high + 0.08, label = label_new),
      colour = plot_df$label_colour,
      fill = NA,
      label.color = NA,
      lineheight = 0.9,
      fontface = 2,
      size = 3.2,
      position = position_dodge(width = 0.75),
      show.legend = FALSE
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.35) +
    scale_colour_manual(values = c("Female" = "#E69F00", "Male" = "#56B4E9")) +
    scale_fill_manual(values = c("Female" = "#FBD685", "Male" = "#B0E6FF")) +
    labs(title = model_title, x = NULL, y = y_label, colour = "Gender", fill = "Gender") +
    theme_bw(base_size = 12) +
    theme(
      legend.position = "bottom",
      panel.grid.major.x = element_blank(),
      strip.text = element_text(size = 12),
      axis.text.x = element_text(angle = ifelse(discipline, 45, 0), hjust = ifelse(discipline, 1, 0.5))
    )

  if (!is.null(y_limits)) {
    p <- p + scale_y_continuous(limits = y_limits)
  }

  p
}

# ---- 2. run propensity-score matching ------------------------------

if (RUN_MATCHING) {
  main_data <- read_required_csv(file.path(DATA_DIR, "5_author_12_16_f_fauthor_oct2024_robust_coauthor_soc_coaCiteAge_processedR_2025.csv"))
  main_aux <- main_data %>%
    select(author_id, max_coa_fncr_5y, average_tw, firstauthor_top_100, coauthor_top_100, discipline_new_pub)

  run_match <- function(input_file, output_file, treatment_formula, exact_formula) {
    raw <- read_required_csv(file.path(DATA_DIR, input_file)) %>%
      filter(author_cnt <= 15) %>%
      left_join(main_aux, by = "author_id") %>%
      filter(!is.na(max_coa_fncr_5y) | !is.na(average_tw)) %>%
      mutate(
        pub_before_cate = case_when(
          pub_before == 0 ~ "0",
          pub_before == 1 ~ "1",
          pub_before > 1 ~ "2+",
          TRUE ~ NA_character_
        ),
        max_coa_fncr_5y_log = log(max_coa_fncr_5y + 1),
        firstauthor_top_100 = ifelse(is.na(firstauthor_top_100), "0", as.character(firstauthor_top_100))
      ) %>%
      prepare_matching_data()

    m_out <- MatchIt::matchit(
      formula = treatment_formula,
      data = raw,
      method = "optimal",
      exact = exact_formula,
      estimand = "ATT"
    )

    matched <- MatchIt::match.data(m_out)
    readr::write_csv(matched, file.path(DATA_DIR, output_file))
    readr::write_csv(as.data.frame(summary(m_out)$sum.matched), file.path(TAB_DIR, paste0("balance_", output_file)))
    matched
  }

  run_match(
    input_file = "1_author_12_16_psm_TW_No.csv",
    output_file = "2_match_psm_TW_No",
    treatment_formula = Type_int ~ gender + pub_before_cate + cohort + discipline_new + Jr_Quantile +
      most_ctr + colla_ctr_Y + author_cnt + max_coa_fncr_5y_log + firstauthor_top_100,
    exact_formula = ~ gender + cohort + discipline_new + Jr_Quantile + firstauthor_top_100
  )

  run_match(
    input_file = "1_author_12_16_psm_self_other.csv",
    output_file = "2_match_psm_self_other.csv",
    treatment_formula = Type_int ~ gender + pub_before_cate + cohort + discipline_new + Jr_Quantile +
      most_ctr + colla_ctr_Y + len_tweet + author_cnt + max_coa_fncr_5y_log + firstauthor_top_100,
    exact_formula = ~ gender + cohort + discipline_new + Jr_Quantile + firstauthor_top_100
  )
}

# ---- 3. Load matched datasets -------------------------------------------------

matching1_twitter <- read_required_csv(file.path(DATA_DIR, "2_match_psm_TW_No")) %>%
  prepare_matching_data()

matching2_self <- read_required_csv(file.path(DATA_DIR, "2_match_psm_self_other.csv")) %>%
  prepare_matching_data()

# Harmonise treatment labels.
matching1_twitter <- matching1_twitter %>%
  mutate(Type = factor(Type, levels = c("NO", "withTW")))

matching2_self <- matching2_self %>%
  mutate(Type = factor(Type, levels = c("OnlyOther", "SelfOther")))

# ---- 4. Fit linear models -----------------------------------------------------

models_twitter <- fit_matching_models(matching1_twitter)
models_self <- fit_matching_models(matching2_self)

saveRDS(models_twitter, file.path(MOD_DIR, "matching1_twitter_lm_models.rds"))
saveRDS(models_self, file.path(MOD_DIR, "matching2_self_promotion_lm_models.rds"))

# Cluster-robust coefficient tables, clustered by matched subclass when available.
export_robust_table <- function(model, data, file_name) {
  if ("subclass" %in% names(data)) {
    tab <- lmtest::coeftest(model, vcov. = sandwich::vcovCL, cluster = data$subclass) %>%
      as.data.frame() %>%
      rownames_to_column("term")
  } else {
    tab <- broom::tidy(model)
  }
  readr::write_csv(tab, file.path(TAB_DIR, file_name))
}

purrr::iwalk(models_twitter, ~ export_robust_table(.x, matching1_twitter, paste0("model_matching1_twitter_", .y, ".csv")))
purrr::iwalk(models_self, ~ export_robust_table(.x, matching2_self, paste0("model_matching2_self_", .y, ".csv")))

# ---- 5. Main Fig. 4: overall AMEs --------------------------------------------

fig4a_data <- make_overall_stacked_data(
  models_twitter,
  matching1_twitter,
  treatment_value = "withTW",
  control_value = "NO",
  comparison_label = "General Twitter mentions vs. no mentions"
)

fig4b_data <- make_overall_stacked_data(
  models_self,
  matching2_self,
  treatment_value = "SelfOther",
  control_value = "OnlyOther",
  comparison_label = "Self-promotion vs. others' promotion only"
)

readr::write_csv(fig4a_data, file.path(TAB_DIR, "fig4a_matching1_twitter_overall_AMEs.csv"))
readr::write_csv(fig4b_data, file.path(TAB_DIR, "fig4b_matching2_self_overall_AMEs.csv"))

p_fig4a <- plot_overall_stacked(
  fig4a_data,
  y_label = "Marginal effects of general Twitter mentions (Group 1)",
  y_max = 2.0
)

p_fig4b <- plot_overall_stacked(
  fig4b_data,
  y_label = "Marginal effects of self-promotion (Group 2)",
  y_max = 2.0
)

fig4 <- p_fig4a + p_fig4b + plot_annotation(tag_levels = "a")

ggsave(file.path(FIG_DIR, "fig4_matching_citation_impact.pdf"), fig4, width = 20, height = 10, limitsize = FALSE)
ggsave(file.path(FIG_DIR, "fig4_matching_citation_impact.png"), fig4, width = 20, height = 10, dpi = 300, limitsize = FALSE)

# ---- 6. Supplementary subgroup figures ---------------------------------------

# Matching 1: Twitter mentions vs. no mentions.
twitter_subgroups_without <- compute_subgroup_ames(
  models_twitter$full_without_coauthors,
  matching1_twitter,
  treatment_value = "withTW",
  control_value = "NO"
)

twitter_subgroups_with <- compute_subgroup_ames(
  models_twitter$full_with_coauthors,
  matching1_twitter,
  treatment_value = "withTW",
  control_value = "NO"
)

readr::write_csv(twitter_subgroups_without, file.path(TAB_DIR, "fig_S17_S18_twitter_AMEs_without_coauthor_citations.csv"))
readr::write_csv(twitter_subgroups_with, file.path(TAB_DIR, "fig_S17_S18_twitter_AMEs_with_coauthor_citations.csv"))

p_s17a <- plot_subgroup_ames(twitter_subgroups_without, "(a) Full model excludes co-author citations", "Average marginal effects of general Twitter mentions", discipline = FALSE, y_limits = c(0, 0.7))
p_s17b <- plot_subgroup_ames(twitter_subgroups_with, "(b) Full model includes co-author citations", "Average marginal effects of general Twitter mentions", discipline = FALSE, y_limits = c(0, 0.7))
p_s17 <- p_s17a / p_s17b

ggsave(file.path(FIG_DIR, "fig_S17_twitter_mentions_subgroups.pdf"), p_s17, width = 14, height = 10, limitsize = FALSE)

p_s18a <- plot_subgroup_ames(twitter_subgroups_without, "(a) Full model excludes co-author citations", "Average marginal effects of general Twitter mentions", discipline = TRUE, y_limits = c(0, 0.8))
p_s18b <- plot_subgroup_ames(twitter_subgroups_with, "(b) Full model includes co-author citations", "Average marginal effects of general Twitter mentions", discipline = TRUE, y_limits = c(0, 0.8))
p_s18 <- p_s18a / p_s18b

ggsave(file.path(FIG_DIR, "fig_S18_twitter_mentions_discipline.pdf"), p_s18, width = 22, height = 12, limitsize = FALSE)

# Matching 2: self-promotion vs. others' promotion only.
self_subgroups_without <- compute_subgroup_ames(
  models_self$full_without_coauthors,
  matching2_self,
  treatment_value = "SelfOther",
  control_value = "OnlyOther"
)

self_subgroups_with <- compute_subgroup_ames(
  models_self$full_with_coauthors,
  matching2_self,
  treatment_value = "SelfOther",
  control_value = "OnlyOther"
)

readr::write_csv(self_subgroups_without, file.path(TAB_DIR, "fig_S21_S22_self_AMEs_without_coauthor_citations.csv"))
readr::write_csv(self_subgroups_with, file.path(TAB_DIR, "fig_S21_S22_self_AMEs_with_coauthor_citations.csv"))

p_s21a <- plot_subgroup_ames(self_subgroups_without, "(a) Full model excludes co-author citations", "Average marginal effects of self-promotion", discipline = FALSE, y_limits = c(-0.25, 1.1))
p_s21b <- plot_subgroup_ames(self_subgroups_with, "(b) Full model includes co-author citations", "Average marginal effects of self-promotion", discipline = FALSE, y_limits = c(-0.25, 1.1))
p_s21 <- p_s21a / p_s21b

ggsave(file.path(FIG_DIR, "fig_S21_self_promotion_subgroups.pdf"), p_s21, width = 14, height = 10, limitsize = FALSE)

p_s22a <- plot_subgroup_ames(self_subgroups_without, "(a) Full model excludes co-author citations", "Average marginal effects of self-promotion", discipline = TRUE, y_limits = c(-0.25, 1.25))
p_s22b <- plot_subgroup_ames(self_subgroups_with, "(b) Full model includes co-author citations", "Average marginal effects of self-promotion", discipline = TRUE, y_limits = c(-0.25, 1.25))
p_s22 <- p_s22a / p_s22b

ggsave(file.path(FIG_DIR, "fig_S22_self_promotion_discipline.pdf"), p_s22, width = 18, height = 12, limitsize = FALSE)

# ---- 7. Session information ---------------------------------------------------

writeLines(capture.output(sessionInfo()), file.path(TAB_DIR, "sessionInfo_matching_citation_analysis.txt"))

message("Done. Matching citation-impact figures and tables were written to Result/.")
