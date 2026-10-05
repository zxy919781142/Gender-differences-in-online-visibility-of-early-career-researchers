################################################################################
# 04b_matching_regression_figures.R
#
# Manuscript: Gender differences in online visibility of early-career researchers
#
# Purpose:
#   Fit the citation-impact regressions on the already-matched datasets and
#   produce all downstream figures/tables:
#     - Main text Fig. 4
#     - Supplementary Figs. S17-S18, S21-S22
#     - Supplementary Fig. S19 (exactly-one-mention robustness check)
#
#   This is Part 2 of a two-part pipeline; run 04a_run_matching.R first (or
#   just make sure its output files already exist in DATA_DIR).
#
# Required input files (in result/; the first two are produced by 04a_run_matching.R):
#   result/2_match_psm_TW_No
#   result/2_match_psm_self_other.csv
#   result/2_match_psm_TW_No_onemention.csv

#
# Outputs:
#   figures/fig_4.pdf      tables/fig_4.csv   (panels a + b, column fig_panel)
#   tables/table_s14.csv, tables/table_s15.csv   (Tables S14/S15 = Fig. 4a/4b estimates)
#   figures/fig_s17.pdf    tables/fig_s17.csv
#   figures/fig_s18.pdf    tables/fig_s18.csv
#   figures/fig_s19.pdf    tables/fig_s19.csv
#   figures/fig_s21.pdf    tables/fig_s21.csv
#   figures/fig_s22.pdf    tables/fig_s22.csv
#   tables/model_matching*_*.csv   (cluster-robust regression tables)

################################################################################

# ---- 0. Setup -----------------------------------------------------------------

required_packages <- c(
  "tidyverse", "broom", "sandwich", "lmtest", "effectsize",
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

DATA_DIR <- "result"  # matched datasets from 04a_run_matching.R live here
FIG_DIR <- "figures"    # every manuscript figure
TAB_DIR <- "tables"     # figure source data + supplementary tables
MOD_DIR <- "result"   # intermediate files (fitted models, session info)

dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TAB_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(MOD_DIR, recursive = TRUE, showWarnings = FALSE)

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

# Source data for each figure: exactly the values plotted, without the
# HTML label/colour helper columns used only for drawing.
write_source_data <- function(data, file_name) {
  readr::write_csv(
    data %>% select(-any_of(c("label_new", "label_colour"))),
    file.path(TAB_DIR, file_name)
  )
}

# Same row filter as plot_subgroup_ames(), so the table matches the figure.
subgroup_source_data <- function(without_df, with_df, discipline) {
  bind_rows(
    without_df %>% mutate(fig_panel = "a", model = "Full model excludes co-author citations", .before = 1),
    with_df %>% mutate(fig_panel = "b", model = "Full model includes co-author citations", .before = 1)
  ) %>%
    filter(group != "Others", group != "Unk", group != "unknown") %>%
    mutate(group = ifelse(group == "Econ", "Eco", group)) %>%
    filter(if (discipline) index_new == "Discipline" else index_new != "Discipline")
}

# ---- 2. Load matched datasets -------------------------------------------------

matching1_twitter <- read_required_csv(file.path(DATA_DIR, "2_match_psm_TW_No")) %>%
  prepare_matching_data()

matching2_self <- read_required_csv(file.path(DATA_DIR, "2_match_psm_self_other.csv")) %>%
  prepare_matching_data()

# Harmonise treatment labels.
matching1_twitter <- matching1_twitter %>%
  mutate(Type = factor(Type, levels = c("NO", "withTW")))

matching2_self <- matching2_self %>%
  mutate(Type = factor(Type, levels = c("OnlyOther", "SelfOther")))

# ---- 3. Fit linear models -----------------------------------------------------

models_twitter <- fit_matching_models(matching1_twitter)
models_self <- fit_matching_models(matching2_self)

saveRDS(models_twitter, file.path(MOD_DIR, "matching1_twitter_lm_models.rds"))
saveRDS(models_self, file.path(MOD_DIR, "matching2_self_promotion_lm_models.rds"))

# Cluster-robust coefficient tables, clustered by matched subclass when available.
export_robust_table <- function(model, data, file_name) {
  if ("subclass" %in% names(data)) {
    # NOTE on a fix: as.data.frame() has no proper method for "coeftest"
    # objects, so it silently wraps the ENTIRE coefficient matrix as one
    # list-column instead of spreading it into separate numeric columns --
    # even the row names collapse to "1","2","3",... instead of the actual
    # term names. This breaks write_csv() ("must not contain list or matrix
    # columns"). Stripping the class with unclass() first forces a normal,
    # correctly-structured conversion.
    raw_tab <- lmtest::coeftest(model, vcov. = sandwich::vcovCL, cluster = data$subclass)
    tab <- as.data.frame(unclass(raw_tab)) %>%
      rownames_to_column("term")
  } else {
    tab <- broom::tidy(model)
  }
  readr::write_csv(tab, file.path(TAB_DIR, file_name))
}

purrr::iwalk(models_twitter, ~ export_robust_table(.x, matching1_twitter, paste0("model_matching1_twitter_", .y, ".csv")))
purrr::iwalk(models_self, ~ export_robust_table(.x, matching2_self, paste0("model_matching2_self_", .y, ".csv")))

# ---- 4. Main Fig. 4: overall AMEs --------------------------------------------

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

write_source_data(
  bind_rows(
    fig4a_data %>% mutate(fig_panel = "a", .before = 1),
    fig4b_data %>% mutate(fig_panel = "b", .before = 1)
  ),
  "fig_4.csv"
)
# Tables S14/S15: AMEs of Twitter mentions (Matching 1) / self-promotion
# (Matching 2) on DNCS5 by gender -- the same estimates as Fig. 4a/4b.
write_source_data(fig4a_data, "table_s14.csv")
write_source_data(fig4b_data, "table_s15.csv")

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

ggsave(file.path(FIG_DIR, "fig_4.pdf"), fig4, width = 20, height = 10, limitsize = FALSE)

# ---- 5. Fig. S19: robustness check (exactly one Twitter mention) -------------
#
# Same 3-model comparison (baseline / full excluding co-author citations /
# full including co-author citations) as Fig. 4a, but run on a separately
# matched dataset that restricts the treatment group to researchers with
# EXACTLY ONE Twitter mention (rather than one-or-more), matching Fig 4a's
# treatment group against a control group with no mentions. This reuses
# fit_matching_models(), make_overall_stacked_data(), and
# plot_overall_stacked() exactly as used for Fig. 4a -- the only thing that
# differs is the input matched dataset.

matching1_onemention <- read_required_csv(
  file.path(DATA_DIR, "2_match_psm_TW_No_onemention.csv")
) %>%
  prepare_matching_data() %>%
  mutate(Type = factor(Type, levels = c("NO", "withTW")))

models_twitter_onemention <- fit_matching_models(matching1_onemention)
saveRDS(models_twitter_onemention, file.path(MOD_DIR, "matching1_onemention_lm_models.rds"))

purrr::iwalk(
  models_twitter_onemention,
  ~ export_robust_table(.x, matching1_onemention, paste0("model_matching1_onemention_", .y, ".csv"))
)

fig_s19_data <- make_overall_stacked_data(
  models_twitter_onemention,
  matching1_onemention,
  treatment_value = "withTW",
  control_value = "NO",
  comparison_label = "General Twitter mentions (exactly one mention) vs. no mentions"
)

write_source_data(fig_s19_data, "fig_s19.csv")

p_s19 <- plot_overall_stacked(
  fig_s19_data,
  y_label = "Marginal effects of receiving exactly one Twitter mention",
  y_max = 2.0
)

ggsave(file.path(FIG_DIR, "fig_s19.pdf"), p_s19, width = 12, height = 8, limitsize = FALSE)

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

write_source_data(subgroup_source_data(twitter_subgroups_without, twitter_subgroups_with, discipline = FALSE), "fig_s17.csv")
write_source_data(subgroup_source_data(twitter_subgroups_without, twitter_subgroups_with, discipline = TRUE), "fig_s18.csv")

p_s17a <- plot_subgroup_ames(twitter_subgroups_without, "(a) Full model excludes co-author citations", "Average marginal effects of general Twitter mentions", discipline = FALSE, y_limits = c(0, 0.7))
p_s17b <- plot_subgroup_ames(twitter_subgroups_with, "(b) Full model includes co-author citations", "Average marginal effects of general Twitter mentions", discipline = FALSE, y_limits = c(0, 0.7))
p_s17 <- p_s17a / p_s17b

ggsave(file.path(FIG_DIR, "fig_s17.pdf"), p_s17, width = 14, height = 10, limitsize = FALSE)

p_s18a <- plot_subgroup_ames(twitter_subgroups_without, "(a) Full model excludes co-author citations", "Average marginal effects of general Twitter mentions", discipline = TRUE, y_limits = c(0, 0.8))
p_s18b <- plot_subgroup_ames(twitter_subgroups_with, "(b) Full model includes co-author citations", "Average marginal effects of general Twitter mentions", discipline = TRUE, y_limits = c(0, 0.8))
p_s18 <- p_s18a / p_s18b

ggsave(file.path(FIG_DIR, "fig_s18.pdf"), p_s18, width = 22, height = 12, limitsize = FALSE)

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

write_source_data(subgroup_source_data(self_subgroups_without, self_subgroups_with, discipline = FALSE), "fig_s21.csv")
write_source_data(subgroup_source_data(self_subgroups_without, self_subgroups_with, discipline = TRUE), "fig_s22.csv")

p_s21a <- plot_subgroup_ames(self_subgroups_without, "(a) Full model excludes co-author citations", "Average marginal effects of self-promotion", discipline = FALSE, y_limits = c(-0.25, 1.1))
p_s21b <- plot_subgroup_ames(self_subgroups_with, "(b) Full model includes co-author citations", "Average marginal effects of self-promotion", discipline = FALSE, y_limits = c(-0.25, 1.1))
p_s21 <- p_s21a / p_s21b

ggsave(file.path(FIG_DIR, "fig_s21.pdf"), p_s21, width = 14, height = 10, limitsize = FALSE)

p_s22a <- plot_subgroup_ames(self_subgroups_without, "(a) Full model excludes co-author citations", "Average marginal effects of self-promotion", discipline = TRUE, y_limits = c(-0.25, 1.25))
p_s22b <- plot_subgroup_ames(self_subgroups_with, "(b) Full model includes co-author citations", "Average marginal effects of self-promotion", discipline = TRUE, y_limits = c(-0.25, 1.25))
p_s22 <- p_s22a / p_s22b

ggsave(file.path(FIG_DIR, "fig_s22.pdf"), p_s22, width = 18, height = 12, limitsize = FALSE)

# ---- 7. Session information ---------------------------------------------------


message("Done. Figures in figures/, tables in tables/.")
