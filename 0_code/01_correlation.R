# 01_make_correlation_figures.R
#
# Reproduces the country-level correlation figures and correlation-statistics
# tables used for Fig. 1 and Supplementary Figs. S1-S7 in:
#   Gender differences in online visibility of early-career researchers
#
# Input files should be placed in the Data/ folder:
#   Data/corr.csv      country-by-cohort correlation input data
#   Data/corr_agg.csv  country-level pooled 2012-2016 correlation input data
#

# Outputs are written to:
#   # Outputs are written to:
#
#   2_result/
#   
#   ├──   fig1_combined.pdf                              # Main Fig. 1
#   │     fig1a_online_visible_vs_all_pooled.pdf         # Main Fig. 1a
#   │     fig1b_self_promotion_vs_all_pooled.pdf         # Main Fig. 1b
#   │     fig_s1_online_visible_vs_all_by_cohort.pdf     # Supplementary Fig. S1
#   │     fig_s2_gii_online_visible_pooled.pdf           # Supplementary Fig. S2
#   │     fig_s3_gii_online_visible_by_cohort.pdf        # Supplementary Fig. S3
#   │     fig_s5_self_promotion_vs_all_by_cohort.pdf     # Supplementary Fig. S5
#   │     fig_s6_gii_self_promotion_pooled.pdf           # Supplementary Fig. S6
#   │     fig_s7_gii_self_promotion_by_cohort.pdf        # Supplementary Fig. S7
#   │
#   └──   fig1a_online_visible_vs_all_pooled_correlation.csv
#         fig1b_self_promotion_vs_all_pooled_correlation.csv
#         fig_s2_gii_online_visible_pooled_correlation.csv
#         fig_s6_gii_self_promotion_pooled_correlation.csv
#         table_s3_online_visible_vs_all_by_cohort.csv
#         table_s4_gii_online_visible_by_cohort.csv
#         table_s5_self_promotion_vs_all_by_cohort.csv
#         table_s6_gii_self_promotion_by_cohort.csv
#

# -----------------------------------------------------------------------------
# 0. Setup
# -----------------------------------------------------------------------------

required_packages <- c(
  "dplyr", "tidyr", "purrr", "readr", "ggplot2", "ggrepel",
  "countrycode", "broom", "patchwork", "scales"
)

install_if_missing <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) install.packages(missing)
}

install_if_missing(required_packages)
invisible(lapply(required_packages, library, character.only = TRUE))

DATA_DIR <- "1_data"
RESULT_DIR <- "2_result"
FIG_DIR <- file.path(RESULT_DIR)
TABLE_DIR <- file.path(RESULT_DIR)

dir.create(RESULT_DIR, showWarnings = FALSE)
dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(TABLE_DIR, showWarnings = FALSE, recursive = TRUE)

# Input paths
corr_file <- file.path(DATA_DIR, "corr.csv")
corr_agg_file <- file.path(DATA_DIR, "corr_agg.csv")
#threshold_f1_file <- file.path(DATA_DIR, "1_sample_2_testresult.csv")
#threshold_gender_file <- file.path(DATA_DIR, "2_gender_ratio_threshold.csv")

if (!file.exists(corr_file)) stop("Missing input file: ", corr_file)
if (!file.exists(corr_agg_file)) stop("Missing input file: ", corr_agg_file)

# Countries highlighted in labels. Add/remove ISO3 codes if needed.
highlight_countries <- c("deu", "chn", "usa", "gbr")

continent_colours <- c(
  "Americas" = "#008C45",
  "Asia"     = "#FFCC00",
  "Europe"   = "#75AADB",
  "Oceania"  = "#ffafcc",
  "Africa"   = "#FF7F11"
)

# -----------------------------------------------------------------------------
# 1. Helper functions
# -----------------------------------------------------------------------------

format_p <- function(p) {
  dplyr::case_when(
    is.na(p) ~ NA_character_,
    p < 0.001 ~ "p < .001",
    TRUE ~ sprintf("p = %.3f", p)
  )
}

format_p_table <- function(p) {
  dplyr::case_when(
    is.na(p) ~ NA_character_,
    p < 0.001 ~ "< .001",
    TRUE ~ sprintf("%.4f", p)
  )
}

add_country_metadata <- function(df) {
  df %>%
    mutate(
      most_ctr = tolower(most_ctr),
      continent = countrycode(most_ctr, origin = "iso3c", destination = "continent"),
      country_name = countrycode(most_ctr, origin = "iso3c", destination = "country.name")
    )
}

add_derived_variables <- function(df) {
  df %>%
    mutate(
      gii_rev = 1 - gii,
      withTW_gender_ratio_weighted = withTW_gender_ratio / all_gender_ratio,
      selfpro_gender_ratio_weighted = selfpro_gender_ratio / all_gender_ratio,
      withTW_female_ratio = withTW_female / (withTW_female + withTW_male),
      selfpro_female_ratio = selfpro_female / (selfpro_female + selfpro_male)
    )
}

pearson_stats <- function(data, x, y, group_var = NULL) {
  x <- rlang::ensym(x)
  y <- rlang::ensym(y)

  compute_one <- function(df) {
    complete_df <- df %>% filter(complete.cases(!!x, !!y))
    if (nrow(complete_df) < 3) {
      return(tibble(
        n = nrow(complete_df), r = NA_real_, ci_low = NA_real_, ci_high = NA_real_,
        df = NA_real_, p_value = NA_real_, p_label = NA_character_, label = NA_character_
      ))
    }

    test <- cor.test(
      x = complete_df %>% pull(!!x),
      y = complete_df %>% pull(!!y),
      method = "pearson",
      alternative = "two.sided"
    )

    tidied <- broom::tidy(test)

    tibble(
      n = nrow(complete_df),
      r = unname(tidied$estimate),
      ci_low = tidied$conf.low,
      ci_high = tidied$conf.high,
      df = unname(tidied$parameter),
      p_value = tidied$p.value,
      p_label = format_p_table(tidied$p.value),
      label = sprintf("r = %.2f", unname(tidied$estimate))
    )
  }

  if (is.null(group_var)) {
    compute_one(data)
  } else {
    group_var <- rlang::ensym(group_var)
    data %>%
      group_by(!!group_var) %>%
      group_modify(~ compute_one(.x)) %>%
      ungroup()
  }
}

save_table <- function(df, filename) {
  readr::write_csv(df, file.path(TABLE_DIR, filename))
}

save_figure <- function(plot, filename, width, height) {
  ggplot2::ggsave(
    filename = file.path(FIG_DIR, filename),
    plot = plot,
    width = width,
    height = height,
    limitsize = FALSE
  )
}

get_annotated_countries <- function(data, x_var, y_var, group_var = NULL) {
  x_var <- rlang::ensym(x_var)
  y_var <- rlang::ensym(y_var)

  annotate_one <- function(df) {
    df %>%
      filter(
        (!!x_var) == max(!!x_var, na.rm = TRUE) |
          (!!y_var) == max(!!y_var, na.rm = TRUE) |
          all_authors == max(all_authors, na.rm = TRUE) |
          most_ctr %in% highlight_countries
      ) %>%
      distinct(most_ctr, .keep_all = TRUE)
  }

  if (is.null(group_var)) {
    annotate_one(data)
  } else {
    group_var <- rlang::ensym(group_var)
    data %>%
      group_by(!!group_var) %>%
      group_modify(~ annotate_one(.x)) %>%
      ungroup()
  }
}

get_medians <- function(data, x_var, y_var, group_var = NULL) {
  x_var <- rlang::ensym(x_var)
  y_var <- rlang::ensym(y_var)

  if (is.null(group_var)) {
    data %>%
      summarise(
        x_med = median(!!x_var, na.rm = TRUE),
        y_med = median(!!y_var, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    group_var <- rlang::ensym(group_var)
    data %>%
      group_by(!!group_var) %>%
      summarise(
        x_med = median(!!x_var, na.rm = TRUE),
        y_med = median(!!y_var, na.rm = TRUE),
        .groups = "drop"
      )
  } %>%
    mutate(
      x_lab = paste0("Median: ", round(x_med, 2)),
      y_lab = paste0("Median: ", round(y_med, 2))
    )
}

get_gap_to_reference <- function(data, x_var, y_var, x_ref, y_ref, group_var = NULL) {
  x_var <- rlang::ensym(x_var)
  y_var <- rlang::ensym(y_var)

  compute_gap <- function(df) {
    model_df <- df %>% filter(complete.cases(!!x_var, !!y_var))
    if (nrow(model_df) < 2) {
      return(tibble(x_gap = x_ref, y_reg = NA_real_, y_ref = y_ref,
                    gap = NA_real_, y_mid = NA_real_, label = NA_character_))
    }

    form <- stats::as.formula(paste(rlang::as_string(y_var), "~", rlang::as_string(x_var)))
    model <- lm(form, data = model_df)
    new_data <- setNames(data.frame(x_ref), rlang::as_string(x_var))
    y_reg <- as.numeric(predict(model, newdata = new_data))

    tibble(
      x_gap = x_ref,
      y_reg = y_reg,
      y_ref = y_ref,
      gap = y_reg - y_ref,
      y_mid = (y_reg + y_ref) / 2,
      label = paste0("Gap: ", round(abs(y_reg - y_ref), 2))
    )
  }

  if (is.null(group_var)) {
    compute_gap(data)
  } else {
    group_var <- rlang::ensym(group_var)
    data %>%
      group_by(!!group_var) %>%
      group_modify(~ compute_gap(.x)) %>%
      ungroup()
  }
}

base_correlation_theme <- function() {
  theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 20),
      axis.text = element_text(size = 20),
      axis.title = element_text(size = 28),
      strip.text = element_text(size = 28),
      legend.text = element_text(size = 24)
    )
}

plot_ratio_correlation <- function(
    data, x_var, y_var, y_label, corr_labels, medians, gaps, annotations,
    facet = FALSE, x_limits = c(0, 2), y_limits = c(0, 2),
    point_legend_size = 5
) {
  x_var <- rlang::ensym(x_var)
  y_var <- rlang::ensym(y_var)

  p <- ggplot(data, aes(x = !!x_var, y = !!y_var)) +
    coord_cartesian(xlim = x_limits, ylim = y_limits) +
    labs(
      x = "Gender ratio of all early-career researchers",
      y = y_label
    ) +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted", colour = "grey30", linewidth = 0.8) +
    geom_smooth(formula = y ~ x, method = "lm", linewidth = 1.2, fullrange = TRUE,
                colour = "lightgrey", fill = "lightgrey") +
    geom_vline(data = medians, aes(xintercept = x_med), linetype = "dashed",
               colour = "grey70", linewidth = 1.2, inherit.aes = FALSE) +
    geom_hline(data = medians, aes(yintercept = y_med), linetype = "dashed",
               colour = "grey70", linewidth = 1.2, inherit.aes = FALSE) +
    geom_point(aes(colour = continent, size = all_authors)) +
    scale_size(range = c(0.8, 10), guide = "none") +
    scale_colour_manual(name = "", values = continent_colours, na.value = "grey60") +
    geom_text(data = corr_labels, aes(x = Inf, y = Inf, label = label),
              hjust = 1.1, vjust = 1.5, size = 10, inherit.aes = FALSE) +
    geom_text_repel(data = annotations, aes(label = country_name), size = 5,
                    box.padding = 0.25, point.padding = 0.15, label.padding = 0.15,
                    segment.size = 0.2, max.overlaps = Inf, show.legend = FALSE) +
    geom_segment(data = gaps, aes(x = x_gap, xend = x_gap, y = y_ref, yend = y_reg),
                 inherit.aes = FALSE, linewidth = 0.6, colour = "grey20",
                 arrow = arrow(length = unit(0.15, "cm"), ends = "both", type = "closed")) +
    geom_text(data = gaps, aes(x = x_gap + 0.2, y = y_mid, label = label),
              inherit.aes = FALSE, size = 6, fontface = "bold") +
    geom_text(data = medians, aes(x = 1.95, y = y_med, label = y_lab),
              inherit.aes = FALSE, hjust = 1, vjust = -0.25, size = 6, fontface = "bold") +
    geom_text(data = medians, aes(x = x_med + 0.2, y = 0.01, label = x_lab),
              inherit.aes = FALSE, vjust = 0, size = 6, fontface = "bold") +
    guides(colour = guide_legend(override.aes = list(size = point_legend_size))) +
    base_correlation_theme()

  if (facet) p <- p + facet_wrap(~ cohort, ncol = 3)
  p
}

plot_gii_correlation <- function(
    data, y_var, y_label, corr_labels, medians, gaps, annotations,
    facet = FALSE, point_legend_size = 5
) {
  y_var <- rlang::ensym(y_var)

  p <- ggplot(data, aes(x = gii_rev, y = !!y_var)) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    labs(
      x = "Gender Inequality Index (reversed)",
      y = y_label
    ) +
    geom_abline(slope = 0.5, intercept = 0, linetype = "dotted", colour = "grey30", linewidth = 0.8) +
    geom_smooth(formula = y ~ x, method = "lm", linewidth = 1.2, fullrange = TRUE,
                colour = "lightgrey", fill = "lightgrey") +
    geom_vline(data = medians, aes(xintercept = x_med), linetype = "dashed",
               colour = "grey70", linewidth = 1.2, inherit.aes = FALSE) +
    geom_hline(data = medians, aes(yintercept = y_med), linetype = "dashed",
               colour = "grey70", linewidth = 1.2, inherit.aes = FALSE) +
    geom_point(aes(colour = continent, size = all_authors)) +
    scale_size(range = c(0.8, 10), guide = "none") +
    scale_colour_manual(name = "", values = continent_colours, na.value = "grey60") +
    geom_text(data = corr_labels, aes(x = Inf, y = Inf, label = label),
              hjust = 1.1, vjust = 1.5, size = 10, inherit.aes = FALSE) +
    geom_text_repel(data = annotations, aes(label = country_name), size = if (facet) 5 else 7,
                    box.padding = 0.25, point.padding = 0.15, label.padding = 0.15,
                    segment.size = 0.2, max.overlaps = Inf, show.legend = FALSE) +
    geom_segment(data = gaps, aes(x = x_gap, xend = x_gap, y = y_ref, yend = y_reg),
                 inherit.aes = FALSE, linewidth = 0.6, colour = "grey20",
                 arrow = arrow(length = unit(0.15, "cm"), ends = "both", type = "closed")) +
    geom_text(data = gaps, aes(x = x_gap, y = y_mid, label = label),
              inherit.aes = FALSE, size = if (facet) 6 else 8, fontface = "bold") +
    geom_text(data = medians, aes(x = 0.2, y = y_med, label = y_lab),
              inherit.aes = FALSE, hjust = 1, vjust = -0.25, size = if (facet) 6 else 8, fontface = "bold") +
    geom_text(data = medians, aes(x = x_med + 0.1, y = 0.01, label = x_lab),
              inherit.aes = FALSE, vjust = 0, size = if (facet) 6 else 8, fontface = "bold") +
    guides(colour = guide_legend(override.aes = list(size = point_legend_size))) +
    base_correlation_theme()

  if (facet) p <- p + facet_wrap(~ cohort, ncol = 3)
  p
}

plot_threshold_heatmap <- function(data, fill_var, title, fill_label, low_colour, high_colour, limits) {
  fill_var <- rlang::ensym(fill_var)

  data %>%
    mutate(
      general_threshold = round(general_threshold, 3),
      chinese_threshold = round(chinese_threshold, 3)
    ) %>%
    ggplot(aes(x = as.factor(general_threshold), y = as.factor(chinese_threshold), fill = !!fill_var)) +
    geom_tile(colour = "white") +
    geom_text(aes(label = sprintf("%.3f", !!fill_var)), size = 5, colour = "black") +
    scale_fill_gradient(low = low_colour, high = high_colour, limits = limits, name = fill_label) +
    labs(title = title, x = "Threshold for non-Chinese names", y = "Threshold for Chinese names") +
    theme_minimal(base_size = 18) +
    theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 18),
      axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 14),
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 14),
      panel.grid = element_blank()
    )
}

# -----------------------------------------------------------------------------
# 2. Load and prepare data
# -----------------------------------------------------------------------------

correlation <- readr::read_csv(corr_file, show_col_types = FALSE) %>%
  add_country_metadata() %>%
  add_derived_variables()

correlation_agg <- readr::read_csv(corr_agg_file, show_col_types = FALSE) %>%
  add_country_metadata() %>%
  add_derived_variables()

# Country inclusion rules used in the paper.
# Online-visibility country plots require at least 10 online-visible female AND
# at least 10 online-visible male early-career researchers.
# Self-promotion country plots require at least 10 self-promoting female OR
# at least 10 self-promoting male early-career researchers.
correlation_tw <- correlation %>% filter(withTW_female >= 10, withTW_male >= 10)
correlation_self <- correlation %>% filter(selfpro_female >= 10 | selfpro_male >= 10)
correlation_gii <- correlation %>% filter(!is.na(gii_rev))

correlation_agg_tw <- correlation_agg %>% filter(withTW_female >= 10, withTW_male >= 10)
correlation_agg_self <- correlation_agg %>% filter(selfpro_female >= 10 | selfpro_male >= 10)
correlation_agg_gii <- correlation_agg %>% filter(!is.na(gii_rev))

# -----------------------------------------------------------------------------
# 3. Correlation statistics tables
# -----------------------------------------------------------------------------

stats_online_by_cohort <- pearson_stats(correlation_tw, all_gender_ratio, withTW_gender_ratio, cohort)
stats_self_by_cohort <- pearson_stats(correlation_self, all_gender_ratio, selfpro_gender_ratio, cohort)
stats_gii_online_by_cohort <- pearson_stats(correlation_gii, gii_rev, withTW_female_ratio, cohort)
stats_gii_self_by_cohort <- pearson_stats(correlation_gii, gii_rev, selfpro_female_ratio, cohort)

stats_online_pooled <- pearson_stats(correlation_agg_tw, all_gender_ratio, withTW_gender_ratio)
stats_self_pooled <- pearson_stats(correlation_agg_self, all_gender_ratio, selfpro_gender_ratio)
stats_gii_online_pooled <- pearson_stats(correlation_agg_gii, gii_rev, withTW_female_ratio)
stats_gii_self_pooled <- pearson_stats(correlation_agg_gii, gii_rev, selfpro_female_ratio)

save_table(stats_online_by_cohort, "table_s3_online_visible_vs_all_by_cohort.csv")
save_table(stats_gii_online_by_cohort, "table_s4_gii_online_visible_by_cohort.csv")
save_table(stats_self_by_cohort, "table_s5_self_promotion_vs_all_by_cohort.csv")
save_table(stats_gii_self_by_cohort, "table_s6_gii_self_promotion_by_cohort.csv")
save_table(stats_online_pooled, "fig1a_online_visible_vs_all_pooled_correlation.csv")
save_table(stats_self_pooled, "fig1b_self_promotion_vs_all_pooled_correlation.csv")
save_table(stats_gii_online_pooled, "fig_s2_gii_online_visible_pooled_correlation.csv")
save_table(stats_gii_self_pooled, "fig_s6_gii_self_promotion_pooled_correlation.csv")

# -----------------------------------------------------------------------------
# 4. Fig. 1 and Figs. S1/S5: country-level ratios
# -----------------------------------------------------------------------------

# Supplementary Fig. S1: online visibility by cohort
fig_s1 <- plot_ratio_correlation(
  data = correlation_tw,
  x_var = all_gender_ratio,
  y_var = withTW_gender_ratio,
  y_label = "Gender ratio of online-visible researchers",
  corr_labels = stats_online_by_cohort,
  medians = get_medians(correlation_tw, all_gender_ratio, withTW_gender_ratio, cohort),
  gaps = get_gap_to_reference(correlation_tw, all_gender_ratio, withTW_gender_ratio, x_ref = 1, y_ref = 1, group_var = cohort),
  annotations = get_annotated_countries(correlation_tw, all_gender_ratio, withTW_gender_ratio, cohort),
  facet = TRUE
)
save_figure(fig_s1, "fig_s1_online_visible_vs_all_by_cohort.pdf", width = 20, height = 11)

# Supplementary Fig. S5: self-promotion by cohort
fig_s5 <- plot_ratio_correlation(
  data = correlation_self,
  x_var = all_gender_ratio,
  y_var = selfpro_gender_ratio,
  y_label = "Gender ratio of self-promoting researchers",
  corr_labels = stats_self_by_cohort,
  medians = get_medians(correlation_self, all_gender_ratio, selfpro_gender_ratio, cohort),
  gaps = get_gap_to_reference(correlation_self, all_gender_ratio, selfpro_gender_ratio, x_ref = 1, y_ref = 1, group_var = cohort),
  annotations = get_annotated_countries(correlation_self, all_gender_ratio, selfpro_gender_ratio, cohort),
  facet = TRUE
)
save_figure(fig_s5, "fig_s5_self_promotion_vs_all_by_cohort.pdf", width = 20, height = 11)

# Main Fig. 1, panel a: online visibility, pooled cohorts
fig1a <- plot_ratio_correlation(
  data = correlation_agg_tw,
  x_var = all_gender_ratio,
  y_var = withTW_gender_ratio,
  y_label = "Gender ratio of online-visible researchers",
  corr_labels = stats_online_pooled,
  medians = get_medians(correlation_agg_tw, all_gender_ratio, withTW_gender_ratio),
  gaps = get_gap_to_reference(correlation_agg_tw, all_gender_ratio, withTW_gender_ratio, x_ref = 1, y_ref = 1),
  annotations = get_annotated_countries(correlation_agg_tw, all_gender_ratio, withTW_gender_ratio),
  facet = FALSE,
  point_legend_size = 7
)
save_figure(fig1a, "fig1a_online_visible_vs_all_pooled.pdf", width = 12.5, height = 11)

# Main Fig. 1, panel b: self-promotion, pooled cohorts
fig1b <- plot_ratio_correlation(
  data = correlation_agg_self,
  x_var = all_gender_ratio,
  y_var = selfpro_gender_ratio,
  y_label = "Gender ratio of self-promoting researchers",
  corr_labels = stats_self_pooled,
  medians = get_medians(correlation_agg_self, all_gender_ratio, selfpro_gender_ratio),
  gaps = get_gap_to_reference(correlation_agg_self, all_gender_ratio, selfpro_gender_ratio, x_ref = 1, y_ref = 1),
  annotations = get_annotated_countries(correlation_agg_self, all_gender_ratio, selfpro_gender_ratio),
  facet = FALSE,
  point_legend_size = 7
)
save_figure(fig1b, "fig1b_self_promotion_vs_all_pooled.pdf", width = 12.5, height = 11)

fig1_combined <- fig1a + fig1b + patchwork::plot_layout(nrow = 1)
save_figure(fig1_combined, "fig1_combined.pdf", width = 25, height = 11)

# -----------------------------------------------------------------------------
# 5. Figs. S2/S3/S6/S7: GII correlations
# -----------------------------------------------------------------------------

# Supplementary Fig. S3: GII and online visibility by cohort
fig_s3 <- plot_gii_correlation(
  data = correlation_gii,
  y_var = withTW_female_ratio,
  y_label = "Female proportion of online-visible researchers",
  corr_labels = stats_gii_online_by_cohort,
  medians = get_medians(correlation_gii, gii_rev, withTW_female_ratio, cohort),
  gaps = get_gap_to_reference(correlation_gii, gii_rev, withTW_female_ratio, x_ref = 1, y_ref = 0.5, group_var = cohort),
  annotations = get_annotated_countries(correlation_gii, gii_rev, withTW_female_ratio, cohort),
  facet = TRUE
)
save_figure(fig_s3, "fig_s3_gii_online_visible_by_cohort.pdf", width = 20, height = 11)

# Supplementary Fig. S7: GII and self-promotion by cohort
fig_s7 <- plot_gii_correlation(
  data = correlation_gii,
  y_var = selfpro_female_ratio,
  y_label = "Female proportion of self-promoting researchers",
  corr_labels = stats_gii_self_by_cohort,
  medians = get_medians(correlation_gii, gii_rev, selfpro_female_ratio, cohort),
  gaps = get_gap_to_reference(correlation_gii, gii_rev, selfpro_female_ratio, x_ref = 1, y_ref = 0.5, group_var = cohort),
  annotations = get_annotated_countries(correlation_gii, gii_rev, selfpro_female_ratio, cohort),
  facet = TRUE
)
save_figure(fig_s7, "fig_s7_gii_self_promotion_by_cohort.pdf", width = 20, height = 11)

# Supplementary Fig. S2: GII and online visibility, pooled cohorts
fig_s2 <- plot_gii_correlation(
  data = correlation_agg_gii,
  y_var = withTW_female_ratio,
  y_label = "Female proportion of online-visible researchers",
  corr_labels = stats_gii_online_pooled,
  medians = get_medians(correlation_agg_gii, gii_rev, withTW_female_ratio),
  gaps = get_gap_to_reference(correlation_agg_gii, gii_rev, withTW_female_ratio, x_ref = 1, y_ref = 0.5),
  annotations = get_annotated_countries(correlation_agg_gii, gii_rev, withTW_female_ratio),
  facet = FALSE
)
save_figure(fig_s2, "fig_s2_gii_online_visible_pooled.pdf", width = 15, height = 11)

# Supplementary Fig. S6: GII and self-promotion, pooled cohorts
fig_s6 <- plot_gii_correlation(
  data = correlation_agg_gii,
  y_var = selfpro_female_ratio,
  y_label = "Female proportion of self-promoting researchers",
  corr_labels = stats_gii_self_pooled,
  medians = get_medians(correlation_agg_gii, gii_rev, selfpro_female_ratio),
  gaps = get_gap_to_reference(correlation_agg_gii, gii_rev, selfpro_female_ratio, x_ref = 1, y_ref = 0.5),
  annotations = get_annotated_countries(correlation_agg_gii, gii_rev, selfpro_female_ratio),
  facet = FALSE
)
save_figure(fig_s6, "fig_s6_gii_self_promotion_pooled.pdf", width = 15, height = 11)

