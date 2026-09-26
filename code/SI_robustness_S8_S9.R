# -----------------------------------------------------------------------------
# SI_robustness_S8_S9.R
#
# Purpose:
#   Fig. S8 -- F1 score heatmap across name-matching similarity threshold
#             pairs (validated against GPT + manual review ground truth)
#   Fig. S9 -- Female-to-male self-promotion ratio heatmap across the same
#             threshold pairs (sensitivity of the substantive gender finding
#             to the threshold choice)
#
# Manuscript: Gender differences in online visibility of early-career researchers
#
# CHANGES FROM THE ORIGINAL CODE:
#   - Uses relative paths (data_dir/results_dir) instead of a hardcoded
#     Windows absolute path (which also used single backslashes -- an
#     escape-character issue in R string literals; see prior discussion).
#   - Wrapped in a single reusable plotting function (plot_threshold_heatmap)
#     instead of two near-duplicate ggplot blocks, so the two figures stay
#     visually consistent and any styling fix only needs to be made once.
#   - Both scripts now write to `results_dir` instead of a fixed private
#     network path.
#
# Required input files:
#   2_result/1_sample_2_testresult.csv     (from SI_robustness_gpt_validation.ipynb)
#   2_result/2_gender_ratio_threshold.csv  (from SI_robustness_gender_ratio.ipynb)
#
# Outputs:
#   figures/fig_s8.pdf    tables/fig_s8.csv
#   figures/fig_s9.pdf    tables/fig_s9.csv
# -----------------------------------------------------------------------------

library(dplyr)
library(ggplot2)

data_dir <- "2_result"
fig_dir <- "figures"
table_dir <- "tables"
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

f1_file <- file.path(data_dir, "1_sample_2_testresult.csv")
gender_file <- file.path(data_dir, "2_gender_ratio_threshold.csv")

if (!file.exists(f1_file)) {
  stop("Required input not found (run SI_robustness_gpt_validation.ipynb first): ", f1_file)
}
if (!file.exists(gender_file)) {
  stop("Required input not found (run SI_robustness_gender_ratio.ipynb first): ", gender_file)
}

f1_data <- readr::read_csv(f1_file, show_col_types = FALSE) %>%
  mutate(across(c(general_threshold, chinese_threshold), ~ round(.x, 3)))

gender_data <- readr::read_csv(gender_file, show_col_types = FALSE) %>%
  mutate(across(c(general_threshold, chinese_threshold), ~ round(.x, 3)))

# Identify the best-performing threshold pair (highest F1), reported in the
# main text/response letter alongside the figure.
best_pair <- f1_data %>% slice_max(f1, n = 1)
cat(
  "Best-performing threshold pair: general =", best_pair$general_threshold,
  ", chinese =", best_pair$chinese_threshold, ", F1 =", round(best_pair$f1, 3), "\n"
)

# ---- Shared plotting function ---------------------------------------------------

plot_threshold_heatmap <- function(data, fill_var, title, legend_name,
                                    low_colour, high_colour, fill_limits) {
  ggplot(
    data,
    aes(
      x = as.factor(general_threshold),
      y = as.factor(chinese_threshold),
      fill = .data[[fill_var]]
    )
  ) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.3f", .data[[fill_var]])), size = 5, color = "black") +
    scale_fill_gradient(low = low_colour, high = high_colour, limits = fill_limits, name = legend_name) +
    labs(
      title = title,
      x = "Threshold for non-Chinese names",
      y = "Threshold for Chinese names"
    ) +
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

# ---- Fig. S8: F1 across threshold pairs ----------------------------------------

fig_s8 <- plot_threshold_heatmap(
  f1_data, fill_var = "f1", title = "F1 across threshold pairs",
  legend_name = "F1", low_colour = "#deebf7", high_colour = "#08306b",
  fill_limits = c(0, 1)
)

ggsave(
  file.path(fig_dir, "fig_s8.pdf"),
  fig_s8, width = 8, height = 6
)
readr::write_csv(f1_data, file.path(table_dir, "fig_s8.csv"))

# ---- Fig. S9: female-to-male self-promotion ratio across threshold pairs ------

fig_s9 <- plot_threshold_heatmap(
  gender_data, fill_var = "gender_ratio",
  title = "Female-to-male ratios across threshold pairs",
  legend_name = "Gender ratio", low_colour = "#fff7bc", high_colour = "goldenrod",
  fill_limits = c(0.5, 1)
)

ggsave(
  file.path(fig_dir, "fig_s9.pdf"),
  fig_s9, width = 8, height = 6
)
readr::write_csv(gender_data, file.path(table_dir, "fig_s9.csv"))

message("Done. Figs. S8-S9 written to ", fig_dir, "; source data to ", table_dir)
