################################################################################
# 04a_run_matching.R
#
# Manuscript: Gender differences in online visibility of early-career researchers
#
# Purpose:
#   Run propensity-score matching (MatchIt::matchit + match.data) for both
#   comparisons used in the citation-impact analyses, and produce the
#   matching-diagnostic outputs (balance tables + density plots).
#
#   This is Part 1 of a two-part pipeline, split out of the original combined
#   05_matching_citation_analysis.R:
#     04a_run_matching.R              (this script) -- raw data -> matched data
#     04b_matching_regression_figures.R -- matched data -> models -> Fig. 4 etc.
#   Splitting these means the (slow, one-off) matching step doesn't need to be
#   re-run every time you tweak the regression/plotting code.
#
# Required input file:
#   result/dataset_demo_processed.csv
#   (produced by 02_online_mentions_models_figure.R -- run that script first)
#   Must include: cit_5_years, cit_3_years, discipline_average_5y_early,
#   discipline_average_3y_early (used to compute the DNCS5 citation outcome,
#   fncr_5_years_early -- see Section 3 below).
#
#
# Outputs:
#   result/2_match_psm_TW_No.csv              (matched data, Matching 1)
#   result/2_match_psm_TW_No_onemention.csv (matched data, exactly one mention vs. none; Fig. S19)
#   result/2_match_psm_self_other.csv      (matched data, Matching 2)
#   tables/balance_2_match_psm_TW_No.csv      (numeric balance table, Matching 1)
#   tables/balance_2_match_psm_TW_No_onemention.csv (numeric balance table, one-mention matching)
#   tables/balance_2_match_psm_self_other.csv (numeric balance table, Matching 2)
#   figures/fig_s16.pdf    tables/fig_s16.csv  (balance plots, Matching 1)
#   figures/fig_s20.pdf    tables/fig_s20.csv  (balance plots, Matching 2)
################################################################################

# ---- 0. Setup -----------------------------------------------------------------

required_packages <- c("tidyverse", "MatchIt", "readr", "ggplot2", "patchwork")

missing_packages <- required_packages[!required_packages %in% rownames(installed.packages())]
if (length(missing_packages) > 0) {
  stop(
    "Please install missing packages before running this script: ",
    paste(missing_packages, collapse = ", ")
  )
}

invisible(lapply(required_packages, library, character.only = TRUE))

set.seed(2026)

DATA_DIR <- "result"   # canonical processed dataset lives here (from 02)
RESULT_DIR <- "result" # matched datasets (intermediate, read by 04b)
FIG_DIR <- "figures"     # every manuscript figure
TAB_DIR <- "tables"      # figure source data + supplementary tables
for (d in c(RESULT_DIR, FIG_DIR, TAB_DIR)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# ---- 1. Helper functions -----------------------------------------------------

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

run_match <- function(data, output_file, treatment_formula, exact_formula,
                       outcome_var = "fncr_5_years_early") {
  # NOTE on a simplification: previously this function read a separate raw
  # PSM-input file and left_join()'d it against main_aux (columns pulled from
  # the main processed dataset), which could silently create suffixed
  # duplicate columns if the raw file already had any of those columns. Now
  # that both Matching 1 and Matching 2's inputs are derived directly from
  # the one canonical processed dataset (see Section 3 below), there is no
  # second file and no join -- this issue no longer applies.
  matched_data <- data %>%
    prepare_matching_data()

  m_out <- MatchIt::matchit(
    formula = treatment_formula,
    data = matched_data,
    method = "optimal",
    exact = exact_formula,
    estimand = "ATT"
  )

  matched <- MatchIt::match.data(m_out)

  # ---- Post-matching cleaning: drop missing-outcome and outlier subclasses ----
  #
  # Ported from the user's real matching-analysis notebook
  # (4_Y_Match_Impact_analysis_June.ipynb, the "2025 version" cells), and
  # matching the manuscript's own SI description ("...123,120 pairs after
  # excluding the pairs of extreme outliers (top 1% of citation scores with
  # extremely high citation scores)"). This was previously NOT implemented
  # anywhere in the R pipeline -- a real gap, not just a style choice.
  #
  # Two exclusion steps, both applied at the SUBCLASS (matched-pair) level
  # -- i.e., if either member of a matched pair triggers exclusion, the
  # WHOLE pair is dropped, not just that one row:
  #   1. Subclasses where the citation outcome is missing for any member.
  #   2. Subclasses where the citation outcome OR the co-author citation
  #      covariate (max_coa_fncr_5y_log) exceeds its own 99th percentile
  #      (computed AFTER step 1's exclusion).
  if (outcome_var %in% names(matched)) {
    missing_outcome_subclass <- matched %>%
      filter(is.na(.data[[outcome_var]])) %>%
      pull(subclass) %>%
      unique()

    matched <- matched %>% filter(!subclass %in% missing_outcome_subclass)

    outcome_p99 <- quantile(matched[[outcome_var]], 0.99, na.rm = TRUE)
    coauthor_p99 <- quantile(matched$max_coa_fncr_5y_log, 0.99, na.rm = TRUE)

    outlier_subclass <- matched %>%
      filter(.data[[outcome_var]] > outcome_p99 | max_coa_fncr_5y_log > coauthor_p99) %>%
      pull(subclass) %>%
      unique()

    matched <- matched %>% filter(!subclass %in% outlier_subclass)

    message(
      "  Dropped ", length(missing_outcome_subclass), " missing-outcome subclasses and ",
      length(outlier_subclass), " outlier subclasses (99th percentile cutoffs: ",
      outcome_var, " > ", round(outcome_p99, 3), ", max_coa_fncr_5y_log > ", round(coauthor_p99, 3), ")"
    )
  } else {
    warning(
      "'", outcome_var, "' not found in the matched data -- skipping missing-outcome/",
      "outlier exclusion. This step requires the citation outcome variable to be present; ",
      "if your main processed dataset doesn't include it yet, add it before matching."
    )
  }

  readr::write_csv(matched, file.path(RESULT_DIR, output_file))
  # NOTE on a fix: output_file sometimes already ends in ".csv" and sometimes
  # doesn't (an inconsistency inherited from the original naming); strip any
  # existing extension before adding "balance_...csv" so we never get a
  # doubled ".csv.csv" suffix.
  balance_stem <- sub("\\.csv$", "", output_file)
  readr::write_csv(as.data.frame(summary(m_out)$sum.matched), file.path(TAB_DIR, paste0("balance_", balance_stem, ".csv")))

  list(raw = matched_data, matched = matched, matchit_object = m_out)
}

# ---- 2. Balance diagnostic plots (Figs. S16, S20) ----------------------------

plot_balance_density <- function(raw, matched, title) {
  # Compares the distribution of key matching covariates between "All"
  # (pre-matching pool) and "Matched" (post-matching sample), the same
  # comparison MatchIt's own balance summary reports numerically -- this is
  # the visual companion to that numeric table (Tables S12/S13).
  categorical_vars <- c("gender", "cohort", "discipline_new", "Jr_Quantile",
                         "pub_before_cate", "colla_ctr_Y", "firstauthor_top_100")
  continuous_vars <- c("author_cnt", "max_coa_fncr_5y_log")

  make_categorical_panel <- function(var, show_legend = FALSE) {
    combined <- bind_rows(
      raw %>% transmute(value = as.character(.data[[var]]), panel = "All"),
      matched %>% transmute(value = as.character(.data[[var]]), panel = "Matched")
    )
    p <- ggplot(combined, aes(x = value, fill = panel)) +
      geom_bar(aes(y = after_stat(prop), group = panel), position = "dodge") +
      labs(title = var, x = NULL, y = NULL, fill = NULL) +
      theme_bw(base_size = 9) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    if (show_legend) {
      p + theme(legend.position = "bottom")
    } else {
      p + theme(legend.position = "none")
    }
  }

  make_continuous_panel <- function(var) {
    combined <- bind_rows(
      raw %>% transmute(value = .data[[var]], panel = "All"),
      matched %>% transmute(value = .data[[var]], panel = "Matched")
    )
    ggplot(combined, aes(x = value, colour = panel)) +
      geom_density() +
      labs(title = var, x = NULL, y = NULL, colour = NULL) +
      theme_bw(base_size = 9) +
      theme(legend.position = "none")
  }

  # Show the fill legend ("All" vs. "Matched") on just the first panel, so
  # readers have one clear key without needing a separate legend-extraction
  # step (cowplot::get_legend() is not reliable across ggplot2 versions).
  panels <- c(
    list(make_categorical_panel(categorical_vars[1], show_legend = TRUE)),
    lapply(categorical_vars[-1], make_categorical_panel),
    lapply(continuous_vars, make_continuous_panel)
  )

  wrap_plots(panels, ncol = 3) +
    plot_annotation(title = title) &
    theme(plot.title = element_text(size = 12, face = "bold"))
}

# Source data for Figs. S16/S20: the exact values drawn in each panel.
# Categorical covariates -> share of each category within "All" / "Matched"
# (the bar heights); continuous covariates -> the kernel-density curve
# (x, density), computed the same way as ggplot2::geom_density() (default
# bandwidth, 512 points over the observed range). This reproduces the figure
# without releasing individual-level records.
balance_source_data <- function(raw, matched) {
  categorical_vars <- c("gender", "cohort", "discipline_new", "Jr_Quantile",
                         "pub_before_cate", "colla_ctr_Y", "firstauthor_top_100")
  continuous_vars <- c("author_cnt", "max_coa_fncr_5y_log")
  samples <- list(All = raw, Matched = matched)

  categorical <- purrr::map_dfr(categorical_vars, function(var) {
    purrr::imap_dfr(samples, function(df, panel) {
      tibble(value = as.character(df[[var]])) %>%
        count(value, name = "n") %>%
        mutate(variable = var, variable_type = "categorical", panel = panel,
               proportion = n / sum(n))
    })
  })

  continuous <- purrr::map_dfr(continuous_vars, function(var) {
    purrr::imap_dfr(samples, function(df, panel) {
      x <- df[[var]][!is.na(df[[var]])]
      if (length(x) < 2) return(tibble())
      d <- stats::density(x, from = min(x), to = max(x), n = 512)
      tibble(variable = var, variable_type = "continuous", panel = panel,
             x = d$x, density = d$y, n = length(x))
    })
  })

  bind_rows(categorical, continuous) %>%
    select(variable, variable_type, panel, value, x, n, proportion, density)
}

# ---- 3. Compute the citation outcome (DNCS5 / fncr_5_years_early) -----------
#
# Matches the manuscript's DNCS5 definition: five-year citations divided by
# the mean five-year citations of all early-career researchers' first
# publications in the same discipline and publication year.
#
# CHANGE: previously this read a separate raw citation file
# (0_citation_processed_paper_newdiscipline.csv) and merged it onto the main
# processed dataset by doi_lower. Since the raw ingredients
# (cit_5_years, cit_3_years, discipline_average_5y_early,
# discipline_average_3y_early) are now included directly in the main
# processed dataset itself, no separate file or join is needed -- just the
# final division.

compute_dncs <- function(data) {
  data %>%
    mutate(
      fncr_5_years_early = cit_5_years / discipline_average_5y_early,
      # A publication with zero citations should have DNCS = 0 exactly,
      # regardless of the discipline-year average (avoids any division
      # artifacts and matches the original notebook's explicit override).
      fncr_5_years_early = ifelse(cit_5_years == 0, 0, fncr_5_years_early)
    )
}

# ---- 4. Derive matching inputs directly from the processed dataset ----------
#
# CHANGE: previously, Matching 1 and Matching 2's inputs were two separately
# provided raw files (1_author_12_16_psm_TW_No.csv,
# 1_author_12_16_psm_self_other.csv), each already containing a pre-built
# Type_int treatment column, which then had to be joined against the main
# processed dataset for the remaining covariates. Since both `with_tw` (did
# this publication receive any Twitter mention?) and `self_pro` (did the
# author self-promote it?) already exist as columns in the one canonical
# processed dataset, both matching-input datasets can be built directly from
# it instead -- removing the need for two separate raw files entirely.

main_data <- read_required_csv(file.path(
  DATA_DIR,
  "dataset_demo_processed.csv"
)) %>%
  compute_dncs()

# Matching 1: general Twitter mentions (with_tw == 1) vs. no mentions (with_tw == 0).
matching1_input <- main_data %>%
  filter(author_cnt <= 15) %>%
  # NOTE on a fix (carried over): MatchIt::matchit() requires complete cases
  # for every covariate in the treatment formula. most_ctr is one of those
  # covariates but is not guaranteed to be complete; without this filter,
  # matchit() errors out entirely rather than just excluding affected rows.
  filter(!is.na(most_ctr), most_ctr != "") %>%
  mutate(
    Type_int = with_tw,
    Type = ifelse(with_tw == 1, "withTW", "NO"),
    firstauthor_top_100 = ifelse(is.na(firstauthor_top_100), "0", as.character(firstauthor_top_100))
  )

# Matching 1 (robustness, Fig. S19): exactly one Twitter mention vs. no mentions.
# Same covariates and matching specification as Matching 1; only the treatment
# group is restricted to researchers whose first publication received exactly
# one Twitter mention.
matching1_onemention_input <- matching1_input %>%
  filter(with_tw == 0 | len_tweet == 1)

# Matching 2: self-promotion (self_pro == 1) vs. others'-promotion-only
# (self_pro == 0), restricted to researchers who received at least one
# mention in the first place (with_tw == 1) -- self-promotion is only a
# meaningful comparison among researchers who were mentioned at all.
matching2_input <- main_data %>%
  filter(with_tw == 1) %>%
  filter(author_cnt <= 15) %>%
  filter(!is.na(most_ctr), most_ctr != "") %>%
  mutate(
    Type_int = self_pro,
    Type = ifelse(self_pro == 1, "SelfOther", "OnlyOther"),
    firstauthor_top_100 = ifelse(is.na(firstauthor_top_100), "0", as.character(firstauthor_top_100))
  )

# ---- 5. Run matching ----------------------------------------------------------

result_twitter <- run_match(
  data = matching1_input,
  output_file = "2_match_psm_TW_No.csv",
  treatment_formula = Type_int ~ gender + pub_before_cate + cohort + discipline_new + Jr_Quantile +
    most_ctr + colla_ctr_Y + author_cnt + max_coa_fncr_5y_log + firstauthor_top_100,
  exact_formula = ~ gender + cohort + discipline_new + Jr_Quantile + firstauthor_top_100
)

result_twitter_onemention <- run_match(
  data = matching1_onemention_input,
  output_file = "2_match_psm_TW_No_onemention.csv",
  treatment_formula = Type_int ~ gender + pub_before_cate + cohort + discipline_new + Jr_Quantile +
    most_ctr + colla_ctr_Y + author_cnt + max_coa_fncr_5y_log + firstauthor_top_100,
  exact_formula = ~ gender + cohort + discipline_new + Jr_Quantile + firstauthor_top_100
)

result_self <- run_match(
  data = matching2_input,
  output_file = "2_match_psm_self_other.csv",
  treatment_formula = Type_int ~ gender + pub_before_cate + cohort + discipline_new + Jr_Quantile +
    most_ctr + colla_ctr_Y + len_tweet + author_cnt + max_coa_fncr_5y_log + firstauthor_top_100,
  exact_formula = ~ gender + cohort + discipline_new + Jr_Quantile + firstauthor_top_100
)

fig_s16 <- plot_balance_density(
  result_twitter$raw, result_twitter$matched,
  "Fig. S16: Balance -- Matching 1 (Twitter mentions vs. no mentions)"
)
ggsave(file.path(FIG_DIR, "fig_s16.pdf"), fig_s16, width = 12, height = 10)
readr::write_csv(balance_source_data(result_twitter$raw, result_twitter$matched), file.path(TAB_DIR, "fig_s16.csv"))

fig_s20 <- plot_balance_density(
  result_self$raw, result_self$matched,
  "Fig. S20: Balance -- Matching 2 (self-promotion vs. others' promotion)"
)
ggsave(file.path(FIG_DIR, "fig_s20.pdf"), fig_s20, width = 12, height = 10)
readr::write_csv(balance_source_data(result_self$raw, result_self$matched), file.path(TAB_DIR, "fig_s20.csv"))

message("Matching complete. Matched data -> ", RESULT_DIR, "; balance tables + fig_s16/fig_s20 source data -> ", TAB_DIR, "; Figs. S16/S20 -> ", FIG_DIR)
