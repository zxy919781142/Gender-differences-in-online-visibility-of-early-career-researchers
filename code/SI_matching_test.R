# -----------------------------------------------------------------------------
# 4_RQ3_Matching_test_March2024.R (cleaned)
#
# Manuscript: Gender differences in online visibility of early-career researchers
#
# Purpose: second-stage refinement of the already-matched PSM datasets
# produced by 04a_run_matching.R -- re-matches with additional exact-matching
# criteria and produces country/covariate balance summaries restricted to the
# top-20 countries.
#
# Required input (from 04a_run_matching.R -- run that script first):
#   2_result/2_match_psm_TW_No
#   2_result/2_match_psm_self_other.csv
#
# NOTE: this script's role in the manuscript is unclear to me -- the
# filename includes "_test_", and it doesn't obviously correspond to any
# figure/table we've identified so far (Figs. S16-S22, Tables S12-S13 are
# already covered by 04a/04b). Please confirm what this script's output
# (tables/table_s12.csv and tables/table_s13.csv = Supplementary Tables S12/S13,
# plus tables/matching_test_1_TW_balance_tests.csv) is used for before
# treating this as final -- see notes throughout for the specific ambiguities.
#
# CHANGES FROM THE ORIGINAL:
#   - Trimmed the library list from 24 to the 2 actually used (tidyverse,
#     MatchIt). The other 22 (sjPlot, lme4, betareg, DHARMa, etc.) were
#     loaded but never referenced anywhere in this script -- likely
#     copy-pasted from a larger master script. betareg was also loaded twice.
#   - Fixed a formula typo: `cohort + + discipline` (double "+", from a
#     deleted term) in all three matchit() calls. R silently tolerates this
#     (parses as unary plus), so it wasn't a functional bug, just untidy --
#     cleaned to `cohort + discipline_new`.
#   - Replaced hardcoded Windows absolute paths with relative paths.
#   - Removed `nt.out0` (matched without exact criteria): fit but never
#     used downstream -- only `nt.out1`'s summary/plots/tests are used. If
#     you want a with/without-exact-matching comparison, say so and I'll
#     add it back deliberately rather than as an orphaned leftover.
#   - `tw.variables.con` (continuous covariates to balance-test) was defined
#     but never actually used -- the loop below only tested categorical
#     variables via chi-square. Added a Wilcoxon rank-sum test for the
#     continuous variable(s), matching the apparent original intent. If you
#     intended something else here, let me know.
#   - Renamed `ctr_top10` to `ctr_top20`, since it actually lists 20country
#     codes, not 10 -- the old name was misleading.
#   - Removed extensive commented-out dead code (old file paths, disabled
#     `method`/`exact`/`distance` arguments) while keeping anything
#     genuinely informative as an actual comment.
#
# ONE THING TO CONFIRM: the original commented-out file reads used
# `mutate_all(na_if, "Null")` (converting literal "Null" strings to NA), but
# this was dropped when the file paths were updated to the 2025 versions,
# with no explanation. Is this intentional (the 2025 files no longer contain
# "Null" strings), or should this be restored? I've left it out, matching
# your most recent code, but flagging this explicitly since it changes which
# rows get treated as missing.
# -----------------------------------------------------------------------------

library(tidyverse)
library(MatchIt)

data_dir <- "2_result"  # 04a_run_matching.R's output lives here
table_dir <- "tables"   # every table this script displays is also written here

dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

data_processing_match <- function(data) {
  data$cohort <- factor(data$cohort)
  data$academic_age <- factor(data$academic_age)
  data$self_pro <- factor(data$self_pro)

  # Standardize continuous covariates (not appropriate to leave unscaled
  # for the matching-distance calculation). Not every input file has every
  # column (e.g. data_psm_NT lacks len_tweet, since it's not used as a
  # covariate for that comparison) -- only scale columns that are actually
  # present, the same guard the original code already used for paper_3y.
  # NOTE on a fix: tw_start_year/cum_tw_year were scaled here but never
  # actually used anywhere else in this script (not in either matchit()
  # formula, not in any balance test or summary) -- removed as dead work.
  candidate_scale_cols <- c("author_cnt", "len_tweet", "paper_3y")
  scale_cols <- candidate_scale_cols[candidate_scale_cols %in% colnames(data)]
  data[, scale_cols] <- scale(data[, scale_cols])

  data$Jr_Quantile <- relevel(factor(data$Jr_Quantile), ref = "Q4")
  data$cohort <- relevel(factor(data$cohort), ref = "2012")
  data$colla_ctr_Y <- relevel(factor(data$colla_ctr_Y), ref = "N")
  data$colla_aff_Y <- relevel(factor(data$colla_aff_Y), ref = "N")

  data
}

# ---- 1. Load the (already-matched, upstream) PSM datasets --------------------
#
# NOTE on a fix: 04a_run_matching.R's own match.data() call already added
# distance/weights/subclass columns from ITS matching stage. This script
# re-matches on top of that (a second matchit() call), which would collide
# with those leftover columns -- match.data() refuses to overwrite an
# existing "distance" column. Drop the first stage's matching columns before
# re-matching; they're specific to that earlier match, not this one.

data_psm_NT <- read.csv(file.path(data_dir, "2_match_psm_TW_No")) %>%
  select(-any_of(c("distance", "weights", "subclass"))) %>%
  filter(author_cnt <= 15) %>%
  data_processing_match()

data_psm <- read.csv(file.path(data_dir, "2_match_psm_self_other.csv")) %>%
  select(-any_of(c("distance", "weights", "subclass"))) %>%
  filter(author_cnt <= 15) %>%
  data_processing_match()

# ---- 2. Re-match: general Twitter mentions vs. no mentions -------------------

nt.out1 <- matchit(
  Type_int ~ gender + academic_age + cohort + discipline_new + Jr_Quantile +
    most_ctr + colla_ctr_Y + author_cnt + firstauthor_top_100,
  data = data.frame(data_psm_NT),
  exact = ~ gender + cohort + discipline_new + Jr_Quantile + firstauthor_top_100,
  estimand = "ATT"
)

match_summary <- summary(nt.out1)

substrings_to_match <- c("gender", "cohort", "discipline_new",
                          "Jr_Quantile", "academic_age", "colla_ctr_Y", "author_cnt", "most_ctr", "firstauthor_top_100")
ctr_top20 <- c("chn", "usa", "gbr", "kor", "jpn", "ind", "bra", "deu", "can", "aus", "fra",
               "esp", "irn", "ita", "nld", "tur", "rus", "mys", "che", "swe")

subset_summary <- match_summary$sum.matched[grepl(paste(substrings_to_match, collapse = "|"), rownames(match_summary$sum.matched)), ]
subset_summary_ctr <- subset_summary[grepl(paste(ctr_top20, collapse = "|"), rownames(subset_summary)), ]
subset_summary_ctr <- subset_summary_ctr[grepl("_ctr", rownames(subset_summary_ctr)), ]
subset_summary_final <- rbind(subset_summary[!grepl("_ctr", rownames(subset_summary)), ], subset_summary_ctr)

# Table S12: balance test for Matching 1 (top-20 countries shown)
write.csv(subset_summary_final, file.path(table_dir, "table_s12.csv"))

plot(nt.out1, type = "jitter", interactive = FALSE)
plot(nt.out1, type = "density", interactive = FALSE, which.xs = ~ firstauthor_top_100 + author_cnt)

# ---- 3. Balance tests on the matched sample -----------------------------------

data.nt <- match.data(nt.out1)
tw.variables.cat <- c("academic_age", "most_ctr", "colla_ctr_Y")
tw.variables.con <- c("author_cnt")

# Collect every balance test (printed below) into one table, so the printed
# results are also saved: tables/matching_test_1_TW_balance_tests.csv
balance_tests <- list()

for (i in tw.variables.cat) {
  cat(i, "\n")
  test <- chisq.test(table(data.nt$Type, data.nt[[i]]))
  print(test)
  balance_tests[[i]] <- data.frame(
    variable = i, test = "Pearson chi-square", statistic = unname(test$statistic),
    df = unname(test$parameter), p_value = test$p.value
  )
  cat("******************\n")
}

# NOTE on a fix: tw.variables.con was defined but never actually tested --
# the loop above only covered categorical variables. Added the natural
# continuous-variable equivalent (Wilcoxon rank-sum test) here; confirm
# this matches your intent.
for (i in tw.variables.con) {
  cat(i, "\n")
  test <- wilcox.test(data.nt[[i]] ~ data.nt$Type)
  print(test)
  balance_tests[[i]] <- data.frame(
    variable = i, test = "Wilcoxon rank-sum", statistic = unname(test$statistic),
    df = NA_real_, p_value = test$p.value
  )
  cat("******************\n")
}

write.csv(do.call(rbind, balance_tests), file.path(table_dir, "matching_test_1_TW_balance_tests.csv"), row.names = FALSE)

# ---- 4. Re-match: self-promotion vs. others'-promotion-only ------------------

m.out0 <- matchit(
  Type_int ~ gender + academic_age + cohort + discipline_new + Jr_Quantile +
    most_ctr + colla_ctr_Y + len_tweet + author_cnt + firstauthor_top_100,
  data = data_psm,
  exact = ~ gender + cohort + discipline_new + Jr_Quantile + firstauthor_top_100,
  estimand = "ATT"
)

plot(m.out0, type = "density", interactive = FALSE, which.xs = ~ firstauthor_top_100)

substrings_to_match2 <- c("gender", "cohort", "discipline_new",
                           "Jr_Quantile", "academic_age", "colla_ctr_Y", "author_cnt", "len_tweet", "firstauthor_top_100", "most_ctr")
match2_summary <- summary(m.out0)

subset2_summary <- match2_summary$sum.matched[grepl(paste(substrings_to_match2, collapse = "|"), rownames(match2_summary$sum.matched)), ]
subset2_summary_ctr <- subset2_summary[grepl(paste(ctr_top20, collapse = "|"), rownames(subset2_summary)), ]
subset2_summary_ctr <- subset2_summary_ctr[grepl("_ctr", rownames(subset2_summary_ctr)), ]
subset2_summary_final <- rbind(subset2_summary[!grepl("_ctr", rownames(subset2_summary)), ], subset2_summary_ctr)

# Table S13: balance test for Matching 2 (top-20 countries shown)
write.csv(subset2_summary_final, file.path(table_dir, "table_s13.csv"))
