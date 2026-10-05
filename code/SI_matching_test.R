# -----------------------------------------------------------------------------
# SI_matching_test.R
#
# Manuscript: Gender differences in online visibility of early-career researchers
#
# Purpose:
#   Produce the covariate-balance tables for the two propensity-score matchings
#   (Supplementary Tables S12 and S13). The matched samples from
#   04a_run_matching.R are re-matched with additional exact-matching criteria,
#   and balance statistics (means in treatment and control groups, standardized
#   mean differences, variance ratios, eCDF statistics) are summarised for the
#   matching covariates, with country shown for the top-20 countries.
#
# Required input (from 04a_run_matching.R -- run that script first):
#   result/2_match_psm_TW_No           (Matching 1: Twitter mentions vs. none)
#   result/2_match_psm_self_other.csv  (Matching 2: self-promotion vs. others only)
#
# Outputs:
#   tables/table_s12.csv   (Table S12: balance test, Matching 1)
#   tables/table_s13.csv   (Table S13: balance test, Matching 2)
#   tables/matching_test_1_TW_balance_tests.csv
#       (chi-square / Wilcoxon balance tests for Matching 1, printed below)
# -----------------------------------------------------------------------------

library(tidyverse)
library(MatchIt)

data_dir <- "result"  # 04a_run_matching.R's output lives here
table_dir <- "tables"   # every table this script displays is also written here

dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

data_processing_match <- function(data) {
  data$cohort <- factor(data$cohort)
  data$academic_age <- factor(data$academic_age)
  data$self_pro <- factor(data$self_pro)

  # Standardize continuous covariates for the matching-distance calculation
  # (only those present in the input; e.g. Matching 1 has no len_tweet).
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
# The matched files from 04a_run_matching.R contain the distance/weights/
# subclass columns of the first matching; they are dropped before re-matching.

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

# Wilcoxon rank-sum tests for the continuous covariates.
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
