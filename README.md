# Gender Differences in Online Visibility of Early-Career Researchers

## Replication materials

This repository contains replication materials for the manuscript:

> **Gender differences in online visibility of early-career researchers**

The repository provides demonstration/replication data and R scripts to reproduce the main analyses, tables, and figures in the manuscript.

**Maintainer:** Xinyi Zhao  
**ORCID:** 0000-0002-2552-7795  
**Affiliations:** Max Planck Institute for Demographic Research; Leverhulme Centre for Demographic Science, Department of Sociology, University of Oxford  
**Website:** https://www.demogr.mpg.de/en/about_us_6113/staff_directory_1899/xinyi_zhao_4083/  
**Email:** zhao@demogr.mpg.de; xinyi.zhao@st-hughs.ox.ac.uk

---

## Repository structure

```text
.
├── Data/
│   ├── 01_dataset.csv
│   ├── 00_field_discipline_OECD_author.csv
│   ├── corr.csv
│   ├── corr_agg.csv
│   ├── 1_sample_2_testresult.csv
│   ├── 2_gender_ratio_threshold.csv
│   ├── 1_author_12_16_psm_TW_No.csv
│   ├── 1_author_12_16_psm_self_other.csv
│   ├── 2_match_psm_TW_No
│   └── 2_match_psm_self_other.csv
│
├── Result/
│   ├── Figures/
│   ├── Tables/
│   └── Models/
│
├── 00_prepare_replication_data.R
├── 01_correlation.R
├── 03_online_mentions_models_figures.R
├── 04_self_promotion_models_figures.R
├── 05_matching_citation_analysis.R
└── README.md
```

---

## Demo data

The original bibliometric data are subject to licensing restrictions. To support reproducibility, this repository provides demonstration data that follow the analytical structure used in the manuscript. Author identifiers and DOIs are anonymised or replaced with synthetic values.

---

## How to reproduce the analyses

Run the scripts from the repository root in the following order.

### 1. Prepare replication data

```r
source("00_prepare_replication_data.R")
```

**Inputs**

- `Data/01_dataset.csv`
- `Data/00_field_discipline_OECD_author.csv`

**Outputs**

- `Result/01_dataset_processed.csv`
- `Result/table1_cohort_by_gender.csv`
- `Result/table1_continuous_variables.csv`
- `Result/top20_countries.csv`
- `Result/countries_with_at_least_1000_researchers.csv`

---

### 2. Country-level correlation analyses

```r
source("01_correlation.R")
```

This script reproduces **Main Figure 1** and Supplementary Figures **S1-S7**.

**Inputs**

- `Data/corr.csv`
- `Data/corr_agg.csv`
- `Data/1_sample_2_testresult.csv`
- `Data/2_gender_ratio_threshold.csv`

**Outputs**

- `Result/Figures/`
- `Result/Tables/`

---

### 3. Online-visibility models and figures

```r
source("03_online_mentions_models_figures.R")
```

This script reproduces the online-visibility analyses for Twitter/X mentions, including **Main Figure 2** and Supplementary Figures **S10-S11**.

**Input**

- `Result/01_dataset_processed.csv`

**Optional input**

- `Data/00_field_discipline_OECD_author.csv`

**Outputs**

- `Result/Figures/Fig2_ab_online_mentions.pdf`
- `Result/Figures/Fig2_c_online_mentions_discipline.pdf`
- `Result/Figures/Fig_S10_online_mentions_by_model.pdf`
- `Result/Figures/Fig_S11_online_mentions_by_country.pdf`
- `Result/Tables/online_mentions_model_predictions.csv`
- `Result/Tables/online_mentions_subgroup_marginal_effects.csv`
- `Result/Tables/online_mentions_country_predictions.csv`
- `Result/Tables/online_mentions_zinb_coefficients.csv`
- `Result/Tables/online_mentions_zero_inflation_OR.csv`
- `Result/Tables/online_mentions_count_component_IRR.csv`
- `Result/Models/online_mentions_zinb_models.rds`

---

### 4. Self-promotion models and figures

```r
source("04_self_promotion_models_figures.R")
```

This script reproduces the self-promotion analyses, including **Main Figure 3** and Supplementary Figures **S12-S13**.

**Input**

- `Result/5_author_12_16_f_fauthor_oct2024_robust_coauthor_soc_coaCiteAge_processedR_2025.csv`

**Optional input**

- `Data/00_field_discipline_OECD_author.csv`

**Outputs**

- `Result/Figures/Fig3_ab_self_promotion.pdf`
- `Result/Figures/Fig3_c_self_promotion_discipline.pdf`
- `Result/Figures/Fig_S12_self_promotion_by_model.pdf`
- `Result/Figures/Fig_S13_self_promotion_by_country.pdf`
- `Result/Tables/self_promotion_model_predictions.csv`
- `Result/Tables/self_promotion_subgroup_marginal_effects.csv`
- `Result/Tables/self_promotion_country_predictions.csv`
- `Result/Tables/self_promotion_logistic_coefficients_OR.csv`
- `Result/Models/self_promotion_logistic_models.rds`

---

### 5. Matching and citation-impact analyses

```r
source("05_matching_citation_analysis.R")
```

This script reproduces the matching-based citation-impact analyses, including **Main Figure 4** and Supplementary Figures **S17-S18** and **S21-S22**.

By default, `RUN_MATCHING <- FALSE`, so the script uses the already matched datasets. Set `RUN_MATCHING <- TRUE` inside the script only if you want to rerun the propensity-score matching step from the unmatched input files.

**Main inputs**

- `Data/1_author_12_16_psm_TW_No.csv`
- `Data/1_author_12_16_psm_self_other.csv`
- `Data/2_match_psm_TW_No`
- `Data/2_match_psm_self_other.csv`

**Outputs**

- `Result/Figures/Fig4_matching_citation_impact.pdf`
- `Result/Figures/Fig4_matching_citation_impact.png`
- `Result/Figures/Fig_S17_twitter_mentions_subgroups.pdf`
- `Result/Figures/Fig_S18_twitter_mentions_discipline.pdf`
- `Result/Figures/Fig_S21_self_promotion_subgroups.pdf`
- `Result/Figures/Fig_S22_self_promotion_discipline.pdf`
- `Result/Tables/Fig4a_matching1_twitter_overall_AMEs.csv`
- `Result/Tables/Fig4b_matching2_self_overall_AMEs.csv`
- `Result/Tables/Fig_S17_S18_twitter_AMEs_without_coauthor_citations.csv`
- `Result/Tables/Fig_S17_S18_twitter_AMEs_with_coauthor_citations.csv`
- `Result/Tables/Fig_S21_S22_self_AMEs_without_coauthor_citations.csv`
- `Result/Tables/Fig_S21_S22_self_AMEs_with_coauthor_citations.csv`
- `Result/Models/matching1_twitter_lm_models.rds`
- `Result/Models/matching2_self_promotion_lm_models.rds`

---

## Main manuscript figures

The current manuscript contains four main figures.

### Figure 1

Cross-national association between the gender female-to-male ratios of all early-career researchers and the gender ratios of:

1. online-visible researchers; and  
2. self-promoting researchers.

Produced by: `01_correlation.R`
![](./2_result/png/fig1.png)

### Figure 2

Predicted counts of Twitter/X mentions on early-career female and male researchers' first publications, overall, by cohort, previous publications, journal rank, and discipline.

Produced by: `03_online_mentions_models_figures.R`
![](./2_result/png/fig2_ab.png)
![](./2_result/png/fig2_c.png)

### Figure 3

Predicted probabilities of early-career female and male researchers self-promoting their first publications, overall, by cohort, previous publications, journal rank, and discipline.

Produced by: `04_self_promotion_models_figures.R`
![](./2_result/png/fig3_ab.png)
![](./2_result/png/fig3_c.png)

### Figure 4

Average marginal effects of online visibility and self-promotion on five-year cumulative discipline-normalized citation scores among early-career female and male researchers.

Produced by: `05_matching_citation_analysis.R`
![](./2_result/png/fig4.png)

---

## R package requirements

The scripts install or check most required packages automatically. Across the full replication pipeline, the following R packages are used:

```r
c(
  "dplyr", "readr", "tidyr", "forcats", "stringr", "purrr",
  "ggplot2", "ggrepel", "countrycode", "broom", "patchwork", "scales",
  "glmmTMB", "sjPlot", "ggeffects", "ggtext", "MatchIt", "sandwich",
  "lmtest", "effectsize"
)
```

The package `ggpattern` is optional. If it is not installed, Figure 4 will be drawn without hatching.

---

## Notes on paths

All scripts use relative paths and should be run from the repository root. Input files should be stored in `Data/`. Outputs are written to `Result/`, especially `Result/Figures/`, `Result/Tables/`, and `Result/Models/`.

---

## Citation

If you use these replication materials, please cite the corresponding manuscript:

Zhao, X., Akbaritabar, A., Kashyap, R., & Zagheni, E. *Gender differences in online visibility of early-career researchers*.
