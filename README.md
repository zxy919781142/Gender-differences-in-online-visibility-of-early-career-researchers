# Gender Differences in Online Visibility of Early-Career Researchers

## Replication materials

This repository contains the replication materials for the manuscript:

> **Gender differences in online visibility of early-career researchers**
>
> Zhao, X., Akbaritabar, A., Kashyap, R., & Zagheni, E.

It provides de-identified demonstration data, the source data for every figure
and table, and the full code (R scripts and Python notebooks) used to produce
all main and supplementary figures and tables.

**Maintainer:** Xinyi Zhao  
**ORCID:** 0000-0002-2552-7795  
**Affiliations:** Max Planck Institute for Demographic Research; Leverhulme Centre for Demographic Science, Department of Sociology, University of Oxford  
**Website:** https://www.demogr.mpg.de/en/about_us_6113/staff_directory_1899/xinyi_zhao_4083/  
**Email:** zhao@demogr.mpg.de; xinyi.zhao@st-hughs.ox.ac.uk

---

## Repository structure

```text
.
├── code/
│   ├── data_preparation/
│   │   ├── 1_Data_Processing_new.ipynb       # early-career sample; Altmetric and Twitter data
│   │   └── 2_self_promotion_new.ipynb        # author-tweeter name matching (self-promotion)
│   ├── 01_correlation.R                      # Fig. 1; Figs. S1-S3, S5-S7; Tables S3-S6
│   ├── 02_online_mentions_models_figure.R    # Fig. 2; Figs. S10-S11; Tables S7-S8
│   ├── 03_self_promotion_models_figures.R    # Fig. 3; Figs. S12-S15; Table S10
│   ├── 04a_run_matching.R                    # propensity-score matching; Figs. S16, S20
│   ├── 04b_matching_regression_figures.R     # Fig. 4; Figs. S17-S19, S21-S22; Tables S14-S15
│   ├── SI_matching_test.R                    # Tables S12-S13 (balance tests)
│   ├── SI_robustness_gpt_validation.ipynb    # validation of the name-matching thresholds (Fig. S8 input)
│   ├── SI_robustness_S8_S9.R                 # Figs. S8-S9
│   ├── SI_Tweet_Text_cleaned.ipynb           # Figs. S23-S26; Tables S16-S17
│   └── py/                                   # sentiment classifiers used by SI_Tweet_Text_cleaned.ipynb
│
├── data/                                     # input data (de-identified demo versions)
├── result/                                   # intermediate files created by the code
├── figures/                                  # all figures (PDF); figures/png/ holds previews
├── tables/                                   # source data of all figures + all tables (CSV)
└── README.md
```

---

## Data

The original data cannot be redistributed:

- **Scopus** bibliometric data were obtained through the German Competence
  Network for Bibliometrics and are subject to commercial licensing.
- **Altmetric** data were obtained under institutional licences held by the
  Max Planck Institute for Demographic Research and the University of Oxford.
- **Twitter/X** data (tweets, display names, handles) were collected through the
  Twitter Academic API before April 2023 and cannot be shared under the
  platform's terms of service.

The `data/` folder therefore contains de-identified demonstration data that
follow the structure used in the analysis: author identifiers and DOIs are
replaced by random values (DOIs take the form `10.1000/anon…`). Results
obtained from the demo data differ from those reported in the manuscript.
The values shown in the published figures and tables are provided in
`tables/` (see *Outputs* below).

| File | Content | Used by |
|---|---|---|
| `0_paper_full_sample.csv` | publications of a sample of authors (author, gender, discipline, DOI, publication year) | `1_Data_Processing_new.ipynb` |
| `00_openalex_tweeterid_name_processed.pkl` | OpenAlex author names linked to Twitter user IDs (Mongeon, Bowman & Costas, 2023) | `2_self_promotion_new.ipynb` |
| `1_dataset_demo.csv` | author-level analysis dataset (random sample of early-career researchers) | `02`, `03` |
| `3_corr.csv`, `3_corr_agg.csv` | country-level numbers of female and male researchers (all, online-visible, self-promoting), by cohort and pooled, with the Gender Inequality Index | `01_correlation.R` |
| `4_TW_text_demo.csv` | sample of tweets mentioning the publications | `SI_Tweet_Text_cleaned.ipynb` |
| `5_gender_ratio_threshold.csv` | female-to-male ratio of self-promoting researchers for each pair of name-similarity thresholds | `SI_robustness_S8_S9.R` |

---

## How to reproduce the analyses

Run all scripts **from the repository root**, e.g. `source("code/01_correlation.R")`
in R, or open Jupyter in the repository root. Output folders are created
automatically. Fitted models are cached in `result/` (`*.rds`); delete them to
refit the models.

### Step 0 (optional). Data preparation -- Python

These notebooks document how the analysis data were built. They require API
access (Altmetric, Twitter) and real author and Twitter names, and are **not
needed** to reproduce the figures from the demo data.

| Notebook | Input | Output |
|---|---|---|
| `data_preparation/1_Data_Processing_new.ipynb` | `data/0_paper_full_sample.csv` | `result/0_author_doi_3y_2012_2016_sample.csv`, `result/1_doi_3y_2012_2016_tweet_sample.pkl` |
| `data_preparation/2_self_promotion_new.ipynb` | the two outputs above; `data/00_openalex_tweeterid_name_processed.pkl` | `result/2_self_promotion_scores_sample.csv` |

The first notebook selects each author's first publication in 2012--2016 and
their publications in the first three career years, and queries the Altmetric
API for original tweets. The second compares author names with the names and
handles of the Twitter users who tweeted each publication and computes
name-similarity scores (thresholds: 0.6 in general, 0.8 for Chinese names).
The resulting self-promotion indicator is part of `data/1_dataset_demo.csv`.

API keys are read from environment variables (`ALTMETRIC_API_KEY`,
`TWITTER_BEARER_TOKEN`, `TWITTER_API_KEY`, `TWITTER_API_SECRET`,
`TWITTER_ACCESS_TOKEN`, `TWITTER_ACCESS_SECRET`) and are never stored in the code.

### Step 1. Country-level correlations -- `01_correlation.R`

- **Input:** `data/3_corr.csv`, `data/3_corr_agg.csv`
- **Figures:** `fig_1`, `fig_s1`, `fig_s2`, `fig_s3`, `fig_s5`, `fig_s6`, `fig_s7`
- **Tables:** their source data; `table_s3`--`table_s6`

### Step 2. Online-visibility models -- `02_online_mentions_models_figure.R`

Zero-inflated negative binomial models of Twitter mention counts (Models 0--9).

- **Input:** `data/1_dataset_demo.csv`
- **Intermediate output:** `result/dataset_demo_processed.csv` (used in Step 4)
- **Figures:** `fig_2ab`, `fig_2c`, `fig_s10`, `fig_s11`
- **Tables:** their source data; `table_s7`, `table_s8`

On the small demo sample, some models cannot be estimated by REML; they are
then refitted by maximum likelihood and a message is printed.

### Step 3. Self-promotion models -- `03_self_promotion_models_figures.R`

Logistic models of self-promotion (Models 0--10) and of promotion by co-authors
and official accounts.

- **Input:** `data/1_dataset_demo.csv`
- **Figures:** `fig_3ab`, `fig_3c`, `fig_s12`--`fig_s15`
- **Tables:** their source data; `table_s10`

### Step 4. Matching and citation impact -- `04a_run_matching.R`, then `04b_matching_regression_figures.R`

`04a` runs the propensity-score matching (*Matching 1*: Twitter mentions vs.
none; exactly one mention vs. none for the robustness check in Fig. S19;
*Matching 2*: self-promotion vs. mentions by others only). `04b` estimates the
association of online visibility and self-promotion with five-year
discipline-normalized citation scores (DNCS⁵) on the matched samples.

- **Input (04a):** `result/dataset_demo_processed.csv` (from Step 2)
- **Intermediate output (04a):** `result/2_match_psm_TW_No.csv`,
  `result/2_match_psm_TW_No_onemention.csv`, `result/2_match_psm_self_other.csv`
- **Figures:** `fig_s16`, `fig_s20` (04a); `fig_4`, `fig_s17`--`fig_s19`, `fig_s21`, `fig_s22` (04b)
- **Tables:** their source data; `table_s14`, `table_s15`

### Step 5. Balance tests -- `SI_matching_test.R`

- **Input:** the matched samples from Step 4
- **Tables:** `table_s12`, `table_s13`

### Step 6. Robustness of the name-matching thresholds -- Figs. S8--S9

1. `SI_robustness_gpt_validation.ipynb` -- an LLM judges 100 borderline
   author-tweeter name matches, disagreements with the algorithm are reviewed
   manually, and precision, recall and F1 are computed for each pair of
   thresholds. This step uses real author and Twitter names and an API key
   (`GWDG_API_KEY`); its input sample is therefore not shared. Its output,
   `result/5_name_match_validation_sample_testresult.csv`, is provided.
2. `SI_robustness_S8_S9.R` -- draws Figs. S8 and S9.
   - **Input:** `result/5_name_match_validation_sample_testresult.csv`,
     `data/5_gender_ratio_threshold.csv`
   - **Figures:** `fig_s8`, `fig_s9`; **Tables:** their source data

### Step 7. Tweet text analysis -- `SI_Tweet_Text_cleaned.ipynb`

Sentiment of tweets (majority vote of three classifiers), word use and tags.

- **Input:** `data/4_TW_text_demo.csv`; classifiers in `code/py/`
- **Figures:** `fig_s23_female`, `fig_s23_male`, `fig_s24`, `fig_s25_female`, `fig_s25_male`, `fig_s26`
- **Tables:** their source data; `table_s16`, `table_s17`

Requires internet access to download the BERTweet model (Hugging Face) and
NLTK corpora.

---

## Outputs

Every figure in `figures/` has a source-data table with the same name in
`tables/`, containing the values plotted (e.g. `figures/fig_2ab.pdf` and
`tables/fig_2ab.csv`). Panels combined in one figure file are identified by a
`fig_panel` column. Figures S2/S6 and S3/S7 are drawn from the same data and
share one table each (`fig_s2_s6.csv`, `fig_s3_s7.csv`). Fig. S4 is a schematic
of the name-matching workflow and has no source data.

All tables of the manuscript and Supplementary Information are provided in
`tables/` as `table_1.csv` and `table_s1.csv`--`table_s17.csv`. Tables S3--S8,
S10 and S12--S17 are written by the code; Tables 1, S1, S2a, S2b, S9 and S11
are provided as they appear in the manuscript.

The same files are provided as the *Source Data* file of the article.

---

## Main figures

### Figure 1
Cross-national association between the female-to-male ratios of all
early-career researchers and those of (a) online-visible and (b)
self-promoting researchers. Produced by `01_correlation.R`.

![](./figures/png/fig_1.png)

### Figure 2
Predicted counts of Twitter mentions of early-career female and male
researchers' first publications: overall, and by cohort, previous
publications, journal rank and discipline. Produced by
`02_online_mentions_models_figure.R`.

![](./figures/png/fig_2ab.png)
![](./figures/png/fig_2c.png)

### Figure 3
Predicted probabilities of early-career female and male researchers
self-promoting their first publications: overall, and by cohort, previous
publications, journal rank and discipline. Produced by
`03_self_promotion_models_figures.R`.

![](./figures/png/fig_3ab.png)
![](./figures/png/fig_3c.png)

### Figure 4
Average marginal effects of online visibility and self-promotion on the
five-year cumulative discipline-normalized citation scores (DNCS⁵) of
early-career female and male researchers. Produced by
`04b_matching_regression_figures.R`.

![](./figures/png/fig_4.png)

---

## Software requirements

### R

```r
c(
  "tidyverse", "dplyr", "tidyr", "readr", "purrr", "forcats", "stringr",
  "ggplot2", "ggrepel", "ggtext", "cowplot", "patchwork", "scales",
  "countrycode", "broom", "broom.mixed", "glmmTMB", "effectsize",
  "MatchIt", "optmatch", "sandwich", "lmtest"
)
```

`optmatch` is required for the optimal pair matching in `04a_run_matching.R`.
The package `ggpattern` is optional; without it, Figure 4 is drawn without
hatching.

### Python

`pandas`, `numpy`, `matplotlib`, `scipy`, `scikit-learn`, `nltk`,
`transformers`, `torch`, `wordcloud`, `textblob`, `langid`, `translate`,
and `openai` (only for `SI_robustness_gpt_validation.ipynb`).

---

## Citation

If you use these replication materials, please cite the associated manuscript:

Zhao, X., Akbaritabar, A., Kashyap, R., & Zagheni, E. *Gender differences in
online visibility of early-career researchers*.

Please also cite the archived repository:
**Zhao, X., Akbaritabar, A., Kashyap, R., & Zagheni, E. (2026). Replication
materials for Gender differences in online visibility of early-career
researchers. Zenodo. https://doi.org/10.5281/zenodo.20773808**
