> Code for: [*Highway paving increased dengue transmission in the Peruvian Amazon.*](https://www.medrxiv.org/content/10.1101/2024.11.15.24317406v1) Preprint, 2024. Code written by Alyson Singleton and TJ Sipin.
>
> Please feel free to email me with any specific, code-related questions at [asinglet\@stanford.edu](mailto:asinglet@stanford.edu){.email}.

# Requirements and timing

All analyses were run in the R (version 4.4.2) programming language or in Python via Google Colab (which provides a ready-to-use Python 3 environment). Installing R usually takes \~30 minutes. Installing the necessary R packages takes usually 1-2 minutes per package, unless packages are installed from source which takes longer. GEE/Colab setup should take \~15 minutes (authentication + package install in notebook). Operating system macOS 14.5 (Sonoma) on a standard laptop is sufficient (no special hardware required). Also expected to work on Windows 11.

# Installation Guide

1)  Install R (<https://cran.r-project.org/>).
2)  Make a Google Earth Engine account (<https://earthengine.google.com/signup/>).
3)  Open Colab notebooks (.ipynb) in this repository.
    -   The first time you run it, you’ll be prompted to:
        -   Authenticate your Google account
        -   Grant Google Earth Engine access
    -   All required Python packages will be installed automatically by the first notebook cell.
    -   You can save outputs directly to your Google Drive and then add to the data/environmental_data folder.

# Data processing and analysis pipeline

## Download/extract data

#### Remotely-sensed environmental data

1)  Download the spatial data located in the data/spatial_data folder and upload to your GEE assests.
2)  Download all environmental covariates and highway buffer files (example output in data/environmental_data folder).
    -   [00_IOH_and_dengue_GEE_downloads.ipynb](https://colab.research.google.com/drive/1NuFvsgjjnNCU4ZElD4kMuuzfHIACS-fD?usp=sharing)

#### Clustering

3)  Identify spatial groups for clustering standard errors.
    -   01_clustering.R

#### DIRESA data

4)  Clean and do manual name matching on DIRESA-provided population data. Output provided in data/diresa_data folder.
    -   02_clean_diresa_pop_data.R
5)  Link DIRESA data to WorldPop data and use WorldPop to impute years with missing DIRESA data.
    -   03_impute_missing_pop_data.R
6)  Process DIRESA dengue and leishmaniasis case data.
    -   04_process_diresa_case_data.R

## Merge data

7)  Link cases with covariates and build final panel datasets.
    -   05_link_cases_w_covariates.R
    -   06_construct_final_panel_data.R

## Fit regressions and run senstivity analyses

8)  Run main models with fixest and glmmTMB packages.
    -   07_main_models_fixest.R
    -   08_main_models_glmmTMB.R
    -   09_percent_change_attributable_cases.R
    -   10_distance_het_models_and_bootstrapping.R
9)  Fit supplementary models for sensitivity analyses.
    -   11_robustness_checks_models.R
10) Conduct permutation inference analysis.
    -   12_permutation_inference_analysis.R

## Make figures and tables

11) Main text
    -   13_fig1_map_incidence.R
    -   14_fig2_main.R
    -   15_fig3_distance_het.R
    -   16_table1_main.R

<!-- -->

12) Supplementary information
    -   17_stable1_yearly_estimates.R
    -   18_stable2_general_robustness.R
    -   19_stable3_precip_quad.R
    -   20_stable4_cost_mapping.R
    -   21_stable5_FE_vals.R
    -   22_stable6_main_glmmTMB.R
    -   23_stable7_dengue_yearly_glmmTMB.R
    -   24_sfig1_unit_coverage.R
    -   25_sfig2_region_map.R
    -   26_sfig3_general_robustness.R
    -   27_sfig4_tx_year.R
    -   28_sfig5_permutation.R
    -   29_sfig6_time_trends.R
    -   30_sfig7_cluster_map.R
    -   31_sfig8_traffic_vols.R
    -   32_sfig9_season_def.R
    -   33_sfig10_biannual.R
