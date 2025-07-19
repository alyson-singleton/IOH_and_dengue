> Code for: [_Highway paving increased dengue transmission in the Peruvian Amazon._](https://www.medrxiv.org/content/10.1101/2024.11.15.24317406v1) Preprint, 2024. Code written by Alyson Singleton and TJ Sipin.
>
> Please feel free to email me with any specific, code-related questions at asinglet@stanford.edu.

# Requirements and timing
All analyses were run in the R (version 4.4.2) programming language or on Python via Google Colab (which provides a ready-to-use Python environment). Installing R usually takes less than 30 minutes, but may vary. Installing the necessary R packages takes usually 1-2 minutes per package, unless packages are installed from source which takes longer.

# Data processing and analysis pipeline

## Download/extract data

#### Remotely-sensed environmental data
1) Download the spatial data located in the data/spatial_data folder and upload to your GEE assests.
2) Download all environmental covariates and highway buffer files (example output in data/covariates_data folder).
   - [00_IOH_and_dengue_GEE_downloads.ipynb](https://colab.research.google.com/drive/1NuFvsgjjnNCU4ZElD4kMuuzfHIACS-fD?usp=sharing)
  
#### Population data
3) Clean and do manual name matching on DIRESA-provided population data.
   - 01_clean_diresa_pop_data.R 
4) Link DIRESA data to WorldPop data and use to impute years with missing DIRESA data.
   - 02_impute_missing_pop_data.R 

#### Clustering
5) Identify spatial groups for clustering standard errors.
   - 03_clustering.R 

#### Disease data
6) Process DIRESA dengue and leishmaniasis case data.
   - 04_process_diresa_case_data.R

## Merge data
7) Link cases with covariates and build final panel dataset.
   - 05_link_cases_w_covariates.R
   - 06_construct_final_panel_data.R

## Fit regressions and run senstivity analyses

## Make main text figures and tables

## Make supplementary figures and tables
