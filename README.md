# econ270b_replication
Replication of Auriol et al. (2020)

# Overview

This is an example of a README file that could be used to expand upon the replication materials of Auriol et al. (2020). The `main.R` file of the replication materials calls upon 8 additionl R scripts, which is then used to generate all of the main figures and tables for the paper and supplementary appendix. \emph{However}, it appears that the posted replication code is missing a necessary data output, and thus one of the files is unable to run. As a result, the replicator should run only 7 of the R scripts and exclude `1d_run_and_format_heterogeneity_regressions.R`. Upon changing filepaths, the replicator should expect this code to take less than 5 minutes to run in total.

In addition to the original replication materials as provided by the authors, this set of replication materials includes a robustness check on a set of main results.

**Citation**: Auriol, E., Lassébie, J., Panin, A., Raiber, E., Seabright, P. (2020). "God Insures those Who Pay? Formal Insurance and Religious Offerings in Ghana," \emph{The Quarterly Journal of Economics}, 135(4): 1799–1848, <https://doi.org/10.1093/qje/qjaa015>.


## Data Availability 

### Details on Each Data Source

| Data.Name  | Data.Files | Location | Provided | Used in Replication Code | 
| -- | -- | -- | -- | -- | 
| "Wave 1 and Wave 2 Church Data" | wave1_and_wave2_church_data.csv | Data/ | TRUE | TRUE |
| "Pastor Questionnaire Responses - 1" | pastor_questionnaire_sumup.xls | Data/additional data/ | TRUE | FALSE |
| "Pastor Questionnaire Responses - 2" | pastor_questionnaire.csv | Data/additional data/ | TRUE | FALSE |
| “Ghana 2010 Census” | census_data_for_balance.csv | Data/additional data/ | TRUE | TRUE |
| "Ghana 2017 Living Standards Survey" | ghana_living_standards_survey_2017.csv | Data/additional data/ | TRUE | FALSE |
| "Ghana 2019 Living Standards Survey" | ghana_living_standards_survey_2019.csv | Data/additional data/ | TRUE | FALSE |


## Software Requirements

The replication materials were run using **R 4.2.2**.

Necessary packages to install in R:

- `AER` 
- `tidyverse`
- `haven`
- `xtable`
- `stargazer`
- `starpolisr` (install from development version by running: devtools::install_github("ChandlerLutz/starpolishr"))
- `stringi`
- `Ggally`
- the file `0_setup_project.R` will load all relevant libraries into R's working memory and should be run once installling all programs that are not already installed in R.

Additional packages to run extra robustness analysis: 

- `betareg`
- `sandwich`
- `lmtest`
- `margins`

## Description of Programs/Code

- `main.R`: Calls all 8 codes to run analyses, tables, and figures for the paper and supplementary appendix. (Note that because `1d_run_and_format_heterogeneity_regressions.R` is missing a data file in order to run, the replicator cannot run this code as-is, and must either comment out the line which calls that R script or run each script manually.)
- `0_setup_project.R`: Sets up paths and loads general libraries for the analysis and creates code to create tex tables and figures. 
- `1_define_regression_helper_functions.R`: Defines a series of functions that run the different regressions and define some of the default specifications for the different regressions. Functions are a mix of functions for estimating and for formatting output.
- `1a_run_main_regressions.R`: Runs main regressions and outputs tex files.
- `1b_analyse_replicability.R`: Produces additional regression tables and outputs tex files.
- `1c_format_main_regression_output_to_tex.R`:  Produces additional regression tables and outputs tex files.
- `1d_run_and_format_heterogeneity_regressions.R`: Produces additional regression tables for supplementary appendix and outputs tex files. (NOTE: This code does not run.)
- `3_make_other_tables.R`: Runs balance and other summary table and outputs tex files.
- `4_make_figures.R`: Runs main figures and outputs PDFs.
- `robutness_tableIII.R`: Runs a robustness check (as part of this replication assignment) for results in Table III (Panel A, Column 1).

## Instructions to Replicators

- Edit working directory in `0_setup_project.R` and set working directory in R.
- Comment out `source("R scripts/1d_run_and_format_heterogeneity_regressions.R")` from `main.R`
- Run `main.R`
