
### Code to setup the analysis for "God Insures Those Who Pay"
### Authors: Auriol, Lassebie, Panin, Raiber, Seabright (2020)
### Scripts updated on 15 April 2020

### This script puts everything together
### Output is written to the "output" folder

### Script should be called from the "Data and code" folder
### Working directory is set in "0_setup_project.R"

### The three sets of the script do the following:
### 1) Regression models and formatting tables tex
### 2) Produce and convert to 'tex' all other tables
###    (e.g. balance tables, summaries, etc.)
### 3) Produce the figures included in the table

source("R scripts/0_setup_project.R")

source("R scripts/1_define_regression_helper_functions.R")
source("R scripts/1a_run_main_regressions.R")
source("R scripts/1b_analyse_replicability.R")
source("R scripts/1c_format_main_regression_output_to_tex.R")
#source("R scripts/1d_run_and_format_heterogeneity_regressions.R")

source("R scripts/3_make_other_tables.R")

source("R scripts/4_make_figures.R")
