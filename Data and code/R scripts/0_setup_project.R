
### Code to setup the analysis for paper "God Insures Those Who Pay"
### -Auriol, Lassebie, Panin, Raiber, Seabright (2020)
### contact: amma.panin@gmail.com

### This script sets up paths and loads general libraries for
### the analysis and code to create tex tables and figures


## Clear the workspace -----------------------------------------------
rm(list =  ls())

### Load libraries ---------------------------------------------------
library(AER) # for the Tobit wrapper to survreg
library(tidyverse) # for the pipe %>%, dplyr, and ggplot
library(haven) # for stata files
library(xtable) # to produce basic latex tables
library(stargazer) # to turn regresstion results into nice tex
library(starpolishr) # to turn tables into panels
## (install from development version)
library(stringi) # easy formatting for strings, used for tables etc.
library(GGally) # contains ggpairs for the corolellogram

## NB! AER loads car which loads recode, which conflicts
##     with dplyr::recode,
##     but it cannot call survreg if it is not loaded, therefore
##     always load tidyverse AFTER AER


### Set up further paths ---------------------------------------------
project.path <- file.path(normalizePath("~"),
                          "Dropbox",
                          "Projects",
                          "God insures",
                          "Journal submissions",
                          "QJE acceptance",
                          "Data and code")
setwd(project.path)

data.path <- file.path(project.path, "Data")
output.path <- file.path(project.path, "Output")
figures.folder <- file.path(output.path, "Figures")
tables.folder <- file.path(output.path, "Tables as tex")

### Read-in data -----------------------------------------------------

dt.path <- file.path(data.path,
                     "wave1_and_wave2_church_data.csv")

dt.in <- read.csv(dt.path)

### Define some common variables to be used throughout the analysis ---
treatments <- c("Insurance", "Insurance information", "No insurance")

keep.choices <-  c("keep_church",
                   "keep_street",
                   "keep_thanks")

column.labels.base <- c("giving to the church",
                        "giving to the street",
                        "giving to thanksgiving")

choice.order <- c("keep_church",
                  "keep_churchnamed",
                  "keep_street",
                  "keep_thanks",
                  "church_street",
                  "churchnamed_street",
                  "church_thanks",
                  "churchnamed_thanks",
                  "church_churchnamed",
                  "street_thanks")

INDIVIDUAL.CONTROLS <- c("age",
                         "gender",
                         "higheducation",
                         "employed",
                         "log_tmonthlyincome",
                         "church_daily",
                         "pray_multiple",
                         "ethnicity_akan",
                         "ethnicity_ewe",
                         "ethnicity_ga")

## Create church dummy list so that Dchurch20 and Dchurch21 are excluded
## Dchurch20 and Dchurch21 are the "other" categories
## for wave 1 and wave 2
CHURCH.DUMMIES <- names(dt.in)[startsWith(names(dt.in), "Dchurch")] %>%
    setdiff(c("Dchurch20", "Dchurch21"))

## Inflation data from World Bank 2015 to 2018, trading economics 2019
## https://databank.worldbank.org/reports.aspx?source=world-development-indicators
## https://tradingeconomics.com/ghana/inflation-cpi
inflation.2015.to.2019 <- Reduce("*",
                                 sapply(c(17.5, 12.4, 9.8, 9.5),
                                        function(x)  1 + x / 100))

### Prepare the data frame for general analysis ----------------------

dt <- dt.in %>%
       rename(tinsurance = Tinsurance,
              tinsuranceinfo = Tinsuranceinfo,
              tnoinsurance = Tnoinsurance) %>%
    mutate(gender = ifelse(gender == "Male", 0, 1),
           revival = factor(revival,
                            levels = c(1, 0),
                            ordered = TRUE),
           wave = factor(wave),
           wave_star = ifelse(wave == 2 & church_name_grouped!= "Other",
                              "*", ""),
           church_disp_name = paste(church_code,
                                    "-",
                                    church_name_grouped,
                                    wave_star),
           log_tmonthlyincome = ifelse(is.infinite(log_tmonthlyincome),
                                       0, log_tmonthlyincome)) %>%
    arrange(church_code, church_disp_name, treatment) %>%
    mutate(church_disp_name = factor(
               church_disp_name,
               levels = unique(church_disp_name),
               ordered  =  TRUE))  %>%
    mutate_at(.vars = vars(starts_with("choice")),
              .funs = as.numeric) %>%
    mutate_at(.vars = vars(starts_with("cedi")),
              .funs = as.numeric) %>%
    mutate_at(.vars = vars(one_of(c(INDIVIDUAL.CONTROLS,
                                    CHURCH.DUMMIES))),
              .funs = as.numeric)

## Prepare and subset the data frame for regressions
## Make revival a Boolean, was originally a factor for plotting
dt.reg <- dt %>%
    mutate(revival = ifelse(as.character(revival) ==  0,
                            FALSE,
                            TRUE))
dt.revival <- dt.reg %>%
    filter(revival == TRUE)

dt.nonrevival <- dt.reg %>%
    filter(revival == FALSE)

### Formatting functions for the tables and figures ------------------

mk.bold <- function(str.in){
    str1 <- ifelse(!is.na(str.in),
                   paste("\\textbf{", str.in, "}"),  NA)
    return(str1)
}

mk.multicol <- function(str.in, ncol, align =  "l"){
    ## str.in in the format col_title

    str1 <- ifelse(!is.na(str.in),
                   paste0("\\multicolumn{", ncol, "}",
                         "{", align, "}",
                         "{", str.in, "}"),  NA)
    return(str1)
}

mk.col.parbox <- function(xstr){
    paste0("\\parbox[t]{2cm}{\\centering ",
           xstr, "}")
}

tidy.col.labels.in.parbox <- function(col_list){
    ## Take in the table choice name, e.g "keep_church"
    ## and return a nice tex parbox e.g 'keep vs church' on two lines

    mk.col.parbox(
        gsub(
            "_", "\\\\\\\\ vs \\\\\\\\",
            col_list)) %>% as.character()
}

tidy.round <- function(x.in, digs = 2){
    ## Make a nicely rounded digit
    round(x.in, digs) %>%
        format(nsmall = digs)
}

as.tex <- function(str.in){return(str.in)}

adjustbox.tabular <- function(tab.in, height = FALSE){
    ## Add an adjust box around a table to ensure that it
    ## scales to the page width and height

    height.tex <- ifelse(height == TRUE,
                         paste0(",totalheight=\\\\textheight",
                                "-2\\\\baselineskip\\"),
                         "")

    adjust.box.tex <- paste0("\\\\begin\\{adjustbox\\}",
                             "\\{width=\\\\textwidth",
                             height.tex,
                             "}",
                             "\\\\begin\\{tabular\\}")

    tex.out.scaled.begin <- gsub("\\\\begin\\{tabular\\}",
                                 adjust.box.tex,
                                 tab.in)

    tex.out.scaled <- gsub(
        "\\\\end\\{tabular\\}",
        "\\\\end\\{tabular\\}\\\\end\\{adjustbox\\}",
        tex.out.scaled.begin)

    return(tex.out.scaled)
}

scale.tabular <- function(tab.in, scale.in = 0.8){
    ##  Add a scalebox around a table to ensure that it
    ##  scales down by a pre-defined factor

    scale.box.tex <- paste0("\\\\scalebox",
                            "\\{",

                            scale.in,
                            "\\}\\{",
                            "\\\\begin\\{tabular\\}")

    tex.out.scaled.begin <- gsub("\\\\begin\\{tabular\\}",
                                 scale.box.tex,
                                 tab.in)

    tex.out.scaled <- gsub(
        "\\\\end\\{tabular\\}",
        "\\\\end\\{tabular\\}\\}",
        tex.out.scaled.begin)

    return(tex.out.scaled)
}
