
### Code to run regression models for "God Insures" paper

### This script should be called after "setup_project.R"
### Load libraries in "setup_project.R"

## call setup script if not done already
## assumes working directory is "Data and code"
source("R scripts/0_setup_project.R")
## call the script to define the helper functions for regressions
source("R scripts/1_define_regression_helper_functions.R")

## The general format of all regression tables is as follows
##  1. define the specifications in a data.frame
##  2. define the subset of data that will be used in the regression
##  3. run the different regressions with run.reg.columns
##  4. run.reg.columns returns a list of model objects


## Table 3 in the paper ----------------------------------------------
## compare 'insurance' to 'insurance info' for weeks with no revival

tab3.specifications <- basic.specifications %>%
    mutate(treatment = "tinsurance")

tab3.data <- dt.nonrevival %>%
    filter(!treatment == "No insurance")

tab3.models.pooled <- run.reg.columns(
    dt.in = tab3.data,
    specs.df = tab3.specifications)

tab3.models.w1 <- run.reg.columns(
    dt.in = tab3.data %>% filter(wave == 1),
    specs.df = tab3.specifications,
    tab.name = "insurance_vs_info_wave1")

tab3.models.w2 <- run.reg.columns(
    dt.in = tab3.data %>% filter(wave == 2),
    specs.df = tab3.specifications,
    tab.name = "insurance_vs_info_wave2")


## Table 4 in the paper ----------------------------------------------
## compare 'insurance info' to 'no insurance' for weeks with no revival

tab4.specifications <- basic.specifications %>%
    mutate(treatment = "tinsuranceinfo")

tab4.data <- dt.nonrevival %>%
    filter(!treatment == "Insurance")

tab4.models.pooled <- run.reg.columns(
    dt.in = tab4.data,
    specs.df = tab4.specifications)

tab4.models.w1 <- run.reg.columns(
    dt.in = tab4.data %>% filter(wave == 1),
    specs.df = tab4.specifications)

tab4.models.w2 <- run.reg.columns(
    dt.in = tab4.data %>% filter(wave == 2),
    specs.df = tab4.specifications)


### Table 6 in the paper ---------------------------------------------
## Compare revival participants in wave 2
## Looking at 'insurance' versus 'info' for participation with:
## i) no randomisation, ii) ITT  and iii) IV

## Use only participants who were randomly assigned to a revival week
## (Explained in footnote 32)

tab6.specifications <- basic.specifications %>%
    mutate(treatment = "tinsurance",
           use.wave.dummy = FALSE)

tab6.data <- dt.reg %>%
    filter(wave == 2) %>%
    filter(!is.na(revival_assignment)) %>%
    filter(!treatment == "No insurance")

tab6.wave2.revival.insurance_info <- run.reg.columns(
    dt.in = tab6.data,
    specs.df = tab6.specifications  %>%
        mutate(additional.controls =
                   "revival * tinsurance"))

tab6.wave2.revival_assigned.insurance_info <- run.reg.columns(
    dt.in = tab6.data,
    specs.df = tab6.specifications  %>%
        mutate(additional.controls =
                   "revival_assignment * tinsurance"))

tab6.wave2.revival_iv.insurance_info <- run.reg.columns(
    dt.in = tab6.data,
    specs.df = tab6.specifications %>%
        mutate(vars.endog =
                   "revival * tinsurance",
               vars.exog =
                   "revival_assignment * tinsurance"),
    reg.function = run.iv_lpm_ols)

## Test the first stage of the IV regression and produce a tex file
get.weak.instruments <- function(model){
    diagnostic.sum <- model %>%
        summary(diagnostic = TRUE)

    sum.df <- diagnostic.sum$diagnostics[
                                 1:2,
                                 c("statistic")] %>%
        data.frame()
    return(sum.df)
}

weak.tab.list <- lapply(tab6.wave2.revival_iv.insurance_info[1:2],
                        get.weak.instruments)

weak.tab <- do.call(cbind, weak.tab.list) %>%
    apply(2, round, digits = 2) %>%
    data.frame() %>%
    rename("col1"=1, "col2"=2) %>%
    mutate(regressor = c("revival", "revival x insurance"))%>%
    rbind(c(mk.multicol("no controls", 1),
            mk.multicol("with controls", 1),
            "replace"))%>%
    data.frame() %>%
    mutate(idx = row_number()) %>%
    arrange(desc(idx)) %>%
    select(regressor, 1, 2)

weak.tab.tex <- gsub(
    "& replace",
    "",
    print(
        xtable(weak.tab,
               caption = paste(
                   "F-statistics for weak instrument tests",
                   "for first stage reported in",
                   "Table \\ref{panel_revival_experiment_wave2}"),
               label = "revival_iv_ftest"),
        include.colnames = FALSE,
        include.rownames = FALSE,
        sanitize.text.function = identity,
        caption.placement = "top"))

weak.tab.tex.out <- gsub("replace",
                         "",
                         weak.tab.tex)

write(weak.tab.tex.out,
      file.path(tables.folder, "revival_IV_firststage_ftest.tex"))

## Table 16 in the paper ---------------------------------------------
### Comparing church and church_named, making comparisons for
## i) insurance versus info and
## ii) info versus no
##
## Use the data subsets for the main regressions
## (tab3 - insurance v info; tab4 info v no)

churchnamed.specifications <- data.frame(
    dependent = c("keep_church",
                  "keep_churchnamed",
                  "church_churchnamed",
                  "churchnamed_street",
                  "churchnamed_thanks"),
    use.individual.controls = TRUE,
    use.church.dummies = TRUE,
    use.wave.dummy =  TRUE,
    treatment = "tinsurance") %>%
    mutate(model.names = dependent)

churchnamed.insurance_info <- run.reg.columns(
    tab3.data,
    churchnamed.specifications)

churchnamed.info_no <- run.reg.columns(
    dt.in = tab4.data,
    specs.df = churchnamed.specifications %>%
        mutate(treatment = "tinsuranceinfo"))


## Table 17 in the paper ---------------------------------------------
## compare 'insurance' to 'no insurance' in weeks with no revival events

tab17.specifications <- basic.specifications %>%
    mutate(treatment = "tinsurance")

tab17.data <- dt.nonrevival %>%
    filter(!treatment == "Insurance information")

tab17.models.pooled <- run.reg.columns(
    dt.in = tab17.data,
    specs.df = tab17.specifications)

tab17.models.w1 <- run.reg.columns(
    dt.in = tab17.data %>% filter(wave == 1),
    specs.df = tab17.specifications)

tab17.models.w2 <- run.reg.columns(
    dt.in = tab17.data %>% filter(wave == 2),
    specs.df = tab17.specifications)


## Table 18 in the paper ---------------------------------------------
## Comparing all remaining choices
## i) insurance versus info and
## ii) info versus no
##
## Use the data subsets for the main regressions
## (tab3 - insurance v info; tab4 info v no)

all.choices.specifications <- data.frame(
    dependent = c("church_street",
                  "church_thanks",
                  "street_thanks"),
    use.individual.controls = TRUE,
    use.church.dummies = TRUE,
    use.wave.dummy =  TRUE,
    treatment = "tinsurance") %>%
    mutate(model.names = dependent)

all.choices.insurance_info <- run.reg.columns(
    dt.in = tab3.data,
    all.choices.specifications)

all.choices.info_no <- run.reg.columns(
    dt.in = tab4.data,
    specs.df = all.choices.specifications %>%
        mutate(treatment = "tinsuranceinfo"))


### Tables 19, 20, 21 and 22 look at data standardized and with cedi
### outcomes; this changes the tobit bounds

### Helper functions for standardized and cedi tables
my.standardize <- function(x){
    (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

make.tobit.ends <- function(dep.var, dt.reg.in, high.x, low.x){
    var.name <- paste0("cedi2019_choice_", dep.var)

    x <- pull(dt.reg.in, var.name)
    mean.x <- mean(x, na.rm = TRUE)

    z.max <- (high.x - mean.x)/ sd(x, na.rm =  TRUE)
    z.min <- (low.x - mean.x)/ sd(x, na.rm =  TRUE)
    return(list(c("left" = z.min, "right" = z.max)))
}

## Table 19 and 20 in the paper --------------------------------------
## Standardized coefficients

## First edit the data to standardize the cedi outcomes
standardized.df <- dt.nonrevival %>%
    select(starts_with("cedi2019"),
           one_of(c("wave",
                    "treatment",
                    "tinsurance",
                    "tinsuranceinfo",
                    "tnoinsurance",
                    INDIVIDUAL.CONTROLS,
                    CHURCH.DUMMIES))) %>%
    ## Standardize outcomes by (x - mean)/sd using the cedi2019
    ## variables
    mutate_at(.vars = vars(starts_with("cedi")),
              .funs = list(zscore=~my.standardize(.))) %>%
    ## Remove the parts of the standardized outcomes names that
    ## mention cedi2019 or zscores, so that they appear as
    ## "keep_church" etc. and can be used in the same regression
    ## wrapper functions
    rename_at(.vars = vars(ends_with("_zscore")),
              .funs = list(~gsub("cedi2019_", "", .))) %>%
    rename_at(.vars = vars(ends_with("_zscore")),
              .funs = list(~gsub("_zscore", "", .)))

standardized.data.w1 <- filter(standardized.df, wave == 1)
standardized.data.w2 <- filter(standardized.df, wave == 2)

## Waves need different specifications because the bounds are different
standardized.w1.specifications <- basic.specifications %>%
    mutate(censor.bounds = sapply(dependent,
                                  make.tobit.ends,
                                  dt.reg.in = standardized.data.w1,
                                  high.x = 11,
                                  low.x = 0))

standardized.w2.specifications <- basic.specifications %>%
    mutate(censor.bounds = sapply(dependent,
                                  make.tobit.ends,
                                  dt.reg.in = standardized.data.w2,
                                  high.x = 19,
                                  low.x = 0))

## Standardized outcomes for wave 1 and wave 2, insurance v info
standardized.insurance_info.w1 <- run.reg.columns(
    dt.in = standardized.data.w1 %>%
        filter(treatment!= "No insurance"),
    specs.df = standardized.w1.specifications %>%
        mutate(treatment = "tinsurance"))

standardized.insurance_info.w2 <- run.reg.columns(
    dt.in = standardized.data.w2 %>%
        filter(treatment!= "No insurance"),
    specs.df = standardized.w2.specifications %>%
        mutate(treatment = "tinsurance"))

## Standardized outcomes for wave 1 and wave 2, info v no
standardized.info_no.w1 <- run.reg.columns(
    dt.in = standardized.data.w1 %>%
        filter(treatment!= "Insurance"),
    specs.df = standardized.w1.specifications %>%
        mutate(treatment = "tinsuranceinfo"))

standardized.info_no.w2 <- run.reg.columns(
    dt.in = standardized.data.w2 %>%
        filter(treatment!= "Insurance"),
    specs.df = standardized.w2.specifications %>%
        mutate(treatment = "tinsuranceinfo"))


## Table 21 and 22 in the paper --------------------------------------
## Outcomes in nominal cedis

## Multiple the fraction outcomes by 11 and 19 respectively
regression.vars <- c("session",
                     "wave",
                     "treatment",
                     "tinsurance",
                     "tinsuranceinfo",
                     "tnoinsurance",
                    INDIVIDUAL.CONTROLS,
                    CHURCH.DUMMIES)

cedis.w1.data <- dt.nonrevival %>%
    filter(wave == 1)  %>%
    select(starts_with("choice"),
           one_of(regression.vars)) %>%
    mutate_at(.vars = vars(starts_with("choice")),
              .funs = list(~. * 11))

cedis.w2.data <- dt.nonrevival %>%
    filter(wave == 2) %>%
    select(starts_with("choice"),
           one_of(regression.vars)) %>%
    mutate_at(.vars = vars(starts_with("choice")),
              .funs = list(~. * 19))

## Different specifications with different Tobit bounds
cedis.w1.specifications <- basic.specifications %>%
    mutate(censor.bounds = list(c("left" = 0, "right" = 11)))

cedis.w2.specifications <- basic.specifications %>%
    mutate(censor.bounds = list(c("left" = 0, "right" = 19)))

## Cedi outcomes for wave 1 and wave 2, insurance v info
cedis.insurance_info.w1 <- run.reg.columns(
    dt.in = cedis.w1.data %>%
        filter(treatment!= "No insurance"),
    specs.df = cedis.w1.specifications %>%
        mutate(treatment = "tinsurance"))

cedis.insurance_info.w2 <- run.reg.columns(
    dt.in = cedis.w2.data %>%
        filter(treatment!= "No insurance"),
    specs.df = cedis.w2.specifications %>%
        mutate(treatment = "tinsurance"))

## Cedi outcomes for wave 1 and wave 2, info v no
cedis.info_no.w1 <- run.reg.columns(
    dt.in = cedis.w1.data %>%
        filter(treatment!= "Insurance"),
    specs.df = cedis.w1.specifications %>%
        mutate(treatment = "tinsuranceinfo"))

cedis.info_no.w2 <- run.reg.columns(
    dt.in = cedis.w2.data %>%
        filter(treatment!= "Insurance"),
    specs.df = cedis.w2.specifications %>%
        mutate(treatment = "tinsuranceinfo"))


## Table 23 in the paper ---------------------------------------------
## Pooled revival effects
rev.tab.insurance_info.specifications <- basic.specifications %>%
    mutate(treatment = "tinsurance",
           additional.controls = "revival * tinsurance")

rev.tab.insurance_info.data <- dt.reg %>%
    filter(!treatment == "No insurance")

rev.tab.insurance_info <-
    run.reg.columns(
    dt.in = rev.tab.insurance_info.data,
    specs.df = rev.tab.insurance_info.specifications  %>%
        mutate(additional.controls = "revival * tinsurance"))


rev.tab.info_no.specifications <- basic.specifications %>%
    mutate(treatment = "tinsuranceinfo",
           additional.controls = "revival * tinsuranceinfo")

rev.tab.info_no.data <- dt.reg %>%
    filter(!treatment == "Insurance")

rev.tab.info_no <- run.reg.columns(
    dt.in = rev.tab.info_no.data,
    specs.df = rev.tab.info_no.specifications  %>%
        mutate(additional.controls = "revival * tinsuranceinfo"))



### Save regression results ------------------------------------------
## Save the main lists of regression models into an Rds object
regression.models <- list(
    insurance_vs_info_pooled = tab3.models.pooled,
    insurance_vs_info_w1 = tab3.models.w1,
    insurance_vs_info_w2 = tab3.models.w2,
    info_vs_no_pooled = tab4.models.pooled,
    info_vs_no_w1 = tab4.models.w1,
    info_vs_no_w2 = tab4.models.w2,
    insurance_vs_no_pooled = tab17.models.pooled,
    insurance_vs_no_w1 = tab17.models.w1,
    insurance_vs_no_w2 = tab17.models.w2,
    revival.insurance_vs_info = tab6.wave2.revival.insurance_info,
    revival.insurance_vs_info.itt =
        tab6.wave2.revival_assigned.insurance_info,
    revival.insurance_vs_info.iv =
        tab6.wave2.revival_iv.insurance_info,
    churchnamed.insurance_info = churchnamed.insurance_info,
    churchnamed.info_no = churchnamed.info_no,
    all.choices.insurance_info = all.choices.insurance_info,
    all.choices.info_no = all.choices.info_no,
    standardized.insurance_info.w1 = standardized.insurance_info.w1,
    standardized.insurance_info.w2 = standardized.insurance_info.w2,
    standardized.info_no.w1 = standardized.info_no.w1,
    standardized.info_no.w2 = standardized.info_no.w2,
    cedis.insurance_info.w1 = cedis.insurance_info.w1,
    cedis.insurance_info.w2 = cedis.insurance_info.w2,
    cedis.info_no.w1 = cedis.info_no.w1,
    cedis.info_no.w2 = cedis.info_no.w2,
    revival.total.insurance_info = rev.tab.insurance_info,
    revival.total.tab.info_no = rev.tab.info_no)

saveRDS(object = regression.models,
        file = file.path(output.path,
                         "Regression models",
                         "regression_model_objects.Rds"))
