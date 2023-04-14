### Code to produce tex panels for "God Insures" paper


## call setup script if not done already
## assumes working directory is "Data and code"
source("R scripts/0_setup_project.R")
## call the script to define the helper functions for regressions
source("R scripts/1_define_regression_helper_functions.R")


library(starpolishr)

## Most tables have some standard text assocciated with them. Write out
## out the note in tex here, leaving gaps to be filled in with string
## formatting e.g. %s.

standard.note.base <- paste("\\multicolumn{${ncols}}{l}{",
                       "\\parbox[t]{0.95\\linewidth}{\\small{",
                       "\\textit{Notes:}",
                       "$^{*}$p$<$0.1;",
                       "$^{**}$p$<$0.05;",
                       "$^{***}$p$<$0.01}.",
                       "${PAP_correspondence}",
                       "${regression_type}",
                       "Standard errors are in parentheses",
                       "below the estimates and are clustered at",
                       "the session level.",
                       "Dependent variable is",
                       "${outcome_description}",
                       "that a participant allocated",
                       "${allocation_description}",
                       "The endowment in Wave 1 was 11 GHS in 2015",
                       "and the",
                       "endowment in Wave 2 was 19 GHS in 2019.",
                       "Individual controls include age, gender",
                       "ethnicity, employment, log(income),",
                       "an indicator for daily church attendance,",
                       "an indicator for praying multiple times per day,",
                       "church fixed effects,",
                       "and a dummy for the experimental wave.",
                       "The size of the sample changes",
                       "across columns because a computer error",
                       "led to some missing demographic variables",
                       "in wave 1.",
                       "}}\\\\")

note.tobit <- "Tobit regressions censored at 0 and 1."
note.sample.size <- paste("The size of the sample changes",
                          "across columns because a computer error",
                          "led to some missing demographic variables",
                          "in wave 1.")
standard.note.outcomes <-  "the fraction of the endowment"
standard.note.columns <- paste("to either their own",
                      "church (columns 1 and 2), a nonprofit charity",
                      "working with street children (columns 3 and 4)",
                      "or a country-wide prayer event called the",
                      "national day of thanksgiving (columns 5 and 6).")

standard.note.list <- list(
    ncols = 7,
    PAP_correspondence = "",
    regression_type = note.tobit,
    outcome_description = standard.note.outcomes,
    allocation_description = standard.note.columns)

table.title <- paste("%s\\emph{%s} compared to",
                     "\\emph{%s}",
                     "for \\emph{%s} sample.")

both.waves.txt <- "Waves 1 and 2. "

pap.note <- paste("This table corresponds to %s",
                  "in the pre-analysis plan.")


### Panel tables -----------------------------------------------------
## Combine results into panels for table
## Read in regression results from stored object

REGRESSION.MODELS <- readRDS(
    file.path(output.path,
              "Regression models",
              "regression_model_objects.Rds"))

### Panel 1, insurance versus info by waves  -------------------------

insurance_vs_info.note.list <- standard.note.list

insurance_vs_info.note.list$PAP_correspondence <- sprintf(
    pap.note,
    "`Main Test 1' and 'Main Test 2.1'")

panel1.models <- REGRESSION.MODELS[
    c("insurance_vs_info_pooled",
      "insurance_vs_info_w1",
      "insurance_vs_info_w2")]

panel.insurance_vs_info <- create.reg.panels(
    list.of.mod.lists = panel1.models,
    panel.names.in = c("Pooled sample",
                       "Wave 1",
                       "Wave 2"),
    title = paste("Effects of",
                  "insurance enrollment compared to insurance",
                  "information for weeks with no revival",
                  "events."),
    tex.file.name = "panel_insurance_vs_info",
    panel.note.list = insurance_vs_info.note.list)

### Panel 2, info versus no by waves ---------------------------------
info_vs_no.note.list <- standard.note.list
info_vs_no.note.list$PAP_correspondence <- sprintf(
    pap.note,
    "'Main Test 1' and 'Main Test 2.1'")

panel2.models <- REGRESSION.MODELS[
    c("info_vs_no_pooled",
      "info_vs_no_w1",
      "info_vs_no_w2")]

info_vs_no.panel <- create.reg.panels(
    list.of.mod.lists = panel2.models,
    panel.names.in = c("Pooled",
                       "Wave 1",
                       "Wave 2"),
    title = paste("Effects of",
                  "insurance information compared",
                  "to no insurance",
                  "for participants in weeks with",
                  "no revival events."),
    tex.file.name = "panel_info_vs_no",
    panel.note.list = info_vs_no.note.list)

### Panel 3, insurance v no ------------------------------------------
secondary.test1.note.list <- standard.note.list
secondary.test1.note.list$PAP_correspondence <- sprintf(
    pap.note, "'Secondary Test 1'")

panel3.models <- REGRESSION.MODELS[
    c("insurance_vs_no_pooled",
      "insurance_vs_no_w1",
      "insurance_vs_no_w2")]

secondary.test1.panel <- create.reg.panels(
    list.of.mod.lists = panel3.models,
    panel.names.in = c("Pooled","Wave 1", "Wave 2"),
    title = paste("Effects of insurance enrollment",
                  "compared",
                  "to receiving nothing",
                  "for participants in weeks with",
                  "no revival events"),
    tex.file.name = "panel_insurance_vs_no",
    panel.note.list = secondary.test1.note.list)

### Panel 4, results with revival ------------------------------------
revival.test1.note.list <- standard.note.list
revival.test1.note.list$PAP_correspondence <- sprintf(
    pap.note, "`Heterogenous Effect 1'")

panel4.models <- REGRESSION.MODELS[
    c("revival.total.insurance_info",
      "revival.total.tab.info_no")]

revival.test1.panel <- create.reg.panels(
    list.of.mod.lists = panel4.models,
    panel.names.in = c(paste("Revival effects, ",
                             "insurance enrollment compared to",
                             "insurance information"),
                       paste("Revival effects",
                             "Insurance information compared to",
                             "nothing")),
    title = paste("Effects of insurance enrollment and ",
                  "insurance information with a dummy for participation",
                  "during revival "),
    tex.file.name = "panel_revival_test1_pooled",
    panel.note.list = revival.test1.note.list)

### Panel 5, revival experiment --------------------------------------
revival.test2.note.list <- standard.note.list
revival.test2.note.list$PAP_correspondence <- sprintf(
    pap.note, "`Heterogenous Effect 2'")

panel5.models <- REGRESSION.MODELS[
    c("revival.insurance_vs_info",
      "revival.insurance_vs_info.itt",
      "revival.insurance_vs_info.iv")]

revival.wave2.panel <- create.reg.panels(
    list.of.mod.lists = panel5.models,
    panel.names.in = c(paste("Non-randomised revival attendance,"),
                       paste("\\parbox[t]{14cm}{",
                             "Intention-to-treat estimates of",
                             "random assignment to participate during",
                             "a revival week",
                             "}"),
                       paste("\\parbox[t]{14cm}{",
                             "Instrumental variables estimate of",
                             "revival week using randomised assignment",
                             "to a revival week as an instrument for ",
                             "attending during a revival week,",
                             "}")),
    title = paste("Effects of attending the experiment during a",
                  "revival  for the subset of Wave 2 participants",
                  "where the assigned participation date was ",
                  "randomised with an encouragement design."),
    tex.file.name = "panel_revival_experiment_wave2",
    panel.note.list = revival.test2.note.list,
    add.adjustbox = TRUE)

### Panel 6 and 7, standardized outcomes -----------------------------
zscore.note.list <- standard.note.list
zscore.note.list$outcome_description = paste(
    "standardized cedi amounts",
    "(standardized with respect to the wave mean and",
    "standard deviation)")

panel6.models <- REGRESSION.MODELS[
    c("standardized.insurance_info.w1",
      "standardized.insurance_info.w2")]

standardized.panel.insurance_info <- create.reg.panels(
    list.of.mod.lists = panel6.models,
    panel.names.in = c(paste("Wave 1"),
                       paste("Wave 2")),
    title = paste("Effects of insurance enrollment",
                  "compared to insurance information",
                  "with standardized outcomes."),
    tex.file.name = "panel_standardized_results_insurance_vs_info",
    panel.note.list = zscore.note.list)

## Panel 7
panel7.models <- REGRESSION.MODELS[
    c("standardized.info_no.w1",
      "standardized.info_no.w2")]

standardized.panel.info_no <- create.reg.panels(
    list.of.mod.lists = panel7.models,
    panel.names.in = c(paste("Wave 1"),
                       paste("Wave 2")),
    title = paste("Effects of insurance information",
                  "compared to no insurance",
                  "with standardized outcomes."),
    tex.file.name = "panel_standardized_results_info_vs_no",
    panel.note.list = zscore.note.list)


### Panels 8 and 9, cedi outcomes ------------------------------------
cedi.note.list <- standard.note.list
cedi.note.list$outcome_description = paste(
    "the cedi amount (unadjusted for inflation)")

panel8.models <- REGRESSION.MODELS[
    c("cedis.insurance_info.w1",
      "cedis.insurance_info.w2")]

cedi.panel.insurance_info <- create.reg.panels(
    list.of.mod.lists = panel8.models,
    panel.names.in = c(paste("Wave 1, 2015 cedi value"),
                       paste("Wave 2, 2019 cedi value",
                             "(approximately 1.6 x 2015 cedi value)")),
    title = paste("Effects of insurance enrollment compared to",
                  "insurance information using",
                  "cedi outcomes, separated by wave."),
    tex.file.name = "panel_cedi_results_insurance_vs_info",
    panel.note.list = cedi.note.list)

### Panel 9
panel9.models <- REGRESSION.MODELS[
    c("cedis.info_no.w1",
      "cedis.info_no.w2")]

cedi.panel.info_no <- create.reg.panels(
    list.of.mod.lists = panel9.models,
    panel.names.in = c(paste("Wave 1, 2015 cedi value"),
                       paste("Wave 2, 2019 cedi value",
                             "(approximately 1.6 x 2015 cedi value)")),
    title = paste("Effects of receiving insurance information",
                  "compared to",
                  "receiving nothing",
                  "using cedi outcomes, separated by wave."),
    tex.file.name = "panel_cedi_results_info_vs_no",
    panel.note.list = cedi.note.list)

### All additional choices -------------------------------------------

all.choice.models <- REGRESSION.MODELS[
    c("all.choices.insurance_info",
      "all.choices.info_no")]

choice.names.raw <- c("church_street",
                      "church_thanks",
                      "street_thanks")

other.choice.cols <- mk.col.parbox(gsub(
    "_", "\\\\\\\\ vs \\\\\\\\",
    choice.names.raw %>% as.character()))

all.choices.notes.list <- standard.note.list
all.choices.notes.list$allocation_description <- paste(
    "between the different outcomes.")

all.choices_panel <-  create.reg.panels(
    list.of.mod.lists = all.choice.models,
    panel.names.in = c(paste("Insurance enrollment compared to",
                             "insurance information, N = %s"),
                      paste("Insurance information compared to",
                             "no insurance, N = %s")),
    title = paste("Effects of insurance treatments on all other",
                  "allocation decisions."),
    tex.file.name = "panel_all_choices",
    panel.note.list = all.choices.notes.list,
    col.labs.in = other.choice.cols,
    col.sep.in = NULL)

### Comparing church and church_named  -------------------------------

churchnamed.models <- REGRESSION.MODELS[
    c("churchnamed.insurance_info",
      "churchnamed.info_no")]

churchnamed.names.raw <- c("keep_church",
                           "keep_churchnamed",
                           "church_churchnamed",
                           "churchnamed_street",
                           "churchnamed_thanks")

churchnamed.choice.cols <- mk.col.parbox(gsub(
    "_", "\\\\\\\\ vs \\\\\\\\",
   churchnamed.names.raw %>% as.character()))

churchnamed.notes.list <- standard.note.list
churchnamed.notes.list$allocation_description <- paste(
    "considering non-anonymous giving to the church.")

churchnamed_panel <-  create.reg.panels(
    list.of.mod.lists = churchnamed.models,
    panel.names.in = c(paste("Insurance enrollment compared to",
                             "insurance information, N = %s"),
                      paste("Insurance information compared to",
                             "no insurance, N = %s")),
    title = paste("Effects of insurance treatments on non-anonymous",
                  "giving to the church."),
    tex.file.name = "panel_churchnamed",
    panel.note.list = churchnamed.notes.list,
    col.labs.in = churchnamed.choice.cols,
    col.sep.in = NULL)
