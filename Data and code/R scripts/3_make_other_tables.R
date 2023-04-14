
### Code to produce balance and other summary tables for God Insures paper
### Most tables combine code to do analysis and code to make a pretty
### tex table

### Load libraries in "setup_project.R" defined below
source("R scripts/0_setup_project.R")

paper.tables.folder <- tables.folder

### Prepare variables ------------------------------------------------

## Use log income for estimates; present level income in tables
balance.control.vars <- c(setdiff(INDIVIDUAL.CONTROLS,
                                  "log_tmonthlyincome"),
                          "tmonthlyincome")

## Include a subset of 'spiritual' variables for the revival tests
additional.revival.vars.to.test <- c("high_giver",
                                     "church_sevweekly",
                                     "believes_god_involved_in_finances",
                                     "travels_more_than_hour")

## Define the balance variables that will be selected from dt
## vars.select are different from vars.test because of some beautification
## considerations such as using 'female' in the table rather than 'gender'
## and using 'wave = 1' or 'wave = 2' instead of 'wave_2 == 0'
balance.vars.select <- c(
    "treatment", "wave", "revival",
    balance.control.vars,
    additional.revival.vars.to.test)

balance.vars.test <- setdiff(c(balance.control.vars,
                               "female",
                               "wave_2",
                               "revival"),
                             "gender")

## Make the list to test balance across revival status, including
## the additional spiritual variables but excluding the revival dummy
revival.balance.vars <- c(setdiff(balance.vars.test,
                                  "revival"),
                          additional.revival.vars.to.test)

## When making a balance table across waves, wave should not be tested
wave.vars.to.test <- setdiff(balance.vars.test, "wave_2")

## Define the order variables should show up in the tables
balance.vars.order <- c("age",
                        "female",
                        "tmonthlyincome",
                        "employed",
                        "higheducation",
                        "ethnicity_akan",
                        "ethnicity_ewe",
                        "ethnicity_ga",
                        "church_daily",
                        "pray_multiple",
                        "revival",
                        "wave_2")

## Define nicer names for the tables
balance.recodes <- c(age = "Age",
                     church_daily = "Attends church daily",
                     employed =  "Employed",
                     ethnicity_akan = "Akan ethnicity",
                     ethnicity_ewe = "Ewe ethnicity",
                     ethnicity_ga = "Ga ethnicity",
                     female = "Female",
                     higheducation = "More than secondary school",
                     pray_multiple = "Prays multiple times per day",
                     revival = "Attended during revival week",
                     wave_2 = "Wave 2",
                     tmonthlyincome = "Monthly income, GHS")

## Reorder recode variables so that spiritual variables are kept
## together for revival comparisons
revival.recodes <- c(
    balance.recodes[names(balance.recodes) != "pray_multiple"],
    pray_multiple = "Prays multiple times per day",
    high_giver = "Top quartile church giving",
    church_sevweekly = "Church more than once a week",
    believes_god_involved_in_finances =
        "Believes God involved in finances",
    travels_more_than_hour = "Travels more than one hour to church")

### Prepare a tidied data frame that can be used for the balance tests
dt.balance.base <- dt %>%
    select(one_of(balance.vars.select)) %>%
    mutate(female = gender == 1,
           wave_2 = wave == 2,
           revival = ifelse(revival ==  1, 1, 0)) %>%
    select(-c(gender, wave))

dt.balance <- dt.balance.base %>%
    select(-one_of(additional.revival.vars.to.test))

dt.balance.revival <- dt.balance.base


### Explore and create summary statistics ----------------------------

dt.choices <- select(dt, starts_with("choice"))
n.median <- sum(round(dt$choice_keep_church, 3) %in% c(round(9/19,3),
                                                       round(10/19,3),
                                                       round(5/11,3),
                                                       round(6/11, 3)))
prop.median <- n.median /  nrow(dt)

n0 <- dt.choices %>%
    mutate_at(vars(everything()), ~.==0) %>%
    colSums(na.rm = TRUE) %>%
    `/`(nrow(dt.choices)) %>%
    rbind() %>%
    data.frame()%>%
    select(one_of(paste0("choice_", keep.choices)))

n1 <- dt.choices %>%
    mutate_at(vars(everything()), ~.==1) %>%
    colSums(na.rm = TRUE) %>%
    `/`(nrow(dt.choices)) %>%
    rbind() %>%
    data.frame()%>%
    select(one_of(paste0("choice_", keep.choices)))

session.size.table <- table(dt$session)
good.sessions <- names(session.size.table)[session.size.table %in%
                                          seq(6, 12)]
prop.in.good.size.sessions <- sum(with(
    dt, session %in% good.sessions)) / nrow(dt)

### Table of total treatments
treatment.counts <- dt %>%
    select(wave, treatment, revival) %>%
    group_by(wave, treatment, revival) %>%
    summarise(count = n()) %>%
    pivot_wider(values_from = count,
                names_from = treatment) %>%
    ungroup() %>%
    mutate(wave_totals = rowSums(
               select(.,
                      one_of(treatments)))) %>%
    arrange(revival) %>%
    group_by(revival) %>%
    mutate(total = sum(wave_totals)) %>%
    ungroup() %>%
    rbind(data.frame(wave = "TOTAL",
                     revival = NA,
                     rbind(colSums(select(., -c(wave,  revival)))),
                     check.names =  FALSE))

## Prepare table for tex
treatment.counts.tab <- treatment.counts
treatment.counts.tab[5, "total"] <- treatment.counts.tab[5, "wave_totals"]
treatment.counts.tab[5, "wave_totals"] <- NA
treatment.counts.tab[treatment.counts.tab$wave == 2, "total"] <- NA

treatment.counts.tab <- data.frame(
    treatment.counts.tab %>%
    select(wave),
    treatment.counts.tab %>%
    select(-wave) %>%
    lapply(as.character) %>%
    lapply(as.integer),
    check.names = FALSE) %>%
    rename(`revival total` = total)

fix.title.names <- function(str.in){
    str0 <- str_to_title(str.in)
    str1 <- gsub("_", " ", str0)
    str2 <- mk.bold(str1)
    return(str2)
}

treatment.counts.tab[nrow(treatment.counts.tab),] <-
    mk.bold(treatment.counts.tab[nrow(treatment.counts.tab),])
names.to.fix <- names(treatment.counts.tab)
names(treatment.counts.tab) <- fix.title.names(names.to.fix)

Nrow <- nrow(treatment.counts.tab)
treatment.counts.align <- c("r", "r", "r", "|r", "r",  "r|", "r", "r")
treatment.counts.hlines <- c(-1, 0, Nrow - 1, Nrow)
treatment.counts.caption <- paste0(
    "Number of people participating across insurance and revival ",
    "treatments and two waves of data collection")

treatment.counts.tex <-
    adjustbox.tabular(
        print(xtable(treatment.counts.tab,
                     label= "treatment_counts",
                     align = treatment.counts.align,
                     caption = treatment.counts.caption),
              include.rownames = FALSE,
          sanitize.text.function = identity))

write(x = treatment.counts.tex,
      file = file.path(paper.tables.folder, "treatment_counts.tex"))


### Balance tables ---------------------------------------------------

make.balance.col <- function(df.in,
                             balance.vars.in = balance.vars.test){

    ## Get the means of the variables listed in balance.vars.in
    ## Round them and save variable names in a new column
    ## called 'variable'
    ## Put 'variable' column at the beginning of the table

    df.base <- df.in %>%
        summarise_at(.vars = vars(one_of(balance.vars.in)),
                     .funs = mean, na.rm  = TRUE) %>%
        mutate_at(2:ncol(.), tidy.round) %>%
        ungroup() %>%
        t() %>%
        data.frame() %>%
        mutate(variable = rownames(.))  %>%
        select(c(ncol(.), 1:(ncol(.) - 1)))

    col.names <- sapply(df.base[1,2:ncol(df.base)], as.character)
    names(df.base) <- c("variable", col.names)

    return(df.base)
}

balance.ttest <- function(var.df.in, var.to.split, value1, value2){
    ## Called by do.balance.ttest()

    ## Take in a dataframe and a character column name and
    ##  the two different values that column could take
    ## (column could take more than two variables but we only test
    ##  two in a ttest...duh)
    ##
    ## Expects relevant outcome to be in a column called 'value'
    ## Get two vectors of 'value' according to var.to.split
    ## Test for the difference in means between the two vectors
    ## Create a data.frame containing
    ##  mean estimates, p-values, stars and format them prettily

    x <- var.df.in %>%
        filter(.[var.to.split] == value1) %>%
        pull(value)

    y <- var.df.in %>%
        filter(.[var.to.split] == value2) %>%
        pull(value)

    mean.test <- t.test(x, y)

    tdf <- data.frame(difference = diff(mean.test$estimate),
                      pval = mean.test$p.value,
                      row.names = NULL) %>%
        mutate(pstar = ifelse(pval<0.1, "*",
                       ifelse(pval<0.05, "**",
                       ifelse(pval<0.01, "***", NA))),
               diff.trim = tidy.round(difference, 2),
               pval.trim = paste0(
                   "(", tidy.round(pval, 2), ")"))
    return(tdf)
}

ftest <- function(trimmed.df.in,
                  treatment.comparison.in,
                  balance.vars.in){

    df.test <- trimmed.df.in %>%
        mutate(Ttreatment = treatment == treatment.comparison.in)

    balance.reg <- lm(as.formula(
        paste("Ttreatment", "~",
              paste(balance.vars.in, collapse =  "+"))),
        data = df.test)  %>%
        summary()

    fvals <- list(fstat = balance.reg$fstatistic[["value"]],
                  fpval = pf(balance.reg$fstatistic[["value"]],
                             balance.reg$fstatistic[["numdf"]],
                             balance.reg$fstatistic[["dendf"]],
                             lower.tail = FALSE)) %>%
        lapply(tidy.round)

    return(fvals)
}

do.balance.ttest <- function(df.in,
                             t1.in, t2.in,
                             var.to.split.in = "treatment",
                             balance.vars.in = balance.vars.test,
                             pstar.only = FALSE,
                             diff.pval = !pstar.only){
    df.out <- df.in %>%
        pivot_longer(cols = one_of(balance.vars.in),
                     names_to = "variable",
                     values_to = "value") %>%
        group_by(variable) %>%
        do(balance.ttest(.,
                         var.to.split = var.to.split.in,
                         value1 = t1.in,
                         value2 = t2.in))

    if(pstar.only  == TRUE){
        return(df.out %>% select(pstar))
    }else if(diff.pval == TRUE){
        return(df.out %>% select(diff.trim, pval.trim))
    }
    return(df.out)
}

merge.balance.and.ttest <- function(means.df,
                                    ttest.df,
                                    vars.order,
                                    vars.recode){
    print(head(means.df))
    print(head(ttest.df))

    df.out <- merge(means.df, ttest.df, by = "variable") %>%
        mutate(variable = factor(variable,
                                 levels = vars.order,
                                 ordered = TRUE)) %>%
        arrange(variable) %>%
        mutate(variable = dplyr::recode(variable,  !!!vars.recode))

    return(df.out)
}

make.treatment.balance.table <- function(dt.in,
                                         vars.in = balance.vars.test){
## Insurance treament balance
    balance.df <- dt.in %>%
        group_by(treatment) %>%
        make.balance.col()

    baltest.info_insurance <- dt.in %>%
        do.balance.ttest(t1 = "Insurance",
                         t2 = "Insurance information",
                         balance.vars.in = vars.in)

    baltest.info_noinsurance <- dt.in %>%
        do.balance.ttest(t1 = "Insurance information",
                         t2 = "No insurance",
                         balance.vars.in = vars.in)

    balance.out <- merge(balance.df,
                         baltest.info_insurance,
                         by = "variable")  %>%
        merge(baltest.info_noinsurance,
              by = "variable") %>%
        mutate(variable = factor(variable,
                                 levels = balance.vars.order,
                                 ordered =  TRUE)) %>%
        arrange(variable) %>%
        mutate(variable = dplyr::recode(variable,  !!!balance.recodes))
    return(balance.out)
}

### Formatting specifications for balance table  ---------------------
si.normal <- "S[table-format=3.2]"
si.diff <- "S[table-format=8.0]"
si.pval <- paste("S[table-format=1.2,",
                 "table-space-text-pre=\\\\,(,",
                 "table-space-text-post=),",
                 "input-symbols = {( )}]")

### Balance tables pooled and by wave  -------------------------------
balance.pooled <- make.treatment.balance.table(dt.balance)

balance.w1 <- make.treatment.balance.table(
    dt.balance %>% filter(wave_2 == FALSE),
    wave.vars.to.test)

balance.w2 <- make.treatment.balance.table(
    dt.balance %>% filter(wave_2 == TRUE),
    wave.vars.to.test)

### Focus only on balance.pooled for the paper
names(balance.pooled) <- c(
    "Variable",
    mk.multicol("Insurance", 1, "c"),
    mk.multicol("Insurance information", 1, "c"),
    mk.multicol("No insurance", 1, "c"),
    mk.multicol("Insurance - info", 2, "c"),
    NA,
    mk.multicol("Info - no", 2, "c"),
    NA)

### Do ftests
finsurance_info <- ftest(dt %>%
                         filter(tinsurance != "noinsurance"),
                         "Insurance",
                         balance.control.vars)  %>%
    cbind() %>%
    data.frame()

finfo_no <- ftest(dt %>%
                  filter(tinsurance != "insurance"),
                  "Insurance information",
                  balance.control.vars) %>%
    cbind()%>%
    data.frame()

n.fstat.cols <- ncol(balance.pooled) - 4

stats <- data.frame(c("F-stat",  "P-value"),
                    cbind(matrix(NA,
                                 ncol = (n.fstat.cols-1),
                                 nrow = 2)),
                    finsurance_info, finfo_no)

colnames(stats) <- paste0("col",  seq(1, ncol(stats)))

ftab.tex.list <- stats %>%
    mutate_at(.vars = vars(one_of( "col5", "col6")),
              .funs = ~mk.multicol(., ncol = 2, align = "r")) %>%
    mutate(col1 = mk.multicol(col1, ncol = 1, align = "l")) %>%
    xtable() %>%
    print(only.contents = TRUE,
          sanitize.text.function = identity,
          include.rownames = FALSE,
          include.colnames = FALSE) %>%
    strsplit("\n") %>%
    pluck(1)

ftab.tex.list[[3]] <-
    "\\multicolumn{6}{l}{F-tests for joint insignificance}\\\\"

ftab.tex <- paste(ftab.tex.list, collapse = "\n")

## should be ncol + 1
ncol.balance.treatments <- ncol(balance.pooled)

treatments.align.vector <- rep("Q", (ncol.balance.treatments + 1))
treatments.align.marker <- paste(treatments.align.vector,
                                 collapse = "")
treatments.replace.marker <- paste(
    treatments.align.vector[1: ncol.balance.treatments],
    collapse = "")

treatments.align.replace <- paste0("l",
                                   si.normal,
                                   si.normal,
                                   si.normal,
                                   si.diff,
                                   si.pval,
                                   si.diff,
                                   si.pval)

balance.treatments.tex <- print(
    xtable(balance.pooled,
           align = treatments.align.marker ),
    floating = FALSE,
    sanitize.text.function = identity,
    include.rownames = FALSE)

balance.treatments.tex.list <- gsub(treatments.replace.marker,
                                    treatments.align.replace,
                                    balance.treatments.tex) %>%
    strsplit("\n") %>%
    pluck(1)

idx.to.insert <- which(
    balance.treatments.tex.list == "\\end{tabular}") - 1

balance.treatments.tex <- append(x = balance.treatments.tex.list,
                                 values = ftab.tex,
                                 after = idx.to.insert)  %>%
    paste(collapse = "\n")


balance.treatments.tex <- gsub("& NA",
                               "",
                               balance.treatments.tex)

panel.A.title <- paste("Summary of covariates ",
                       "across treatments, pooled sample.")

row1 <- sprintf("\\\\multicolumn\\{%s\\}\\{l\\}\\{%s\\}\\\\\\\\",
                ncol.balance.treatments, panel.A.title)

first.rows <- paste(
    row1,
    "&&&&\\\\multicolumn{4}{c}{Differences(p-value)} \\\\")

balance.treatments.tex <- gsub("Variable",
                               paste0(first.rows,
                                     "\\\\Variable"),
                               balance.treatments.tex) %>%
    scale.tabular()


### Balance between wave 1 and wave 2 --------------------------------

census2010 <- read.csv(file.path(data.path,
                                 "additional data",
                                 "census_data_for_balance.csv"))

ghana.2010 <- data.frame(
    variable = c("age",
                 "higheducation",
                 "employed",
                 "church_daily",
                 "pray_multiple",
                 "ethnicity_akan",
                 "ethnicity_ewe",
                 "ethnicity_ga",
                 "tmonthlyincome",
                 "female",
                 "revival"),
    values = c(mean(census2010$AGE, na.rm  = TRUE),
               mean(census2010$HIGHEST_EDUCATION > 8, na.rm = TRUE),
               mean(census2010$WORKED == 1, na.rm = TRUE),
               NA,
               NA,
               mean(census2010$ETHNICITY %in% seq(1, 20), na.rm = TRUE),
               mean(census2010$ETHNICITY == 30, na.rm = TRUE),
               mean(census2010$ETHNICITY %in% seq(21, 23), na.rm = TRUE),
               NA,
               mean(census2010$SEX == 2, na.rm = TRUE),
               NA))

balance.waves <- make.balance.col(dt.balance %>%
                                  group_by(wave_2),
                 balance.vars.in = wave.vars.to.test)

balance.waves.t <- do.balance.ttest(
    dt.balance %>% group_by(wave_2),
    "wave_2", t1.in = 0, t2.in =  1,
    balance.vars.in = wave.vars.to.test)

balance.wave.tab <- balance.waves  %>%
    merge(balance.waves.t, by = "variable") %>%
    merge(ghana.2010, by = "variable") %>%
    mutate(variable = factor(variable,
                             levels = balance.vars.order,
                             ordered =  TRUE)) %>%
    arrange(variable) %>%
    mutate(variable = dplyr::recode(as.character(variable),
                             !!!balance.recodes))

names(balance.wave.tab) <- c(
    "Variable",
    mk.multicol("Wave 1", 1, "c"),
    mk.multicol("Wave 2", 1, "c"),
    mk.multicol("Difference (p-value)", 2, "r"),
    mk.multicol("Ghana census 2010", 1, "c"))

## should be ncol + 1
ncol.balance.waves <- ncol(balance.wave.tab)
wave.align.vector <- rep("Q", (ncol.balance.waves + 1))
wave.align.marker <- paste(wave.align.vector, collapse = "")
wave.align.replace <- paste(wave.align.vector[1:ncol(balance.wave.tab)],
                            collapse = "")

balance.wave.tex <- gsub(
    "& NA",
    "",
    print(xtable(balance.wave.tab,
                 align = wave.align.marker),
          floating = FALSE,
          include.rownames = FALSE,
          sanitize.text.function = identity))

replace.align <- paste0("l",
                        si.normal,
                        si.normal,
                        si.diff,
                        si.pval,
                        si.normal)

panel.B.title <- paste("Summary of covariates ",
                       "across waves.")

waves.row1 <- sprintf(
    "\\\\multicolumn\\{%s\\}\\{l\\}\\{%s\\}\\\\",
    ncol.balance.waves, panel.B.title)

balance.wave.tex <- gsub("Variable",
                         paste0(waves.row1,
                                "\\\\Variable"),
                         balance.wave.tex)

balance.wave.tex <- gsub(wave.align.replace,
                         replace.align,
                         balance.wave.tex) %>%
    scale.tabular()

###  Make a balance panel --------------------------------------------
## Changed later to have separate balance tables
table.title <- paste("Pre-registed covariates compared across",
                     "treatments and waves.")

table.notes.base <- paste("\\\\",
                          "\\medskip\\par",
                          "\\begin{tabular}{p{15cm}}",
                          "\\hline",
                          "\\small",
                          "\\emph{Note} P-values reported in parentheses",
                          "are from a t-test of equality",
                          "of means.",
                          "%s",
                          "\\end{tabular}")

ghana.census.note <- paste(
    "`Ghana census 2010' column reflects data for",
    "census population above the age of 18 living",
    "in the Greater Accra region.")

table.note.panel <- sprintf(table.notes.base, ghana.census.note)

balance.panel <- paste("\\begin{table}",
                       "\\caption{",
                       table.title,
                       "}",
                       "\\label{Balance}",
                       balance.treatments.tex,
                       "\\\\",
                       "\\bigskip",
                       balance.wave.tex,
                       table.note.panel,
                       "\\end{table}")

write(balance.panel, file.path(paper.tables.folder,
                               "balance_panel.tex"))


## Write separate balance tables
treatments.title <- paste("Pre-registed covariates compared across",
                          "treatments.")

table.note.treatments <- sprintf(table.notes.base, "")
treatment.balance.tex.out <-  paste("\\begin{table}",
                                    "\\caption{",
                                    treatments.title,
                                    "}",
                                    "\\label{Balance}",
                                    balance.treatments.tex,
                                    table.note.treatments,
                                    "\\end{table}")
write(treatment.balance.tex.out, file.path(paper.tables.folder,
                               "balance_treatments.tex"))

waves.title <- paste("Pre-registed covariates compared across",
                     "waves.")
waves.balance.tex.out <-  paste("\\begin{table}",
                                "\\caption{",
                                waves.title,
                                "}",
                                "\\label{Balance_waves}",
                                balance.wave.tex,
                                table.note.panel,
                                "\\end{table}")

write(waves.balance.tex.out, file.path(paper.tables.folder,
                               "balance_waves.tex"))


### Revival tables  --------------------------------------------------
### Prepare some formatting for the revival match table
new.col.names.for.revival <- c(
    "Variable",
    mk.multicol("Revival = 0", 1, "c"),
    mk.multicol("Revival = 1", 1, "c"),
    mk.multicol("Difference (p-value)", 2, "r"))

col.indexes <- seq(1, 4)
names(col.indexes) <- new.col.names.for.revival

revival.means <- dt.balance.revival %>%
    group_by(revival) %>%
    make.balance.col(balance.vars.in = revival.balance.vars)

revival.ttest <- dt.balance.revival %>%
    do.balance.ttest(t1.in = 0,
                     t2.in = 1,
                     var.to.split.in = "revival",
                     balance.vars.in = revival.balance.vars)

revival.match <- merge.balance.and.ttest(revival.means,
                                         revival.ttest,
                                         names(revival.recodes),
                                         revival.recodes)%>%
    rename(!!!col.indexes)

### More formatting to get the revival table in good tex
ncol.revival.match <- ncol(revival.match)
revival.match.align.vector <- rep("Q", (ncol.revival.match + 1))
revival.match.align.marker <- paste(
    revival.match.align.vector, collapse = "")
revival.match.align.replace <- paste(
    revival.match.align.vector[1:ncol(revival.match)], collapse = "")

revival.match.tex <- gsub(
    "& pval.trim",
    "",
    print(
        xtable(revival.match,
            align = revival.match.align.marker),
        floating = FALSE,
        include.rownames = FALSE,
        sanitize.text.function = identity))

revival.replace.align <- paste0("l",
                                si.normal,
                                si.normal,
                                si.diff,
                                si.pval)

revival.table.notes <- paste(
    "\\\\",
    "\\medskip\\par",
    "\\begin{tabular}{p{15cm}}",
    "\\small",
    "\\emph{Note} P-values reported in parentheses",
    "are from a t-test of equality",
    "of means.",
    "Comparisons of pre-registered covariates and",
    "some additional variables describing church",
    "and spiritual behaviour",
    "\\end{tabular}")

revival.match.tex.out <- paste(
    "\\begin{table}[htbp!]",
    "\\label{",
   "revival_covariates",
    "}",
    "\\caption{",
   paste("Comparing demographic variables and religious",
         "behaviour across participants who took part",
         "in regular and revival weeks"),
   "}",
   gsub(revival.match.align.replace,
        revival.replace.align,
        revival.match.tex),
   revival.table.notes,
   "\\end{table}")

write(revival.match.tex.out, file.path(paper.tables.folder,
                               "revival_covariates.tex"))

### Revival experiment summary table ---------------------------------

revival.experiment <- dt %>%
    filter(wave == 2 &
           !is.na(revival_assignment))

assignment.table <- revival.experiment %>%
    pull(revival_assignment) %>%
    table()

N.assigned.to.revival <- assignment.table["1"]
N.assigned.to.regular <- assignment.table["0"]

prop.regular.comply <- revival.experiment %>%
    filter(revival_assignment == 0 &
           revival_compliance_type == "complier") %>%
    nrow() %>%
    `/`(N.assigned.to.regular) %>%
    round(2)

prop.revival.comply <- revival.experiment %>%
    filter(revival_assignment == 1 &
           revival_compliance_type == "complier") %>%
    nrow() %>%
    `/`(N.assigned.to.revival)%>%
    round(2)

revival.summary <- data.frame(
    column.labels = c(
        "Total recruited in Wave 2",
        paste("Number randomly assigned to attend the experiment"),
        "",
        "Proportion complying with treatment assignment"),
    col1 = c("",
             "regular week",
             N.assigned.to.regular,
             prop.regular.comply),
    col2 = c("",
             "revival week",
             N.assigned.to.revival,
             prop.revival.comply),
    col3 = c(dt %>% filter(wave == 2) %>% nrow(),
             "total",
             (N.assigned.to.regular + N.assigned.to.revival),
             ""))

revival.summary.tex <- revival.summary %>%
    xtable(align = paste0(c("l", "p{10cm}", rep("l", 3))),
           caption =
               paste("Recruitment and assignment of",
                     "revival participation in Wave 2"),
           label = "revival_experiment_summary") %>%
    print(include.colnames =  FALSE,
          include.rownames = FALSE,
          caption.placement = "top")

write(revival.summary.tex,
      file.path(paper.tables.folder,
                "revival_experiment_summary.tex"))

### Balance across revival participants

revival.experiment <- dt %>%
    select(one_of(c(balance.vars.select,
                    "revival_assignment",
                    "revival_compliance_type"))) %>%
    mutate(female = gender == 1,
           wave_2 = wave == 2) %>%
    filter(wave_2 == 1 &
           !is.na(revival_assignment)) %>%
    select(-c(gender, wave,  wave_2))

revival.experiment.means <- revival.experiment  %>%
    group_by(revival) %>%
    make.balance.col(balance.vars.in = revival.balance.vars)

revival.experiment.ttest <- revival.experiment %>%
    do.balance.ttest(t1.in = 0,
                     t2.in = 1,
                     var.to.split.in = "revival",
                     balance.vars.in = revival.balance.vars)

revival.experiment.balance <- merge.balance.and.ttest(
    revival.experiment.means,
    revival.experiment.ttest,
    names(revival.recodes),
    revival.recodes)

revival.complier <- revival.experiment %>%
    filter(revival_compliance_type == "complier")

revival.experiment.complier.means <- revival.complier  %>%
    group_by(revival) %>%
    make.balance.col(balance.vars.in = revival.balance.vars)

revival.experiment.complier.ttest <- revival.complier %>%
    do.balance.ttest(t1.in = 0,
                     t2.in = 1,
                     var.to.split.in = "revival",
                     balance.vars.in = revival.balance.vars)

revival.experiment.complier.balance <- merge.balance.and.ttest(
    revival.experiment.complier.means,
    revival.experiment.complier.ttest,
    names(revival.recodes),
    revival.recodes)


check.compliers <- revival.experiment %>%
    mutate(complier = revival_compliance_type == "complier")%>%
    select(-revival_compliance_type)

check.complier.means <- check.compliers  %>%
    group_by(complier) %>%
    make.balance.col(balance.vars.in = revival.balance.vars)

check.complier.ttest <- check.compliers %>%
    do.balance.ttest(t1.in = 0,
                     t2.in = 1,
                     var.to.split.in = "complier",
                     balance.vars.in = revival.balance.vars)

check.complier.balance <- merge.balance.and.ttest(
    check.complier.means,
    check.complier.ttest,
    names(revival.recodes),
    revival.recodes)

balance_to_tex <- function(balance.tab.in,
                           col.names.in,
                           tab.notes.in = revival.table.notes,
                           label.in =  "",
                           caption.in = "",
                           file.path.in){
    ## Function that takes in a balance data.frame and transforms
    ##  it into tex, formatting the table according to the
    ##  latex si units package for tables

    new.col.names <- c(
        "Variable",
        mk.multicol(col.names.in[[1]], 1, "c"),
        mk.multicol(col.names.in[[2]], 1, "c"),
        mk.multicol("Difference (p-value)", 2, "r"))

    col.indexes <- seq(1, 4)
    names(col.indexes) <- new.col.names

    balance.tab.in <- balance.tab.in %>%
        rename(!!!col.indexes)

    ncol.tab <- ncol(balance.tab.in)
    tab.align.vector <- rep("Q", (ncol.tab + 1))

    tab.align.marker <- paste(tab.align.vector,
                              collapse = "")
    tab.align.to.replace <- paste(tab.align.vector[1:ncol.tab],
                               collapse = "")

    tab.replace.align <- paste0("l",
                                si.normal,
                                si.normal,
                                si.diff,
                                si.pval)

    tab.tex <- gsub(
        "& pval.trim",
        "",
        print(
            xtable(balance.tab.in,
                   align = tab.align.marker),
            floating = FALSE,
            include.rownames = FALSE,
            sanitize.text.function = identity))

    tab.tex.out <- paste(
        "\\begin{table}[htbp!]",
        "\\caption{",
        caption.in,
        "}",
        "\\label{",
        label.in,
        "}",
        gsub(tab.align.to.replace,
             tab.replace.align,
             tab.tex),
        tab.notes.in,
        "\\end{table}")

    write(x = tab.tex.out,
          file = file.path(paper.tables.folder, file.path.in))

    return(tab.tex.out)
}

balance_to_tex(revival.experiment.balance,
               c("Revival = 0", "Revival = 1"),
               "",
               label.in = "revival_experiment_only_balance",
               caption.in = paste("Balance amongst randomly assigned",
                                  "revival participants"),
               "revival_experiment_only_balance.tex")

balance_to_tex(revival.experiment.complier.balance,
               c("Revival = 0", "Revival = 1"),
               "",
               label.in = "revival_experiment_complier_balance",
               caption.in = paste("Comparing participants who complied",
                                  "with revival week",
                                  "treatment assignment:",
                                  "assigned to revival and complied",
                                  "compared to assigned to regular week",
                                  "and complied."),
               "revival_experiment_complier_balance.tex")

balance_to_tex(check.complier.balance,
               c("Complier = 0", "Not complier = 1"),
               "",
               label.in = "check_compliance",
               caption.in = paste("Comparing participants who complied",
                                  "with treatment assignment to",
                                  "participants who did not comply with",
                                  "treatment assignment."),
               "revival_experiment_check_compliers.tex")

### Create a summary table of additional non-preregistered covariates---

non.preregistered.vars <- c("is_married",
                            "always_church",
                            "has_nih",
                            "has_other_insurance",
                            "spouse_in_church")

other.covariates.recodes <- c("Is married",
                              "Has always belonged to this church",
                              "Registered for National Health Insurance",
                              "Has any other sort of insurance",
                              paste("Met or will meet spouse in church"))

names(other.covariates.recodes) <- non.preregistered.vars

other.covariates.df <- dt %>%
    mutate(spouse_in_church = spouse == "Church") %>%
    select(one_of(c("wave",
                    non.preregistered.vars))) %>%
    mutate(wave_2 = wave == 2)

other.covariates.diff <- do.balance.ttest(
    other.covariates.df,
    t1.in = 0,
    t2.in = 1,
    var.to.split.in = "wave_2",
    balance.vars.in = non.preregistered.vars)

other.covariates.tab <- other.covariates.df %>%
    group_by(wave_2) %>%
    make.balance.col(balance.vars.in = non.preregistered.vars)%>%
    merge.balance.and.ttest(other.covariates.diff,
                            non.preregistered.vars,
                            other.covariates.recodes) %>%
    rename("wave = 1" = 3, "wave = 2" = 2)

other.covariate.table.notes <- paste(
    "\\\\",
    "\\medskip\\par",
    "\\begin{tabular}{p{15cm}}",
    "\\small",
    "\\emph{Note} P-values reported in parentheses",
    "are from a t-test of equality",
    "of means.",
    "\\end{tabular}")

other.covariates.tex <- balance_to_tex(
    balance.tab.in = other.covariates.tab,
    col.names.in = c("wave = 1", "wave = 2"),
    tab.notes.in = other.covariate.table.notes,
    label.in = "other_covariates",
    caption.in = paste("Mean values of additional demographic variables",
                       "that were not pre-registered"),
    file.path.in = "summary_other_covariates.tex")


### Summarise allocation decisions ----------------------------------

allocation.summary.df <- dt %>%
    select(one_of(paste("choice", keep.choices, sep = "_"))) %>%
    apply(2, summary) %>%
    t()

### Functions to summarise the role of the church --------------------
support.recodes <- c(
    no_one = "No one",
    government_or_ngo = paste("Formal social services",
                              "(government or NGO"),
    friends = "Friends",
    family =  "Family",
    pastor = "Church leadership",
    imam = "Church leadership",
    work_superior = "Work superior",
    medical_professional = "Medical professional",
    government_hospital = "Medical professional",
    bank = "Formal financial services (bank, microfinance, etc.)",
    other =  "Other",
    church_community = "Church community",
    church_member = "Church community",
    traditional_healer = "Traditional healer")

get.summary.categorical <- function(var.in, dt.in = dt){
    ntotal <- nrow(dt.in)

    sum.tab <- dt.in %>%
        select(one_of(var.in)) %>%
        table()

    sum.df <- (sum.tab / ntotal) %>%
        data.frame() %>%
        rename("variable" = 1, "frequency" = 2) %>%
        mutate(variable = as.character(variable)) %>%
        filter(!variable == "NA")  %>%
        mutate(variable = dplyr::recode(variable, !!!support.recodes)) %>%
        group_by(variable) %>%
        summarise(frequency = mean(frequency, na.rm = TRUE))

    return(sum.df)
}

get.summary.dummies <- function(var.prefix, dt.in = dt){
    sum.df <- dt.in %>%
        select(starts_with(var.prefix)) %>%
        mutate_at(.vars = vars(everything()),
                  .funs = ~as.numeric(., na.rm  = TRUE)) %>%
        rename_at(.vars = vars(everything()),
                  .funs = ~gsub(var.prefix, "", .)) %>%
        colMeans(na.rm = TRUE) %>%
        data.frame() %>%
        rename("frequency" = 1) %>%
        mutate(variable = rownames(.)) %>%
        mutate(variable = dplyr::recode(variable, !!!support.recodes)) %>%
        group_by(variable) %>%
        summarise(frequency = mean(frequency, na.rm = TRUE)) %>%
        filter(!is.na(frequency)) %>%
        arrange(desc(frequency)) %>%
        filter(variable != "other_details")

    return(sum.df)
}

### Role of church formatting functions ------------------------------

mk.tex.role <- function(qtab){
    ## Takes in a list containing
    ##  i) a table (qtab$table) OR a table already in tex (qtab$table.tex)
    ##  ii) a question as a string (qtab$question)
    ##  iii) (optional) a  numberof columns to stretch the question

    question <- paste0("\\vspace{0.1em}",
                       "\\emph{",
                       qtab$question,
                       "}")

    if(is.null(qtab$table.tex)){
        ## For sub-tables that containtwo columns
        ## col 1 = an option label
        ## col 2 = a proportion
        question.ncol <- 2
        question.tex <- paste(mk.multicol(question,
                                          question.ncol,
                                          "p{\\linewidth}"))
        table <- qtab$table
        table.tex <- xtable(table) %>%
            print(include.rownames = FALSE,
                  include.colnames = FALSE,
                  only.contents = TRUE,
                  hline.after = NULL)
        tex.out <- paste(question.tex,
                         table.tex,
                         sep = "\\\\")
    }else{
        ## For sub-tables that need special formatting, e.g because
        ##  they present a distribution rather than proportions
        ##  responding to a particular answer

        question.ncol <- qtab$question.ncol
        question.tex <- paste(mk.multicol(question,
                                          question.ncol,
                                          "p{\\linewidth}"))

        ## Used fixed = TRUE to double the backslashes to avoid
        ##  them getting swallowed between gsub and paste

        begin.tabular.tex <- paste0(
            "\\\\begin\\{tabular\\}\\{",
            paste0(qtab$ncol.align[1:(length(qtab$ncol.align) - 1)],
                   collapse = ""),
            "\\}\n")

        print("###")
        print(begin.tabular.tex)

        question.tex.escaped <- gsub("\\", "\\\\", question.tex,
                                     fixed = TRUE)

        question.tex.and.begin <- paste0(
            begin.tabular.tex,
            question.tex.escaped,
            "\\\\\\\\")

        table.tex <- qtab$table.tex

        tex.out <- gsub(begin.tabular.tex,
                        question.tex.and.begin,
                        table.tex)
#        print(table.tex)
    }

    print(tex.out)
    return(tex.out)
}

put.role.table.together <- function(table.question.list,
                                    label.in,
                                    caption.in,
                                    table.for.paper = TRUE){
    ## Function that takes a list of questions and summary tables
    ## and puts them together into a larger table
    tex.meat <- lapply(table.question.list,
                      mk.tex.role) %>%
        paste(collapse = "")

    tex.out <- paste(
        "\\begin{table}[htbp!]",
        "\\label{",
        label.in,
        "}",
        "\\caption{",
        caption.in,
        "}",
        "\\begin{tabular}{p{12cm}r}",
        "\\hline",
        tex.meat,
        "\\\\\\hline",
        "\\end{tabular}",
        "\\end{table}")

    if(table.for.paper == TRUE){
        write(tex.out,
              file.path(paper.tables.folder, paste0(label.in, ".tex")))
    }

    return(tex.out)
}

### Summarise the variables according to their type ------------------
role.categorical.vars <- c("spouse",
                           "business_partners",
                           "close_friends",
                           "prayercamp",
                           "god_finances",
                           "charity",
                           "church_debt",
                           "church_assistance",
                           "church_move",
                           "church_distance",
                           "church_attendance",
                           "spiritual_attack")

role.dummies.vars <- c("help_family_",
                     "help_financial_",
                     "help_health_",
                     "church_convince_",
                     "church_ministry_",
                     "church_reasons_",
                     "church_ministry_",
                     "church_convince_")

## Some tidying lists and variables
business.options <- c(
    paste("Yes, I only work or do business",
          "with people from my church"),
    paste("Yes, I try to do business with people",
          "from my church but it is not so important"),
    paste("No, I prefer not to work or do",
          "business with people from my church"),
    "No, it is not important at all")

friends.options = c(
    "Yes, I try to only make friends with people from my church",
    paste("Yes, I try to seek people from my church but",
          "it is not so important"),
    "No, I prefer not to be friends with people from my church",
    "No, it is not important at all")

yes.no.options <- c("Yes", "No")

fix.yes.no <- function(df.in){
    df.out <- df.in %>%
        mutate(variable = factor(variable,
                                 levels = yes.no.options,
                                 ordered = TRUE)) %>%
        arrange(variable)
    return(df.out)
}

## Apply summarizing functions to groups of variables
role.categorical.list <- lapply(role.categorical.vars,
                                get.summary.categorical)
names(role.categorical.list) <- role.categorical.vars

role.dummies.list <- lapply(role.dummies.vars,
                            get.summary.dummies)
names(role.dummies.list) <- role.dummies.vars

## Extract individual tables
spouse.sum <- role.categorical.list[[1]] %>%
    arrange(desc(frequency))

business.sum <- role.categorical.list[[2]] %>%
    mutate(variable = factor(variable,
                             levels = business.options,
                             ordered = TRUE)) %>%
    arrange(variable)

friends.sum <- role.categorical.list[[3]] %>%
    mutate(variable = factor(variable,
                             levels = friends.options,
                             ordered = TRUE)) %>%
    arrange(variable)

prayercamp.sum <- role.categorical.list$prayercamp %>%
    fix.yes.no()

god_finances.sum <- role.categorical.list[[5]]

charity.sum <- role.categorical.list[[6]]

church_debt.sum <- role.categorical.list[[7]] %>%
    fix.yes.no()

church_assistance.sum <- role.categorical.list$church_assistance %>%
    fix.yes.no()

church_move.sum <- role.categorical.list$church_move %>%
    fix.yes.no()

spiritual_attack.sum <- role.categorical.list$spiritual_attack %>%
    fix.yes.no()

church.distance.options <- c("Less than 30 minutes",
                             "30 minutes to 1 hour",
                             "More than 1 hour")

church_distance.sum <- role.categorical.list$church_distance %>%
    mutate(variable = factor(variable,
                             levels = church.distance.options,
                             ordered = TRUE))

church.attendance.options <- c("Daily",
                               "More than once per week",
                               "Weekly",
                               "A few times per month",
                               "A few times per year",
                               "Less than once a year")

church_attendance.sum <- role.categorical.list$church_attendance %>%
    mutate(variable = factor(variable,
                             levels = church.attendance.options,
                             ordered = TRUE)) %>%
    arrange(variable)

### Unpack the variables that are summarised as dummies
counselling.sum <- role.dummies.list[[1]]

not.asked.financial <- c("Church leadership")
financial.sum <- role.dummies.list[[2]] %>%
    filter(!variable %in% not.asked.financial)

health.sum <- role.dummies.list[[3]]

convince.recodes <- c(
    "inviting" = "Inviting friends or family to church",
    "prayer" = "Praying for others to attend church",
    "preaching_outside_church" = paste(
        "Preaching or teaching outsidethe church"),
    "distributing_material" = "Distributing church material",
    "preaching_in_church" = "Preaching or teaching in the church",
    "public" = paste("Preaching, writing or",
                     "distributing material outside the church"))

convince.sum <- role.dummies.list$church_convince_ %>%
    mutate(variable = dplyr::recode(variable, !!!convince.recodes)) %>%
    arrange(desc(frequency))

church_reasons.recodes <- c(
    "teaching" = paste("The teaching about God corresponds",
                       "to what I believe in"),
    "moral_guidance" = paste("I go for the moral guidance to",
                             "me and my family"),
    "atmosphere" = paste("I like the atmosphere of the services"),
    "Friends" = paste("Friends or relatives brought me there"),
    "welcoming_members" = paste("Other members made an effort",
                                "to welcome me"),
    "interesting_congregation" = paste(
        "The congregation contains many interesting and",
        "successful people"),
    "close_to_home" = paste("I hope to meet a good marriage",
                            "partner for me or my children"),
    "marriage_partner" = paste("The building is close to my home"),
    "comfortable_facilities"  = paste(
        "The facilities are comfortable",
        "(e.g. air-conditioning, comfortable seating, etc.)"),
    "Other" = "Other")

reasons.sum <- role.dummies.list$church_reasons_ %>%
    mutate(variable = dplyr::recode(variable,
                             !!!church_reasons.recodes)) %>%
    arrange(desc(frequency))%>%
    filter(variable != "Other")

ministry.recodes <- c("all_ages" = "Men, women, or youth fellowship",
                      "home_fellowship"= "Home fellowship",
                      "choir" = "Choir, praise and worship",
                      "prayer" = "Prayer ministry",
                      "outreach" = "Outreach ministry",
                      "ushering_and_protocol" = "Ushering or protocol",
                      "children" = "Children's ministry",
                      "pastoring_or_deacon" = "Pastoring or deacon")

ministry.sum <- role.dummies.list$church_ministry_ %>%
    mutate(variable = dplyr::recode(variable, !!!ministry.recodes)) %>%
    arrange(desc(frequency))

ministry.count <- dt$ministry_count %>%
    table() %>%
    `/`(nrow(dt)) %>%
    tidy.round() %>%
    rbind(names(.)) %>%
    data.frame() %>%
    map_df(rev)

ministry.count.align = rep("l", ncol(ministry.count) + 1)

ministry.count.tex0 <-
    xtable(ministry.count,
           align = ministry.count.align) %>%
    print(include.rownames = FALSE,
        include.colnames = FALSE,
        hline.after = NULL)

ministry.count.tex1 <- gsub("\\\\begin\\{table\\}\\[ht\\]",
                           "",
                           ministry.count.tex0)

ministry.count.tex2 <- gsub("\\\\centering",
                           "",
                           ministry.count.tex1)

ministry.count.tex <- gsub("\\\\end\\{table\\}",
                           "",
                           ministry.count.tex2)

giving_share.sum <- dt %>%
    filter(!is.infinite(giving_share)) %>%
    pull(giving_share) %>%
    summary() %>%
    rbind()%>%
    data.frame() %>%
    select(1:6) %>%
    rename("Min" = 1,
           "Q1" = 2,
           "Median" = 3,
           "Mean"=4,
           "Q3" = 5,
           "Max" = 6)

giving.share.align = rep("l", ncol(giving_share.sum) + 1)

giving_share.tex0 <- giving_share.sum %>%
    xtable(align = giving.share.align) %>%
    print(include.rownames = FALSE,
          include.colnames = TRUE,
          hline.after = NULL)

giving_share.tex1 <- gsub("\\\\begin\\{table\\}\\[ht\\]",
                           "",
                           giving_share.tex0)

giving_share.tex2 <- gsub("\\\\centering",
                           "",
                           giving_share.tex1)

giving_share.tex <- gsub("\\\\end\\{table\\}",
                           "\\\\\\\\",
                         giving_share.tex2)

### Certain variables need extra pre-processing before summarising
change_behaviour <- dt %>%
    select(food, alcohol_reasons)%>%
    mutate_at(.vars = vars(everything()),
              ~. == "Yes") %>%
    mutate(change_behaviour = ifelse(
    (change_behaviour = rowSums(.) > 0),
    "Yes", "No")) %>%
    pull(change_behaviour)

behaviour.sum <- data.frame(table(change_behaviour) /
                            length(change_behaviour))%>%
    rename("variable"  = 1, "frequency" = 2) %>%
    fix.yes.no()

## Put the summary tables with the questions asked into a list
select.all.text <- " (Select all that apply)"

mk.qlist <- function(question, sumtab){
    return(list(question = question,
                table = sumtab))
}

attitude.panel <- list(
    mk.qlist(paste("What are the main reasons you",
                   "are with your current church?"),
             reasons.sum),
    list(question = paste("Does giving to charity serve the",
                          "same spiritual duty as giving directly",
                          "to the church?"),
         table = charity.sum),
    list(question = paste("Within the last 2 years, have you ",
                          "ever attended a prayer camp either",
                          "for yourself or on behalf of a friend",
                          "or family member?"),
         table = prayercamp.sum),
    mk.qlist(paste("Do you believe the last death in your family",
                   "was a spiritual attack?"),
             spiritual_attack.sum))

behaviour.panel <- list(
    list(question =
             paste("Share of income given to the church each month,",
                   "excluding tithes."),
         table.tex = giving_share.tex,
         question.ncol = ncol(giving_share.sum),
         ncol.align = giving.share.align),
    mk.qlist(paste("How many times per week or per",
                   "year do you attend your church?"),
             church_attendance.sum),
    list(question = paste("How many hours do you travel",
                          "to attend your regular church?"),
          table = church_distance.sum),
    list(question = paste("Have you moved your place of ",
                          "residence in order to be closer to your",
                          "church?"),
         table  =  church_move.sum),
    mk.qlist(paste("Have you changed the food you eat or drink",
                  "for religious reasons"),
             behaviour.sum),
    mk.qlist(paste("Have you ever gone into debt to pay a",
                  "church debt?"),
            church_debt.sum),
    list(question = paste("In the last 6 weeks, have you",
                          "engaged in any of the following",
                          "activities to bring others to your church?",
                          select.all.text),
         table = convince.sum),
    mk.qlist(paste("Are you engaged in any of the following",
                   "ministries of your church?",
                   select.all.text),
             ministry.sum),
    list(question = paste(
             "Number of church ministries, and proportion",
             "of participants enagaged in that number",
             "of ministries"),
         table.tex = ministry.count.tex,
         question.ncol = ncol(ministry.count),
         ncol.align = ministry.count.align))

social.panel <- list(
    list(question = paste("Is it important for you that your close",
                          "friends come from the same church as you?"),
         table = friends.sum),
    list(question = paste("Who do you go to when you need counselling",
                          "about family or personal issues",
                          select.all.text),
         table = counselling.sum),
    list(question = paste("Who do you go to for support when you",
                          "have a health emergency?",
                          select.all.text),
         table = health.sum))

financial.panel <- list(
    list(question = paste("Do you work or do business with people",
                          "from the same church as you?"),
         table = business.sum),
    list(question = paste("Who do you go to when you need",
                          "financial help?",
                          select.all.text),
         table = financial.sum),
    list(question = paste("How is God involved in your finances?"),
         table = god_finances.sum),
    list(question = paste("Have you received financial",
                          "assistance from your church in the",
                          "last 5 years?"),
         table = church_assistance.sum))


## Tex versions of the tables
## Write them out here as well
## Remember that put.roles.together can take a flag 'table.for.paper = T'

attitude.panel.tex <- put.role.table.together(
    attitude.panel,
    "summary_religious_attitudes",
    "Religious beliefs and attitudes")

behaviour.panel.tex <- put.role.table.together(
    behaviour.panel,
    "summary_costly_behaviour",
    "Costly religious behaviour")

social.panel.tex <- put.role.table.together(
    social.panel,
    "summary_role_social",
    "The social role of the church")

financial.panel.tex <- put.role.table.together(
    financial.panel,
    "summary_role_financial",
    "The financial role of the church")

print("End of script -- made some nice tables")
