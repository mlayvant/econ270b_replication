
### Code to produce regression tables for "God Insures" paper
### Last updated: 25 March 2020
### Heterogeneity analysis

#source("define_regression_helper_functions.R")

dt.nonrevival.insurance_vs_info <- dt %>%
    filter(revival == 0) %>%
    filter(treatment!= "No insurance")


heterogeneity_families <- readRDS(
    "useful objects/heterogeneity_variable_families.Rds")

print("These are the grouping of heterogeneity variables")
print(names(heterogeneity_families))


## Define some functions that will allow us to run the treatment
## regression for each of the choice outcomes and to
## include each heterogeneity index as a separate control.
## Use the wrapper 'apply' function to loop over choice outcomes

apply_specifications_by_choice <- function(spec_df,
                                           index_name,
                                           dt.in,
                                           table.for.paper.in = FALSE){

    ## Creates a set of tables with a single dependent variable
    ##  and multiple specifications according to the index
    ## spec_df should contain a single dependent variable

    choice_name <- spec_df[["dependent"]] %>%
        unique() %>%
        pluck()

    tex_name <- paste(index_name, choice_name, sep = "_")

    tab_title <- paste0(stri_trans_totitle(
        gsub("_", " ", index_name)),
        ": ",
        gsub("_", " versus ", choice_name))

    mod_out <- run.reg.columns(
        dt.in = dt.in,
        specs.df = spec_df,
        tab.title = tab_title,
        tab.name = tex_name,
        col.labs.in = NULL,
        col.sep.in = NULL,
        fit.to.width.in = TRUE,
        table.for.paper = table.for.paper.in)

    return(mod_out)
}

do_heterogeneity_by_choice <- function(index_name,
                                       index_vars,
                                       choice_names.in = choice_names,
                                       table.for.paper.in =  FALSE){

    heterogeneity.controls <- paste("tinsurance *",
                                    c(index_name,
                                      index_vars))

    heterogeneity.specifications <- data.frame(
        use.individual.controls = TRUE,
        use.church.dummies = TRUE,
        use.wave.dummy = TRUE,
        treatment = "tinsurance",
    additional.controls = heterogeneity.controls,
    dependent = rep(choice_names.in,
                    each = length(heterogeneity.controls)))

    heterogeneity.models <-
        plyr::dlply(.data = heterogeneity.specifications,
                    .variables = "dependent",
                    .fun = apply_specifications_by_choice,
                    dt.in = dt.nonrevival.insurance_vs_info,
                    index_name = index_name,
                    table.for.paper = table.for.paper.in)

    return(heterogeneity.models)
}


## Define some functions that will allow us to create the tables that
## show each index and its component parts, and to apply this function
## in a loop over the main choice outcomes

apply_specifications_by_index <- function(spec_df,
                                          dt.in,
                                          choice.outcomes,
                                          tex.suffix = NULL,
                                          table.for.paper.in = FALSE){

    ## Creates a set of tables with a single index and different
    ##  choice outcomes
    ## spec_df should contain a single set of additional.controls
    ## (the index is specified in the column "additional.controls")

    index_name_base <- spec_df[["additional.controls"]] %>%
        unique() %>%
        pluck()

    index_name <- gsub("tinsurance \\* ", "", index_name_base)

    tex_name <- paste("heterogeneity", index_name, sep = "_")

    if(!is.null(tex.suffix)){
        tex_name = paste0(tex_name, "_", tex.suffix)
    }

    tab.title.base <- paste0(stri_trans_totitle(
        gsub("_", " ", index_name)))

    tab.title = paste0(
        tab.title.base,
        ": ",
        gsub("_", " v ", paste(choice.outcomes, collapse =  ", ")))

    col.labs.for.paper <- tidy.col.labels.in.parbox(choice.outcomes)

    mod_out <- run.reg.columns(
        dt.in = dt.in,
        specs.df = spec_df,
        tab.title = tab.title,
        tab.name = tex_name,
        col.labs.in = col.labs.for.paper,
        col.sep.in = NULL,
        fit.to.width.in = TRUE,
        table.for.paper = table.for.paper.in)

    return(mod_out)
}


do_heterogeneity_by_index <- function(choice.names.in,
                                      tex.suffix.in = FALSE,
                                      table.for.paper.in = FALSE){

    ## Heterogeneity indexes use the names defined in dt
    heterogeneity_indexes = c("costly_behaviour",
                              "use_church_spiritually",
                              "use_church_network")

    index_specifications <- data.frame(
        use.individual.controls = TRUE,
        use.church.dummies = TRUE,
        use.wave.dummy = TRUE,
        treatment = "tinsurance",
        additional.controls = paste("tinsurance *",
                                    heterogeneity_indexes),
        dependent = rep(choice.names.in,
                        each = length(heterogeneity_indexes)))

    models_by_index <-
        plyr::dlply(.data = index_specifications,
                    .variables = "additional.controls",
                    .fun = apply_specifications_by_index,
                    dt.in = dt.nonrevival.insurance_vs_info,
                    choice.outcomes = choice.names.in,
                    tex.suffix = tex.suffix.in,
                    table.for.paper = table.for.paper.in)

    return(models_by_index)
}

## Run the heterogeneity regressions
## Families of tables for each outcome are saved with the format
##  index_name_[keep_church].tex

heterogeneity.main.outcomes <- c(
    "keep_church",
    "keep_street",
    "keep_thanks")

## Creates Tables 29, 30 and 31
main.choices.by.index <-
    do_heterogeneity_by_index(heterogeneity.main.outcomes,
                              tex.suffix.in = "main_outcomes",
                              table.for.paper.in = TRUE)

## Creates Tables 32, 33, 34
costly <- do_heterogeneity_by_choice(
    "costly_behaviour",
    heterogeneity_families$behaviour,
    choice_names.in = heterogeneity.main.outcomes,
    table.for.paper.in = TRUE)

## Creates Tables 35, 36, 37
spiritual <- do_heterogeneity_by_choice(
    "use_church_spiritually",
    heterogeneity_families$attitudes,
    choice_names.in = heterogeneity.main.outcomes,
    table.for.paper.in = TRUE)

## Creates Tables 38, 39, 40
network <- do_heterogeneity_by_choice(
    "use_church_network",
    heterogeneity_families$network,
    choice_names.in = heterogeneity.main.outcomes,
    table.for.paper.in = TRUE)

