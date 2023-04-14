### Defines a series of functions that run the different regressions
### discussed in the God Insures paper

### Also define some of the default specifications for the different
### regressions

### Functions are a mix of functions for estimating and for
### formatting output


### Default specifications -------------------------------------------

## Variables included in the regression but excluded from all the
## tables. These include the scale variable from the Tobit regression, the
## individual controls, church dummies, and the wave control
VARS.NOT.SHOWN <- c(INDIVIDUAL.CONTROLS,
                    "Log\\(scale\\)",
                    "Dchurch",
                    "wave")

## Across most tables, we first have a column where we include controls
## and then a column where we do not include controls
controls.ix <- rep(c(FALSE, TRUE), length(keep.choices))

## Prepare the specification in a data frame. Our basic specifications
## run the treatment regression for the three main outcomes
basic.specifications <- data.frame(
    dependent = rep(keep.choices, each = 2),
    use.individual.controls = controls.ix,
    use.wave.dummy = controls.ix,
    use.church.dummies = controls.ix) %>%
    mutate(model.names = paste0(dependent,
                               rep(c("", "_controls"),
                                   length_out = nrow(.))))

### Define functions -------------------------------------------------
run.ols <- function(df.in,
                      dep.var,
                      treatments,
                      controls,
                      vars.exog = NULL,
                      vars.endog = NULL){
    ## Use lm function to run the regression
    ## -build the specification as a formula from character variables
    ##  controls should already be a single character string,
    ##  e.g. 'age+gender'
    ## -dep.var is some form of "keep_church" or "keep_street" etc.
    ## Written to accept the same arguments as other function run.iv_etc()
    ## Don't forget! censored amounts are hardcoded (1 and 0)
    ## Cluster standard errors before sending out the model object

    reg.formula <- paste0("choice_", dep.var,
                          " ~ ",
                          paste(treatments, controls, sep = "+"))

    reg.formula <- clean.controls(reg.formula)

    mod <- lm(as.formula(reg.formula), data = df.in)

    ## Cluster standard errors by session
    robust.var <- sandwich::vcovCL(mod,
                                   cluster = df.in$session) %>%
        diag()

    mod$var <- robust.var
    return(mod)
}

run.tobit <- function(df.in,
                      dep.var,
                      treatments,
                      controls,
                      censor.left = 0,
                      censor.right = 1,
                      vars.exog = NULL,
                      vars.endog = NULL){
    ## Use the AER tobit function to run the regression
    ## -build the specification as a formula from character variables
    ##  controls should already be a single character string,
    ##  e.g. 'age+gender'
    ## Written to accept the same arguments as an function run.iv_etc()
    ## Don't forget! censored amounts are hardcoded (1 and 0),
    ##  see the arguments above, censor.left and censor.right
    ## Cluster standard errors before sending out the model object

    reg.formula <- paste0("choice_", dep.var,
                          " ~ ",
                          paste(treatments, controls, sep = "+"))

    reg.formula <- clean.controls(reg.formula)

    mod <- AER::tobit(as.formula(reg.formula),
                      left = censor.left,
                      right = censor.right,
                      data = df.in)

    ## Cluster standard errors by session
    robust.var <- sandwich::vcovCL(mod,
                                   cluster = df.in$session) %>%
        diag()

    diag(mod$var) <- robust.var
    return(mod)
}

run.iv_lpm_ols <- function(df.in,
                           dep.var,
                           treatments,
                           vars.endog,
                           vars.exog,
                           controls = NULL,
                           censor.left = NULL,
                           censor.right = NULL){

    ## Consider first stage as a linear probability model
    ## and estimate second stage with OLS

    controls <- paste(controls, collapse = "+")

    reg.formula <- paste0(
        paste0("choice_", dep.var),
        " ~ ",
        clean.controls(paste(treatments,
                             vars.endog,
                             controls, sep = "+")),
        "|",
        clean.controls(paste(treatments,
                             vars.exog,
                             controls, sep = "+")))

    print(reg.formula)
    mod <- AER::ivreg(as.formula(reg.formula),
                      data = df.in)
    return(mod)
}

clean.controls <- function(xstr){
    ## Controls string should be 'control1 + control2"
    ## Sometimes comes in as '+control1 +  control2 + "
    ## Function removes "+" from beginning and end of controls string
    ## Arg:
    ##    character string: e.g. "+control1 + control2"
    ## Returns
    ##    character string: e.g "control1 + control2"

        gsub("^\\+[:alum:]{0+}|[:alum:]{0+}\\+$",
             "",
             xstr)
}
run.regression.from.df <- function(df.row.in,
                                   dt.in,
                                   reg.function){
    ## This function is a wrapper to 'run.tobit' or 'run.ols' or 'run.iv'
    ##   will be called by an 'apply' function later in the script
    ##   makes it easier to run multiple specifications
    ##
    ## It takes in a row of a data.frame which defines the details
    ##   of the specification in the columns
    ##
    ## Builds the arguments for run.tobit, run.ols, or run.iv according
    ##   to the details specified in the data.frame row
    ##
    ##  The data.frame row has the following column names and types
    ##   -treatment: a column name of the data e.g. 'tinsurance'(a dummy)
    ##   -dependent: a column name of the data e.g. 'keep_choice"
    ##   -use.individual.controls: Boolean
    ##   -use.church.dummies: Boolean
    ##   -use.wave.dummy: Boolean
    ##   -additional.controls: a character string 'control1+control2'
    ##   -vars.exog: char string of exog vars for 2SLS
    ##   -vars.endog: char string of endog vars for 2SLS
    ##   -censor.bounds: list(left = minimum, right = maximum)

    ## Function args:
    ##     df.row.in - a row of a "specifications data.frame"
    ##     dt.in - data.frame used for the regression
    ##     reg.function - 'run.iv' or 'run.tobit' or 'run.iv'
    ## Returns:
    ##    regression model object


    if(df.row.in[["use.individual.controls"]] == TRUE){
        ind.controls <- INDIVIDUAL.CONTROLS
    }else{
        ind.controls <- NULL
    }

    ## The sets of churches in wave 1 and wave 2 are disjoint
    if(df.row.in[["use.church.dummies"]] == TRUE){
        n.church.nonzero <- select(dt.in,
                                   one_of(CHURCH.DUMMIES)) %>%
            colSums(na.rm = TRUE)

        church.dummies <- names(
            n.church.nonzero[n.church.nonzero != 0])

    }else{
        church.dummies <- NULL
    }

    if(df.row.in[["use.wave.dummy"]] == TRUE){
        wave.dummy = "wave"
    }else{
        wave.dummy <- NULL
    }

    if("additional.controls" %in% names(df.row.in)){
        additional.controls <- df.row.in[["additional.controls"]]
    }else{
        additional.controls <- NULL
    }

    if("vars.exog" %in% names(df.row.in)){
        vars.exog.in <- df.row.in[["vars.exog"]]
    }else{
        vars.exog.in <- NULL
    }

    if("vars.endog" %in% names(df.row.in)){
        vars.endog.in <- df.row.in[["vars.endog"]]
    }else{
        vars.endog.in <- NULL
    }

    if("censor.bounds" %in% names(df.row.in)){
        censor.bounds <- df.row.in[["censor.bounds"]]
        censor.left.in <- censor.bounds[["left"]]
        censor.right.in <- censor.bounds[["right"]]
    }else{
        censor.left.in = 0
        censor.right.in  = 1
    }

    ## Create a single string of all right-hand side variables
    mod.controls.base <- paste(c(ind.controls,
                                 church.dummies,
                                 wave.dummy,
                                 additional.controls),
                            collapse =  "+")

    ## Tidy the string so that there is no '+' at the end
    mod.controls <- clean.controls(mod.controls.base)

    mod.out <- reg.function(df.in = dt.in,
                            dep.var = df.row.in[["dependent"]],
                            treatments = df.row.in[["treatment"]],
                            controls = mod.controls,
                            vars.exog = vars.exog.in,
                            vars.endog = vars.endog.in,
                            censor.left = censor.left.in,
                            censor.right = censor.right.in)

    ## Add some slots to the model object for formatting the table
    mod.out$church_dummies <- ifelse(
        df.row.in[["use.church.dummies"]] == TRUE, "YES", "NO")

    mod.out$individual_controls <- ifelse(
        df.row.in[["use.individual.controls"]] == TRUE, "YES", "NO")

    return(mod.out)

}

extract.model.info <- function(mod.in){
    ## Takes a regression model object and returns the
    ## coefficients, N,and likelihood
    ##
    ## These are extracted here so that they can be sent to a function
    ## that puts them somewhere pretty in Latex

    coefs <- coeftest(mod.in)
    N <- mod.in$y %>% length()
    ll <- mod.in$loglik[[2]]
    ll <- ifelse(is.null(ll), NA, ll)
    return(list(coefs = coefs, N = N, ll = ll))
}

fix.interaction.term <- function(ix, tex.tab.in, width.in = 5.3){
    ## This function takes an interaction term and fits it across the
    ##  rows of the estimate and the standard error
    ## It is called by make.multirow as many times as there are
    ##  interactions
    ## Args:
    ##    ix: integer - the row of the covariate
    ##    tex.tab.in: list of characters - each list entry is a row of tex
    ##    width.in: numeric - hardcoded width of covariate label column
    ## Retruns
    ##    tex.tab.out: list of characters
    ##                 - each list entry is a row of tex with label column
    ##                   prettified

    tex.tab.out <- tex.tab.in
    cols <- str_split(tex.tab.in[ix], "&")[[1]]
    name.col <- paste("\\multirow{2}{",
                      width.in,
                      "cm}{",
                      gsub(":",
                           " x \\\\\\\\ \\\\hspace{0.7em}",
                           cols[[1]]),
                      "}")

    cols.out <- c(name.col, cols[2: length(cols)]) %>%
        paste(collapse = "&")

    tex.tab.out[ix] <- cols.out
    return(tex.tab.out)
}

make.multirow <- function(tex.tab.in){
    ## Takes the tex form of a table and prettifies all interaction
    ##  terms by replacing the ':' and placing them across the
    ##  two rows

    row.ix <- grep("[a-z]:[a-z]", tex.tab.in)
    next.row.ix <- row.ix + 1

    tex.tab.out <- tex.tab.in
    if(length(row.ix) > 0){
        for(ix in row.ix){
            tex.tab.out <- fix.interaction.term(ix, tex.tab.out)
        }}
    return(tex.tab.out)
}

rename.covariate.labs <- function(tex.tab.in){
    ## Make known labels look nicer
    ## e.g. replacing capitals etc.

    t0 <- gsub("\\bConstant\\b",
               "constant",
               tex.tab.in)

    t1 <- gsub("\\btinsurance\\b",
               "insurance enrollment",
               t0)

    t2 <- gsub("\\btinsuranceinfo\\b",
               "insurance information",
               t1)

    t3 <- gsub("\\btnoinsurance\\b",
               "no insurance",
               t2)

    t4 <- gsub("\\bwave2\\b",
               "wave2",
               t3)

    t5 <- gsub("\\brevival\\s",
               "revival week",
               t4)

    t6 <- gsub("\\brevival\\\\_assignment\\b",
               "assigned to revival",
               t5)

    t7 <- gsub("\\bweekevents\\b",
               "events",
               t6)

    tout <- make.multirow(t7)

    return(tout)
}

get.tex.paths <- function(exploration = TRUE,
                              paper = FALSE,
                              write.file){
    ## Quick function to decide where to write out tex tables
    ## Args:
    ##     exploration: Boolean - include in explore folder
    ##     paper: Boolean - include tex table in paper folder
    ##     write.file: character - basename of tex file e.g. "paper1"
    ## Returns:
    ##     a list of .tex paths where the output will be written

    if(paper == TRUE){
        paper.path <- file.path(paper.tables.folder,
                                paste0(write.file,".tex"))
    }else{
        paper.path = NULL
    }

    if(exploration == TRUE){
        exploration.path <- file.path(exploration.tables.folder,
                                      paste0(write.file,".tex"))
    }else{
        exploration.path = NULL
    }

    path.list <- list(paper = paper.path,
                      exploration = exploration.path)

    list.to.keep <- path.list[!sapply(path.list, is.null)]
    return(list.to.keep)
}

paper.stargaze <- function(mod.list,
                           cov.labs = NULL,
                           title = NULL,
                           col.labs = column.labels.base,
                           col.sep = c(2, 2, 2),
                           write.file = NULL,
                           order.in = NULL,
                           notes.text.base = "",
                           notes.info.list = NULL,
                           keep.in = c("controls", "dummies"),
                           model.numbers.in = TRUE,
                           vars.not.shown = VARS.NOT.SHOWN,
                           table.for.paper = FALSE,
                           table.for.exploration = TRUE,
                           caption.in = NULL,
                           fit.to.width = FALSE){

    ## Use stargazer::stargazer to make pretty tex tables
    ## -- stargazer does not accept tobit.model
    ##    therefore translate models with coeftest
    ## Args:
    ##    mod.list: a list of regression models
    ## Returns:
    ##    tex.out.final: character string - tex output that may have
    ##                                      been written to a file

    if(!is.null(write.file)){
        table.paths <- get.tex.paths(
            exploration = table.for.exploration,
            paper = table.for.paper,
            write.file)

        label.for.tex = write.file
    }else{
        table.paths <- NULL
        label.for.tex = ""
    }

    if(is.null(caption.in)){
        label.for.tex = label.for.tex
    }else{
        label.for.tex = caption.in
    }

    if(is.null(title)){
        title <- "No title supplied"
    }

    mod.objects <- lapply(mod.list, extract.model.info)

    coefs.list <- lapply(mod.objects, function(x) x[["coefs"]])

    nobs.list <-  c("N", sapply(mod.objects, function(x) x[["N"]]))

    ll.list <- c("Log-likelihood",
                 round(digits = 3,
                       sapply(mod.objects, function(x) x[["ll"]])))

    ind.controls <- c("Individual controls",
                      sapply(mod.list,
                             function(x) x$individual_controls))

    church.dummies <- c("Church dummies",
                        sapply(mod.list,
                               function(x) x$church_dummies))

    add.lines.list.base <-  list(N = nobs.list,
                                 ll = ll.list,
                                 controls = ind.controls,
                                 dummies = church.dummies)
    add.lines.list <- add.lines.list.base[keep.in]

    ## invisible(capture.output() to not print the tex
    tex.out.base <- stargazer(coefs.list,
                              covariate.labels = cov.labs,
                              dep.var.labels.include = FALSE,
                              model.numbers = model.numbers.in,
                              title = title,
                              column.labels = col.labs,
                              column.separate = col.sep,
                              label = label.for.tex,
                              notes.align = "l",
                              add.lines = add.lines.list,
                              omit = vars.not.shown)

    if(!is.null(notes.info.list)){
        n.cols <- length(mod.list) + 1
        notes.info.list$ncols <- n.cols
        notes.out <- str_interp(notes.text.base,
                                notes.info.list)

        note.idx <- grep("Note", tex.out.base)
        tex.out.base[note.idx] <- notes.out
    }

    tex.out.renamed <- rename.covariate.labs(tex.out.base)

    if(fit.to.width == TRUE){
        tex.out.final <- adjustbox.tabular(tex.out.renamed)
        print(tex.out.final)
    }else{
        tex.out.final <- tex.out.renamed
    }

    if(!is.null(write.file)){
        sapply(table.paths, write, x = tex.out.final)
    }

   return(tex.out.final)
}

run.reg.columns <- function(dt.in,
                            specs.df,
                            tab.title = NULL,
                            tab.name = "unamed.tex",
                            table.for.paper = FALSE,
                            table.for.exploration = FALSE,
                            col.labs.in = column.labels.base,
                            col.sep.in = c(2, 2, 2),
                            reg.function.in = run.tobit,
                            notes.list.in = "",
                            keep.in.list = c("N",
                                             "controls",
                                             "dummies"),
                            fit.to.width.in = FALSE){

    ## This function pulls everything together:
    ##  i) takes in a sub-set of the data
    ##  ii) runs the regression (specified in specs.df)
    ## iii) produces a neat tex table where columns are rows of specs.df
    ## Requires a specs.df to run the regressions
    ## - specs.df is a data.frame with columns
    ## - dependent, treatment, individual.controls, church.dummies
    ## - these should all be strings corresponding to variable names
    ##   that can be found in main dataframe
    ## - look at function 'run.regression.from.df for more details'
    ## Returns:
    ##    mod.list - a list of regression models

    mods.list <- apply(specs.df, 1,
                       run.regression.from.df,
                       dt.in = dt.in,
                       reg.function = reg.function.in)

    names(mods.list) <- specs.df$model.names

    tex.text <- paper.stargaze(mods.list,
                               title = tab.title,
                               write.file = tab.name,
                               table.for.paper = table.for.paper,
                               table.for.exploration =
                                   table.for.exploration,
                               col.labs = col.labs.in,
                               col.sep = col.sep.in,
                               keep.in = keep.in.list,
                               notes.info.list = notes.list.in,
                               fit.to.width = fit.to.width.in)
    return(mods.list)
}

create.reg.panels <- function(list.of.mod.lists,
                              panel.names.in,
                              title.in,
                              tex.file.name,
                              panel.note.list = standard.notes.list,
                              keep.in = c("N", "controls", "dummies"),
                              same.summary.stats.in = FALSE,
                              col.labs.in = column.labels.base,
                              col.sep.in = c(2, 2, 2),
                              add.adjustbox = FALSE){

    ## Put a bunch of regression columns into a panels
    ## Args:
    ##    list.of.mod.lists: a list of a list of regression models
    ##       -list of columns to appear in a single panel, and list of
    ##         all columns to appear in a panel
    ##    panel.names.in: character vectos
    ## Returns:
    ##   panel.out: a tex file that contains a panel
    ##

    stargazed.list <- lapply(list.of.mod.lists,
                             paper.stargaze,
                             title = title.in,
                             caption.in = tex.file.name,
                             notes.text.base = standard.note.base,
                             notes.info.list = panel.note.list,
                             keep.in = keep.in,
                             col.labs = col.labs.in,
                             col.sep = col.sep.in)

    N.complete.cases <- lapply(list.of.mod.lists,
                               function(x) x[[1]]$y %>% length())

    panel.names.out <- sapply(seq_along(panel.names.in),
                              function(ix){
                                  sprintf(panel.names.in[[ix]],
                                          N.complete.cases[[ix]])})

    panel.out <- star_panel(starlist = stargazed.list,
                            panel.names = panel.names.out,
                            panel.label.fontface = "bold",
                            same.summary.stats = same.summary.stats.in)

    info.controls <- grep("Individual controls &", panel.out)
    n.info.controls <- length(info.controls)
    if(n.info.controls > 1){
        n.end <- n.info.controls - 1
        panel.out[info.controls[1:n.end]] <- NA
    }

    info.churches <- grep("Church dummies &", panel.out)
    n.info.churches <- length(info.churches)
    if(n.info.churches > 1){
        n.end <- n.info.churches - 1
        panel.out[info.churches[1:n.end]] <- NA
    }

    info.N <- grep("N &", panel.out)
    n.of.Ns <- length(info.N)
    if(n.of.Ns > 1){
        mod.n <- panel.out[info.N[[n.of.Ns]]]
        mod.n.out <- paste(mod.n, "\\hline \\\\[-1.8ex] ")
        panel.out[info.N[[n.of.Ns]]] <- mod.n.out
    }

    panel.out <- panel.out[!sapply(panel.out, is.na)]

    if(add.adjustbox == TRUE){
        panel.out <- adjustbox.tabular(panel.out, height = TRUE)
    }

    if(!is.null(tex.file.name)){
        write(panel.out,
              file.path(tables.folder,
                        paste0(tex.file.name, ".tex")))
    }
    return(panel.out)
}


