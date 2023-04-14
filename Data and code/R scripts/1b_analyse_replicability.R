### Code to produce regression tables for "God Insures" paper

REGRESSION.MODELS <- readRDS(
    file.path(output.path,
              "Regression models",
              "regression_model_objects.Rds"))

### Get meta-analytic estimates of the separate wave effects ---------
get.estimate.and.se <- function(mod, covariate.name){
    coeftest(mod)[covariate.name,c("Estimate", "Std. Error")]
}

meta.combine.results <- function(mod.list,
                                 model.names = names(mod.list),
                                 covariate.name = "tinsurance",
                                 outcome.name = "outcome"){

    ## Take in a list of estimated regression models and return
    ## the precision weighted avergae
    ## Done according to Christensen, Freese and Miguel, 2019
    ##  pg 87 - 89
    ## Also good references in Camerer et al. Science

    est.df <- plyr::ldply(mod.list,
                          get.estimate.and.se,
                          covariate.name = covariate.name) %>%
        rename(estimate = 1, se = 2) %>%
        mutate(recip_se_sq = 1/(se^2),
               weighted_estimate = recip_se_sq * estimate)

    num <- sum(est.df$weighted_estimate)
    denom <- sum(est.df$recip_se_sq)
    est.precision.weighted <- num /denom

    se.precision.weighted <- sqrt(1 / denom)

    meta.df <- data.frame(
        estimate = est.precision.weighted,
        se = se.precision.weighted)

    est.df.out <- est.df %>%
        select(estimate, se) %>%
        rbind(meta.df)%>%
        mutate(model_name = c(model.names, "meta_estimate"),
               outcome = outcome.name)

    return(est.df.out)
}

get.meta.estimates <- function(outcome,
                               mod.group.list,
                               mod.names = names(mod.group.list),
                               covariate = "tinsurance"){

    ## Take in a group of models and a specific outcome as a string
    ## outcomes are e.g. 'keep_church', 'keep_thanks'
    ## Combine results with the meta analytic estimates

        mods.list <- lapply(mod.group.list,
                            function(x) x[[outcome]])

        meta.df.out <- meta.combine.results(
            mods.list,
            model.names = mod.names,
            covariate.name = covariate,
            outcome.name = outcome)

        return(meta.df.out)
    }

do.meta.for.test <- function(mod.group.list,
                             mod.names = wave.names,
                             outcomes.to.test.list = main.meta.outcomes,
                             covariate.to.test,
                             comparison){

    grouped.estimates <- plyr::ldply(outcomes.to.test.list,
                                     get.meta.estimates,
                                     mod.group.list = mod.group.list,
                                     mod.names = mod.names,
                                     covariate = covariate.to.test)

    grouped.estimates.out <- grouped.estimates %>%
        mutate(comparison = comparison)

    return(grouped.estimates.out)
}

get.prediction.interval <- function(effect.size, n.orig, n.rep){
    ## Defined according to Patil, Peng and Leek, 2016

    qz <- qnorm(0.95)
    interval <- qz * sqrt(1/(n.orig -3)+1/( n.rep -3))

    return(data.frame(lower.prediction = effect.size - interval,
                      upper.prediction = effect.size + interval))
}

apply.prediction.intervals <- function(choice.in,
                                       covariate.in,
                                       insurance.models.standardized,
                                       info.models.standardized){

    comp1.models <- insurance.models.standardized
    comp2.models <- info.models.standardized

    choice.name <- paste0(choice.in, "_controls")

    if(covariate.in == "tinsurance"){
        N.orig <- dt %>%
            filter(wave == 1 & treatment != "No insurance") %>%
            nrow()
        N.replicate <- dt %>%
            filter(wave == 2 & treatment != "No insurance") %>%
            nrow()
        w1.model <- comp1.models[[1]][[choice.name]]
        w2.model <- comp1.models[[2]][[choice.name]]
        test.name <- "insurance_vs_info"
    }else{
        N.orig <- dt %>%
            filter(wave == 1 &
                   treatment != "Insurance") %>%
            nrow()
        N.replicate <- dt %>%
            filter(wave == 2 &
                   treatment != "Insurance") %>%
            nrow()
        w1.model <- comp2.models[[1]][[choice.name]]
        w2.model <- comp2.models[[2]][[choice.name]]
        test.name <- "info_vs_no"
    }

    estimate.w1 <- get.estimate.and.se(w1.model,
                                       covariate.in)[["Estimate"]]
    estimate.w2 <- get.estimate.and.se(w2.model,
                                       covariate.in)[["Estimate"]]

    pred.interval <- get.prediction.interval(estimate.w1,
                                             n.orig = N.orig,
                                             n.rep = N.replicate)  %>%
        mutate(w1 = estimate.w1,
               w2 = estimate.w2,
               succes_replicate = ifelse(w2 > lower.prediction &
                                         w2 < upper.prediction,
                                         1, 0),
               choice = choice.in,
               comparison = test.name)
    return(pred.interval)
}


comp1.models <- REGRESSION.MODELS[
    c("standardized.insurance_info.w1",
      "standardized.insurance_info.w2")]

comp2.models <- REGRESSION.MODELS[
    c("standardized.info_no.w1",
      "standardized.info_no.w2")]

standardized.w.intervals <- rbind(
    plyr::ldply(keep.choices,
                apply.prediction.intervals,
                covariate.in = "tinsurance",
                insurance.models.standardized = comp1.models,
                info.models.standardized = comp2.models),
        plyr::ldply(keep.choices,
                apply.prediction.intervals,
                covariate.in = "tinsuranceinfo",
                insurance.models.standardized = comp1.models,
                info.models.standardized = comp2.models))


## Do the aggregation mainly for the keep choices
## Use the estimates with controls as our main estimates
main.meta.outcomes <- paste0(keep.choices, "_controls")
wave.names <- c("wave_1", "wave_2")

## List the different hypotheses we are testing
## Code kicks up a fuss if list names are not null
insurance.v.info <- REGRESSION.MODELS[
    c("insurance_vs_info_w1",
      "insurance_vs_info_w2")]
names(insurance.v.info) <- NULL

info.v.no <- REGRESSION.MODELS[
    c("info_vs_no_w1",
      "info_vs_no_w2")]
names(info.v.no) <- NULL

meta.test1 <- do.meta.for.test(insurance.v.info,
                               covariate.to.test = "tinsurance",
                               comparison = "insurance_vs_info")

meta.test2 <- do.meta.for.test(info.v.no,
                               covariate.to.test = "tinsuranceinfo",
                               comparison = "info_vs_no")

meta.estimates.df <- rbind(meta.test1, meta.test2) %>%
    data.frame()

## Add pooled estimates before writing out data frame
get.pooled <- function(outcome.name, pooled.model, covariate){
    mod <- pooled.model[[outcome.name]]

    est.df <- get.estimate.and.se(mod,
                                  covariate.name = covariate) %>%
        rbind() %>%
        data.frame() %>%
        rename(estimate = 1,
               se = 2)

    est.df.out <- est.df %>%
        mutate(outcome = outcome.name,
        model_name = "pooled")

    return(est.df.out)
}

test1.model <- REGRESSION.MODELS[["insurance_vs_info_pooled"]]
test2.model <- REGRESSION.MODELS[["info_vs_no_pooled"]]

pooled.test1 <- plyr::ldply(main.meta.outcomes,
                            get.pooled,
                            pooled.model = test1.model,
                            covariate = "tinsurance") %>%
    mutate(comparison = "insurance_vs_info")

pooled.test2 <- plyr::ldply(main.meta.outcomes,
                            get.pooled,
                            pooled.model = test2.model,
                            covariate = "tinsuranceinfo") %>%
    mutate(comparison = "info_vs_no")


main.estimates.df <- rbind(meta.test1,
                           meta.test2,
                           pooled.test1,
                           pooled.test2) %>%
    data.frame() %>%
    mutate(pvalue = 2 * (1 - pnorm(abs(estimate/se)))) %>%
    arrange(comparison, outcome)

write.csv(x = main.estimates.df,
          file = file.path(output.path,
                           "Regression models",
                           "main_estimates.csv"),
          row.names = FALSE)
