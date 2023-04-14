### Create the figures in God Insures

### Define paths -----------------------------------------------------
source("R scripts/0_setup_project.R")

### Read-in regression output (Rds saved by make_regression_tables.R)
REGRESSION.MODELS <- readRDS(
    file.path(output.path,
              "Regression models",
              "regression_model_objects.Rds"))

### Define a general theme for the plots --------------------------------

## Prefer to have plot in lmroman to match Latex, but  very sensitive
## on some people's computers.
## Comment out according to preference

paper.theme <- theme(#text = element_text(family = "lmroman10"),
                     legend.key = element_blank(),
                     legend.title = element_blank(),
                     legend.text = element_text(size = 12),
                     axis.text = element_text(size = 12),
                     axis.title = element_text(size = 12),
                     axis.title.x = element_text(
                         margin = margin(t = 10, b = 0, r = 0, l = 0,
                                         unit = "pt")),
                     axis.title.y = element_text(
                         margin = margin(t = 0, r = 10, b = 0, l = 0,
                                         unit = "pt")),
                     axis.ticks = element_line(colour = "grey",
                                               linetype = "dotted",
                                               size = 0.3),
                     panel.background = element_rect(fill = "white"),
                     panel.grid.major = element_line(colour = "grey",
                                                     linetype = "dotted",
                                                     size = 0.3))



### Figure 1 in the paper  -------------------------------------------
### Make a histogram of allocation decisions

dt.keep <- dt %>%
    filter(revival == 0) %>%
    select(one_of(paste("choice", keep.choices, sep = "_"))) %>%
    pivot_longer(cols =  starts_with("choice_keep"),
                 names_to = "choice",
                 names_prefix = "choice_") %>%
    mutate(choice = gsub("_", " versus ", choice))

plot.hist.keep <- ggplot(dt.keep,
                         aes(x = value, stat(density))) +
    facet_wrap(~choice, nrow = 3) +
    geom_histogram(binwidth = 0.08,
                   fill = "white",
                   col = "grey55",
                   alpha = 0.9) +

    labs(y = element_blank(),
         x = "Proportion of endowment given away") +
    paper.theme +
    theme(axis.text.y = element_blank(),
          strip.text =  element_text(size = 12),
          axis.title.x = element_text(size = 16))

## plot.hist.keep

ggsave(file.path(figures.folder,
                 "histogram_giving.pdf"),
       plot = plot.hist.keep,
       width = 18,
       height = 16,
       units = "cm")

### Figure 2 in the paper --------------------------------------------
## Main treatment effects
main.axis.choices <- rev(keep.choices)
order.of.estimates <- c("Wave 2", "Wave 1", "Pooled", "Meta-estimate")
sig.level <- 0.05
qval <- qnorm(sig.level/2)

## Read estimates from saved csv object
## Recode wave tag to be pretty and ordered
## Recode choices to be ordered and not read '_controls'
keep.results.df <- read.csv(file.path(output.path,
                                      "Regression models",
                                      "main_estimates.csv")) %>%
    mutate(lower = estimate - qval * se,
           upper = estimate + qval * se,
           choice = gsub("_controls", "",  outcome),
           choice = factor(choice,
                           levels = main.axis.choices,
                           ordered = TRUE),
           wave = dplyr::recode(model_name,
                         pooled = "Pooled",
                         meta_estimate = "Meta-estimate",
                         wave_1 = "Wave 1",
                         wave_2 = "Wave 2"),
           wave = factor(wave,
                         levels = order.of.estimates,
                         ordered = TRUE))

main.caption <- paste("Main estimates from Tobit regressions with",
                      "pre-registered controls.",
                      "Pooled combines data from waves and estimates",
                      "a single effect.",
                      "Meta-estimate gives precision-weighted means",
                      "of disaggregated estimates.",
                      "95% confidence intervals.")

## Define some plotting parameters
effect.colours <- c("gray53", "gray43", "skyblue1", "royalblue4")
error.whisker.linetypes <- c("solid", "solid", "solid", "solid")
effect.linesize <- c(0.6, 0.6, 1.3, 1.3)
error.bar.linetypes <- c("blank", "blank", "dotdash", "dotted")
geom.point.size <- c(1.5,  1.5, 3, 3)
guide.linetypes = c("solid", "solid", tail(error.bar.linetypes, 2))

## Write a function to create a treatment effects plot
plot.treatment.effects <- function(df.in,
                                   caption.in = NULL,
                                   facets = FALSE,
                                   main.theme = paper.theme){

    n.aes <- df.in %>%
        group_by(comparison, choice) %>%
        n_groups()

    plot.base <- df.in %>%
        ggplot(aes(y = estimate,
                   x = choice,
                   colour = wave)) +
        geom_errorbar(aes(ymin = lower,
                          ymax = upper,
                          size = wave),
                      linetype = "solid",
                      position = position_dodge(width = 0.5),
                      width = 0.18) +
        geom_linerange(aes(x = choice,
                           ymin = lower,
                           ymax = upper,
                           size = wave),
                       linetype = rep(error.bar.linetypes,
                                      times = n.aes),
                       colour = "white",
                       position = position_dodge(width = 0.5),
                       show.legend = FALSE) +
        geom_point(position = position_dodge2(width = 0.5),
                   size = rep(geom.point.size, times =  n.aes)) +
        scale_colour_manual(values = effect.colours) +
        scale_size_manual(values = effect.linesize) +
        scale_linetype_manual(values = error.bar.linetypes) +
        guides(colour = guide_legend(
                   reverse = TRUE,
                   override.aes = list(
                       shape = NA,
                       linetype = guide.linetypes)),
               size = guide_legend(reverse = TRUE)) +
        ylim(-0.2, 0.2) +
        coord_flip() +
        labs(x = "Allocation pair",
             y = paste("Difference in allocation after insurance as",
                       "% of endowment")
             #,caption = str_wrap(caption.in, width = 90)
             ) +
        main.theme +
        theme(plot.caption = element_text(
                  #plot.caption = element_text(family = "italic",
                  size = 10,
                  lineheight = 1.1,
                  hjust = 0,
                  margin = margin(t = 12, unit = "pt")),
              plot.caption.position =  "plot")

    if(facets == TRUE){
        plot.out <- plot.base + facet_wrap(~comparison, nrow = 2)
    }else{
        plot.out <- plot.base
    }

    return(plot.out)
}

## Do the plot
keep.results.plot_insurance_vs_info <- plot.treatment.effects(
    keep.results.df %>%
    filter(comparison == "insurance_vs_info"),
    caption.in = main.caption,
    main.theme = paper.theme)

keep.results.plot_insurance_vs_info

ggsave(filename = file.path(
           figures.folder,
           "keep_results_insurance_vs_info_meta.pdf"),
       plot = keep.results.plot_insurance_vs_info,
       width = 18.5,
       height = 15.5,
       units = "cm")


### Figure 3 in the  paper ------------------------------------------
## Plot the distribution of the heterogeneity index

dt.index <- dt %>%
    select(costly_behaviour,
           use_church_spiritually,
           use_church_network)

corolelogram.church <- dt.index %>%
    ggpairs()

ggsave(plot = corolelogram.church,
       filename = file.path(figures.folder,
                            "index_correlations.pdf"))

