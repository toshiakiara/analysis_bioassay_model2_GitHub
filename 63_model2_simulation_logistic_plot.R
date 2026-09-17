#!/usr/bin/env Rscript

library(dplyr)
library(purrr)
library(ggplot2)
library(ggforce)


########################################
# load analyzed data
########################################

source("common_plots.R")
load("rda/model2_simulation_logistic.RData")


## use subscript in strip of figure
d50 <- d50 |>
    mutate(reaction = factor(reaction, labels = label_reaction))
d50_summary <- d50_summary |>
    mutate(reaction = factor(reaction, labels = label_reaction))


## seed for sina plot
seed_p <- 123  # seed number for sinaplot
p_d50 <- plot_d50_time_violin(d50, d50_summary, seed_p)


## probit analysis / plot of each simulation
for (i in 1:2) {
    res_d50[[i]] <- res_d50[[i]] |>
        mutate(plot = pmap(list(model, response_tbl,
                                Dose, lwr, upr,
                                c("TD", "LD")[i],
                                paste(Time, "min")),
                           plot_fit_probit_curve))
}



####################
# save as PDF
####################

# cairo_pdf(file = "Fig/fig_model2_simulation_logistic_D50.pdf", width = 8, height = 4)
# print(p_d50)
# dev.off()


## plot fit probit curve in each simulation
for (i in seq_along(jt)) {
    ## TD50
    cairo_pdf(file = sprintf("Supplement/fig_model2_simulation_logistic_toxic_%02d.pdf", jt[i]),
              width = 5, height = 4, onefile = TRUE)
        res_d50[[1]] |>
            dplyr::filter(Time == jt[i]) |>
            pull(plot) |>
            print()
    dev.off()

    ## LD50
    cairo_pdf(file = sprintf("Supplement/fig_model2_simulation_logistic_lethal_%02d.pdf", jt[i]),
              width = 5, height = 4, onefile = TRUE)
        res_d50[[2]] |>
            dplyr::filter(Time == jt[i]) |>
            pull(plot) |>
            print()
    dev.off()
}



####################
## archive as zip file
####################

model <- "model2"
filepattern <- sprintf("fig_%s_simulation_logistic_%s", model, c("toxic", "lethal"))
zipfile <- sprintf("fig_%s_simulation_logistic_%s.zip", model, c("toxic", "lethal"))

setwd("Supplement")
for (x in zipfile) {
    if (file.exists(x)) {
        file.remove(x)
    }
}

for (i in 1:2) {
    filelist <- list.files(".", pattern = filepattern[i])
    zip(zipfile = zipfile[i], files = filelist)
    file.remove(filelist)
}
setwd("..")

