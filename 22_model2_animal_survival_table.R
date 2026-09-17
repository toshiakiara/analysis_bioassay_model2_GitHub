#!/usr/bin/env Rscript

library(dplyr)
library(tidyr)
library(xtable)


########################################
## survival analysis
## load analyzed data
########################################

## dat_raw
load("rda/animal_data.RData")
## res_surv
## tbl_surv,
load("rda/model2_animal_survival_analysis.RData")


## label for LaTeX table
labels <- c("Toxic", "Lethal")



#################
## save Table as LaTeX
#################

##
## function for formatting table
##
## input:
##   x1: median
##   x2: lower
##   x3: upper
## output:
##   median [lower, upper]
##
strMedianCI <- function(x1, x2, x3, str = "\\\\text{--}") {
    sprintf("%.2f [%.2f, %.2f]", x1, x2, x3) |>
        gsub(pattern = "NA", replacement = str)
}

tbl_dose_survival <- lapply(1:2, function(i) {
    .tbl <- tbl_surv[[i]] |>
        mutate(
            Dose = sprintf("%.1f", as.numeric(as.character(Dose))),
            Reaction = "",
            median_ci = strMedianCI(median, lower, upper)
        ) |>
        dplyr::select(Reaction, Dose, n, Events, median_ci)
    .tbl$Reaction[1] <- labels[i]
    return(.tbl)
}) |>
    bind_rows()

## comsum of rows for hline
n_rows_cumsum <- sapply(tbl_surv, nrow) |>
    cumsum()


## set col labels
colnames(tbl_dose_survival) <- c(
    "Reaction",
    "Dose (mg/kg)",
    "n",
    "Events",
    "Median [95\\% CI]"
)

print(file = "Table/Table1.tex",
    xtable(
        tbl_dose_survival,
        digits = c(0, 0, 0, 0, 0, 0),
        align = c("c", "c", "c", "c", "c", "c")
    ),
    include.rownames = FALSE,
    only.contents = TRUE,
    hline.after = c(0, n_rows_cumsum),
    booktabs = TRUE,
    sanitize.text.function = identity,
    timestamp = NULL
)

