#!/usr/bin/env Rscript

# library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(parallel)
library(survival)
library(survminer)


source("common_scripts.R", local = TRUE)
load("rda/animal_data.RData")



########################################
# survival analysis in each dose
########################################

##
## Time (min)
## status
## Dose
##
res_surv <- mclapply(1:2, function(i) {
    survfit(Surv(Time, status) ~ Dose, data = dat_raw[[i]])
}, mc.cores = detectCores())

## convert to data.frame
## Dose / n / Events / strata
.tbl1 <- mclapply(1:2, function(i) {
    surv_summary(res_surv[[i]], dat_raw[[i]]) |>
        group_by(Dose) |>
        summarise(
            n = max(n.risk),
            Events = sum(n.event),
            strata = unique(strata)
        )
}, mc.cores = n_mc)

## strata / median / lower / upper
.tbl2 <- mclapply(1:2, function(i) {
    surv_median(res_surv[[i]])
}, mc.cores = n_mc)


##
## merge
## tibble:
##   Dose, n, Events, median, lower, upper
##
tbl_surv <- mclapply(1:2, function(i) {
    left_join(.tbl1[[i]], .tbl2[[i]], by = "strata") |>
        dplyr::select(-strata) |>
        dplyr::select(Dose, everything())
}, mc.cores = n_mc)



########################################
## save analyzed data
########################################

save(
    res_surv, tbl_surv,
    file = "rda/model2_animal_survival_analysis.RData"
)

