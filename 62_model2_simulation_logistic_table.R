#!/usr/bin/env Rscript

library(dplyr)
library(tidyr)
library(readr)
library(xtable)


########################################
## load analyzed data
########################################

source("common_scripts.R")
load("rda/model2_simulation_logistic.RData")



#################
## save simulation results as CSV
#################

tbl_sim_raw <- dat_sim_raw |>
    bind_rows(.id = "reaction") |>
    mutate(reaction = factor(reaction, labels = c("TD50", "LD50")),
           logDose = log(Dose)) |>
    dplyr::select(reaction, N, ID, Dose, logDose, Time)

write_csv(tbl_sim_raw, file = "csv/model2_simulation_logistic_raw.csv")


tbl_sim_response <- lapply(1:2, function(i) {
    dat_sim_response[[i]] |>
        unnest(cols = data)
}) |>
    bind_rows(.id = "reaction") |>
    mutate(reaction = factor(reaction, labels = c("TD50", "LD50"))) |>
    dplyr::select(reaction, judgment_time, ID, Dose, logDose, Time, response)

write_csv(tbl_sim_response, file = "csv/model2_simulation_logistic_response.csv")


tbl_d50_summary <- d50_summary |>
    mutate(across(log_mean:Max, \(x) round(x, digits = 3)))

write_csv(tbl_d50_summary, file = "csv/model2_simulation_logistic_summary.csv")


#################
## save Table as LaTeX
#################

## comsum of rows for hline
n_rows_cumsum <- d50_summary |>
    group_by(reaction) |>
    count() |>
    pull(n) |>
    cumsum()

## set row labels
D50_labels <- paste0(c("TD", "LD"), "\\textsubscript{50}")
d50_summary$reaction <- NA
d50_summary$reaction[c(0, nrow(d50_summary) / 2) + 1] <- D50_labels

## set col labels
colnames(d50_summary) <- c(
    " ",
    "Time (min)",
    "$n$",
    "Mean$_{\\log \\text{Dose}}$",
    "SD$_{\\log \\text{Dose}}$",
    "geoMean",
    "Min",
    "Q\\textsubscript{1}",
    "Median",
    "Q\\textsubscript{3}",
    "Max"
)

print(file = "Table/Table5.tex",
    xtable(d50_summary,
           digits = c(0, 0, 0, 0, 3, 3, 1, 1, 1, 1, 1, 1)),
    include.rownames = FALSE,
    only.contents = TRUE,
    hline.after = c(0, n_rows_cumsum),
    booktabs = TRUE,
    sanitize.text.function = identity,
    timestamp = NULL
)

