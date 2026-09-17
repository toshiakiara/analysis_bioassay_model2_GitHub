#!/usr/bin/env Rscript

library(dplyr)
library(tidyr)
library(purrr)
library(parallel)
library(xtable)


########################################
## load analyzed data
########################################

load("rda/model2_cv_RMSE.RData")


## label for LaTeX table
labels <- c("Toxic", "Lethal")



#################
## Table: RMSE
## save Table as LaTeX
#################
str_mean_sd <- function(Mean, SD) {
    ifelse(is.na(Mean), "-- (--)", sprintf("%.3f (%.3f)", Mean, SD))
}

tbl_RMSE <- res_RMSE |>
    bind_rows(.id = "Reaction") |>
    mutate(Reaction = as.character(factor(Reaction, labels = labels)))

tbl_summary_RMSE <- summary_RMSE |>
    bind_rows(.id = "Reaction") |>
    mutate(
        Reaction = as.character(factor(Reaction, labels = labels)),
        fold = 1,
        Mean_SD_logis = str_mean_sd(Mean_logis, SD_logis),
        Mean_SD_gompertz = str_mean_sd(Mean_gompertz, SD_gompertz)
    ) |>
    dplyr::select(Reaction, Dose, fold, Mean_SD_logis, Mean_SD_gompertz)

## merge
tbl_RMSE <- tbl_RMSE |>
    left_join(tbl_summary_RMSE, by = c("Reaction", "Dose", "fold")) |>
    mutate(Dose = sprintf("%.1f", Dose))


## set row labels
n_row <- nrow(tbl_RMSE)
.bool <- seq_len(n_row) %% k == 1

tbl_RMSE$Reaction[!.bool] <- ""
tbl_RMSE$Dose[!.bool] <- ""
tbl_RMSE$Mean_SD_logis[!.bool] <- ""
tbl_RMSE$Mean_SD_gompertz[!.bool] <- ""

## set col labels
colnames(tbl_RMSE) <- c(
    "Reaction",
    "Dose (mg/kg)",
    "fold",
    "Logistic",
    "Gompertz",
    "Logistic",
    "Gompertz"
)


## comsum of rows for hline
## k is number of fold
n_rows_cumsum <- seq(k, n_row, k)


print(file = "Supplement/TableS1.tex",
    xtable(tbl_RMSE,
           digits = c(0, 0, 1, 0, 3, 3, 0, 0)),
    include.rownames = FALSE,
    only.contents = TRUE,
    hline.after = c(0, n_rows_cumsum),
    booktabs = TRUE,
    sanitize.text.function = identity,
    NA.string = "--",
    timestamp = NULL
)

