#!/usr/bin/env Rscript

library(dplyr)
library(tidyr)
library(purrr)
library(modelr)
library(xtable)
library(readr)


########################################
# load analyzed data
########################################

source("common_scripts.R", local = TRUE)

## x
load("rda/animal_data.RData")
## res_model
load("rda/model2_fit_model.RData")



res_index <- lapply(1:2, function(i) {
    res_model[[i]] |>
        mutate(
            R2 = pmap_dbl(list(model = model, data = data),
                          get_index, rsquare),
            RMSE = pmap_dbl(list(model = model, data = data),
                            get_index, rmse),
            AIC = map_dbl(model, ~ myAIC(.)),
            BIC = map_dbl(model, ~ myBIC(.)),
        ) |>
        dplyr::select(Dose, Model, R2, RMSE, AIC, BIC)
})


tbl_index <- res_index |>
    bind_rows(.id = "Reaction") |>
    mutate(Reaction = factor(Reaction, labels = c("Toxic", "Lethal")),
           Reaction = as.character(Reaction),
           Dose = sprintf("%.1f", Dose))

n_row <- nrow(tbl_index)
n_reaction <- nrow(res_index[[1]])
n_model <- 4

bool_model <- seq_len(n_row) %% n_model == 1

tbl_index$Reaction[!bool_model] <- ""
tbl_index$Dose[!bool_model] <- ""

## cumsum of rows for hline
n_rows_cumsum <- seq(0, n_row, n_model)


## set col labels
colnames(tbl_index) <- c(
    "Reaction",
    "Dose (mg/kg)",
    "Model",
    "$R^{2}$",
    "RMSE",
    "AIC",
    "BIC"
)

print(file = "Table/Table2.tex",
    xtable(
        tbl_index,
        digits = c(0, 0, 1, 0, 3, 4, 1, 1),
        align = rep("c", ncol(tbl_index) + 1)
    ),
    include.rownames = FALSE,
    only.contents = TRUE,
    hline.after = c(n_rows_cumsum),
    booktabs = TRUE,
    sanitize.text.function = identity,
    NA.string = "--",
    timestamp = NULL
)

