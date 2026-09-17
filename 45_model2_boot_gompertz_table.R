#!/usr/bin/env Rscript

library(dplyr)
library(tidyr)
library(purrr)
library(boot)
library(readr)
library(xtable)



########################################
## load analyzed data
########################################

load("rda/model2_boot_gompertz.RData")
source("common_scripts.R")

label_params <- c("Asym", "b2", "b3")

## label for LaTeX table
label_reaction <- c("Toxic", "Lethal")



##
## input:
##     x: (boot)$t0
##     name_cols: parameter names
##
## output: data.frame
##     Parameter
##     Value
##
get_t0 <- function(x, name_cols) {
    names(x) <- name_cols
    df <- data.frame(t0 = x) |>
        tibble::rownames_to_column(var = "Parameter")
    return(df)
}

##
## input:
##     x: (boot)$t
##     name_cols: parameter names
## 
## output: data.frame
##
get_t <- function(x, name_cols) {
    df <- data.frame(x)
    colnames(df) <- name_cols
    return(df)
}

##
## 95% CI by BCa
##
## input:
##     x: object by boot function
##
get_ci_boot <- function(x, name_cols) {
    lapply(seq_len(length(name_cols)), function(i) {
        ci <- try(boot.ci(x, index = i, type = "bca"), silent = TRUE)
        if (class(ci) == "try-error") {
            lwr <- NA; upr <- NA
        } else {
            lwr <- ci$bca[4]; upr <- ci$bca[5]

        }

        data.frame(
            Parameter = name_cols[i],
            lwr = lwr,
            upr = upr
        )
    }) |>
        bind_rows()
}


##
## merge t0 and CI
##
merge_t0_ci <- function(t0, ci) {
    merge(t0, ci, by = "Parameter")
}


res_boot_plot <- lapply(res_boot, function(x) {
    x |>
        mutate(t0 = map(boot, ~ get_t0(.$t0, label_params)),
               ci = map(boot, ~ get_ci_boot(., label_params)),
               param = map2(t0, ci, merge_t0_ci),
               t = map(boot, ~ get_t(.$t, label_params))) |>
        dplyr::select(Dose, param, t)
})


res_boot_param <- lapply(res_boot_plot, function(x) {
    x |>
        dplyr::select(-t) |>
        unnest(cols = param)
})


## make table
tbl_boot_param <- lapply(res_boot_param, function(x) {
    x |>
        pivot_wider(names_from = Parameter,
                    values_from = c(t0, lwr, upr)) |>
        dplyr::select(Dose,
                      ends_with("_Asym"),
                      ends_with("_b2"),
                      ends_with("_b3"))
}) |>
    bind_rows(.id = "reaction") |>
    mutate(reaction = factor(reaction, labels = label_reaction))



########################################
## save data
########################################
save(res_boot_plot, file = "rda/model2_boot_params_gompertz.RData")


#################
## save parameters as CSV
## merge Toxic an Lethal
#################
write_csv(tbl_boot_param, file = "csv/model2_boot_params_gompertz.csv")



#################
## Table: Asym / b2 / b3 (95% CI)
## save Table as LaTeX
#################
## override
tbl_boot_param <- lapply(res_boot_param, function(x) {
    x |>
        mutate(
            ## b2: 2-digits / others: 3-digits
            ## NA: "--"
            str_t0 = ifelse(Parameter == "b2",
                            ifelse(is.na(t0), "--", sprintf("%.2f", t0)),
                            ifelse(is.na(t0), "--", sprintf("%.3f", t0))
            ),
            str_lwr = ifelse(Parameter == "b2",
                             ifelse(is.na(lwr), "--", sprintf("%.2f", lwr)),
                             ifelse(is.na(lwr), "--", sprintf("%.3f", lwr))
            ),
            str_upr = ifelse(Parameter == "b2",
                             ifelse(is.na(upr), "--", sprintf("%.2f", upr)),
                             ifelse(is.na(upr), "--", sprintf("%.3f", upr))
            ),
            param = sprintf("%s [%s, %s]", str_t0, str_lwr, str_upr)
        ) |>
        select(Dose, Parameter, param) |>
        pivot_wider(names_from = Parameter, values_from = param)
}) |>
    bind_rows(.id = "Reaction") |>
    mutate(Reaction = factor(Reaction, labels = label_reaction))


n_rows_cumsum <- tbl_boot_param |>
    group_by(Reaction) |>
    count() |>
    pull(n) |>
    cumsum()

## set row labels
n_dose <- 5
n_row <- nrow(tbl_boot_param)
bool_reaction <- seq_len(n_row) %% n_dose == 1
tbl_boot_param$Reaction[!bool_reaction] <- NA

## set col labels
colnames(tbl_boot_param) <- c(
    "Reaction",
    "Dose (mg/kg)",
    "Asym [95\\% CI]",
    "$b_{2}$ [95\\% CI]",
    "$b_{3}$ [95\\% CI]"
)

print(file = "Table/Table4.tex",
    xtable(tbl_boot_param,
           digits = c(0, 0, 1, 0, 0, 0)),
    include.rownames = FALSE,
    only.contents = TRUE,
    hline.after = c(0, n_rows_cumsum),
    booktabs = TRUE,
    sanitize.text.function = identity,
    timestamp = NULL
)

