#!/usr/bin/env Rscript

library(dplyr)
library(tidyr)
library(purrr)
library(caret)
library(parallel)


########################################
## load analyzed data
########################################

load("rda/model2_fit_model.RData")
source("common_scripts.R", local = TRUE)



########################################
## split data (k-folds)
##
## args:
##    x: original data
##    k: number for k-folds
##
## output: tibble
##    fold
##    data_train
##    data_test
########################################
split_data <- function(x, k = 10) {
    seq_k <- seq_len(k)
    idx <- createFolds(seq_len(nrow(x)), k = k, list = FALSE)

    data_train <- lapply(seq_k, function(i) {
        subset(x, idx != i)
    })

    data_test <- lapply(seq_k, function(i) {
        subset(x, idx == i)
    })

    tibble(
        fold = seq_k,
        data_train = data_train,
        data_test = data_test
    )
}


RMSE <- function(model, test) {
    if (class(model) == "try-error") {
        NA_real_
    } else {
        train <- predict(model, newdata = test)
        sqrt(mean((test$rate - train)^2))
    }
}


########################################
## output:
##    Dose
##    fold
##    data_train <tibble>
##    data_test  <tibble>
##    rate_train <tibble>
##    rate_test  <tibble>
########################################

k <- 3
seed <- 12

dat_split_cv <- mclapply(dat_fit, function(x) {
    set.seed(seed)
    x |>
        mutate(split_data = map(data, ~ split_data(., k = k))) |>
        dplyr::select(Dose, split_data) |>
        unnest(cols = split_data) |>
        ## cumsum rate
        mutate(rate_train = map(data_train, ~
                                get_cumsum_rate(., max_time = max_time)),
               rate_test = map(data_test, ~
                               get_cumsum_rate(., max_time = max_time)))
}, mc.cores = n_mc)



########################################
## analysis
########################################
res_fit_models <- mclapply(1:2, function(i) {
    dat_split_cv[[i]] |>
        transmute(
            Dose = Dose,
            fold = fold,
            rate_train = rate_train,
            rate_test = rate_test,
            ## Logistic curve
            model_logis = map(rate_train, ~
                              safe_nls_try(
                                 rate ~ SSlogis(Time, Asym, xmid, scal),
                                 data = .)),
            ## Gompertz curve
            model_gompertz = map(rate_train, ~
                               safe_nls_try(
                                  rate ~ SSgompertz(Time, Asym, b2, b3),
                                  data = .)),
            Time = map(rate_test, ~ .$Time)
        )
}, mc.cores = n_mc)


## RMSE
res_RMSE <- mclapply(1:2, function(i) {
    res_fit_models[[i]] |>
        transmute(
            Dose = Dose,
            fold = fold,
            RMSE_logis = map2_dbl(model_logis, rate_test, RMSE),
            RMSE_gompertz = map2_dbl(model_gompertz, rate_test, RMSE)
        )
}, mc.cores = n_mc)

summary_RMSE <- lapply(res_RMSE, function(x) {
    x |>
        summarize(
            .by = Dose,
            N = n(),
            Mean_logis = mean(RMSE_logis),
            SD_logis = sd(RMSE_logis),
            Mean_gompertz = mean(RMSE_gompertz),
            SD_gompertz = sd(RMSE_gompertz),
        )
})



########################################
## save analyzed data
########################################

save(k, dat_split_cv,
     res_fit_models, res_RMSE, summary_RMSE,
     file = "rda/model2_cv_RMSE.RData")

